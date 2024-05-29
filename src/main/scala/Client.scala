package rerocc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

class ReRoCCInstBundle(b: ReRoCCBundleParams)(implicit p: Parameters) extends Bundle {
  val cmd = new RoCCCommand
  val client_id = UInt(b.clientIdBits.W)
  val manager_id = UInt(b.managerIdBits.W)
}

class InstructionSender(b: ReRoCCBundleParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new ReRoCCInstBundle(b)))
    val rr = Decoupled(new ReRoCCMsgBundle(b))
    val busy = Output(Bool())
  })
  val cmd = Queue(io.cmd, 1, flow=false, pipe=true)

  val s_inst :: s_rs1 :: s_rs2 :: Nil = Enum(3)
  val state = RegInit(s_inst)

  io.rr.valid := cmd.valid
  io.rr.bits.opcode := ReRoCCProtocol.mInst
  io.rr.bits.client_id := cmd.bits.client_id
  io.rr.bits.manager_id := cmd.bits.manager_id
  io.rr.bits.data := MuxLookup(state, 0.U)(Seq(
    (s_inst     -> cmd.bits.cmd.inst.asUInt(31,0)),
    (s_rs1      -> cmd.bits.cmd.rs1),
    (s_rs2      -> cmd.bits.cmd.rs2)))
  cmd.ready := io.rr.ready && state === s_rs2

  val next_state = WireInit(state)

  when (state === s_inst) {
    next_state := Mux(cmd.bits.cmd.inst.xs1, s_rs1,
      Mux(cmd.bits.cmd.inst.xs1, s_rs2, s_inst))
  } .elsewhen (state === s_rs1) {
    next_state := Mux(cmd.bits.cmd.inst.xs2, s_rs2, s_inst)
  } .elsewhen (state === s_rs2) {
    next_state := s_inst
  }

  when (io.rr.fire) {
    state := next_state
  }

  io.busy := state =/= s_inst || cmd.valid
}


class ReRoCCClient(_params: ReRoCCClientParams = ReRoCCClientParams())(implicit p: Parameters) extends
    LazyRoCC(OpcodeSet.all, 2, roccCSRs = _params.customCSRs) with HasNonDiplomaticTileParameters {
  val params = _params.copy(tileId = tileId)
  override def shouldBeInlined = false

  val reRoCCNode = ReRoCCClientNode(params)

  override lazy val module = new LazyRoCCModuleImp(this) {
    val (rerocc, edge) = reRoCCNode.out(0)
    val (resp_first, resp_last, resp_beat) = ReRoCCMsgFirstLast(rerocc.resp, false)
    val nCfgs = params.nCfgs

    val inst_sender = Module(new InstructionSender(edge.bundle))

    val csr_opc_io = io.csrs.take(4)
    val csr_bar_io = io.csrs(4)
    val csr_cfg_io = io.csrs.drop(5)

    val csr_opc = Reg(Vec(4, UInt(log2Ceil(nCfgs).W)))
    val csr_opc_next = WireInit(csr_opc)
    val csr_cfg = RegInit(VecInit.fill(nCfgs) { 0.U.asTypeOf(new ReRoCCCfg) })
    val csr_cfg_next = WireInit(csr_cfg)
    val cfg_credits = RegInit(VecInit.fill(nCfgs) { p(ReRoCCIBufEntriesKey).U })
    val cfg_updatestatus = Reg(Vec(nCfgs, Bool()))
    val cfg_updateptbr = Reg(Vec(nCfgs, Bool()))
    val cfg_credit_enq = Wire(Valid(UInt(log2Ceil(nCfgs).W)))
    val cfg_credit_deq = Wire(Valid(UInt(log2Ceil(nCfgs).W)))

    for (i <- 0 until 4) {
      csr_opc_io(i).set := true.B
      csr_opc_io(i).sdata := csr_opc_next(i)
    }
    csr_opc := csr_opc_next

    for (i <- 0 until nCfgs) {
      csr_cfg_io(i).set := true.B
      csr_cfg_io(i).sdata := csr_cfg_next(i).asUInt
    }
    csr_cfg := csr_cfg_next

    val s_idle :: s_acq :: s_acq_ack :: s_rel :: s_rel_ack :: s_status0 :: s_status1 :: s_ptbr :: Nil = Enum(8)
    val cfg_acq_state = RegInit(s_idle)
    val cfg_acq_id = Reg(UInt())
    val cfg_acq_mgr_id = Reg(UInt())

    for (i <- 0 until nCfgs) { csr_cfg_io(i).stall := cfg_acq_state =/= s_idle }

    when (csr_cfg_io.map(_.wen).orR && cfg_acq_state === s_idle) {
      val sel_oh = csr_cfg_io.map(_.wen)
      val cfg_id = OHToUInt(sel_oh)
      val wdata = Mux1H(sel_oh, csr_cfg_io.map(_.wdata)).asTypeOf(new ReRoCCCfg)
      val old = csr_cfg(cfg_id)
      val valid_mgr = edge.mParams.managers.map(_.managerId.U === wdata.mgr).orR
      when (wdata.acq && !old.acq && valid_mgr && cfg_acq_state === s_idle) {
        cfg_acq_state := s_acq
        cfg_acq_id := cfg_id
        cfg_acq_mgr_id := wdata.mgr
      } .elsewhen (!wdata.acq && old.acq && cfg_acq_state === s_idle) {
        cfg_acq_state := s_rel
        cfg_acq_id := cfg_id
        cfg_acq_mgr_id := old.mgr
        csr_cfg_next(cfg_id) := wdata
      } .elsewhen (wdata.acq && old.acq) {

      } .elsewhen (!wdata.acq && !old.acq) {
        csr_cfg_next(cfg_id).mgr := wdata.mgr
      }
    } .elsewhen(cfg_acq_state === s_idle) {
      for (i <- 0 until nCfgs) {
        when (cfg_updatestatus(i) && csr_cfg(i).acq) {
          cfg_acq_state := s_status0
          cfg_acq_id := i.U
          cfg_acq_mgr_id := csr_cfg(i).mgr
        }
        when (cfg_updateptbr(i) && csr_cfg(i).acq) {
          cfg_acq_state := s_ptbr
          cfg_acq_id := i.U
          cfg_acq_mgr_id := csr_cfg(i).mgr
        }
      }
    }

    for (i <- 0 until 4) {
      when (csr_opc_io(i).wen) {
        csr_opc(i) := csr_opc_io(i).wdata
      }
    }

    val f_idle :: f_req :: f_ack :: Nil = Enum(3)
    val cfg_fence_state = RegInit(VecInit.fill(nCfgs) { f_idle })
    when (csr_bar_io.wen && cfg_fence_state(csr_bar_io.wdata) === f_idle && csr_cfg(csr_bar_io.wdata).acq) {
      cfg_fence_state(csr_bar_io.wdata) := f_req
    }

    // 0 -> cfg, 1 -> inst, 2 -> unbusy
    val req_arb = Module(new ReRoCCMsgArbiter(edge.bundle, 3, true))
    rerocc.req <> req_arb.io.out

    def Mux1HSel[T <: Data](sel: UInt, lookup: Seq[(UInt, T)]) = Mux1H(
      lookup.map(_._1 === sel), lookup.map(_._2))


    req_arb.io.in(0).valid := cfg_acq_state.isOneOf(s_acq, s_rel, s_status0, s_status1, s_ptbr)
    req_arb.io.in(0).bits.opcode := Mux1HSel(cfg_acq_state, Seq(
      s_acq     -> ReRoCCProtocol.mAcquire,
      s_rel     -> ReRoCCProtocol.mRelease,
      s_status0 -> ReRoCCProtocol.mUStatus,
      s_status1 -> ReRoCCProtocol.mUStatus,
      s_ptbr    -> ReRoCCProtocol.mUPtbr))
    req_arb.io.in(0).bits.client_id := cfg_acq_id
    req_arb.io.in(0).bits.manager_id := cfg_acq_mgr_id
    req_arb.io.in(0).bits.data := Mux1HSel(cfg_acq_state, Seq(
      s_status0 -> io.ptw(0).status.asUInt,
      s_status1 -> (io.ptw(0).status.asUInt >> 64),
      s_ptbr    -> io.ptw(0).ptbr.asUInt))
    when (req_arb.io.in(0).fire) {
      cfg_acq_state := Mux1HSel(cfg_acq_state, Seq(
        s_acq -> s_acq_ack,
        s_rel -> s_rel_ack,
        s_status0 -> s_status1,
        s_status1 -> s_idle,
        s_ptbr -> s_idle
      ))
    }
    when (cfg_acq_state === s_status0 && req_arb.io.in(0).fire) {
      cfg_updatestatus(cfg_acq_id) := false.B
    }
    when (cfg_acq_state === s_ptbr && req_arb.io.in(0).fire) {
      cfg_updateptbr(cfg_acq_id) := false.B
    }

    val cmd_opc = io.cmd.bits.inst.opcode(6,5)
    val cmd_cfg_id = csr_opc(cmd_opc)
    val cmd_cfg = csr_cfg(cmd_cfg_id)
    val credit_available = cfg_credits(cmd_cfg_id) =/= 0.U && cmd_cfg.acq
    val status_ready = !cfg_updatestatus(cmd_cfg_id)
    val ptbr_ready = !cfg_updateptbr(cmd_cfg_id)
    inst_sender.io.cmd.valid := io.cmd.valid && credit_available && status_ready && ptbr_ready && cfg_acq_state === s_idle
    inst_sender.io.cmd.bits.cmd := io.cmd.bits
    io.cmd.ready := inst_sender.io.cmd.ready && credit_available && status_ready && ptbr_ready && cfg_acq_state === s_idle
    inst_sender.io.cmd.bits.client_id := cmd_cfg_id
    inst_sender.io.cmd.bits.manager_id := cmd_cfg.mgr
    req_arb.io.in(1) <> inst_sender.io.rr
    cfg_credit_deq.valid := inst_sender.io.cmd.fire
    cfg_credit_deq.bits := cmd_cfg_id

    val f_req_val = cfg_fence_state.map(_ === f_req)
    val f_req_oh = PriorityEncoderOH(f_req_val)
    req_arb.io.in(2).valid := f_req_val.orR && !inst_sender.io.busy && !io.cmd.valid
    req_arb.io.in(2).bits.opcode := ReRoCCProtocol.mUnbusy
    req_arb.io.in(2).bits.client_id := OHToUInt(f_req_oh)
    req_arb.io.in(2).bits.manager_id := Mux1H(f_req_val, csr_cfg.map(_.mgr))
    req_arb.io.in(2).bits.data := 0.U
    when (req_arb.io.in(2).fire) {
      cfg_fence_state(OHToUInt(f_req_oh)) := f_ack
    }

    rerocc.resp.ready := false.B
    when (rerocc.resp.bits.opcode === ReRoCCProtocol.sAcqResp) {
      rerocc.resp.ready := true.B
      when (rerocc.resp.valid) {
        assert(cfg_acq_state === s_acq_ack)
        cfg_acq_state := s_idle
        csr_cfg_next(cfg_acq_id).acq := rerocc.resp.bits.data(0)
        csr_cfg_next(cfg_acq_id).mgr := cfg_acq_mgr_id
        cfg_updatestatus(cfg_acq_id) := true.B
        cfg_updateptbr(cfg_acq_id) := true.B
      }
    }
    when (rerocc.resp.bits.opcode === ReRoCCProtocol.sInstAck) { rerocc.resp.ready := true.B }
    cfg_credit_enq.valid := rerocc.resp.bits.opcode === ReRoCCProtocol.sInstAck && rerocc.resp.fire
    cfg_credit_enq.bits := rerocc.resp.bits.client_id

    val resp_data = Reg(UInt(64.W))
    when (rerocc.resp.bits.opcode === ReRoCCProtocol.sWrite) {
      rerocc.resp.ready := io.resp.ready || resp_first
      when (resp_first) { resp_data := rerocc.resp.bits.data }
    }
    io.resp.valid := rerocc.resp.valid && rerocc.resp.bits.opcode === ReRoCCProtocol.sWrite && resp_last
    io.resp.bits.rd := rerocc.resp.bits.data
    io.resp.bits.data := resp_data

    when (rerocc.resp.bits.opcode === ReRoCCProtocol.sRelResp) {
      rerocc.resp.ready := true.B
      when (rerocc.resp.valid) {
        assert(cfg_acq_state === s_rel_ack)
        cfg_acq_state := s_idle
      }
    }

    when (rerocc.resp.bits.opcode === ReRoCCProtocol.sUnbusyAck) {
      rerocc.resp.ready := true.B
      when (rerocc.resp.valid) {
        assert(cfg_fence_state(rerocc.resp.bits.client_id) === f_ack)
        cfg_fence_state(rerocc.resp.bits.client_id) := f_idle
      }
    }

    when (cfg_credit_enq.valid) {
      assert(cfg_credits(cfg_credit_enq.bits) =/= p(ReRoCCIBufEntriesKey).U)
      cfg_credits(cfg_credit_enq.bits) := cfg_credits(cfg_credit_enq.bits) + 1.U
    }
    when (cfg_credit_deq.valid) {
      assert(cfg_credits(cfg_credit_deq.bits) =/= 0.U)
      cfg_credits(cfg_credit_deq.bits) := (cfg_credits(cfg_credit_deq.bits) -
        Mux(cfg_credit_deq.bits === cfg_credit_enq.bits && cfg_credit_enq.valid, 0.U, 1.U)
      )
    }

    io.busy := (cfg_acq_state =/= s_idle ||
      cfg_credits.map(_ =/= p(ReRoCCIBufEntriesKey).U).orR ||
      cfg_fence_state.map(_ =/= f_idle).orR
    )

    when (io.ptw(0).ptbr.asUInt =/= RegNext(io.ptw(0).ptbr).asUInt) { cfg_updateptbr.foreach(_ := true.B) }
    when (io.ptw(0).status.asUInt =/= RegNext(io.ptw(0).status).asUInt) { cfg_updatestatus.foreach(_ := true.B) }
  }
}
