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
  val send_mstatus = Bool()
}

class InstructionSender(b: ReRoCCBundleParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new ReRoCCInstBundle(b)))
    val rr = Decoupled(new ReRoCCMsgBundle(b))
  })
  val cmd = Queue(io.cmd, 1, flow=false, pipe=true)

  val s_inst :: s_mstatus0 :: s_mstatus1 :: s_rs1 :: s_rs2 :: Nil = Enum(5)
  val state = RegInit(s_inst)

  io.rr.valid := cmd.valid
  io.rr.bits.opcode := ReRoCCProtocolOpcodes.mInst
  io.rr.bits.client_id := cmd.bits.client_id
  io.rr.bits.manager_id := cmd.bits.manager_id
  io.rr.bits.data := MuxLookup(state, 0.U, Seq(
    (s_inst     -> Cat(cmd.bits.send_mstatus, cmd.bits.cmd.inst.asUInt(31,0))),
    (s_mstatus0 -> cmd.bits.cmd.status.asUInt),
    (s_mstatus1 -> (cmd.bits.cmd.status.asUInt >> 64)),
    (s_rs1      -> cmd.bits.cmd.rs1),
    (s_rs2      -> cmd.bits.cmd.rs2)))
  io.rr.bits.last := false.B
  io.rr.bits.first := false.B
  cmd.ready := io.rr.ready && io.rr.bits.last

  val next_state = WireInit(state)

  when (state === s_inst) {
    next_state := Mux(cmd.bits.send_mstatus, s_mstatus0,
      Mux(cmd.bits.cmd.inst.xs1, s_rs1,
        Mux(cmd.bits.cmd.inst.xs1, s_rs2, s_inst)))
    io.rr.bits.last := !cmd.bits.send_mstatus && !cmd.bits.cmd.inst.xs1 && !cmd.bits.cmd.inst.xs2
    io.rr.bits.first := true.B
  } .elsewhen (state === s_mstatus0) {
    next_state := s_mstatus1
  } .elsewhen (state === s_mstatus1) {
    next_state := Mux(cmd.bits.cmd.inst.xs1, s_rs1,
      Mux(cmd.bits.cmd.inst.xs2, s_rs2, s_inst))
    io.rr.bits.last := !cmd.bits.cmd.inst.xs1 && !cmd.bits.cmd.inst.xs2
  } .elsewhen (state === s_rs1) {
    next_state := Mux(cmd.bits.cmd.inst.xs2, s_rs2, s_inst)
    io.rr.bits.last := !cmd.bits.cmd.inst.xs2
  } .elsewhen (state === s_rs2) {
    next_state := s_inst
    io.rr.bits.last := true.B
  }

  when (io.rr.fire()) {
    state := next_state
  }
}


class ReRoCCClient(_params: ReRoCCClientParams = ReRoCCClientParams())(implicit p: Parameters) extends
    LazyRoCC(OpcodeSet.all, 2, roccCSRs = _params.customCSRs) with HasNonDiplomaticTileParameters {
  val params = _params.copy(tileId = staticIdForMetadataUseOnly)
  override def shouldBeInlined = false

  val reRoCCNode = ReRoCCClientNode(params)

  override lazy val module = new LazyRoCCModuleImp(this) {
    val (rerocc, edge) = reRoCCNode.out(0)
    val nCfgs = params.nCfgs

    val cmd_q = Module(new Queue(new RoCCCommand, 2))
    cmd_q.io.enq <> io.cmd
    val inst_sender = Module(new InstructionSender(edge.bundle))

    val csr_opc_io = io.csrs.take(4)
    val csr_bsy_io = io.csrs(4)
    val csr_bar_io = io.csrs(5)
    val csr_cfg_io = io.csrs.drop(6)

    val csr_bsy = RegInit(false.B)
    val csr_bsy_next = WireInit(csr_bsy)
    val csr_opc = Reg(Vec(4, UInt(log2Ceil(nCfgs).W)))
    val csr_opc_next = WireInit(csr_opc)
    val csr_cfg = RegInit(VecInit.fill(nCfgs) { 0.U.asTypeOf(new ReRoCCCfg) })
    val csr_cfg_next = WireInit(csr_cfg)
    val cfg_credits = RegInit(VecInit.fill(nCfgs) { p(ReRoCCIBufEntriesKey).U })
    val cfg_status = Reg(Vec(nCfgs, new MStatus))
    val cfg_credit_enq = Wire(Valid(UInt(log2Ceil(nCfgs).W)))
    val cfg_credit_deq = Wire(Valid(UInt(log2Ceil(nCfgs).W)))


    csr_bsy_io.set := true.B
    csr_bsy_io.sdata := csr_bsy_next
    csr_bsy := csr_bsy_next

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

    val s_idle :: s_acq0 :: s_acq1 :: s_acq2 :: s_acq_ack :: Nil = Enum(5)
    val cfg_acq_state = RegInit(s_idle)
    val cfg_acq = Reg(Bool())
    val cfg_acq_id = Reg(UInt())
    val cfg_acq_mgr_id = Reg(UInt())
    val cfg_acq_status = Reg(new MStatus)
    val cfg_acq_ptbr = Reg(new PTBR)


    when (csr_cfg_io.map(_.wen).orR && !csr_bsy) {
      val sel_oh = csr_cfg_io.map(_.wen)
      val cfg_id = OHToUInt(sel_oh)
      val wdata = Mux1H(sel_oh, csr_cfg_io.map(_.wdata)).asTypeOf(new ReRoCCCfg)
      val old = csr_cfg(cfg_id)
      val valid_mgr = edge.mParams.managers.map(_.managerId.U === wdata.mgr).orR
      when (wdata.set && !old.set && valid_mgr) {
        assert(cfg_acq_state === s_idle)
        cfg_acq_state := s_acq0
        cfg_acq := true.B
        cfg_acq_id := cfg_id
        cfg_acq_mgr_id := wdata.mgr
        cfg_acq_status := io.ptw(0).status
        cfg_acq_ptbr := io.ptw(0).ptbr
        csr_bsy_next := true.B
      } .elsewhen (!wdata.set && old.set) {
        assert(false.B)
      } .elsewhen (wdata.set && old.set) {

      } .otherwise {
        csr_cfg_next(cfg_id).mgr := wdata.mgr
      }
    }

    for (i <- 0 until 4) {
      when (csr_opc_io(i).wen) {
        csr_opc(i) := csr_opc_io(i).wdata
      }
    }

    // 0 -> cfg, 1 -> inst
    val req_arb = Module(new HellaPeekingArbiter(
      new ReRoCCMsgBundle(edge.bundle), 2,
      (b: ReRoCCMsgBundle) => b.last,
      Some((b: ReRoCCMsgBundle) => true.B)))
    rerocc.req <> req_arb.io.out

    def Mux1HSel[T <: Data](sel: UInt, lookup: Seq[(UInt, T)]) = Mux1H(
      lookup.map(_._1 === sel), lookup.map(_._2))


    req_arb.io.in(0).valid := cfg_acq_state.isOneOf(s_acq0, s_acq1, s_acq2)
    req_arb.io.in(0).bits.opcode := ReRoCCProtocolOpcodes.mAcquire
    req_arb.io.in(0).bits.client_id := cfg_acq_id
    req_arb.io.in(0).bits.manager_id := cfg_acq_mgr_id
    req_arb.io.in(0).bits.data := Mux1HSel(cfg_acq_state, Seq(
      s_acq0 -> cfg_acq_status.asUInt,
      s_acq1 -> (cfg_acq_status.asUInt >> 64),
      s_acq2 -> cfg_acq_ptbr.asUInt))
    req_arb.io.in(0).bits.first := cfg_acq_state === s_acq0
    req_arb.io.in(0).bits.last := cfg_acq_state === s_acq2
    when (req_arb.io.in(0).fire) {
      cfg_acq_state := Mux1HSel(cfg_acq_state, Seq(
        s_acq0 -> s_acq1,
        s_acq1 -> s_acq2,
        s_acq2 -> s_acq_ack))
    }

    val cmd_opc = cmd_q.io.deq.bits.inst.opcode(6,5)
    val cmd_cfg_id = csr_opc(cmd_opc)
    val cmd_cfg = csr_cfg(cmd_cfg_id)
    val credit_available = cfg_credits(cmd_cfg_id) =/= 0.U && cmd_cfg.set
    inst_sender.io.cmd.valid := cmd_q.io.deq.valid && credit_available
    inst_sender.io.cmd.bits.cmd := cmd_q.io.deq.bits
    cmd_q.io.deq.ready := inst_sender.io.cmd.ready && credit_available
    inst_sender.io.cmd.bits.send_mstatus := cmd_q.io.deq.bits.status.asUInt =/= cfg_status(cmd_cfg_id).asUInt
    inst_sender.io.cmd.bits.client_id := cmd_cfg_id
    inst_sender.io.cmd.bits.manager_id := cmd_cfg.mgr
    req_arb.io.in(1) <> inst_sender.io.rr
    cfg_credit_deq.valid := inst_sender.io.cmd.fire()
    cfg_credit_deq.bits := cmd_cfg_id

    rerocc.resp.ready := false.B
    when (rerocc.resp.bits.opcode === ReRoCCProtocolOpcodes.sAcqResp) {
      rerocc.resp.ready := true.B
      when (rerocc.resp.valid) {
        assert(cfg_acq_state === s_acq_ack)
        cfg_acq_state := s_idle
        csr_bsy_next := false.B
        csr_cfg_next(cfg_acq_id).set := rerocc.resp.bits.data(0)
        csr_cfg_next(cfg_acq_id).mgr := cfg_acq_mgr_id
        cfg_status(cfg_acq_id) := cfg_acq_status
      }
    }
    when (rerocc.resp.bits.opcode === ReRoCCProtocolOpcodes.sInstAck) {
      rerocc.resp.ready := true.B
    }
    val resp_data = Reg(UInt(64.W))
    when (rerocc.resp.bits.opcode === ReRoCCProtocolOpcodes.sWrite) {
      rerocc.resp.ready := io.resp.ready || rerocc.resp.bits.first
      when (rerocc.resp.bits.first) { resp_data := rerocc.resp.bits.data }
    }
    io.resp.valid := rerocc.resp.valid && rerocc.resp.bits.opcode === ReRoCCProtocolOpcodes.sWrite && rerocc.resp.bits.last
    io.resp.bits.rd := rerocc.resp.bits.data
    io.resp.bits.data := resp_data

    cfg_credit_enq.valid := rerocc.resp.bits.opcode === ReRoCCProtocolOpcodes.sInstAck && rerocc.resp.fire
    cfg_credit_enq.bits := rerocc.resp.bits.client_id

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

    io.busy := cfg_acq_state =/= s_idle || cmd_q.io.deq.valid || cfg_credits.map(_ =/= p(ReRoCCIBufEntriesKey).U).orR

    // val cfg_io = io.csrs
    // cfg_io.foreach(_.set := false.B)

    // val s_idle :: s_req :: s_flight :: Nil = Enum(3)

    // val cfg_wens = cfg_io.map(_.wen)
    // val cfg_values = cfg_io.map(_.value.asTypeOf(new ReRoCCCfg))
    // val cfg_wdata = Mux1H(cfg_wens, cfg_io.map(_.wdata)).asTypeOf(new ReRoCCCfg)
    // val cfg_credits = Reg(Vec(nCfgs, UInt(log2Ceil(p(ReRoCCIBufEntriesKey)+1).W)))
    // val cfg_credit_enq = Wire(Valid(UInt(log2Ceil(nCfgs).W)))
    // cfg_credit_enq.valid := false.B
    // cfg_credit_enq.bits := DontCare
    // val cfg_credit_deq = Wire(Valid(UInt(log2Ceil(nCfgs).W)))
    // val cfg_status = Reg(Vec(nCfgs, new MStatus))
    // assert(PopCount(cfg_wens) <= 1.U)

    // val cfg_old = Reg(new ReRoCCCfg)
    // val cfg_new = Reg(new ReRoCCCfg)
    // val cfg_newidx = Reg(UInt(log2Ceil(nCfgs).W))
    // val cfg_acq_state = RegInit(s_idle)
    // val cfg_newptbr = Reg(new PTBR)
    // val cfg_newstatus = Reg(new MStatus)

    // val cfg_sdata = Wire(Vec(nCfgs, new ReRoCCCfg))
    // for (i <- 0 until nCfgs) {
    //   cfg_sdata(i) := cfg_io(i).value.asTypeOf(new ReRoCCCfg)
    //   cfg_io(i).sdata := cfg_sdata(i).asUInt
    // }

    // dontTouch(cfg_old)
    // dontTouch(cfg_new)

    // io.busy := cfg_acq_state =/= s_idle || cmd_q.io.deq.valid

    // val cfg_opc_oh = VecInit((0 until 4).map { o =>
    //   VecInit(cfg_values.map(v => v.map && v.acq && v.opc === o.U))
    // })

    // when (cfg_wens.orR) {
    //   val valid_mgr = edge.mParams.managers.map(_.managerId.U === cfg_wdata.mgr).orR
    //   cfg_sdata.foreach(_.bsy := valid_mgr)
    //   cfg_io.foreach(_.set := true.B)

    //   when (cfg_acq_state === s_idle && valid_mgr) {
    //     cfg_acq_state := s_req
    //     cfg_old := Mux1H(cfg_wens, cfg_io.map(_.value)).asTypeOf(new ReRoCCCfg)
    //     cfg_new := cfg_wdata
    //     cfg_newptbr := io.ptw(0).ptbr
    //     cfg_newstatus := io.ptw(0).status
    //     cfg_newidx := OHToUInt(cfg_wens)
    //   }
    // }

    // // 0 -> cfg, 1 -> inst
    // val req_arb = Module(new HellaPeekingArbiter(
    //   new ReRoCCMsgBundle(edge.bundle), 2,
    //   (b: ReRoCCMsgBundle) => b.last,
    //   Some((b: ReRoCCMsgBundle) => true.B)))

    // req_arb.io.in.foreach(_.valid := false.B)
    // req_arb.io.in.foreach(_.bits := DontCare)
    // rerocc.req <> req_arb.io.out

    // val cfg_beat = RegInit(0.U(2.W))
    // when (req_arb.io.in(0).fire) { cfg_beat := cfg_beat + 1.U }
    // when (cfg_acq_state === s_req) {
    //   when (cfg_new.acq && !cfg_old.acq) {
    //     req_arb.io.in(0).valid := true.B
    //     req_arb.io.in(0).bits.opcode := ReRoCCProtocolOpcodes.mAcquire
    //     req_arb.io.in(0).bits.client_id := cfg_newidx
    //     req_arb.io.in(0).bits.manager_id := cfg_new.mgr
    //     req_arb.io.in(0).bits.data := VecInit(
    //       cfg_newstatus.asUInt,
    //       cfg_newstatus.asUInt >> 64,
    //       cfg_newptbr.asUInt)(cfg_beat)
    //     req_arb.io.in(0).bits.first := cfg_beat === 0.U
    //     req_arb.io.in(0).bits.last := cfg_beat === 2.U
    //     when (cfg_beat === 2.U && req_arb.io.in(0).ready) {
    //       cfg_beat := 0.U
    //       cfg_acq_state := s_flight
    //     }
    //   } .elsewhen (cfg_new.opc =/= cfg_old.opc || cfg_new.map =/= cfg_old.map) {
    //     for (i <- 0 until nCfgs) {
    //       cfg_io(i).set := true.B
    //       when (i.U =/= cfg_newidx) {
    //         when (cfg_new.map && cfg_values(i).map && cfg_values(i).opc === cfg_new.opc) {
    //           cfg_sdata(i).map := false.B
    //         }
    //       } .otherwise {
    //         cfg_sdata(i).map := cfg_new.map
    //         cfg_sdata(i).opc := cfg_new.opc
    //       }
    //     }
    //   } .otherwise {
    //     assert(false.B)
    //   }
    // }

    // val cmd_opc_oh = UIntToOH(cmd_q.io.deq.bits.inst.opcode(6,5))
    // val cmd_cfg_oh = Mux1H(cmd_opc_oh, cfg_opc_oh)
    // assert(!cmd_q.io.deq.valid || PopCount(cmd_cfg_oh) === 1.U)

    // val credit_available = Mux1H(cmd_cfg_oh, cfg_credits) =/= 0.U
    // inst_sender.io.cmd.valid := cmd_q.io.deq.valid && credit_available
    // inst_sender.io.cmd.bits.cmd := cmd_q.io.deq.bits
    // cmd_q.io.deq.ready := inst_sender.io.cmd.ready && credit_available
    // inst_sender.io.cmd.bits.send_mstatus := cmd_q.io.deq.bits.status.asUInt =/= Mux1H(cmd_cfg_oh, cfg_status.map(_.asUInt))
    // inst_sender.io.cmd.bits.client_id := OHToUInt(cmd_cfg_oh)
    // inst_sender.io.cmd.bits.manager_id := Mux1H(cmd_cfg_oh, cfg_values.map(_.mgr))
    // req_arb.io.in(1) <> inst_sender.io.rr
    // cfg_credit_deq.valid := inst_sender.io.cmd.fire()
    // cfg_credit_deq.bits := OHToUInt(cmd_cfg_oh)

    // rerocc.resp.ready := false.B
    // when (rerocc.resp.bits.opcode === ReRoCCProtocolOpcodes.sAcqResp) {
    //   rerocc.resp.ready := true.B
    //   when (rerocc.resp.valid) {
    //     assert(cfg_acq_state === s_flight)
    //     cfg_acq_state := s_idle
    //     for (i <- 0 until nCfgs) {
    //       cfg_sdata(i).bsy := false.B
    //       when (i.U === cfg_newidx) {
    //         cfg_sdata(i).mgr := cfg_new.mgr
    //         cfg_sdata(i).opc := cfg_new.opc
    //         cfg_sdata(i).acq := rerocc.resp.bits.data(0)
    //         cfg_sdata(i).map := false.B
    //         cfg_credits(i) := p(ReRoCCIBufEntriesKey).U
    //         cfg_status(i) := cfg_newstatus
    //       }
    //       cfg.set := true.B
    //       cfg.sdata := sdata.asUInt
    //     }
    //   }
    // }

    // when (cfg_credit_enq.valid) {
    //   assert(cfg_credits(cfg_credit_enq.bits) =/= p(ReRoCCIBufEntriesKey).U)
    //   cfg_credits(cfg_credit_enq.bits) := cfg_credits(cfg_credit_enq.bits) + 1.U
    // }
    // when (cfg_credit_deq.valid) {
    //   assert(cfg_credits(cfg_credit_deq.bits) =/= 0.U)
    //   cfg_credits(cfg_credit_deq.bits) := (cfg_credits(cfg_credit_deq.bits) -
    //     Mux(cfg_credit_deq.bits === cfg_credit_enq.bits && cfg_credit_enq.valid, 0.U, 1.U)
    //   )
    // }
  }
}
