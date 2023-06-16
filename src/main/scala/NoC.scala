package rerocc

import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.SourceInfo
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

import constellation.noc.{NoCParams, NoCTerminalIO}
import constellation.protocol.{ProtocolNoC, ProtocolParams, ProtocolNoCParams}
import constellation.channel.{FlowParams}
import constellation.soc.{CanAttachToGlobalNoC}

import scala.collection.immutable.{ListMap}

case class ReRoCCNoCParams(
  // maps tile IDs to noc node ids
  tileClientMapping: ListMap[Int, Int] = ListMap[Int, Int](),
  // maps client IDs to noc node ids
  managerMapping: ListMap[Int, Int] = ListMap[Int, Int](),
  nocParams: NoCParams = NoCParams(),
  useGlobalNoC: Boolean = false
)

class ReRoCCInterconnectInterface(edgesIn: Seq[ReRoCCEdgeParams], edgesOut: Seq[ReRoCCEdgeParams])(implicit val p: Parameters) extends Bundle {
  val in = MixedVec(edgesIn.map { e => Flipped(new ReRoCCBundle(e.bundle)) })
  val out = MixedVec(edgesOut.map { e => new ReRoCCBundle(e.bundle) })
}

case class ReRoCCNoCProtocolParams(
  edgesIn: Seq[ReRoCCEdgeParams],
  edgesOut: Seq[ReRoCCEdgeParams],
  edgeInNodes: Seq[Int],
  edgeOutNodes: Seq[Int]
) extends ProtocolParams with HasReRoCCBusRemapper {
  val bundles = (edgesIn ++ edgesOut).map(_.bundle)
  val wideBundle = bundles.reduce((l,r) => l.union(r))
  val minPayloadWidth = (new ReRoCCMsgBundle(wideBundle)).getWidth
  val ingressNodes = edgeInNodes ++ edgeOutNodes
  val egressNodes = edgeOutNodes ++ edgeInNodes
  val nVirtualNetworks = 2
  val vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee
  val flows = Seq.tabulate(edgesIn.size, edgesOut.size) { case (i, o) =>
    Seq(FlowParams(i, o, 1), FlowParams(o + edgesIn.size, i + edgesOut.size, 0, fifo=true))
  }.flatten.flatten
  def genIO()(implicit p: Parameters): Data = new ReRoCCInterconnectInterface(edgesIn, edgesOut)
  def interface(terminals: NoCTerminalIO,
    ingressOffset: Int, egressOffset: Int, protocol: Data)(implicit p: Parameters) = {
    val ingresses = terminals.ingress
    val egresses = terminals.egress
    protocol match { case protocol: ReRoCCInterconnectInterface => {
      edgesIn.zipWithIndex.map { case (e, i) =>
        val wide_req = Wire(new ReRoCCMsgBundle(wideBundle))
        wide_req := protocol.in(i).req.bits
        wide_req.client_id := remapInToOut(i, protocol.in(i).req.bits.client_id)
        val (req_first, req_last, _) = ReRoCCMsgFirstLast(protocol.in(i).req, true)
        ingresses(i).flit.valid := protocol.in(i).req.valid
        ingresses(i).flit.bits.payload := wide_req.asUInt
        ingresses(i).flit.bits.head := req_first
        ingresses(i).flit.bits.tail := req_last
        ingresses(i).flit.bits.egress_id := edgesOut.zipWithIndex.map { case (e, ei) => {
          Mux(e.mParams.managers.map(_.managerId.U === wide_req.manager_id).orR, ei.U, 0.U)
        }}.reduce(_|_) +& egressOffset.U
        protocol.in(i).req.ready := ingresses(i).flit.ready

        val wide_resp = Wire(new ReRoCCMsgBundle(wideBundle))
        protocol.in(i).resp.bits := wide_resp
        protocol.in(i).resp.bits.client_id := remapOutToIn(i, wide_resp.client_id)
        protocol.in(i).resp.valid := egresses(edgesOut.size + i).flit.valid
        wide_resp := egresses(edgesOut.size + i).flit.bits.payload.asTypeOf(new ReRoCCMsgBundle(wideBundle))
        egresses(edgesOut.size + i).flit.ready := protocol.in(i).resp.ready
      }
      edgesOut.zipWithIndex.map { case (e, o) =>
        val wide_resp = Wire(new ReRoCCMsgBundle(wideBundle))
        wide_resp := protocol.out(o).resp.bits
        val (resp_first, resp_last, _) = ReRoCCMsgFirstLast(protocol.out(o).resp, false)
        ingresses(edgesIn.size + o).flit.valid := protocol.out(o).resp.valid
        ingresses(edgesIn.size + o).flit.bits.payload := wide_resp.asUInt
        ingresses(edgesIn.size + o).flit.bits.head := resp_first
        ingresses(edgesIn.size + o).flit.bits.tail := resp_last
        ingresses(edgesIn.size + o).flit.bits.egress_id := edgesIn.zipWithIndex.map { case (e, ei) => {
          Mux(isClient(ei, wide_resp.client_id), (ei + edgesOut.size).U, 0.U)
        }}.reduce(_|_) +& egressOffset.U
        protocol.out(o).resp.ready := ingresses(edgesIn.size + o).flit.ready

        val wide_req = Wire(new ReRoCCMsgBundle(wideBundle))
        protocol.out(o).req.bits := wide_req
        protocol.out(o).req.valid := egresses(o).flit.valid
        wide_req := egresses(o).flit.bits.payload.asTypeOf(new ReRoCCMsgBundle(wideBundle))
        egresses(o).flit.ready := protocol.out(o).req.ready
      }
    }}
  }
}

abstract class ReRoCCNoCModuleImp(outer: LazyModule) extends LazyModuleImp(outer) {
  val edgesIn: Seq[ReRoCCEdgeParams]
  val edgesOut: Seq[ReRoCCEdgeParams]
  val tileClientMapping: ListMap[Int, Int]
  val managerMapping: ListMap[Int, Int]
  val nocName: String
  lazy val inTileIds: Seq[Seq[Int]] = edgesIn.map(_.cParams.clients.map(_.tileId))
  lazy val outManagerIds: Seq[Seq[Int]] = edgesOut.map(_.mParams.managers.map(_.managerId))
  lazy val edgeInNodes: Seq[Option[Int]] = inTileIds.map(ids => {
    val s = ids.map(i => tileClientMapping(i))
    require(s.toSet.size <= 1)
    s.headOption
  })
  lazy val edgeOutNodes: Seq[Option[Int]] = outManagerIds.map(ids => {
    val s = ids.map(i => managerMapping(i))
    require(s.toSet.size <= 1)
    s.headOption
  })
  lazy val protocolParams = ReRoCCNoCProtocolParams(
    edgesIn = edgesIn,
    edgesOut = edgesOut,
    edgeInNodes = edgeInNodes.flatten,
    edgeOutNodes = edgeOutNodes.flatten
  )
  def printNodeMappings() {
    println(s"Constellation: ReRoCC $nocName manager mapping:")
    for ((n, i) <- inTileIds zip edgeInNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- Tiles ${n.toSet}")
    }
    println(s"Constellation: ReRoCC $nocName outwards mapping:")
    for ((n, i) <- outManagerIds zip edgeOutNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- Managers: $n")
    }
  }
}

class ReRoCCNoC(params: ReRoCCNoCParams, name: String = "rerocc")(implicit p: Parameters) extends ReRoCCBus {
  lazy val module = new ReRoCCNoCModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val tileClientMapping = params.tileClientMapping
    val managerMapping = params.managerMapping
    val nocName = name
    printNodeMappings()
    val noc = Module(new ProtocolNoC(ProtocolNoCParams(
      params.nocParams.copy(hasCtrl=false, nocName=name),
      Seq(protocolParams)
    )))

    noc.io.protocol(0) match {
      case protocol: ReRoCCInterconnectInterface => {
        (protocol.in zip io_in).foreach { case (l, r) => l <> r }
        (io_out zip protocol.out).foreach { case (l, r) => l <> r }
      }
    }
  }
}

class ReRoCCGlobalNoC(params: ReRoCCNoCParams, name: String = "rerocc")(implicit p: Parameters) extends ReRoCCBus {
  lazy val module = new ReRoCCNoCModuleImp(this) with CanAttachToGlobalNoC {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val tileClientMapping = params.tileClientMapping
    val managerMapping = params.managerMapping
    val nocName = name
    printNodeMappings()

    val io_global = IO(Flipped(protocolParams.genIO()))
    io_global match {
      case protocol: ReRoCCInterconnectInterface => {
        (protocol.in zip io_in).foreach { case (l, r) => l <> r }
        (io_out zip protocol.out).foreach { case (l, r) => l <> r }
      }
    }
  }

}
