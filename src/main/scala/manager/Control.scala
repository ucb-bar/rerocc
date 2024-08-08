package rerocc.manager

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.regmapper._

case object ReRoCCManagerControlBase extends Field[BigInt](0x20000)
case object ReRoCCManagerControlSize extends Field[BigInt](0x1000)

// Adjusts the manager ctrl address to be unique globally
class ReRoCCManagerControlRemapper(mgrId: Int)(implicit p: Parameters) extends LazyModule()(p) {
  val node = new TLAdapterNode(
    clientFn = { cp => cp },
    managerFn = { mp => {
      require(mp.managers.size == 1)
      val address = AddressSet(p(ReRoCCManagerControlBase) + mgrId * p(ReRoCCManagerControlSize), p(ReRoCCManagerControlSize)-1)
      mp.v1copy(managers = mp.managers.map(_.v1copy(address = Seq(address))))
    }}
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }
  }
}

class ReRoCCManagerControl(mgrId: Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule()(p) {
  val device: SimpleDevice = new SimpleDevice("rerocc-mgr", Seq("ucb-bar,rerocc-mgr"))

  val baseRegion = AddressSet(0, p(ReRoCCManagerControlSize)-1)
  // Integration will remap addresses for us
  val ctrlNode = TLRegisterNode(
    address = Seq(baseRegion),
    device = device,
    concurrency = 1,
    beatBytes = beatBytes)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val mgr_busy = Input(Bool())
      val rocc_busy = Input(Bool())
    })

    val regmap = ctrlNode.regmap(
      0x000 -> Seq(RegField.r(8, io.mgr_busy, RegFieldDesc("mgr_busy", s"ReRoCC $mgrId Manager busy"))),
      0x008 -> Seq(RegField.r(8, io.rocc_busy, RegFieldDesc("rocc_busy", s"ReRoCC $mgrId RoCC busy")))
    )
  }

}
