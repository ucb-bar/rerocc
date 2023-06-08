package rerocc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

class ReRoCCClient(params: ReRoCCClientParams = ReRoCCClientParams())(implicit p: Parameters) extends
  LazyRoCC(OpcodeSet.all, 2, roccCSRs = params.csrs) {

  override lazy val module = new LazyRoCCModuleImp(this) {

  }
}
