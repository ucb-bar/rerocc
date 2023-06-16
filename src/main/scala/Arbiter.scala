package rerocc

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{HellaLockingArbiter}

class ReRoCCMsgArbiter(bundle: ReRoCCBundleParams, arbN: Int, isReq: Boolean) extends HellaLockingArbiter(new ReRoCCMsgBundle(bundle), arbN, false) {

  when (io.out.fire) {
    when (!locked) {
      lockIdx := choice
      locked := true.B
    }
    when (ReRoCCMsgFirstLast(io.out, isReq)._2) {
      locked := false.B
    }
  }
}
