package rerocc

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.{CustomCSR}


object ReRoCCCSRs {
  val MAX_CFGS = 16

  val rropc0 = (0x800, log2Ceil(MAX_CFGS))
  val rropc1 = (0x801, log2Ceil(MAX_CFGS))
  val rropc2 = (0x802, log2Ceil(MAX_CFGS))
  val rropc3 = (0x803, log2Ceil(MAX_CFGS))
  val rrbar = (0x804, log2Ceil(MAX_CFGS))

  val rrcfg0 = (0x810, 9)
  val rrcfg1 = (0x811, 9)
  val rrcfg2 = (0x812, 9)
  val rrcfg3 = (0x813, 9)
  val rrcfg4 = (0x814, 9)
  val rrcfg5 = (0x815, 9)
  val rrcfg6 = (0x816, 9)
  val rrcfg7 = (0x817, 9)
  val rrcfg8 = (0x818, 9)
  val rrcfg9 = (0x819, 9)
  val rrcfg10 = (0x81a, 9)
  val rrcfg11 = (0x81b, 9)
  val rrcfg12 = (0x81c, 9)
  val rrcfg13 = (0x81d, 9)
  val rrcfg14 = (0x81e, 9)
  val rrcfg15 = (0x81f, 9)

  def customCSRs(nCfgs: Int) = (Seq(
    rropc0, rropc1, rropc2, rropc3, rrbar,
  ) ++ Seq(
    rrcfg0, rrcfg1, rrcfg2, rrcfg3,
    rrcfg4, rrcfg5, rrcfg6, rrcfg7,
    rrcfg8, rrcfg9, rrcfg10, rrcfg11,
    rrcfg12, rrcfg13, rrcfg14, rrcfg15).take(nCfgs)
  ).map { case (csr, sz) => CustomCSR(csr, (BigInt(1) << sz) - 1, Some(0)) }
}

class ReRoCCCfg extends Bundle {
  val acq = Bool()
  val mgr = UInt(8.W)
}
