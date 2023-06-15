package rerocc

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.{CustomCSR}


object ReRoCCCSRs {
  val MAX_CFGS = 16

  val reroccopc0 = (0x800, log2Ceil(MAX_CFGS))
  val reroccopc1 = (0x801, log2Ceil(MAX_CFGS))
  val reroccopc2 = (0x802, log2Ceil(MAX_CFGS))
  val reroccopc3 = (0x803, log2Ceil(MAX_CFGS))
  val reroccbar = (0x804, log2Ceil(MAX_CFGS))

  val rerocccfg0 = (0x810, 9)
  val rerocccfg1 = (0x811, 9)
  val rerocccfg2 = (0x812, 9)
  val rerocccfg3 = (0x813, 9)
  val rerocccfg4 = (0x814, 9)
  val rerocccfg5 = (0x815, 9)
  val rerocccfg6 = (0x816, 9)
  val rerocccfg7 = (0x817, 9)
  val rerocccfg8 = (0x818, 9)
  val rerocccfg9 = (0x819, 9)
  val rerocccfg10 = (0x81a, 9)
  val rerocccfg11 = (0x81b, 9)
  val rerocccfg12 = (0x81c, 9)
  val rerocccfg13 = (0x81d, 9)
  val rerocccfg14 = (0x81e, 9)
  val rerocccfg15 = (0x81f, 9)

  def customCSRs(nCfgs: Int) = (Seq(
    reroccopc0, reroccopc1, reroccopc2, reroccopc3, reroccbar,
  ) ++ Seq(
    rerocccfg0, rerocccfg1, rerocccfg2, rerocccfg3,
    rerocccfg4, rerocccfg5, rerocccfg6, rerocccfg7,
    rerocccfg8, rerocccfg9, rerocccfg10, rerocccfg11,
    rerocccfg12, rerocccfg13, rerocccfg14, rerocccfg15).take(nCfgs)
  ).map { case (csr, sz) => CustomCSR(csr, (BigInt(1) << sz) - 1, Some(0)) }
}

class ReRoCCCfg extends Bundle {
  val set = Bool()
  val mgr = UInt(8.W)
}
