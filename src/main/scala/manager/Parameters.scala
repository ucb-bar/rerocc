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

case class ReRoCCTileParams(
  genRoCC: Option[Parameters => LazyRoCC] = None,
  reroccId: Int = 0,
  rowBits: Int = 64,
  dcacheParams: Option[DCacheParams] = Some(DCacheParams(nSets = 4, nWays = 4)),
  mergeTLNodes: Boolean = true,
  l2TLBEntries: Int = 0,
  l2TLBWays: Int = 4,
  pgLevels: Int = 3
) extends TileParams {
  val core = new EmptyCoreParams(l2TLBEntries, l2TLBWays, pgLevels)
  val icache = None
  val dcache = Some(dcacheParams.getOrElse(DCacheParams()).copy(rowBits=rowBits))
  val btb = None
  val hartId = -1
  val beuAddr = None
  val blockerCtrlAddr = None
  val name = None
  val clockSinkParams = ClockSinkParameters()

  val tileId = -1
  val baseName = s"rerocc_tile"
  val uniqueName = s"rerocc_tile_$reroccId"
}

case object ReRoCCTileKey extends Field[Seq[ReRoCCTileParams]](Nil)
case object ReRoCCIBufEntriesKey extends Field[Int](4)

class EmptyCoreParams(val nL2TLBEntries: Int, val nL2TLBWays: Int, val pgLevels: Int) extends CoreParams {
  // Most fields are unused, or make no sense in the context of a ReRoCC tile
  lazy val xLen: Int                        = 64
  lazy val bootFreqHz: BigInt               = ???
  lazy val useVM: Boolean                   = true
  lazy val useUser: Boolean                 = ???
  lazy val useSupervisor: Boolean           = ???
  lazy val useHypervisor: Boolean           = false
  lazy val useDebug: Boolean                = ???
  lazy val useAtomics: Boolean              = false
  lazy val useAtomicsOnlyForIO: Boolean     = false
  lazy val useCompressed: Boolean           = true
  lazy val useRVE: Boolean                  = ???
  lazy val useSCIE: Boolean                 = false
  lazy val nLocalInterrupts: Int            = ???
  lazy val useNMI: Boolean                  = false
  lazy val nBreakpoints: Int                = 0
  lazy val useBPWatch: Boolean              = ???
  lazy val mcontextWidth: Int               = ???
  lazy val scontextWidth: Int               = ???
  lazy val nPMPs: Int                       = 0
  lazy val nPerfCounters: Int               = 0
  lazy val haveBasicCounters: Boolean       = ???
  lazy val haveCFlush: Boolean              = false;
  lazy val misaWritable: Boolean            = ???
  lazy val nPTECacheEntries: Int            = 0
  lazy val mtvecInit: Option[BigInt]        = None
  lazy val mtvecWritable: Boolean           = false
  lazy val fastLoadWord: Boolean            = ???
  lazy val fastLoadByte: Boolean            = ???
  lazy val branchPredictionModeCSR: Boolean = ???
  lazy val clockGate: Boolean               = ???
  lazy val mvendorid: Int                   = ???
  lazy val mimpid: Int                      = ???
  lazy val useConditionalZero: Boolean      = false
  lazy val mulDiv: Option[MulDivParams]     = None
  lazy val fpu: Option[FPUParams]           = Some(FPUParams())
  lazy val traceHasWdata: Boolean           = false
  lazy val useZba: Boolean                  = false
  lazy val useZbb: Boolean                  = false
  lazy val useZbs: Boolean                  = false

  lazy val decodeWidth: Int                 = 0
  lazy val fetchWidth: Int                  = 0
  lazy val haveFSDirty: Boolean             = ???
  lazy val instBits: Int                    = 16
  lazy val lrscCycles: Int                  = 20
  lazy val pmpGranularity: Int              = 0
  lazy val retireWidth: Int                 = 0
}
