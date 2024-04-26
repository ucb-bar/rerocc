package rerocc

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

import boom.v4.common.{BoomTile}

case object ReRoCCNoCKey extends Field[Option[ReRoCCNoCParams]](None)

trait CanHaveReRoCCTiles { this: BaseSubsystem with InstantiatesHierarchicalElements with constellation.soc.CanHaveGlobalNoC =>
  // WARNING: Not multi-clock safe
  val reRoCCClients = totalTiles.values.map { t => t match {
    case r: RocketTile => r.roccs collect { case r: ReRoCCClient => (t, r) }
    case b: BoomTile => b.roccs collect { case r: ReRoCCClient => (t, r) }
    case _ => Nil
  }}.flatten

  val reRoCCManagerIds = (0 until p(ReRoCCTileKey).size)
  val reRoCCManagerIdNexusNode = LazyModule(new BundleBridgeNexus[UInt](
    inputFn = BundleBridgeNexus.orReduction[UInt](false) _,
    outputFn = (prefix: UInt, n: Int) =>  Seq.tabulate(n) { i => {
      dontTouch(prefix | reRoCCManagerIds(i).U(7.W)) // dontTouch to keep constant prop from breaking tile dedup
    }},
    default = Some(() => 0.U(7.W)),
    inputRequiresOutput = true, // guard against this being driven but then ignored in tileHartIdIONodes below
    shouldBeInlined = false // can't inline something whose output we are are dontTouching
  )).node
  val reRoCCManagers = p(ReRoCCTileKey).zipWithIndex.map { case (g,i) =>
    val rerocc_prci_domain = locateTLBusWrapper(SBUS).generateSynchronousDomain.suggestName(s"rerocc_prci_domain_$i")
    val rerocc_tile = rerocc_prci_domain { LazyModule(new ReRoCCManagerTile(g.copy(reroccId = i), p)) }
    println(s"ReRoCC Manager id $i is a ${rerocc_tile.rocc}")
    locateTLBusWrapper(SBUS).coupleFrom(s"port_named_rerocc_$i") {
      (_ :=* TLBuffer() :=* rerocc_tile.tlNode)
    }
    rerocc_tile.reroccManagerIdSinkNode := reRoCCManagerIdNexusNode
    rerocc_tile
  }
  require(!(reRoCCManagers.isEmpty ^ reRoCCClients.isEmpty))

  if (!reRoCCClients.isEmpty) {
    val rerocc_bus_domain = locateTLBusWrapper(SBUS).generateSynchronousDomain
    rerocc_bus_domain {
      val rerocc_bus = p(ReRoCCNoCKey).map { k =>
        if (k.useGlobalNoC) {
          globalNoCDomain { LazyModule(new ReRoCCGlobalNoC(k)) }
        } else {
          LazyModule(new ReRoCCNoC(k))
        }
      }.getOrElse(LazyModule(new ReRoCCXbar()))
      reRoCCClients.foreach { case (t, c) => rerocc_bus.node := t { ReRoCCBuffer() := c.reRoCCNode } }
      reRoCCManagers.foreach { m => m.reRoCCNode := rerocc_bus.node }
    }
  }
}
