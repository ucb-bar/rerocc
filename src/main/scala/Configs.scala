package rerocc

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{BuildRoCC}
import freechips.rocketchip.diplomacy.{LazyModule}

class WithReRoCC(clientParams: ReRoCCClientParams = ReRoCCClientParams(), reRoCCManagerParams: ReRoCCTileParams = ReRoCCTileParams()) extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    val rerocc_client = LazyModule(new ReRoCCClient(clientParams)(p))
    rerocc_client
  })
  case ReRoCCTileKey => up(BuildRoCC).map(gen => reRoCCManagerParams.copy(genRoCC=Some(gen)))
})

class WithReRoCCNoC(nocParams: ReRoCCNoCParams) extends Config((site, here, up) => {
  case ReRoCCNoCKey => Some(nocParams)
})
