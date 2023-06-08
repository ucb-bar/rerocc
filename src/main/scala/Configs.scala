package rerocc

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{BuildRoCC}
import freechips.rocketchip.diplomacy.{LazyModule}

class WithReRoCCClients(clientParams: ReRoCCClientParams = ReRoCCClientParams()) extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    val rerocc_client = LazyModule(new ReRoCCClient(clientParams)(p))
    rerocc_client
  })
})
