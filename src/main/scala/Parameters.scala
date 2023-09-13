package rerocc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{CustomCSR}

case class ReRoCCClientParams(
  nCfgs: Int = 16,
  tileId: Int = 0
) {
  require(nCfgs <= 16)
  def customCSRs = ReRoCCCSRs.customCSRs(nCfgs)
}

case class ReRoCCManagerParams(
  managerId: Int,
)

case class ReRoCCClientPortParams(
  clients: Seq[ReRoCCClientParams]
)

case class ReRoCCManagerPortParams(
  managers: Seq[ReRoCCManagerParams]
)

case class ReRoCCEdgeParams(
  mParams: ReRoCCManagerPortParams,
  cParams: ReRoCCClientPortParams
) {
  require(cParams.clients.size >= 1 && mParams.managers.size >= 1)
  val bundle = ReRoCCBundleParams(
    log2Ceil(cParams.clients.map(_.nCfgs).sum),
    if (mParams.managers.size == 1) 1 else log2Ceil(mParams.managers.map(_.managerId).max + 1)
  )
}

case class ReRoCCBundleParams(
  clientIdBits: Int,
  managerIdBits: Int
) {
  def union(x: ReRoCCBundleParams) = ReRoCCBundleParams(
    clientIdBits = clientIdBits.max(x.clientIdBits),
    managerIdBits = managerIdBits.max(x.managerIdBits))
}
