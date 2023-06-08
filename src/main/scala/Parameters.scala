package rerocc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

case class ReRoCCClientParams(
  nTrackers: Int = 16
) {
  require(nTrackers <= 16)
}
