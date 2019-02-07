package myCore

import chisel3._

trait BHTParams {
  val nEntries = 128
  val cntLen = 2
  val historyLength = 4
  val historyBits = 2
}

class BHT extends Module with BHTParams {
  val io = IO(new Bundle {
//    val

  })

  val table = Mem(nEntries, UInt(cntLen.W))
}
