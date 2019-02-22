package myCore

import chisel3._
import chisel3.util.{PriorityEncoderOH, log2Ceil}

class OneHotEncoder(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(w.W))
    val out = Output(Bool())
  })
  val n = 7.U((w-1).W)
  io.out := io.in > n
  printf(p"result = ${io.out}\n")
}
