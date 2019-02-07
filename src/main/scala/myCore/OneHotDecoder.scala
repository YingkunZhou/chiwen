package myCore

import chisel3._
import chisel3.util.{PriorityEncoder, log2Ceil}

class OneHotDecoder extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(2.W))
    val en = Input(Bool())
    val out = Output(UInt(4.W))
  })

  val myReg = Reg(Vec(4, UInt(4.W)))
  when (io.en) {
    for (i <- 0 until 4) {
      myReg(i) := 0.U
    }
    myReg(io.in) := 1.U
  }
  io.out := myReg(io.in)
//  printf(p"in = ${io.in}\n")
//  printf(p"out = ${io.out}\n")

}