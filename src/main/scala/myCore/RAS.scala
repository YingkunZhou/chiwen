package myCore

import chisel3._
import chisel3.util.{ValidIO, log2Ceil}
import common.CPUConfig


class RAS(val nRas: Int)(implicit val conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val push = Flipped(new ValidIO(UInt(conf.xprlen.W)))
    val peek = Output(UInt(conf.xprlen.W))
    val pop = Input(Bool())
  })

  val stack = Reg(Vec(nRas, UInt(conf.xprlen.W)))
  val pos   = RegInit(0.U(log2Ceil(nRas).W))
  val count = RegInit(0.U(log2Ceil(nRas+1).W))
  val empty: Bool = count === 0.U
  val nextPos = Wire(UInt(log2Ceil(nRas).W))
  nextPos := pos + 1.U
//  printf(p"count = $count\n")
  io.peek := stack(pos)

  when(io.push.valid) {
    when(io.pop) {
//      printf("push and pop ")
      stack(pos) := io.push.bits
    }.otherwise {
      when (count =/= nRas.U) { count := count + 1.U }
      stack(nextPos) := io.push.bits
      pos := nextPos
//      printf("only push ")
    }
//    printf(p"push addr = ${io.push.bits}, count = $count\n")
  }.elsewhen(io.pop & !empty) {
//    printf("only pop\n")
    count := count - 1.U
    pos := pos - 1.U
  }
}
