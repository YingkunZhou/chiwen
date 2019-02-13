package fuxi

import chisel3._
import chisel3.util.{Valid, ValidIO, log2Ceil}
import common.CPUConfig

class RasIO(val addr_width: Int) extends Bundle {
  val push = Input(new Valid(UInt(addr_width.W)))
  val pop  = Input(Bool())
  val peek = Output(UInt(addr_width.W))
}

class RAS(val nRas: Int)(implicit val conf: CPUConfig) extends Module {
  val io = IO(new RasIO(conf.xprlen))
  val stack = Reg(Vec(nRas, UInt(conf.xprlen.W)))
  val pos   = RegInit(0.U(log2Ceil(nRas).W))
  val count = RegInit(0.U(log2Ceil(nRas+1).W))
  val empty: Bool = count === 0.U
  val nextPos = Wire(UInt(log2Ceil(nRas).W))
  nextPos := pos + 1.U
  io.peek := stack(pos)

  when(io.push.valid) {
    when(io.pop) {
      stack(pos) := io.push.bits
    }.otherwise {
      when (count =/= nRas.U) { count := count + 1.U }
      stack(nextPos) := io.push.bits
      pos := nextPos
    }
  }.elsewhen(io.pop & !empty) {
    count := count - 1.U
    pos := pos - 1.U
  }
}
