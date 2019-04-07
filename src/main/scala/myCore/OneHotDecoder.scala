package myCore

import chisel3._
import chiwen.Pulse

class OneHotDecoder extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
  })
  val kill = RegInit(false.B)
  kill := true.B
  val pulse_kill = Pulse(kill, io.en)
  printf(p"pulse = ${pulse_kill} kill = $kill en = ${io.en}\n")

}