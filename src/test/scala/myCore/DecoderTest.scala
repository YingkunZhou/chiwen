package myCore

import chisel3._
import chisel3.iotesters.PeekPokeTester

class DecoderTest(c: OneHotDecoder) extends PeekPokeTester(c) {
  poke(c.io.in, 2.U)
  poke(c.io.en, true.B)
  step(1)
  poke(c.io.in, 2.U)
  expect(c.io.out, 1.U)
}
