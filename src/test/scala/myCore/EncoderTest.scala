package myCore
import chisel3.iotesters.PeekPokeTester

class EncoderTest (c: OneHotEncoder) extends PeekPokeTester(c) {
  for (s <- 0 until 16) {
    poke(c.io.in, s)
    step(1)
  }
}
