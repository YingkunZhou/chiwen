package myCore
import chisel3.core.{Bits, VecInit}
import chisel3.iotesters.PeekPokeTester

class EncoderTest (c: OneHotEncoder) extends PeekPokeTester(c) {
  for (s <- 0 until c.w) {
    for (i <- 0 until c.w) {
      if (i == s) poke(c.io.in(i), 1)
      else poke(c.io.in(i), 0)
    }
    step(1)
    expect(c.io.out, s)
  }
}
