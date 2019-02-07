package myCore

import chisel3._
import chisel3.iotesters.PeekPokeTester

class RASTest(c: RAS) extends PeekPokeTester(c) {
  def push(addr: UInt) = {
    poke(c.io.push.valid, true.B)
    poke(c.io.push.bits, addr)
    step(1)
    poke(c.io.push.valid, false.B)
  }
  def mypeek(addr: UInt, valid: Int) = {
    if (valid == 1) expect(c.io.peek, addr)
    step(1)
  }
  def pop() = {
    poke(c.io.pop, true.B)
    step(1)
    poke(c.io.pop, false.B)
  }

  push(1.U)
  mypeek(1.U, 1)
  pop()
  mypeek(1.U, 0)
}
