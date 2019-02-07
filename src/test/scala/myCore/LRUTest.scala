package myCore

import chisel3._
import chisel3.iotesters.PeekPokeTester

class LRUTest(c: LRU) extends PeekPokeTester(c) {
  def test(x: Int) = {
    poke(c.io.newest.bits, x.U)
    poke(c.io.newest.valid, x.U)
    step(1)
  }
  test(0)
  test(1)
  test(2)
  test(3)
  expect(c.io.oldest, 0.U)
  test(0)
  test(1)
  expect(c.io.oldest, 2.U)
  test(0)
  test(2)
  expect(c.io.oldest, 3.U)
  test(3)
  expect(c.io.oldest, 1.U)

}
