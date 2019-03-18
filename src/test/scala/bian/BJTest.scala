package bian

import chisel3.iotesters.PeekPokeTester

class FrontqueueTest(c: FrontQueue) extends PeekPokeTester(c) {
  step(1)
}
