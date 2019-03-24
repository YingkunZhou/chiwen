package bian

import chisel3.iotesters.PeekPokeTester

class InstQTest(c: InstQueue) extends PeekPokeTester(c) {
  step(1)
}
