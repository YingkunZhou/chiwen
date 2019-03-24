package bian

import chisel3.iotesters.PeekPokeTester

class FrontQTest(c: FrontQueue) extends PeekPokeTester(c) {
  step(1)
}
