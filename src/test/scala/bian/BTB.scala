package bian

import chisel3.iotesters.PeekPokeTester

class PredTest(c: BTB) extends PeekPokeTester(c) {
  step(1)
}
