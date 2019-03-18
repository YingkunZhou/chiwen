package bian

import chisel3.iotesters.PeekPokeTester

class FrontendTest(c: BTB) extends PeekPokeTester(c) {
  step(1)
}
