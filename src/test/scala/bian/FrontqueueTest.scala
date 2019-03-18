package bian

import chisel3.iotesters.PeekPokeTester

class FrontendTest(c: FrontEnd) extends PeekPokeTester(c) {
  step(1)
}
