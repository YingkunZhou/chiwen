package bian

import chisel3.iotesters.PeekPokeTester

class BTBTest(c: BTB) extends PeekPokeTester(c) {
  step(1)
}
