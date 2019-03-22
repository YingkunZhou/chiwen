package bian

import chisel3.iotesters.PeekPokeTester

class BJTest(c: BranchJump) extends PeekPokeTester(c) {
  step(1)
}
