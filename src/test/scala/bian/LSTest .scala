package bian

import chisel3.iotesters.PeekPokeTester

class LSTest(c: LoadStore) extends PeekPokeTester(c) {
  step(1)
}
