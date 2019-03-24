package bian

import chisel3.iotesters.PeekPokeTester

class FtendTest(c: FrontEnd) extends PeekPokeTester(c) {
  step(1)
}
