package bian

import chisel3.iotesters.PeekPokeTester

class IssueQTest(c: IssueQueue) extends PeekPokeTester(c) {
  step(1)
}
