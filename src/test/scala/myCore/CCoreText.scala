package myCore

import chisel3._
import chisel3.iotesters.PeekPokeTester

class CCoreText(c: CaheCore) extends PeekPokeTester(c) {
  poke(c.io.cyc, 179.U)
//  for (i <- 0 until c.nLine) {
//    poke(c.io.wdata(i), i.U)
//  }
//  poke(c.io.wstatus, true.B)
//  poke(c.io.wen, true.B)
//  poke(c.io.addr, "h80000000".U(32.W))
//  step(1)
  for (i <- 0 until c.nLine) {
    poke(c.io.wdata(i), (i+16).U)
  }
  poke(c.io.wstatus, true.B)
  poke(c.io.wen, true.B)
  poke(c.io.addr, "h80001a00".U(32.W))
  step(1)
  for (i <- 0 until c.nLine) {
    poke(c.io.wdata(i), (i+32).U)
  }
  poke(c.io.wstatus, true.B)
  poke(c.io.wen, true.B)
  poke(c.io.addr, "h80000040".U(32.W))
  step(1)
  poke(c.io.wen, false.B)
  poke(c.io.addr, "h80001a10".U(32.W))
  step(1)
  poke(c.io.cyc, 233.U)
  step(1)
//  step(1)
//  poke(c.io.wen, false.B)
//  poke(c.io.addr, "h80001a10".U(32.W))
//  step(1)
}
