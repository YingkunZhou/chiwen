package myCore

import chisel3._
import chisel3.iotesters.PeekPokeTester

//  val redirect = Input(Bool())
//  val pc       = Input(UInt(conf.xprlen.W))
//  val target   = Input(UInt(conf.xprlen.W))
//  val cfiType  = Input(UInt(CFIType.SZ.W))
//  val sel      = Flipped(new ValidIO(UInt(wEntries.W)))

class PredTest(c: Predictor) extends PeekPokeTester(c) {

  def myPoke(pc_1: UInt, ras: UInt, redic: Bool, pc_3: UInt, tg: UInt, cfi: UInt, selValid: Bool, sel: UInt) = {
    poke(c.io.pc, pc_1)
    poke(c.io.peekRAS, ras)
    poke(c.io.feedBack.redirect, redic)
    poke(c.io.feedBack.pc, pc_3)
    poke(c.io.feedBack.target, tg)
    poke(c.io.feedBack.cfiType, cfi)
    poke(c.io.feedBack.sel.valid, selValid)
    poke(c.io.feedBack.sel.bits, sel)
  }
  myPoke(pc_1 = "h80000004".U(32.W), ras = 0.U,
    redic = true.B, pc_3 = "h80001b44".U(32.W), tg = "h80001b44".U(32.W), cfi = CFIType.jump.U, selValid = false.B, sel = 0.U)
  expect(c.io.target.bits, "h80000008".U(32.W))
  expect(c.io.target.cifType, CFIType.invalid.U)
  step(1)
  myPoke(pc_1 = "h80001b44".U(32.W), ras = 0.U,
    redic = true.B, pc_3 = "h80000014".U(32.W), tg = "h80000014".U(32.W), cfi = CFIType.jump.U, selValid = false.B, sel = 0.U)
  expect(c.io.target.bits, "h80001b44".U(32.W))
  expect(c.io.target.cifType, CFIType.jump.U)
  expect(c.io.target.sel, 0.U)
  step(1)
  myPoke(pc_1 = "h80001b44".U(32.W), ras = 0.U,
    redic = false.B, pc_3 = "h80000014".U(32.W), tg = "h80000014".U(32.W), cfi = CFIType.jump.U, selValid = false.B, sel = 0.U)
  expect(c.io.target.bits, "h80001b44".U(32.W))
  expect(c.io.target.cifType, CFIType.jump.U)
  expect(c.io.target.sel, 0.U)
  step(1)
  myPoke(pc_1 = "h80000014".U(32.W), ras = 0.U,
    redic = false.B, pc_3 = "h80000014".U(32.W), tg = "h80000014".U(32.W), cfi = CFIType.jump.U, selValid = false.B, sel = 0.U)
  expect(c.io.target.bits, "h80000014".U(32.W))
  expect(c.io.target.cifType, CFIType.jump.U)
  expect(c.io.target.sel, 1.U)
}
