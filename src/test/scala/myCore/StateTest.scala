package myCore

import chisel3.iotesters.PeekPokeTester

class StateTest (c: StateCtrl) extends PeekPokeTester(c){
  def commit(valid: Seq[Boolean], id: Seq[Int]): Unit = {
    for (i <- 0 until 4) {
      poke(c.io.commit(i).valid, valid(i))
      poke(c.io.commit(i).bits, id(i))
    }
  }

  def logic(rd_bits: Seq[Int], rd_valid: Seq[Boolean], rs_bits: Seq[Int], rs_valid: Seq[Boolean], valid: Seq[Boolean]): Unit = {
    for (i <- 0 until 2) {
      poke(c.io.logic(i).rd.bits, rd_bits(i))
      poke(c.io.logic(i).rd.valid, rd_valid(i))
      for (j <- 0 until 2) {
        poke(c.io.logic(i).rs(j).bits, rs_bits(i))
        poke(c.io.logic(i).rs(j).valid, rs_valid(i))
      }
      poke(c.io.logic(i).valid, valid(i))
    }
  }

  def physic(ready: Seq[Boolean]): Unit = {
    for (i <- 0 until 2) {
      poke(c.io.physic(i).ready, ready(i))
    }
  }

  def xcpt(valid: Boolean, id: Int): Unit = {
    poke(c.io.xcpt.valid, valid)
    poke(c.io.xcpt.bits.id, id)
  }

  def kill(valid: Boolean, id: Int, bJidx: Int): Unit = {
    poke(c.io.kill.valid, valid)
    poke(c.io.kill.id, id)
    poke(c.io.kill.bJidx, bJidx)
  }

  xcpt(false, 0)
  kill(false, 0, 0)
  physic(ready = Seq(true, true))

  logic(rd_bits = Seq(1,2), rd_valid = Seq(true, true), rs_bits = Seq(1,2), rs_valid = Seq(true, true), valid = Seq(true, true))
  commit(valid = Seq.fill(4)(false), id = Seq.fill(4)(0))
  step(1)
  logic(rd_bits = Seq(1,2), rd_valid = Seq(true, false), rs_bits = Seq(1,2), rs_valid = Seq(true, true), valid = Seq(true, true))
  commit(valid = Seq(true, true, false, false), id = Seq(0, 1, 0, 0))
  step(1)
  physic(ready = Seq(true, false))
  logic(rd_bits = Seq(1,2), rd_valid = Seq(true, true), rs_bits = Seq(1,2), rs_valid = Seq(true, true), valid = Seq(true, true))
  commit(valid = Seq(true, true, false, false), id = Seq(1, 2, 0, 0))
  step(1)
//  for (i <- 0 until 4) {
//  }
}
