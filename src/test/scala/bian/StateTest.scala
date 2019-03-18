package bian

import chisel3._
import chisel3.iotesters.PeekPokeTester

class StateInput extends Pram {
  var ready = Seq(true, true)
  var bidx1H = 0.U
  var logic_valid  = Seq(true, true)
  var logic_rs_val = Seq(Seq(true, true), Seq(true, true))
  var logic_rs_addr= Seq(Seq(0.U, 0.U), Seq(0.U, 0.U))
  var logic_rd_val = Seq(true, true)
  var logic_rd_addr= Seq(0.U, 0.U)
  var logic_brchjr = Seq(false, false)

  var commit_valid   = Seq(false, false, false, false)
  var commit_id      = Seq(0.U, 0.U, 0.U, 0.U)
  var commit_wb_val  = Seq(false, false, false, false)
  var commit_wb_addr = Seq(0.U, 0.U, 0.U, 0.U)

  var xcpt_val = false
  var xcpt_id  = 0.U

  var kill_val  = false
  var kill_id   = 0.U
  var kill_bidx = 0.U
}

class StateTest(c: StateCtrl) extends PeekPokeTester(c){
  def input(in: StateInput): Unit = {
    poke(c.io.bidx1H, in.bidx1H)
    poke(c.io.kill.valid, in.kill_val)
    poke(c.io.kill.id, in.kill_id)
    poke(c.io.kill.bidx, in.kill_bidx)
    poke(c.io.xcpt.valid, in.xcpt_val)
    poke(c.io.xcpt.id, in.xcpt_id)
    for (i <- 0 until 2) {
      poke(c.io.ready(i), in.ready(i))
      poke(c.io.logic(i).valid, in.logic_valid(i))
      for (j <- 0 until 2) {
        poke(c.io.logic(i).rs(j).valid, in.logic_rs_val(i)(j))
        poke(c.io.logic(i).rs(j).addr, in.logic_rs_addr(i)(j))
      }
      //info don't care
      poke(c.io.logic(i).rd.valid, in.logic_rd_val(i))
      poke(c.io.logic(i).rd.addr, in.logic_rd_addr(i))
      poke(c.io.logic(i).brchjr, in.logic_brchjr(i))
    }
    for (i <- 0 until 4) {
      poke(c.io.commit(i).valid, in.commit_valid(i))
      poke(c.io.commit(i).id, in.commit_id(i))
      poke(c.io.commit(i).wb.valid, in.commit_wb_val(i))
      poke(c.io.commit(i).wb.addr, in.commit_wb_addr(i))
    }
    step(1)
  }
  val in = new StateInput
  in.logic_valid  = Seq(true, true)
  in.logic_rs_val = Seq(Seq(false, false), Seq(false, false))
  in.logic_rs_addr= Seq(Seq(1.U, 2.U), Seq(3.U, 4.U))
  in.logic_rd_val = Seq(true, true)
  in.logic_rd_addr= Seq(1.U, 2.U)
  input(in)
  in.logic_valid  = Seq(true, true)
  in.logic_rs_val = Seq(Seq(false, false), Seq(false, false))
  in.logic_rs_addr= Seq(Seq(1.U, 2.U), Seq(3.U, 4.U))
  in.logic_rd_val = Seq(true, true)
  in.logic_rd_addr= Seq(3.U, 4.U)
  input(in)

//  def commit(valid: Seq[Boolean], id: Seq[Int]): Unit = {
//    for (i <- 0 until 4) {
//      poke(c.io.commit(i).valid, valid(i))
//      poke(c.io.commit(i).bits, id(i))
//    }
//  }
//
//  def logic(rd_bits: Seq[Int], rd_valid: Seq[Boolean], rs_bits: Seq[Int], rs_valid: Seq[Boolean], valid: Seq[Boolean]): Unit = {
//    for (i <- 0 until 2) {
//      poke(c.io.logic(i).rd.bits, rd_bits(i))
//      poke(c.io.logic(i).rd.valid, rd_valid(i))
//      for (j <- 0 until 2) {
//        poke(c.io.logic(i).rs(j).bits, rs_bits(i))
//        poke(c.io.logic(i).rs(j).valid, rs_valid(i))
//      }
//      poke(c.io.logic(i).valid, valid(i))
//    }
//  }
//
//  def physic(ready: Seq[Boolean]): Unit = {
//    for (i <- 0 until 2) {
//      poke(c.io.physic(i).ready, ready(i))
//    }
//  }
//
//  def xcpt(valid: Boolean, id: Int): Unit = {
//    poke(c.io.xcpt.valid, valid)
//    poke(c.io.xcpt.bits.id, id)
//  }
//
//  def kill(valid: Boolean, id: Int, bJidx: Int): Unit = {
//    poke(c.io.kill.valid, valid)
//    poke(c.io.kill.id, id)
//    poke(c.io.kill.bJidx, bJidx)
//  }
//
//  xcpt(false, 0)
//  kill(false, 0, 0)
//  physic(ready = Seq(true, true))
//
//  logic(rd_bits = Seq(1,2), rd_valid = Seq(true, true), rs_bits = Seq(1,2), rs_valid = Seq(true, true), valid = Seq(true, true))
//  commit(valid = Seq.fill(4)(false), id = Seq.fill(4)(0))
//  step(1)
//  logic(rd_bits = Seq(1,2), rd_valid = Seq(true, false), rs_bits = Seq(1,2), rs_valid = Seq(true, true), valid = Seq(true, true))
//  commit(valid = Seq(true, true, false, false), id = Seq(0, 1, 0, 0))
//  step(1)
//  physic(ready = Seq(true, false))
//  logic(rd_bits = Seq(1,2), rd_valid = Seq(true, true), rs_bits = Seq(1,2), rs_valid = Seq(true, true), valid = Seq(true, true))
//  commit(valid = Seq(true, true, false, false), id = Seq(1, 2, 0, 0))
//  step(1)
////  for (i <- 0 until 4) {
////  }

}
