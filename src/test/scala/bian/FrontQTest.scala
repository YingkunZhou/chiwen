package bian
import chisel3.iotesters.PeekPokeTester
class FrontInput {
  var xcpt_val = false
  var kill_val = false
  var xcpt_addr = 0
  var kill_addr = 0
  var inst_split = false
  val pc_split = false
  var inst_val = Seq(true, true)
  var inst_bit = Seq(0, 0)
  var ready =  Seq(false, false)
  var pred_brchjr = Seq(false, false)
  var pred_tgt = 0
  var pred_direct = false
}

class FrontQTest(c: FrontQueue) extends PeekPokeTester(c) {
    def input(in: FrontInput): Unit = {
    poke(c.io.xcpt.valid, in.xcpt_val)
    poke(c.io.xcpt.bits,  in.xcpt_addr)
    poke(c.io.kill.valid, in.kill_val)
    poke(c.io.kill.bits,  in.kill_addr)
    poke(c.io.in.pred.split, in.inst_split)
    poke(c.io.in.pc_split,   in.pc_split)
    for (i <- 0 until 2) {
      poke(c.io.in.inst(i).valid, in.inst_val(i))
      poke(c.io.in.inst(i).bits,  in.inst_bit(i))
      poke(c.io.in.pred.brchjr(i),in.pred_brchjr(i))
      poke(c.io.inst(i).ready, in.ready(i))
    }
    poke(c.io.in.pred.tgt, in.pred_tgt)
    poke(c.io.in.pred.redirect, in.pred_direct)
    step(1)
  }
  val in = new FrontInput
  in.inst_val = Seq(true,false)
  in.inst_bit = Seq(1,2)
  in.ready = Seq(true,true)
input(in)
  in.inst_bit = Seq(3,4)
  in.inst_val = Seq(true,true)
  in.pred_direct = true
  in.pred_tgt = 0
input(in)
  in.inst_val = Seq(true,true)
  in.inst_bit = Seq(5,6)
  in.ready = Seq(false,true)
  in.pred_direct = false
  in.pred_tgt = 8
input(in)
  in.inst_val = Seq(false,false)
  in.pred_tgt = 0
  in.pred_direct = true
input(in)
  in.pred_direct = false
input(in)
  in.ready = Seq(true,false)
input(in)
  in.ready = Seq(true,true)
input(in)
  in.ready = Seq(true,true)
input(in)
  in.ready = Seq(true,true)
input(in)
  in.ready = Seq(true,true)
input(in)
}
