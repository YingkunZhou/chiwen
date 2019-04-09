package bian
import chisel3.iotesters.PeekPokeTester
class FrontInput {
  var xcpt_val = false
  var kill_val = false
  var xcpt_addr = 0
  var kill_addr = 0
  var inst_split = false
  var pc_split = false
  var in_val = Seq(true, true)
  var in_inst_bit = Seq(0, 0)

  var pred_tgt = 0
  var pred_direct  = false
  var pred_brchjr  = Seq(false, false)
  var pred_rectify = Seq(false, false)
  var pred_branch  = false
  var pred_is_jal  = false
  val pred_split   = false

  var inst_ready =  Seq(false, false)
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
      poke(c.io.in.inst(i).valid, in.in_val(i))
      poke(c.io.in.inst(i).bits,  in.in_inst_bit(i))
      poke(c.io.in.pred.brchjr(i),in.pred_brchjr(i))
      poke(c.io.in.pred.rectify(i), in.pred_rectify(i))
      poke(c.io.inst(i).ready, in.inst_ready(i))
    }
    poke(c.io.in.pred.tgt, in.pred_tgt)
    poke(c.io.in.pred.redirect, in.pred_direct)
    poke(c.io.in.pred.branch, in.pred_branch)
    poke(c.io.in.pred.is_jal, in.pred_is_jal)
    poke(c.io.in.pred.split,  in.pred_split)
    step(1)
  }
  val in = new FrontInput
  in.in_val = Seq(true,false)
  in.in_inst_bit = Seq(1,2)
input(in)//1
  in.in_inst_bit = Seq(3,4)
  in.in_val = Seq(true,true)
  in.inst_ready = Seq(true,true)
  in.pc_split = true
  in.pred_direct = true
  in.pred_tgt = 4
input(in)//2
  in.in_inst_bit = Seq(5,6)
  in.in_val = Seq(true,true)
  in.inst_ready = Seq(false,false)
  in.pc_split = false
  in.pred_direct = false
  in.pred_tgt = 8
input(in)//3
  in.in_inst_bit = Seq(7,8)
  in.pred_tgt = 0
  in.pred_direct = true
input(in)//4
  in.in_val = Seq(false,false)
  in.pred_tgt = 4
  in.pred_direct = false
input(in)//5
  in.inst_ready = Seq(true,false)
input(in)//6
  in.inst_ready = Seq(true,true)
input(in)//7
  in.inst_ready = Seq(true,true)
input(in)//8
  in.inst_ready = Seq(true,true)
input(in)//9
  in.inst_ready = Seq(true,true)
input(in)//10
}
