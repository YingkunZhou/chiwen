package bian

import chisel3.iotesters.PeekPokeTester
class BJInput {
  var in_valid = true
  var in_id = 0
  var in_pc = 0
  var in_tgt = 0
  var in_cont = 0
  var in_jump = 0
  var in_br_type = 0
  var in_branch = false
  var in_expect = false

  var issue_val = Seq(false, false, false)
  var issue_id = Seq(0,0,0)
  var issue_actual = Seq(false,false,false)
  var issue_branch = Seq(false,false,false)
  var target = Seq(0,0,0)

  var id_head = 0
  var xcpt = false
}
class BJTest(c: BranchJump) extends PeekPokeTester(c) {
  def input(in: BJInput): Unit = {
    poke(c.io.in.valid, in.in_valid)
    poke(c.io.in.bits.id, in.in_id)
    poke(c.io.in.bits.pc, in.in_pc)
    poke(c.io.in.bits.tgt, in.in_tgt)
    poke(c.io.in.bits.cont, in.in_cont)
    poke(c.io.in.bits.brtype, in.in_br_type)
    poke(c.io.in.bits.branch, in.in_branch)
    poke(c.io.in.bits.redirect, in.in_expect)
    for (i <- 0 until 3) {
      poke(c.io.issue(i).valid, in.issue_val(i))
      poke(c.io.issue(i).id, in.issue_id(i))
      poke(c.io.issue(i).actual, in.issue_actual(i))
      poke(c.io.issue(i).branch, in.issue_branch(i))
      poke(c.io.target(i), in.target(i))
    }
    step(1)
  }
  val in = new BJInput
  in.in_id   = 1
  in.in_pc   = 1
  in.in_tgt  = 1+100
  in.in_cont = 1+1
  in.in_jump = 1
  in.in_branch = true
  in.in_expect = true
  in.in_br_type = BR_EQ.toInt
input(in)
  in.in_id   = 2
  in.in_pc   = 2
  in.in_tgt  = 2+100
  in.in_cont = 2+1
  in.in_jump = 2
  in.in_branch = true
  in.in_expect = true
  in.in_br_type = BR_GE.toInt
input(in)
  in.in_id   = 3
  in.in_pc   = 3
  in.in_tgt  = 3+100
  in.in_cont = 3+1
  in.in_jump = 3
  in.in_branch = true
  in.in_expect = true
  in.in_br_type = BR_GEU.toInt
input(in)
  in.in_valid = false
  in.issue_val = Seq(true,true,true)
  in.issue_id = Seq(3,2,1)
  in.issue_branch = Seq(true,true,true)
  in.issue_actual = Seq(true,false,true)
  in.target = Seq(2+100,0,0)
  input(in)
  in.issue_val = Seq(false,false,false)
  input(in)
  input(in)
}
