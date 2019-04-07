package bian

import chisel3.iotesters.PeekPokeTester
class InstInput {
  var in_valid = true
  var in_id = 0
  var in_rs_val = Seq(false,false)
  var in_rs_addr = Seq(1,1)
  var in_rd_val = true
  var in_rd_addr = 1
  var in_f1 = true
  var in_mem_en = false

  var issue_ready = true
  var issueable_id = 0
  var issueable_var = false
  var head_id = 0

  var bypass_val = Seq(false,false,false,false)
  var forwrd_val = Seq(false,false,false)
  var bypass_addr = Seq(0,0,0,0)
  var forwrd_addr = Seq(0,0,0)

  var xcpt = false
  var kill_val = false
  var kill_id = 0
}
class InstQTest(c: InstQueue) extends PeekPokeTester(c) {
  def input(in: InstInput): Unit = {
    poke(c.io.in.valid, in.in_valid)
    for (i <- 0 until 2) {
      poke(c.io.in.bits.rs(i).valid, in.in_rs_val(i))
      poke(c.io.in.bits.rs(i).addr, in.in_rs_addr(i))
    }
    poke(c.io.in.bits.id, in.in_id)
    poke(c.io.in.bits.info.rd.valid, in.in_rd_val)
    poke(c.io.in.bits.info.rd.addr, in.in_rd_addr)
    poke(c.io.in.bits.info.f1, in.in_f1)
    poke(c.io.in.bits.mem_en, in.in_mem_en)

    poke(c.io.issue.ready, in.issue_ready)
    poke(c.io.issueable.valid, in.issueable_var)
    poke(c.io.issueable.bits, in.issueable_id)
    poke(c.io.head, in.head_id)
    poke(c.io.xcpt, in.xcpt)
    poke(c.io.kill.valid, in.kill_val)
    poke(c.io.kill.bits, in.kill_id)
    for (i <- 0 until 4) {
      poke(c.io.bypass(i).valid, in.bypass_val(i))
      poke(c.io.bypass(i).addr, in.bypass_addr(i))
    }
    for (i <- 0 until 3) {
      poke(c.io.speed(i).valid, in.bypass_val(i))
      poke(c.io.speed(i).addr, in.bypass_addr(i))
    }
    step(1)
  }
  val in = new InstInput
  in.issue_ready = false
input(in)
  in.in_id = 1
  in.in_rs_addr = Seq(1,2)
input(in)
  in.in_id = 2
  in.in_mem_en = true
  in.issueable_id = 2
  in.issueable_var = true
  in.in_rs_addr = Seq(2,3)
input(in)
  in.in_id = 3
  in.in_mem_en = false
  in.in_rs_addr = Seq(2,4)
input(in)
  in.in_id = 5
  in.in_rs_addr = Seq(3,4)
input(in)
  in.in_id = 6
  in.in_rs_addr = Seq(5,6)
input(in)
  in.in_id = 7
  in.in_rs_addr = Seq(7,8)
input(in)
  in.in_valid = false
  in.issue_ready = true
  in.bypass_val = Seq(true,true,true,false)
  in.bypass_addr = Seq(2,3,4,0)
input(in)
  in.kill_val = true
  in.kill_id = 0
  in.issueable_var = false
  in.bypass_val = Seq(false,false,false,false)
input(in)
  in.issue_ready = false
  in.kill_val = false
input(in)

}
