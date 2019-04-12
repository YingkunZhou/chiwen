package bian

import chisel3.iotesters.PeekPokeTester
class IssueInput {
  var in_vald = true
  var in_id = 0
  var in_mem_en = false
  var in_rd_addr = 0
  var in_data = Seq(0,0)
  var in_rs_val = Seq(false,false)
  var in_rs_addr = Seq(0,0)
  var bypass_val = Seq(false,false,false,false)
  var bypass_addr = Seq(0,0,0,0)
  var bypass_data = Seq(0,0,0,0)
  val xcpt = false
  var kill_val = false
  var kill_id = 0
  var head_id = 0
}

class IssueQTest(c: IssueQueue) extends PeekPokeTester(c) {
  def input(in: IssueInput): Unit = {
    poke(c.io.in.valid, in.in_vald)
    poke(c.io.in.bits.id, in.in_id)
    poke(c.io.in.bits.mem_en, in.in_mem_en)
    poke(c.io.in.bits.info.rd.addr, in.in_rd_addr)
    for (i <- 0 until 2) {
      poke(c.io.in.bits.info.data(i), in.in_data(i))
      poke(c.io.in.bits.rs(i).valid, in.in_rs_val(i))
      poke(c.io.in.bits.rs(i).addr, in.in_rs_addr(i))
    }
    for (i <- 0 until 4) {
      poke(c.io.bypass(i).valid, in.bypass_val(i))
      poke(c.io.bypass(i).addr, in.bypass_addr(i))
      poke(c.io.bydata(i), in.bypass_data(i))
    }
    poke(c.io.xcpt, in.xcpt)
    poke(c.io.kill.valid, in.kill_val)
    poke(c.io.kill.bits, in.kill_id)
    poke(c.io.head, in.head_id)
    step(1)
  }
  val in = new IssueInput
  in.in_vald = false
input(in)//1
  in.in_vald = true
  in.in_id = 1
  in.in_rs_val = Seq(false, false)
  in.in_mem_en = true
  in.in_rd_addr = in.in_id
  in.in_rs_addr = Seq(2,1)
input(in)//2
  in.in_id = 2
  in.in_rs_val = Seq(false, false)
  in.in_mem_en = false
  in.in_rd_addr = in.in_id
  in.in_rs_addr = Seq(2,3)
input(in)//3
  in.bypass_val  = Seq(true, false, false, false)
  in.bypass_addr = Seq(2,3,3,3)
  in.bypass_data = Seq(2,3,3,4)
  in.in_id = 4
  in.in_rd_addr = in.in_id
  in.in_rs_val = Seq(true, true)
  in.in_data = Seq(4,4)
  in.in_rs_addr = Seq(2,4)
input(in)//4
  in.bypass_val  = Seq(false, false, false, false)
  in.in_id = 5
  in.in_rd_addr = in.in_id
  in.in_rs_val = Seq(false, false)
  in.in_rs_addr = Seq(4,3)
input(in)//5
  in.in_vald = false
  in.kill_val = true
  in.kill_id = 3
  in.bypass_val  = Seq(false, true, false, false)
  in.bypass_addr = Seq(2,3,3,3)
  in.bypass_data = Seq(2,3,3,4)
input(in)//6
  // in.kill_val = false
  in.bypass_val = Seq(false, false, false, false)
input(in)
input(in)
  
}
