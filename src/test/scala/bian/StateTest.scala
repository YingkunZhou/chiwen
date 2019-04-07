package bian

import chisel3.iotesters.PeekPokeTester

class StateInput {
  var bidx1H = 0
  var brchjr = Seq(false, false)

  var logic_valid  = Seq(true, true)
  var logic_rs_val = Seq(Seq(false, false), Seq(false, false))
  var logic_rs_addr= Seq(Seq(0, 0), Seq(0, 0))
  var logic_rd_val = Seq(true, true)
  var logic_rd_addr= Seq(0, 0)
  var order_inc  = Seq(true, true)
  var physic_inc = Seq(true, true)

  var commit_valid   = Seq(false, false, false, false)
  var commit_id      = Seq(0, 0, 0, 0)
  var commit_wb_val  = Seq(false, false, false, false)
  var commit_wb_addr = Seq(0, 0, 0, 0)
  var br_commit_val = false 
  var br_commit_id = 0
  var st_commit_val = false
  var st_commit_id = 0

  var xcpt_val = false
  var xcpt_id  = 0

  var kill_val  = false
  var kill_id   = 0
  var kill_bidx = 0
}

class StateTest(c: StateCtrl) extends PeekPokeTester(c){
  def input(in: StateInput): Unit = {
    poke(c.io.bidx1H, in.bidx1H)
    for (i <- 0 until 2) {
    poke(c.io.brchjr(i), in.brchjr(i))
      for (j <- 0 until 2) {
        poke(c.io.logic(i).rs(j).valid, in.logic_rs_val(i)(j))
        poke(c.io.logic(i).rs(j).addr, in.logic_rs_addr(i)(j))
      }
      //info don't care
      poke(c.io.logic(i).rd.valid, in.logic_rd_val(i))
      poke(c.io.logic(i).rd.addr, in.logic_rd_addr(i))
      poke(c.io.inc_order(i), in.order_inc(i))
      poke(c.io.phy_valid(i), in.physic_inc(i))
    }
    for (i <- 0 until 4) {
      poke(c.io.commit(i).valid, in.commit_valid(i))
      poke(c.io.commit(i).id, in.commit_id(i))
      poke(c.io.commit(i).wb.valid, in.commit_wb_val(i))
      poke(c.io.commit(i).wb.addr, in.commit_wb_addr(i))
    }
    poke(c.io.br_commit.valid, in.br_commit_val)
    poke(c.io.br_commit.bits, in.br_commit_id)
    poke(c.io.st_commit.valid, in.br_commit_id)
    poke(c.io.st_commit.bits, in.br_commit_val)

    poke(c.io.xcpt_i.valid, in.xcpt_val)
    poke(c.io.xcpt_i.id, in.xcpt_id)

    poke(c.io.kill.id, in.kill_id)
    poke(c.io.kill.valid, in.kill_val)
    poke(c.io.kill.bidx, in.kill_bidx)
    step(1)
  }
  val in = new StateInput
  in.logic_rd_addr = Seq(1,2)
input(in)
  in.logic_rs_addr = Seq(Seq(1,2), Seq(0,0))
  in.logic_rd_val = Seq(false, true)
  in.commit_valid = Seq(true,true,false,false)
  in.commit_wb_val = Seq(true,true,false,false)
  in.commit_id = Seq(0,1,0,0)
  in.commit_wb_addr = Seq(0,59,0,0)
  in.bidx1H = 1
  in.brchjr = Seq(true, false)
input(in)
  in.brchjr = Seq(false, false)
  in.order_inc  = Seq(false,true)
  in.physic_inc = Seq(false,true)
  in.commit_valid = Seq(false,false,false,false)
  in.commit_wb_val = Seq(false,false,false,false)
input(in)
  in.commit_id = Seq(2,3,0,0)
  in.commit_wb_addr = Seq(1,58,0,0)
  in.commit_valid = Seq(true,true,false,false)
  in.commit_wb_val = Seq(true,true,false,false)
  in.order_inc  = Seq(true,true)
  in.physic_inc = Seq(true,true)
  in.logic_rd_addr = Seq(3,4)
  in.kill_val = true
  in.kill_bidx = 0
  in.kill_id = 2
input(in)
  in.kill_val = false
  in.order_inc  = Seq(true,true)
  in.physic_inc = Seq(true,true)
  in.logic_rd_addr = Seq(3,4)
input(in)

}
