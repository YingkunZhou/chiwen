package bian

import chisel3.iotesters.PeekPokeTester
class LSInput {
  var in_valid = Seq(true, true)
  var in_fcn = Seq(0,0)
  var in_id = Seq(0,0)
  var in_typ = Seq(0,0)
  var in_rd = Seq(0,0)
  var mem_fcn = Seq(false,false)
  var issue_val = Seq(false,false,false)
  var issue_id = Seq(0,0,0)
  var issue_addr = Seq(0,0,0)
  var issue_data = Seq(0,0,0)
  var issue_data_ok = Seq(false,false,false)
  val xcpt = false
  var kill_val = false
  var kill_id = 0
  var mem_req_ready = false
  var mem_resp_valid = false
  var mem_resp_data = 0
}

class LSTest(c: LoadStore) extends PeekPokeTester(c) {
  def input(in: LSInput): Unit = {
    for (i <- 0 until 2) {
      poke(c.io.in(i).valid, in.in_valid(i))
      poke(c.io.in(i).bits.fcn, in.in_fcn(i))
      poke(c.io.in(i).bits.id, in.in_id(i))
      poke(c.io.in(i).bits.typ, in.in_typ(i))
      poke(c.io.in(i).bits.rd, in.in_rd(i))
      poke(c.io.mem_first(i), in.mem_fcn(i))
    }
    for (i <- 0 until 3) {
      poke(c.io.issue(i).valid, in.issue_val(i))
      poke(c.io.issue(i).id, in.issue_id(i))
      poke(c.io.issue(i).addr, in.issue_addr(i))
      poke(c.io.issue(i).data, in.issue_data(i))
      poke(c.io.issue(i).data_ok, in.issue_data_ok(i))
    }
    poke(c.io.xcpt, in.xcpt)
    poke(c.io.kill.valid, in.kill_val)
    poke(c.io.kill.bits, in.kill_id)
    poke(c.io.mem.req.ready, in.mem_req_ready)
    poke(c.io.mem.resp.valid, in.mem_resp_valid)
    poke(c.io.mem.resp.bits.data, in.mem_resp_data)
    step(1)
  }
  val in = new LSInput
  in.in_id  = Seq(1,2)
  in.in_fcn = Seq(M_XRD.toInt,M_XWR.toInt)
  in.in_typ = Seq(MT_W.toInt,MT_W.toInt)
  in.in_rd  = Seq(0,1)
  in.mem_fcn = Seq(true,false)
  input(in)
  in.in_id  = Seq(3,4)
  in.in_fcn = Seq(M_XRD.toInt,M_XRD.toInt)
  in.in_typ = Seq(MT_W.toInt,MT_W.toInt)
  in.in_rd  = Seq(2,3)
  in.mem_fcn = Seq(true,false)
  input(in)
  in.in_id  = Seq(5,6)
  in.in_fcn = Seq(M_XRD.toInt,M_XRD.toInt)
  in.in_typ = Seq(MT_W.toInt,MT_W.toInt)
  in.in_rd  = Seq(2,3)
  in.mem_fcn = Seq(true,false)
  input(in)
  in.in_valid = Seq(true, false)
  in.in_id  = Seq(7,8)
  in.in_fcn = Seq(M_XRD.toInt,M_XWR.toInt)
  in.in_typ = Seq(MT_W.toInt,MT_W.toInt)
  in.in_rd  = Seq(2,3)
  in.mem_fcn = Seq(true,false)
  input(in)
  in.in_valid = Seq(true, true)
  in.in_id  = Seq(1,2)
  in.in_fcn = Seq(M_XRD.toInt,M_XWR.toInt)
  in.in_typ = Seq(MT_W.toInt,MT_W.toInt)
  in.in_rd  = Seq(0,1)
  in.mem_fcn = Seq(true,false)
  input(in)
  in.in_valid = Seq(false,false)
  in.issue_val = Seq(true, true, false)
  in.issue_id = Seq(1,3,2)
  in.issue_addr = Seq(200,300,300)
  in.issue_data = Seq(0,0,2002)
  in.issue_data_ok = Seq(true, true, true)
  input(in)
  in.issue_val = Seq(false, false, false)
  in.issue_data_ok = Seq(false, false, false)
  in.mem_req_ready = true
  input(in)
  in.mem_resp_valid = true
  in.mem_resp_data = 1001
  input(in)
  input(in)
  in.mem_resp_valid = true
  in.mem_resp_data = 3003
  // in.issue_val = Seq(true, false, false)
  // in.issue_id = Seq(2,0,0)
  // in.issue_addr = Seq(300,0,0)
  input(in)
  in.mem_resp_valid = false
  in.issue_val = Seq(false,false,false)
  input(in)
  input(in)

}
