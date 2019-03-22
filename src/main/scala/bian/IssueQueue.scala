package bian

import chisel3._
import chisel3.util._

trait IssueBufParam {
  val nEntry = 8
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class ExeIssue(val addr_width: Int, val id_width: Int, val data_width: Int) extends Bundle {
  val id   = UInt(id_width.W)
  val rd   = new ByPass(addr_width)
  val f1   = Bool()
  val mem_en = Bool()
  val data = Vec(2, UInt(data_width.W))
}

class IssueQueue extends Module with Pram with IssueBufParam {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ExeIssue(wPhyAddr, wOrder, data_width)))
    val rsdata = Input(Vec(2, UInt(data_width.W)))
    val rsaddr = Input(Vec(2, new ByPass(wPhyAddr)))
    val bypass = Input(Vec(nCommit, new ByPass(wPhyAddr)))
    val bydata = Input(Vec(nCommit, UInt(data_width.W)))
    val forward= Output(new ByPass(wPhyAddr))
    val forward_id = Output(UInt(wOrder.W)) // to access inst code
    val issue  = Output(Valid(new ExeIssue(wPhyAddr, wOrder, data_width)))
    val mem_en = Output(Bool())
  })

  val bydata = Reg(Vec(nCommit, UInt(data_width.W)))
  val bypass = Reg(Vec(nCommit, new ByPass(wPhyAddr)))
  bydata := io.bydata
  bypass := io.bypass

  val queue = Reg(Vec(nEntry, new F1Issue(wPhyAddr, wOrder)))
  val tidxs = Reg(Vec(nEntry, UInt(wEntry.W)))
  val lsacc = Reg(Vec(nEntry, Bool()))

  val count = RegInit(0.U(wCount.W))

  val rdata = Reg(Vec(nEntry, Vec(2, UInt(data_width.W))))
  val raddr = Reg(Vec(nEntry, Vec(2, UInt(wPhyAddr.W))))
  val tbval = RegInit(0.U(nEntry.W))


  val snoop = Wire(Vec(nEntry, Vec(2, Bool())))
  val sdata = Wire(Vec(nEntry, Vec(2, UInt(data_width.W))))

  val ready_ptr = Wire(UInt(log2Ceil(nEntry).W))
  val lsacc_ptr = Wire(UInt(log2Ceil(nEntry).W))
  val ready_vec = snoop.map(_.reduce(_&&_))
  val lsacc_vec = (0 until nEntry).map(i => (snoop(i).reduce(_&&_) && queue(i).mem_en) || (snoop(i)(0) && lsacc(i))) // just for addr ok
  val ready_go = ready_vec.reduce(_||_) && ready_ptr < count
  val lsacc_go = lsacc_vec.reduce(_||_) && lsacc_ptr < count
  ready_ptr := PriorityEncoder(ready_vec)
  lsacc_ptr := PriorityEncoder(lsacc_vec)
  val forward_ptr = Mux(lsacc_go, lsacc_ptr, ready_ptr)
  val issue_fire = ready_go || lsacc_go
  for (i <- 1 until nEntry) {
    when (ready_go && forward_ptr < i.U && i.U < count) {
      for (j <- 0 until 2) {
        queue(i-1).rs(j).addr  := queue(i).rs(j).addr
        queue(i-1).rs(j).valid := snoop(i)(j)
      }
      queue(i-1).id := queue(i).id
      queue(i-1).rd := queue(i).rd
      queue(i-1).f1 := queue(i).f1
      tidxs(i-1) := tidxs(i)
    }
    when (ready_go && forward_ptr < i.U && i.U < count) {
      lsacc(i-1) := lsacc(i)
    }.elsewhen(lsacc_go) { lsacc(lsacc_ptr) := false.B }
  }

  io.in.ready := (~tbval).asUInt.orR | ready_go
  io.forward.addr  := queue(forward_ptr).rd.addr
  io.forward.valid := queue(forward_ptr).rd.valid && queue(forward_ptr).f1 && ready_go
  io.forward_id := queue(forward_ptr).id

  def insert(cnt: UInt, idx: UInt): Unit = {
    queue(cnt).f1     := io.in.bits.f1
    queue(cnt).mem_en := io.in.bits.mem_en
    queue(cnt).rd     := io.in.bits.rd
    queue(cnt).id     := io.in.bits.id
    queue(cnt).rs     := io.rsaddr
    tidxs(cnt) := idx
    lsacc(cnt) := io.in.bits.mem_en
  }
  // about count and tbval
  val in_tidx   = Mux(ready_go, forward_ptr, PriorityEncoder(~tbval))
  val in_set    = UIntToOH(in_tidx)
  val out_reset = (~UIntToOH(forward_ptr)).asUInt

  for (i <- 0 until nEntry) {
    for (j <- 0 until 2) {
      snoop(i)(j) := queue(i).rs(j).valid || bypass.map(by => by.addr === queue(i).rs(j).addr && by.valid).reduce(_||_)
      sdata(i)(j) := Mux(bypass.map(by => by.addr =/= raddr(i)(j) || !by.valid).reduce(_&&_), rdata(i)(j),
        (0 until nCommit).map(k => Fill(data_width, bypass(k).addr === raddr(i)(j)) & bydata(k)).reduce(_|_))
    }
    when (io.in.fire && in_set(i) ) { rdata(in_tidx) := io.rsdata
    }.otherwise { rdata(i) := sdata(i) }
  }

  when (io.in.fire) { raddr(in_tidx) := io.rsaddr }

  when (io.in.fire && ready_go) {
    insert(count - 1.U, in_tidx)
  }.elsewhen(io.in.fire) {
    insert(count, in_tidx)
    tbval := tbval | in_set
    count := count + 1.U
  }.elsewhen(ready_go) {
    tbval := tbval & out_reset
    count := count - 1.U
  }

  val forward_tid = tidxs(forward_ptr)
  val issue = Reg(new ExeIssue(wPhyAddr, wOrder, data_width))
  io.issue.bits:= issue
  issue.id       := queue(forward_ptr).id
  issue.rd       := queue(forward_ptr).rd
  issue.f1       := queue(forward_ptr).f1
  issue.mem_en   := queue(forward_ptr).mem_en
  issue.data     := sdata(forward_tid)
  io.issue.valid := RegNext(issue_fire)
}
