package myCore
import chisel3._
import chisel3.util._

class Info(val data_width: Int) extends Bundle {
  val op  = new InnerOp
  val pc  = UInt(data_width.W)
  val imm = UInt(13.W)
}

class UImmInfo(data_width: Int)
  extends Info(data_width) {
  val u_imm7_0 = UInt(8.W)
}

class Issue(val addr_width: Int, val id_width: Int) extends Bundle {
  val id = UInt(id_width.W)
  val rs = Vec(2, new ByPass(addr_width))
}

class WbIssue(addr_width: Int, id_width: Int)
  extends Issue(addr_width, id_width) {
  val rd = new ByPass(addr_width)
}

class F1Issue(addr_width: Int, id_width: Int)
  extends WbIssue(addr_width, id_width) {
  val f1 = Bool()
  val mem_en = Bool()
}

class InfoIssue(addr_width: Int, id_width: Int, val data_width: Int)
  extends F1Issue(addr_width, id_width) {
  val info = new UImmInfo(data_width)
}

class ByPass(val addr_width: Int) extends Bundle {
  val valid = Bool()
  val addr = UInt(addr_width.W)
}

trait IssueParam extends Pram {
  val nEntry = 8
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class InstQueue(val data_width: Int) extends Module with IssueParam {
  val io = IO(new Bundle{
    val in  = Flipped(Decoupled(new F1Issue(wPhyAddr, wOrder)))
    val issue = Decoupled(new InfoIssue(wPhyAddr, wOrder, data_width))

    val pump_in = Input(new Info(data_width))
    val pump_id = Output(UInt(wOrder.W))
    val pump    = Output(new ByPass(wPhyAddr))

    val issueable = Input(Valid(UInt(wOrder.W)))
    val id_head = Input(UInt(wOrder.W))
    val bypass  = Input(Vec(nCommit, new ByPass(wPhyAddr)))
    val forward = Input(Vec(nCommit, new ByPass(wPhyAddr)))

    val counter = Output(UInt(wCount.W))
  })

  val reg_issue   = Reg(new F1Issue(wPhyAddr, wOrder))
  val issue_valid = RegInit(false.B)
  val issue_ready = reg_issue.rs.map(rs => io.bypass.map(bypass =>
    bypass.addr === rs.addr && bypass.valid).reduce(_||_) || rs.valid)

  io.pump.addr  := reg_issue.rd.addr
  io.pump.valid := reg_issue.rd.valid && reg_issue.f1 && issue_ready.reduce(_||_)
  io.pump_id    := reg_issue.id

  io.issue.valid     := issue_valid
  io.issue.bits.id   := reg_issue.id
  io.issue.bits.rd   := reg_issue.rd
  io.issue.bits.f1   := reg_issue.f1
  io.issue.bits.info.op  := io.pump_in.op
  io.issue.bits.info.pc  := io.pump_in.pc
  io.issue.bits.info.imm := io.pump_in.imm
  io.issue.bits.info.u_imm7_0 := Cat(reg_issue.rs(1).addr(7-wPhyAddr, 0), reg_issue.rs(0).addr)
  for (i <- 0 until 2) {
    io.issue.bits.rs(i).addr  := reg_issue.rs(i).addr
    io.issue.bits.rs(i).valid := issue_ready(i)
  }

  val queue = Reg(Vec(nEntry, new F1Issue(wPhyAddr, wOrder)))
  val count = RegInit(0.U(wCount.W))
  val snoop = Wire(Vec(nEntry, Vec(2, Bool())))
  val limit = Wire(Vec(nEntry, Bool()))

  io.counter   := count
  io.in.ready  := count =/= nEntry.U

  val pump_ptr   = Wire(UInt(log2Ceil(nEntry).W))
  when (!io.issue.valid || (io.issue.fire && count === 0.U)) {
    reg_issue   := io.in
    issue_valid := io.in.valid
  }.elsewhen(io.issue.fire) {
    reg_issue   := queue(pump_ptr)
    issue_valid := true.B
  }.otherwise {
    for (i <- 0 until 2)
      reg_issue.rs(i).valid := issue_ready(i)
  }

  for (i <- 1 until nEntry) {
    when (io.issue.fire && pump_ptr < i.U && i.U < count) {
      for (j <- 0 until 2) {
        queue(i-1).rs(j).addr  := queue(i).rs(j).addr
        queue(i-1).rs(j).valid := snoop(i)(j)
      }
      queue(i-1).f1 := queue(i).f1
      queue(i-1).id := queue(i).id
      queue(i-1).rd := queue(i).rd
      queue(i-1).mem_en := queue(i).mem_en
    }
  }

  when (issue_valid) {
    when (io.in.fire && io.issue.fire && count =/= 0.U) {
      queue(count - 1.U) := io.in
    }.elsewhen(io.in.fire) {
      queue(count) := io.in
      count := count + 1.U
    }.elsewhen(io.issue.fire && count =/= 0.U) {
      count := count - 1.U
    }
  }


  for (i <- 0 until nEntry) {
    limit(i) := io.issueable.valid && CmpId(io.issueable.bits, queue(i).id, io.id_head) && queue(i).mem_en //only for load & store inst
    for (j <- 0 until 2) {
      snoop(i)(j) := queue(i).rs(j).valid ||
        io.bypass.map( bypass => bypass.addr === queue(i).rs(j).addr && bypass.valid).reduce(_||_)  ||
        io.forward.map(bypass => bypass.addr === queue(i).rs(j).addr && bypass.valid).reduce(_||_)
    }
  }

  val ready_go  = (0 until nEntry).map(i => snoop(i).reduce(_&&_) && !limit(i))
  val ready_ptr = Wire(UInt(wEntry.W))
  ready_ptr := PriorityEncoder(ready_go)
  pump_ptr  := Mux(ready_go.reduce(_||_) && ready_ptr < count, ready_ptr, 0.U(wEntry.W))
}
