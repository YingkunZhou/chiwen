package bian
import chisel3._
import chisel3.util._

class Info(val data_width: Int) extends Bundle {
  val op  = new InnerOp
  val pc  = UInt(data_width.W)
  val imm = UInt(12.W)
}

class UImmInfo(data_width: Int)
  extends Info(data_width) {
  val imm7_0 = UInt(8.W)
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

trait InstParam extends Pram {
  val nEntry = 8
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class InstQueue extends Module with InstParam {
  val io = IO(new Bundle{
    val in  = Input(Valid(new F1Issue(wPhyAddr, wOrder)))
    val ready = Output(Bool())
    val issue = Decoupled(new InfoIssue(wPhyAddr, wOrder, data_width))
    val data_sel = Output(Vec(2, UInt(nCommit.W)))

    val resp_in = Input(new Info(data_width))
    val pop_out = Output(new ByPass(wPhyAddr))

    val issueable = Input(Valid(UInt(wOrder.W)))
    val head_id = Input(UInt(wOrder.W))
    val bypass  = Input(Vec(nCommit, new ByPass(wPhyAddr)))
    val forward = Input(Vec(nCommit, new ByPass(wPhyAddr)))

    val xcpt = Input(Bool())
    val kill = Input(Valid(UInt(wOrder.W)))
    val counter = Output(UInt(wCount.W))
  })
  //begin to read Regfile
  val issue = RegInit({
    val w = Wire(Valid(new F1Issue(wPhyAddr, wOrder)))
    w.valid := false.B
    w.bits := DontCare
    w
  })
  val queue_ctrl = Wire(new Bundle {
    val snoop = Vec(nEntry, Vec(2, Bool()))
    val limit = Vec(nEntry, Bool())
    val empty = UInt(nEntry.W)
    val valid = Vec(2, Bool())
    val tail = UInt(wEntry.W)
    val kill = new PriorityKill(nEntry)
    val ready = UInt(nEntry.W)
    def forward: UInt = VecInit((0 until nEntry).map(i => snoop(i).reduce(_&&_) && !limit(i))).asUInt
    def fwd_ptr: UInt = Mux(ready.orR, PriorityEncoder(ready), 0.U(wEntry.W))
  })
  io.data_sel := issue.bits.rs.map(rs => VecInit(io.bypass.map(bypass =>
    bypass.addr === rs.addr && bypass.valid)).asUInt)
  queue_ctrl.valid := (0 until 2).map(i => io.data_sel(i).orR || issue.bits.rs(i).valid)
  io.pop_out.addr  := issue.bits.rd.addr
  io.pop_out.valid := issue.bits.rd.valid && issue.bits.f1 && queue_ctrl.valid.reduce(_&&_)

  io.issue.valid     := issue.valid
  io.issue.bits.id   := issue.bits.id
  io.issue.bits.rd   := issue.bits.rd
  io.issue.bits.f1   := issue.bits.f1
  io.issue.bits.mem_en := issue.bits.mem_en
  io.issue.bits.info.op  := io.resp_in.op
  io.issue.bits.info.pc  := io.resp_in.pc
  io.issue.bits.info.imm := io.resp_in.imm
  io.issue.bits.info.imm7_0 := Cat(issue.bits.rs(1).addr(7-wPhyAddr, 0), issue.bits.rs(0).addr)
  for (i <- 0 until 2) {
    io.issue.bits.rs(i).addr  := issue.bits.rs(i).addr
    io.issue.bits.rs(i).valid := queue_ctrl.valid(i)
  }

  val inst_count = RegInit(0.U(wCount.W))
  val inst_valid = RegInit(VecInit(Seq.fill(nEntry)(false.B)))
  val inst_queue = Reg(Vec(nEntry, new F1Issue(wPhyAddr, wOrder)))
  queue_ctrl.empty := ~inst_valid.asUInt
  queue_ctrl.ready := inst_valid.asUInt & queue_ctrl.forward
  io.counter := inst_count
  io.ready := queue_ctrl.empty.orR || io.issue.ready

  val flush = io.xcpt || (CmpId(io.kill.bits, issue.bits.id, io.head_id) && io.kill.valid)
  when (flush) {
    issue.valid := false.B
  }.elsewhen (issue.valid) { 
    when (io.issue.ready) {
      when (inst_count === 0.U) {
        when (io.in.valid) {
          issue := io.in
          issue.valid := true.B
        }.otherwise {
          issue.valid := false.B
        }
      }.otherwise {
        issue.bits  := inst_queue(queue_ctrl.fwd_ptr)
        issue.valid := true.B
      }
    }
  }.elsewhen(!io.issue.ready && io.in.valid) {
    issue := io.in
    issue.valid := true.B
  }.otherwise {
    for (i <- 0 until 2) issue.bits.rs(i).valid := queue_ctrl.ready(i)
  }

  queue_ctrl.kill.cmp := VecInit(inst_queue.map(i => CmpId(io.kill.bits, i.id, io.head_id))).asUInt

  def updateQueue(i0: Int, i1: Int): Unit = {
    for (j <- 0 until 2) inst_queue(i0).rs(j).valid := queue_ctrl.snoop(i1)(j)
  }
  for (i <- 0 until nEntry) {
    when (!io.issue.fire) {
      updateQueue(i,i)
    }.otherwise {
      when (i.U < queue_ctrl.fwd_ptr) {
        updateQueue(i,i)
      }.elsewhen (i.U < queue_ctrl.tail) {
        if (i < nEntry-1) {
          updateQueue(i, i + 1)
          for (j <- 0 until 2) inst_queue(i).rs(j).addr := inst_queue(i + 1).rs(j).addr
          inst_queue(i).id := inst_queue(i + 1).id
          inst_queue(i).rd := inst_queue(i + 1).rd
          inst_queue(i).f1 := inst_queue(i + 1).f1
          inst_queue(i).mem_en := inst_queue(i + 1).mem_en
        }
      }
    }
    // issue_reg valid
    when (flush || (queue_ctrl.kill.cmp(i) && io.kill.valid)) {
      inst_valid(i) := false.B
    }.elsewhen (io.issue.fire) {
      if (i < nEntry-1) {
        when(queue_ctrl.fwd_ptr <= i.U && i.U < queue_ctrl.tail) {
          inst_valid(i) := inst_valid(i + 1)
        }.elsewhen(i.U === queue_ctrl.tail && !io.in.valid) {
          inst_valid(i) := false.B
        }
      }
    }
    //only for load & store inst
    queue_ctrl.limit(i) := io.issueable.valid && CmpId(io.issueable.bits, inst_queue(i).id, io.head_id) && inst_queue(i).mem_en
    for (j <- 0 until 2) {
      queue_ctrl.snoop(i)(j) := inst_queue(i).rs(j).valid ||
        io.bypass.map( bypass => bypass.addr === inst_queue(i).rs(j).addr && bypass.valid).reduce(_||_)  ||
        io.forward.map(bypass => bypass.addr === inst_queue(i).rs(j).addr && bypass.valid).reduce(_||_)
    }
  }
  
  queue_ctrl.tail := (inst_count - 1.U)(wEntry-1,0)
  
  when (flush) { inst_count := 0.U
  }.elsewhen(io.kill.valid &&
    queue_ctrl.kill.valid(inst_valid.asUInt)) {
    inst_count := queue_ctrl.kill.ptr
  }.elsewhen (issue.valid) {
    when(!io.issue.ready &&  io.in.valid) {inst_count := inst_count + 1.U }
    when( io.issue.ready && !io.in.valid && inst_count =/= 0.U) {inst_count := queue_ctrl.tail}
  }

  when (issue.valid && io.in.valid) {
    when (!io.issue.ready) { inst_queue(inst_count(wEntry-1, 0)) := io.in.bits
    }.elsewhen (inst_count =/= 0.U) { inst_queue(queue_ctrl.tail) := io.in.bits }
  }
}
