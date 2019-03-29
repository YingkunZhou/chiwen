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

trait InstParam extends BackParam {
  val nEntry = 8
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class InstQueue extends Module with InstParam {
  val io = IO(new Bundle{
    val ready = Output(Bool())
    val in  = Input(Valid(new F1Issue(wPhyAddr, wOrder))) //TODO: check if kill or not
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
  val issue = RegInit({
    val w = Wire(Valid(new F1Issue(wPhyAddr, wOrder)))
    w.valid := false.B
    w.bits := DontCare
    w
  })
  val inst_ctrl = Wire(new Bundle {
    // for head
    val invalid = Bool()
    val ready = Vec(2, Bool())
    // for queue
    val snoop = Vec(nEntry, Vec(2, Bool()))
    val limit = Vec(nEntry, Bool())
    val valid = UInt(nEntry.W)
    val kill  = new PriorityKill(nEntry)
    val tail  = UInt((wEntry+1).W)
    def pop_out: Bool = (valid & forward).orR
    def forward: UInt = VecInit((0 until nEntry).map(i => snoop(i).reduce(_&&_) && !limit(i))).asUInt
    def fwd_ptr: UInt = Mux(pop_out, PriorityEncoder(forward), 0.U(wEntry.W))
  })

  val inst_count = RegInit(0.U(wCount.W))
  val inst_valid = RegInit(VecInit(Seq.fill(nEntry)(false.B)))
  val inst_queue = Reg(Vec(nEntry, new F1Issue(wPhyAddr, wOrder)))

  io.counter := inst_count
  io.ready := ~inst_valid.asUInt.orR //|| io.issue.ready FIXME: save time and logic???

  io.data_sel := issue.bits.rs.map(rs => VecInit(io.bypass.map(bypass =>
    bypass.addr === rs.addr && bypass.valid)).asUInt)

  inst_ctrl.invalid := io.kill.valid && CmpId(io.kill.bits, issue.bits.id, io.head_id)
  inst_ctrl.ready := (0 until 2).map(i => io.data_sel(i).orR || issue.bits.rs(i).valid)

  io.pop_out.addr  := issue.bits.rd.addr
  io.pop_out.valid := issue.bits.rd.valid && issue.bits.f1 && inst_ctrl.ready.reduce(_&&_) //&& io.issue.ready->no needed
  io.issue.valid            := issue.valid
  io.issue.bits.id          := issue.bits.id
  io.issue.bits.rd          := issue.bits.rd
  io.issue.bits.f1          := issue.bits.f1
  io.issue.bits.mem_en      := issue.bits.mem_en
  io.issue.bits.info.op     := io.resp_in.op
  io.issue.bits.info.pc     := io.resp_in.pc
  io.issue.bits.info.imm    := io.resp_in.imm
  io.issue.bits.info.imm7_0 := Cat(issue.bits.rs(1).addr(7-wPhyAddr, 0), issue.bits.rs(0).addr)
  for (i <- 0 until 2) {
    io.issue.bits.rs(i).addr  := issue.bits.rs(i).addr
    io.issue.bits.rs(i).valid := inst_ctrl.ready(i)
  }

  inst_ctrl.tail := inst_count - 1.U
  inst_ctrl.valid := inst_valid.asUInt & inst_ctrl.kill.survive
  inst_ctrl.kill.cmp := inst_queue.map(i => CmpId(io.kill.bits, i.id, io.head_id))
  inst_ctrl.kill.cmp_val := io.kill.valid
  when (io.xcpt) { issue.valid := false.B
  }.elsewhen(issue.valid) {
    when (inst_count === 0.U) {
      when (inst_ctrl.invalid) {
        issue.valid := false.B
      }.elsewhen(io.issue.ready) {
        issue  := io.in
      }
    }.elsewhen(inst_ctrl.invalid || io.issue.ready) {
      issue.bits  := inst_queue(inst_ctrl.fwd_ptr)
      issue.valid := inst_ctrl.kill.survive(0)
    }.otherwise {for (i <- 0 until 2)
      issue.bits.rs(i).valid := inst_ctrl.valid(i)
    }
  }.elsewhen(!io.issue.ready) {
    issue  := io.in
  }

  when (io.xcpt) { inst_count := 0.U
  }.elsewhen (issue.valid) {
    when (io.kill.valid && inst_ctrl.kill.cmp(0)) {
      inst_count := 0.U
    }.elsewhen (inst_count =/= 0.U) {
      when (inst_ctrl.kill.valid(inst_valid.asUInt)) {
        inst_count := inst_ctrl.kill.ptr - io.issue.ready.asUInt
      }.elsewhen(io.issue.ready && !io.in.valid) {
        inst_count := inst_ctrl.tail
      }
    }
    when(!io.issue.ready && io.in.valid) {
      inst_count := inst_count + 1.U
    }
  }

  def updateQueue(i: Int): Unit = {for (j <- 0 until 2) inst_queue(i).rs(j).valid := inst_ctrl.snoop(i)(j)}
  def stable_queue: Seq[Bool] = (0 until nEntry).map(i => i.U  <  inst_ctrl.fwd_ptr)
  def shift_queue : Seq[Bool] = (0 until nEntry).map(i => i.U  <  inst_ctrl.tail(wEntry-1,0))
  def touch_tail  : Seq[Bool] = (0 until nEntry).map(i => i.U === inst_ctrl.tail(wEntry-1,0))
  def touch_count : Seq[Bool] = (0 until nEntry).map(i => i.U === inst_count(wEntry-1,0))

  for (i <- 0 until nEntry) {
    when (io.xcpt) {
      inst_valid(i) := false.B
    }.elsewhen (issue.valid) {
      when (io.issue.ready) {
        when (stable_queue(i)) {
          inst_valid(i) := inst_ctrl.valid(i)
        }.elsewhen(shift_queue(i)) { if (i < nEntry-1)
          inst_valid(i) := inst_ctrl.valid(i + 1)
        }
        when (touch_tail(i)) {
          inst_valid(i) := io.in.valid && inst_count =/= 0.U
        }
      }.otherwise {
        when(touch_count(i)) {
          inst_valid(i) := io.in.valid
        }.otherwise {
          inst_valid(i) := inst_ctrl.valid(i)
        }
      }
    }

    when (issue.valid) {
      when (io.issue.ready) {
        when(stable_queue(i)){ updateQueue(i)
        }.elsewhen(shift_queue(i)) { if (i < nEntry-1) {
          for (j <- 0 until 2) {
            inst_queue(i).rs(j).addr := inst_queue(i + 1).rs(j).addr
            inst_queue(i).rs(j).valid := inst_ctrl.snoop(i + 1)(j)
          }
          inst_queue(i).id := inst_queue(i + 1).id
          inst_queue(i).rd := inst_queue(i + 1).rd
          inst_queue(i).f1 := inst_queue(i + 1).f1
          inst_queue(i).mem_en := inst_queue(i + 1).mem_en }
        }
        when (touch_tail(i) && io.in.valid && inst_count =/= 0.U) {
          inst_queue(i) := io.in.bits
        }
      }.otherwise {
        when(touch_count(i) && io.in.valid) {
          inst_queue(i) := io.in.bits
        }.otherwise { updateQueue(i)
        }
      }
    }
    //only for load & store inst
    inst_ctrl.limit(i) := io.issueable.valid && inst_queue(i).mem_en &&
      CmpId(io.issueable.bits, inst_queue(i).id, io.head_id)
    for (j <- 0 until 2) inst_ctrl.snoop(i)(j) := inst_queue(i).rs(j).valid ||
        io.bypass.map( bypass => bypass.addr === inst_queue(i).rs(j).addr && bypass.valid).reduce(_||_)  ||
        io.forward.map(bypass => bypass.addr === inst_queue(i).rs(j).addr && bypass.valid).reduce(_||_)
  }

  val ids = Wire(Vec(nEntry, UInt()))
  ids := inst_queue.map(_.id)
  printf(p"issue $issue\n")
  printf(p"count $inst_count ids $ids valid $inst_valid\n")
  printf(p"snoop ${inst_ctrl.snoop}\n")
  printf(p"ready ${inst_ctrl.ready}\n")
  printf(p"ready ${inst_ctrl.limit}\n")
  val cnt = RegInit(0.U(32.W))
  cnt := cnt + 1.U
  printf(p"=======================cnt = $cnt=============================\n")
}
