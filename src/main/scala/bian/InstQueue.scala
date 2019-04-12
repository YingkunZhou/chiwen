package bian
import chisel3._
import chisel3.util._

class ByPass(val addr_width: Int) extends Bundle {
  val valid = Bool()
  val addr = UInt(addr_width.W)
}

class ValidID(val id_width: Int) extends Bundle {
  val valid = Bool()
  val id = UInt(id_width.W)
}

class Issue(val id_width: Int) extends Bundle {
  val id = UInt(id_width.W)
  val mem_en = Bool()
}

class IndexIssue(id_width: Int, val nEntry: Int)
  extends Issue(id_width) {
  val tidx = UInt(log2Ceil(nEntry).W)
}

class Basic(val addr_width: Int) extends Bundle {
  val rd = new ByPass(addr_width)
  val f1 = Bool()
  val branch = Bool()
  val imm = UInt(12.W)
  def wb_val: Bool = rd.valid && f1
}

class InstInfo(addr_width: Int)
  extends Basic(addr_width) {
  val op1_sel = UInt(OP1_X.getWidth.W)
  val op2_sel = UInt(OP22_X.getWidth.W)
}

class InstIssueI(id_width: Int, val addr_width: Int)
  extends Issue(id_width) {
  val rs = Vec(2, new ByPass(addr_width))
  val info = new InstInfo(addr_width)
}

class InstIssueO(id_width: Int, addr_width: Int, val nCommit: Int)
  extends InstIssueI(id_width, addr_width) {
  val data_sel = Vec(2, UInt(nCommit.W))
}

class PriorityKill(val nEntry: Int) extends Bundle {
  val kill = Bool()
  val valid = UInt(nEntry.W)
  val cmp = Vec(nEntry, Bool())
  def ptr: UInt = PriorityEncoder(cmp)
  def use_ptr: Bool = (cmp.asUInt & valid).orR && kill
  def survive: UInt = VecInit((0 until nEntry).map(i => !cmp(i) || !kill)).asUInt & valid
}

trait InstParam extends BackParam {
  val nEntry = 8
  val nTable = nEntry + 1
  require(isPow2(nEntry))
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nTable)
}

class InstQueue extends Module with InstParam {
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new InstIssueI(wOrder, wPhyAddr)))
    val issue = DecoupledIO(new InstIssueO(wOrder, wPhyAddr, nCommit))

    val issueable = Input(Valid(UInt(wOrder.W)))
    val bypass  = Input(Vec(nCommit, new ByPass(wPhyAddr)))
    val speed   = Input(Vec(3, new ByPass(wPhyAddr)))
    val forward = Output(new ByPass(wPhyAddr))

    val head = Input(UInt(wOrder.W))
    val xcpt = Input(Bool())
    val kill = Input(Valid(UInt(wOrder.W)))
  })
  /*TODO List
  * how to deal with accelerate bypass to save time and enhance efficiency
  * io.in.ready := !inst_count(wEntry) || io.issue.ready how much logic gates will ready signal take
  * */
  val issue = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val entry = new IndexIssue(wOrder, nEntry)
      val rs = Vec(2, new ByPass(wPhyAddr))
      val snoop = Vec(nEntry, Vec(2, new ByPass(wPhyAddr)))
    })
    w.valid := false.B
    w.entry := DontCare
    w.rs := DontCare
    w.snoop := DontCare
    w
  })
  val inst_ctrl = Wire(new Bundle {
    // for head
    val issue_kill  = Bool() /*whether to invalidate the reg issue or not*/
    val issue_valid = Vec(2, Bool()) /*whether the 2 op-data ready or not*/
    val issue_stall = Bool()
    val in_issue = new IndexIssue(wOrder, nTable) /*input issue from outside*/
    // for table
    val entry  = new InstInfo(wPhyAddr)
    val empty  = UInt(nTable.W)
    val tbkill = Vec(nTable, Bool()) /*table kill vector*/
    val tidx   = UInt(log2Ceil(nTable).W) /*input alloc table index*/
    val tidx1H = UInt(nTable.W) /*issue table index one hot vector*/
    // for queue
    val count = UInt(wCount.W)
    val snoop = Vec(nEntry, Vec(2, Bool())) /*snoop bypass info*/
    val speed = Vec(nEntry, Vec(2, Bool())) /*accelerate issuing*/
    val limit = Vec(nEntry, Bool()) /*memory inst issue limitation*/
    val kill  = new PriorityKill(nEntry) /*kill action*/
    /*the tail of queue*/
    def tail: UInt = count - 1.U
    /*issue forward enable vector*/
    def forward: UInt = VecInit((0 until nEntry).map(i => limit(i) &&
      (snoop(i)(0) || speed(i)(0)) && (snoop(i)(1) || speed(i)(1)))).asUInt
    /*issue forward ptr of queue*/
    def fwd_ptr: UInt = Mux((kill.survive & forward).orR, PriorityEncoder(forward),
      PriorityEncoder(limit.asUInt))
    /*under limit constraint, exist such case that the queue has entry, but none can be issued*/
    def fwd_val: Bool = (kill.survive & limit.asUInt).orR
  })

  val inst_count = RegInit(0.U(wCount.W))
  val inst_table = Reg(Vec(nTable, new InstInfo(wPhyAddr)))
  val inst_valid = RegInit(VecInit(Seq.fill(nTable){
    val w = Wire(new ValidID(wOrder))
    w.valid := false.B
    w.id := DontCare
    w
  }))
  val inst_queue  = Reg(Vec(nEntry, new IndexIssue(wOrder, nTable)))
  val queue_valid = RegInit(VecInit(Seq.fill(nEntry)(false.B)))

  io.in.ready := !inst_count(wEntry) //TODO: use some trick to save time and logic

  io.issue.valid := issue.valid && !inst_ctrl.issue_kill
  io.issue.bits.id := issue.entry.id
  for (i <- 0 until 2) {
    io.issue.bits.rs(i).valid := inst_ctrl.issue_valid(i)
    io.issue.bits.rs(i).addr  := issue.rs(i).addr
  }
  io.issue.bits.mem_en   := issue.entry.mem_en
  io.issue.bits.info     := inst_ctrl.entry
  io.issue.bits.data_sel := issue.rs.map(rs => VecInit(
    io.bypass.map(bypass => bypass.addr === rs.addr && bypass.valid)).asUInt)

  io.forward.addr  := inst_ctrl.entry.rd.addr
  io.forward.valid := inst_ctrl.entry.rd.valid && inst_ctrl.entry.f1 &&
    inst_ctrl.issue_valid.reduce(_&&_) //&& io.issue.ready no needed

  inst_ctrl.entry  := inst_table(issue.entry.tidx)
  inst_ctrl.empty  := VecInit(inst_valid.map(!_.valid)).asUInt
  inst_ctrl.tidx   := Mux(io.issue.fire, issue.entry.tidx, PriorityEncoder(inst_ctrl.empty))
  inst_ctrl.tidx1H := Mux(io.issue.fire, UIntToOH(issue.entry.tidx), PriorityEncoderOH(inst_ctrl.empty))
  inst_ctrl.tbkill := inst_valid.map(i => CmpId(io.kill.bits, i.id, io.head))

  inst_ctrl.issue_kill  := io.kill.valid && CmpId(io.kill.bits, issue.entry.id, io.head)
  inst_ctrl.issue_stall := !io.issue.ready && !inst_ctrl.issue_kill
  inst_ctrl.issue_valid := (0 until 2).map(i => io.issue.bits.data_sel(i).orR || issue.rs(i).valid)

  inst_ctrl.kill.cmp   := inst_queue.map(i => CmpId(io.kill.bits, i.id, io.head))
  inst_ctrl.kill.kill  := io.kill.valid
  inst_ctrl.kill.valid := queue_valid.asUInt

  inst_ctrl.in_issue.id     := io.in.bits.id
  inst_ctrl.in_issue.mem_en := io.in.bits.mem_en
  inst_ctrl.in_issue.tidx   := inst_ctrl.tidx

  inst_ctrl.count := Mux(inst_ctrl.kill.use_ptr, inst_ctrl.kill.ptr, inst_count)
  when (io.xcpt) {
    inst_count := 0.U
  }.elsewhen (issue.valid) {//if issue.valid begin count
    when (inst_ctrl.issue_stall) {
      when (io.in.valid) {
        inst_count := inst_count + 1.U
      }.otherwise {
        inst_count := inst_ctrl.count
      }
    }.elsewhen (inst_ctrl.count =/= 0.U && !io.in.valid) {
      inst_count := inst_ctrl.tail
    }.otherwise {
      inst_count := inst_ctrl.count
    }
  }

  when (io.xcpt) {
    issue.valid := false.B
  }.elsewhen(issue.valid) {
    when (inst_ctrl.issue_stall) {
      for (i <- 0 until 2) issue.rs(i).valid := inst_ctrl.issue_valid(i)
    }.elsewhen (inst_ctrl.count =/= 0.U) {
      issue.valid := inst_ctrl.fwd_val
      issue.entry := inst_queue(inst_ctrl.fwd_ptr)
      for (i <- 0 until 2) {
        issue.rs(i).valid := inst_ctrl.snoop(inst_ctrl.fwd_ptr)(i)
        issue.rs(i).addr  := issue.snoop(inst_ctrl.fwd_ptr)(i).addr
      }
    }.otherwise {
      issue.valid := io.in.valid
      issue.entry := inst_ctrl.in_issue
      issue.rs := io.in.bits.rs
    }
  }.elsewhen(!io.issue.ready) {
    issue.valid := io.in.valid
    issue.entry := inst_ctrl.in_issue
    issue.rs := io.in.bits.rs
  }

  def updateQueue(i: Int): Unit = {
    for (j <- 0 until 2) issue.snoop(i)(j).valid := inst_ctrl.snoop(i)(j)
  }
  def insertQueue(i: Int): Unit = {
    issue.snoop(i) := io.in.bits.rs
    inst_queue(i)  := inst_ctrl.in_issue
  }
  def stable_queue: Seq[Bool] = (0 until nEntry).map(i => i.U  <  inst_ctrl.fwd_ptr)
  def shift_queue : Seq[Bool] = (0 until nEntry).map(i => i.U  <  inst_ctrl.tail(wEntry-1,0))
  def touch_tail  : Seq[Bool] = (0 until nEntry).map(i => i.U === inst_ctrl.tail(wEntry-1,0))
  def touch_count : Seq[Bool] = (0 until nEntry).map(i => i.U === inst_count(wEntry-1,0))
  for (i <- 0 until nEntry) {
    //only for load & store inst
    inst_ctrl.limit(i) := !io.issueable.valid || !inst_queue(i).mem_en ||
      !CmpId(io.issueable.bits, inst_queue(i).id, io.head)
    for (j <- 0 until 2) {
      inst_ctrl.snoop(i)(j) := issue.snoop(i)(j).valid ||
        io.bypass.map(b => b.addr === issue.snoop(i)(j).addr && b.valid).reduce(_||_)
      inst_ctrl.speed(i)(j) :=
        io.speed.map(f => f.addr === issue.snoop(i)(j).addr && f.valid).reduce(_||_)
    }
    //inst queue valid
    when (issue.valid) {
      when (io.xcpt) {
        queue_valid(i) := false.B
      }.elsewhen(inst_ctrl.issue_stall) {
        when(touch_count(i)) {
          queue_valid(i) := io.in.valid
        }.otherwise {
          queue_valid(i) := inst_ctrl.kill.survive(i)
        }
      }.elsewhen (inst_ctrl.count =/= 0.U) {
        when (touch_tail(i)) {
          queue_valid(i) := io.in.valid
        }.elsewhen(stable_queue(i)) {
          queue_valid(i) := inst_ctrl.kill.survive(i)
        }.elsewhen(shift_queue(i)) { if (i < nEntry-1)
          queue_valid(i) := inst_ctrl.kill.survive(i + 1)
        }.otherwise {
          queue_valid(i) := false.B
        }
      }.otherwise {
        queue_valid(i) := false.B
      }
      //inst queue context
      when (inst_ctrl.issue_stall) {
        when(touch_count(i) && io.in.valid) {
          insertQueue(i)
        }.otherwise {
          updateQueue(i)
        }
      }.elsewhen(inst_ctrl.count =/= 0.U) {
        when (touch_tail(i) && io.in.valid) {
          insertQueue(i)
        }.elsewhen(stable_queue(i)){
          updateQueue(i)
        }.elsewhen(shift_queue(i)) { if (i < nEntry-1) {
          inst_queue(i) := inst_queue(i + 1)
          for (j <- 0 until 2) {
            issue.snoop(i)(j).valid := inst_ctrl.snoop(i + 1)(j)
            issue.snoop(i)(j).addr  := issue.snoop(i + 1)(j).addr
          }
        }}
      }
    }
    //inst table valid
    when (io.xcpt || (inst_ctrl.tbkill(i) && io.kill.valid)) {
      inst_valid(i).valid := false.B
    }.elsewhen(inst_ctrl.tidx1H(i)) {
      when (io.in.valid && (!io.issue.ready || issue.valid)) {
        inst_valid(i).valid := true.B
      }.elsewhen(io.issue.fire) {
        inst_valid(i).valid := false.B
      }
    }
    //inst table context
    when (inst_ctrl.tidx1H(i) && io.in.valid && (!io.issue.ready || issue.valid)) {
      inst_valid(i).id := io.in.bits.id
      inst_table(i) := io.in.bits.info
    }
  }

//  printf(p"issue $issue\n")
//  printf(p"count $inst_count queue ")
//  for (i <- 0 until nEntry) {
//    printf(p"${queue_valid(i)}->${inst_queue(i).id} ")
//  }
//  printf(p"\nsnoop ${inst_ctrl.snoop}\n")
//  printf(p"valid_head ${inst_ctrl.issue_valid}\n")
//  printf(p"ready ${inst_ctrl.limit}\n")
//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}
