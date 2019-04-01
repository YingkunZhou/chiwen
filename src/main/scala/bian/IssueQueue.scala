package bian

import chisel3._
import chisel3.util._

trait IssueParam extends BackParam {
  val nEntry = 8
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class ExeIssue(val addr_width: Int, val id_width: Int)
  extends Bundle {
  val id   = UInt(id_width.W)
  val rd   = new ByPass(addr_width)
  val f1   = Bool()
  val mem_en = Bool()
  val branch = Bool()
}

class ExeIssueIO(addr_width: Int, id_width: Int, val data_width: Int)
  extends ExeIssue(addr_width, id_width) {
  val data = Vec(2, UInt(data_width.W))
}

class ExeIssueEntry(addr_width: Int, id_width: Int, val nEntry: Int)
  extends ExeIssue(addr_width, id_width) {
  val tidx = UInt(log2Ceil(nEntry).W)
}

class ExeIssueO(addr_width: Int, id_width: Int, data_width: Int, val nCommit: Int)
  extends ExeIssueIO(addr_width, id_width, data_width) {
  val data_ok = Bool()
  val data_sel = Vec(2, UInt(nCommit.W))
}

class ExeIssueI(addr_width: Int, id_width: Int, data_width: Int)
  extends ExeIssueIO(addr_width, id_width, data_width) {
  val rs = Vec(2, new ByPass(addr_width))
}

class PriorityKill(val nEntry: Int) extends Bundle {
  val valid = Bool()
  val cmp = Vec(nEntry, Bool())
  def ptr: UInt = PriorityEncoder(cmp)
  def use_ptr(v: UInt): Bool = (cmp.asUInt & v).orR && valid
  def survive: UInt = VecInit((0 until nEntry).map(i => !cmp(i) || !valid)).asUInt
}

class IssueTentry(val data_width: Int, val addr_width: Int, val id_width: Int)
  extends Bundle {
  val valid = Bool()
  val id = UInt(id_width.W)
  val rs = Vec(2, new ByPass(addr_width))
  val data = Vec(2, UInt(data_width.W))
}

class IssueQueue extends Module with IssueParam {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ExeIssueI(wPhyAddr, wOrder, data_width))) //TODO: in check if kill or not

    val bypass = Input(Vec(nCommit, new ByPass(wPhyAddr)))
    val bydata = Input(Vec(nCommit, UInt(data_width.W)))

    val forward= Output(new ByPass(wPhyAddr))

    val issue  = Output(Valid(new ExeIssueO(wPhyAddr, wOrder, data_width, nCommit)))
    val head_id = Input(UInt(wOrder.W))
    val xcpt = Input(Bool())
    val kill = Input(Valid(UInt(wOrder.W)))
  })

  val bydata = RegNext(io.bydata)
  val bypass = RegNext(io.bypass)
  val issue_valid = RegInit(VecInit(Seq.fill(nEntry)(false.B)))
  val issue_table = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(new IssueTentry(data_width,wPhyAddr,wOrder))
    w.valid := false.B
    w.id := DontCare
    w.rs := DontCare
    w.data := DontCare
    w
  }))
  val issue_queue = Reg(Vec(nEntry, new ExeIssueEntry(wPhyAddr,wOrder,nEntry)))
  val issue_lsacc = Reg(Vec(nEntry, Bool()))
  val issue_rs    = Reg(Vec(nEntry, Vec(2, new ByPass(wPhyAddr))))
  val issue = RegInit({
    val w = Wire(new Bundle {
      val count = UInt(wCount.W) /*the number of issue in the queue*/
      val valid = Bool() /*whether output issue is valid or not*/
      val pop = new ExeIssueEntry(wPhyAddr, wOrder, nEntry) /*the pop entry*/
      val pop_out = Bool() /*whether the entry pop out or not*/
      val data_ok = Bool() /*only for the store inst, whether data is ok*/
      val data_sel = Vec(2, UInt(nCommit.W)) /*op-data sel from the bypass data*/
    })
    w.count := 0.U
    w.valid := false.B
    w.pop   := DontCare
    w.pop_out  := false.B
    w.data_ok  := DontCare
    w.data_sel := DontCare
    w
  })
  val issue_ctrl = Wire(new Bundle {
    val push = new ExeIssueEntry(wPhyAddr, wOrder, nEntry) /*push entry to the queue*/
    val peek = new ExeIssueEntry(wPhyAddr, wOrder, nEntry) /*peek entry of queue*/
    val count = UInt(wCount.W) /*the number of entry in the queue*/
    val tidx  = UInt(wEntry.W)  /*the index of table to insert the pushed entry*/
    val tidx1H = UInt(nEntry.W) /*the one hot index of table to insert the pushed entry*/
    val snoop = Vec(nEntry, Vec(2, Bool())) /*bypass snoop info*/
    val lsacc = UInt(nEntry.W) /*memory inst accelerate vector*/
    val data_sel  = Vec(nEntry, Vec(2, UInt(nCommit.W))) /*data select of bypass data and also already cached data*/
    val table_sel = Vec(nEntry, Vec(2, UInt(nCommit.W))) /*table update data select of bypass data*/
    val kill = new PriorityKill(nEntry) /*kill action*/
    val tb_kill = Vec(nEntry, Bool()) /*table kill info*/
    val empty = UInt(nEntry.W) /*table empty vector*/
    val valid = UInt(nEntry.W) /*queue valid vector*/
    /*the last valid entry index of queue*/
    def tail: UInt = count - 1.U
    /*the data ready vector*/
    def ready: UInt = VecInit(snoop.map(_.reduce(_&&_))).asUInt
    def ready_orR: Bool = (ready & valid).orR
    def lsacc_orR: Bool = (lsacc & valid).orR
    /*forward index of issue in the queue*/
    def fwd_ptr: UInt = Mux(lsacc_orR, PriorityEncoder(lsacc), PriorityEncoder(ready))
    /*the data select of forward issue*/
    def fwd_sel: Seq[UInt] = Mux(lsacc_orR,
      PriorityMux(lsacc, data_sel), PriorityMux(ready, data_sel))
    /*whether forward entry is valid or not*/
    def fwd_valid: Bool = lsacc_orR | ready_orR
    /*data ok only for store inst when its entry forward*/
    def fwd_data_ok: Bool  = PriorityMux(lsacc, snoop.map(_(1)))
    /*whether or not */
    def fwd_pop_out: Bool = Mux(lsacc_orR, fwd_data_ok, ready_orR)
    def io_fwd_valid: Bool = peek.rd.valid && peek.f1 && ready_orR
    def lsacc_ptr1H: UInt = PriorityEncoderOH(lsacc & valid)
  })
  io.in.ready := issue_ctrl.empty.orR || issue.pop_out
  io.issue.bits.id := issue.pop.id
  io.issue.bits.rd := issue.pop.rd
  io.issue.bits.f1 := issue.pop.f1
  io.issue.bits.mem_en := issue.pop.mem_en
  io.issue.bits.branch := issue.pop.branch

  issue_ctrl.push.id := io.in.bits.id
  issue_ctrl.push.rd := io.in.bits.rd
  issue_ctrl.push.f1 := io.in.bits.f1
  issue_ctrl.push.mem_en := io.in.bits.mem_en
  issue_ctrl.push.branch := io.in.bits.branch
  issue_ctrl.push.tidx := issue_ctrl.tidx

  io.forward.addr  := issue_ctrl.peek.rd.addr
  io.forward.valid := issue_ctrl.io_fwd_valid

  io.issue.valid := issue.valid
  io.issue.bits.data := issue_table(issue.pop.tidx).data
  io.issue.bits.data_ok  := issue.data_ok
  io.issue.bits.data_sel := issue.data_sel

  issue.valid := issue_ctrl.fwd_valid && !io.xcpt
  issue.pop   := issue_ctrl.peek
  issue.pop_out  := issue_ctrl.fwd_pop_out
  issue.data_ok  := issue_ctrl.fwd_data_ok
  issue.data_sel := issue_ctrl.fwd_sel

  issue_ctrl.empty := VecInit(issue_table.map(!_.valid)).asUInt()
  issue_ctrl.valid := issue_valid.asUInt & issue_ctrl.kill.survive
  issue_ctrl.lsacc := VecInit((0 until nEntry).map(i => issue_ctrl.snoop(i)(0) &&
    ((issue_ctrl.snoop(i)(1) && issue_queue(i).mem_en) || issue_lsacc(i)))).asUInt()
  issue_ctrl.peek := Mux(issue_ctrl.lsacc_orR,
    PriorityMux(issue_ctrl.lsacc, issue_queue),
    PriorityMux(issue_ctrl.ready, issue_queue))
  //the issue need to pop next cycle
  issue_ctrl.count := Mux(issue_ctrl.kill.use_ptr(issue_valid.asUInt), issue_ctrl.kill.ptr, issue.count)
  //the count of issue in queue currently
  issue_ctrl.tidx := Mux(issue.pop_out, issue.pop.tidx, PriorityEncoder(issue_ctrl.empty))
  //the insert slot idx of table
  issue_ctrl.tidx1H  := Mux(issue.pop_out, UIntToOH(issue.pop.tidx), PriorityEncoderOH(issue_ctrl.empty))
  //the one hot insert slot idx of table
  issue_ctrl.tb_kill := issue_table.map(i => CmpId(io.kill.bits, i.id, io.head_id))
  //table kill vector
  issue_ctrl.kill.cmp := issue_queue.map(i => CmpId(io.kill.bits, i.id, io.head_id))
  issue_ctrl.kill.valid := io.kill.valid
  //queue kill vector
  when (io.xcpt) {
    issue.count := 0.U
  }.otherwise {
    when (io.in.fire) {
      when (issue_ctrl.fwd_pop_out) {
        issue.count := issue_ctrl.count
      }.otherwise {
        issue.count := issue_ctrl.count + 1.U
      }
    }.otherwise {
      when (issue_ctrl.fwd_pop_out) {
        issue.count := issue_ctrl.tail
      }.otherwise {
        issue.count := issue_ctrl.count
      }
    }
  }
  def updateQueue(i: Int): Unit = {for (j <- 0 until 2) issue_rs(i)(j).valid := issue_ctrl.snoop(i)(j)}
  def stable_queue: Seq[Bool] = (0 until nEntry).map(i => i.U  <  issue_ctrl.fwd_ptr)
  def shift_queue : Seq[Bool] = (0 until nEntry).map(i => i.U  <  issue_ctrl.tail(wEntry-1,0))
  def touch_tail  : Seq[Bool] = (0 until nEntry).map(i => i.U === issue_ctrl.tail(wEntry-1,0))
  def touch_count : Seq[Bool] = (0 until nEntry).map(i => i.U === issue_ctrl.count(wEntry-1,0))

  for (i<- 0 until nEntry) {
    //issue_queue valid
    when (io.xcpt) {
      issue_valid(i) := false.B
    }.elsewhen(issue_ctrl.fwd_pop_out) {
      when (touch_tail(i)) {
        issue_valid(i) := io.in.fire // TODO figure out whether to be cancled or not outside
      }.elsewhen(stable_queue(i)) {
        issue_valid(i) := issue_ctrl.valid(i)
      }.elsewhen(shift_queue(i)) { if (i < nEntry-1)
        issue_valid(i) := issue_ctrl.valid(i + 1)
      }.otherwise {
        issue_valid(i) := false.B
      }
    }.otherwise {
      when (touch_count(i)) {
        issue_valid(i) := io.in.fire
      }.otherwise {
        issue_valid(i) := issue_ctrl.valid(i)
      }
    }
    //issue_queue context
    when(issue_ctrl.fwd_pop_out) {
      when (touch_tail(i) && io.in.fire) {
        issue_rs(i) := io.in.bits.rs
        issue_lsacc(i) := io.in.bits.mem_en
        issue_queue(i) := issue_ctrl.push
      }.elsewhen(stable_queue(i)) { updateQueue(i)
      }.elsewhen(shift_queue(i)) { if (i < nEntry-1) {
        issue_lsacc(i) := issue_lsacc(i + 1)
        for (j <- 0 until 2) {
          issue_rs(i)(j).addr  := issue_rs(i + 1)(j).addr
          issue_rs(i)(j).valid := issue_ctrl.snoop(i + 1)(j)
        }
        issue_queue(i).id := issue_queue(i + 1).id
        issue_queue(i).rd := issue_queue(i + 1).rd
        issue_queue(i).f1 := issue_queue(i + 1).f1
        issue_queue(i).tidx := issue_queue(i + 1).tidx
        issue_queue(i).mem_en := issue_queue(i + 1).mem_en
      }}
    }.otherwise {
      when (touch_count(i) && io.in.fire) {
        issue_rs(i) := io.in.bits.rs
        issue_lsacc(i) := io.in.bits.mem_en
        issue_queue(i) := issue_ctrl.push
      }.otherwise {
        when (issue_ctrl.lsacc_ptr1H(i)) {
          issue_lsacc(i) := false.B }
        updateQueue(i)
      }
    }
    //issue_table valid
    when (io.xcpt || (issue_ctrl.tb_kill(i) && io.kill.valid)) {
      issue_table(i).valid := false.B
    }.elsewhen(issue_ctrl.tidx1H(i)) {
      when (io.in.fire) {
        issue_table(i).valid := true.B
      }.elsewhen(issue.pop_out) {
        issue_table(i).valid := false.B
      }
    }
    //issue_table context
    when (io.in.fire && issue_ctrl.tidx1H(i)) {
      issue_table(i).id   := io.in.bits.id
      issue_table(i).rs   := io.in.bits.rs
      issue_table(i).data := io.in.bits.data
    }.otherwise {
      for (j <- 0 until 2) {
        when(issue_ctrl.table_sel(i)(j).orR) {
          issue_table(i).data(j) := (0 until nCommit).map(k => bydata(k) &
            Fill(data_width, issue_ctrl.table_sel(i)(j)(k))).reduce(_ | _)
        }
      }
    }

    for (j <- 0 until 2) {
      issue_ctrl.data_sel(i)(j) := VecInit(io.bypass.map(by =>
        by.addr === issue_rs(i)(j).addr && by.valid)).asUInt
      issue_ctrl.table_sel(i)(j) := VecInit(bypass.map(by =>
        by.addr === issue_table(i).rs(j).addr && by.valid)).asUInt
      issue_ctrl.snoop(i)(j) := issue_rs(i)(j).valid || issue_ctrl.data_sel(i)(j).orR
    }
  }

  val ids = Wire(Vec(nEntry, UInt()))
  ids := issue_queue.map(_.id)
  printf(p"in->${io.in.fire} tidx->${issue_ctrl.tidx} kill->${io.kill}\n")
  printf(p"fwd_pop_out ${issue_ctrl.fwd_pop_out}\n")
  printf(p"issue: count->${issue.count} valid->${issue.valid} pop_out->${issue.pop_out} " +
    p"data_ok->${issue.data_ok} data_sel->${issue.data_sel} data->${io.issue.bits.data}\n")
  printf(p"issue entry: ${issue.pop}\n")
  printf(p"issue_valid: $issue_valid issue_queue: $ids\n")
  printf(p"ctrl: snoop ${issue_ctrl.snoop} lsacc ${issue_ctrl.lsacc_orR}->${issue_ctrl.lsacc}\n")
  val cnt = RegInit(0.U(32.W))
  cnt := cnt + 1.U
  printf(p"=======================cnt = $cnt=============================\n")
}
