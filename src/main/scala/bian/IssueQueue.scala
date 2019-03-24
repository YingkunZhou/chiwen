package bian

import chisel3._
import chisel3.util._

trait IssueParam extends Pram {
  val nEntry = 8
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class ExeIssue(val addr_width: Int, val id_width: Int, val data_width: Int)
  extends Bundle {
  val id   = UInt(id_width.W)
  val rd   = new ByPass(addr_width)
  val f1   = Bool()
  val mem_en = Bool()
  val data = Vec(2, UInt(data_width.W))
}

class ExeIssueO(addr_width: Int, id_width: Int, data_width: Int, val nCommit: Int)
  extends ExeIssue(addr_width, id_width, data_width) {
  val data_ok = Bool()
  val data_sel = Vec(2, UInt(nCommit.W))
}

class ExeIssueI(addr_width: Int, id_width: Int, data_width: Int)
  extends ExeIssue(addr_width, id_width, data_width) {
  val rs = Vec(2, new ByPass(addr_width))
}

class PriorityVec(val nEntry: Int) extends Bundle {
  val vec = UInt(nEntry.W)
  def oht: UInt = PriorityEncoderOH(vec)
  def ptr: UInt = PriorityEncoder(vec)
  def valid: Bool = vec.orR
}

class PriorityKill(val nEntry: Int) extends Bundle {
  val cmp = UInt(nEntry.W)
  def ptr: UInt = PriorityEncoder(cmp)
  def valid(valids: UInt): Bool = (cmp & valids).orR
}

class IssueQentry(val nEntry: Int, addr_width: Int, id_width: Int)
  extends F1Issue(addr_width, id_width) {
  val tidx = UInt(log2Ceil(nEntry).W)
  val ls_acc = Bool()
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
    val in = Flipped(DecoupledIO(new ExeIssueI(wPhyAddr, wOrder, data_width)))

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

  val issue_queue = Reg(Vec(nEntry, new IssueQentry(nEntry, wPhyAddr, wOrder)))
  val issue_valid = RegInit(VecInit(Seq.fill(nEntry)(false.B)))
  val issue_table = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(new IssueTentry(data_width,wPhyAddr,wOrder))
    w.valid := false.B
    w.id := DontCare
    w.rs := DontCare
    w.data := DontCare
    w
  }))

  val issue = RegInit({
    val w = Wire(new Bundle {
      val count = UInt(wCount.W)
      val valid = Bool()
      val inc_head = Bool()
      val fwd_ptr  = UInt(wEntry.W)
      val data_sel = Vec(2, UInt(nCommit.W))
      val lac_ptr  = UInt(wEntry.W)
      val lac_val  = Bool()
      val id = UInt(wOrder.W)
      val rd = new ByPass(wPhyAddr)
      val f1 = Bool()
      val mem_en  = Bool()
      val data_ok = Bool()
      val tidx = UInt(wEntry.W) //to index table
    })
    w.count := 0.U
    w.valid := false.B
    w.inc_head := false.B
    w.fwd_ptr  := DontCare
    w.lac_ptr  := DontCare
    w.lac_val  := DontCare
    w.data_sel := DontCare
    w.id := DontCare
    w.rd := DontCare
    w.f1 := DontCare
    w.mem_en  := DontCare
    w.data_ok := DontCare
    w.tidx := DontCare
    w
  })
  val issue_ctrl = Wire(new Bundle {
    val table_empty = UInt(nEntry.W)
    val push = new IssueQentry(nEntry, wPhyAddr, wOrder)
    val tidx = UInt(wEntry.W) //insert
    val qidx = UInt(wEntry.W) //insert
    val tidx1H = UInt(nEntry.W)
    val tail = UInt(wEntry.W)
    val snoop = Vec(nEntry, Vec(2, Bool()))
    val ready = new PriorityVec(nEntry)
    val lsacc = new PriorityVec(nEntry)
    val data_sel  = Vec(nEntry, Vec(2, UInt(nCommit.W)))
    val table_sel = Vec(nEntry, Vec(2, UInt(nCommit.W)))
    val kill = new PriorityKill(nEntry)
    val tb_kill = Vec(nEntry, Bool())
    def able: UInt = VecInit(snoop.map(_.reduce(_&&_))).asUInt
    def fwd_ptr: UInt = Mux(lsacc.valid, lsacc.ptr, ready.ptr)
    def fwd_valid: Bool = ready.valid | lsacc.valid
    def fwd_data_ok: Bool   = snoop(lsacc.ptr)(1)
    def fwd_inc_head: Bool  = Mux(lsacc.valid, fwd_data_ok, ready.valid)
  })
  issue.valid     := issue_ctrl.fwd_valid
  issue.inc_head  := issue_ctrl.fwd_inc_head
  issue.fwd_ptr   := issue_ctrl.fwd_ptr
  issue.lac_val   := issue_ctrl.lsacc.valid
  issue.lac_ptr   := issue_ctrl.lsacc.ptr
  issue.data_ok   := issue_ctrl.fwd_data_ok

  io.in.ready := issue_ctrl.table_empty.orR || issue.inc_head
  issue_ctrl.table_empty := VecInit(issue_table.map(!_.valid)).asUInt()
  issue_ctrl.ready.vec := issue_valid.asUInt & issue_ctrl.able
  issue_ctrl.lsacc.vec := issue_valid.asUInt & VecInit((0 until nEntry).map(i =>
    (issue_queue(i).ls_acc || (issue_queue(i).mem_en &&
     issue_ctrl.snoop(i)(1))) && issue_ctrl.snoop(i)(0))).asUInt

  issue_ctrl.tidx   := Mux(issue.inc_head, issue.fwd_ptr, PriorityEncoder(issue_ctrl.table_empty))
  issue_ctrl.tidx1H := Mux(issue.inc_head, UIntToOH(issue.fwd_ptr),
    PriorityEncoderOH(issue_ctrl.table_empty))
  issue_ctrl.tail := (issue.count - 1.U)(wEntry-1,0)
  issue_ctrl.qidx := Mux(issue.inc_head, issue_ctrl.tail, issue.count(wEntry-1,0))
  issue_ctrl.tb_kill := issue_table.map(i => CmpId(io.kill.bits, i.id, io.head_id))
  issue_ctrl.kill.cmp := VecInit(issue_queue.map(i => CmpId(io.kill.bits, i.id, io.head_id))).asUInt

  def updateQueue(i0: Int, i1: Int): Unit = {
    for (j <- 0 until 2) issue_queue(i0).rs(j).valid := issue_ctrl.snoop(i1)(j)
  }
  when(!issue.inc_head) {
    when(issue.lac_val) {
      issue_queue(issue.lac_ptr).ls_acc := false.B
    }
    for (i <- 0 until nEntry) updateQueue(i, i)
  }.otherwise {
    for (i<- 0 until nEntry-1) {
      when(i.U < issue.fwd_ptr) {
        updateQueue(i, i)
      }.elsewhen(i.U < issue_ctrl.tail) {
        updateQueue(i, i + 1)
        for (j <- 0 until 2) issue_queue(i).rs(j).addr := issue_queue(i + 1).rs(j).addr
        issue_queue(i).id := issue_queue(i + 1).id
        issue_queue(i).rd := issue_queue(i + 1).rd
        issue_queue(i).f1 := issue_queue(i + 1).f1
        issue_queue(i).tidx := issue_queue(i + 1).tidx
        issue_queue(i).mem_en := issue_queue(i + 1).mem_en
        issue_queue(i).ls_acc := issue_queue(i + 1).ls_acc
      }
    }
  }

  for (i<- 0 until nEntry) {
    // issue valid
    when (io.xcpt || (issue_ctrl.kill.cmp(i) && io.kill.valid)) {
      issue_valid(i) := false.B
    }.elsewhen (issue.inc_head) {
      if (i < nEntry-1) {
        when(issue.fwd_ptr <= i.U && i.U < issue_ctrl.tail) {
          issue_valid(i) := issue_valid(i + 1)
        }.elsewhen(i.U === issue_ctrl.tail && !io.in.fire) {
          issue_valid(i) := false.B
        }
      }
    }

    //issue_table valid
    when (io.xcpt || (issue_ctrl.tb_kill(i) && io.kill.valid)) {
      issue_table(i).valid := false.B
    }.elsewhen (issue_ctrl.tidx1H(i)) {
      when (io.in.fire) {
        issue_table(i).valid := true.B
      }.elsewhen(issue.inc_head) {
        issue_table(i).valid := false.B
      }
    }
    //issue_table context
    when (io.in.fire && issue_ctrl.tidx1H(i)) {
      issue_table(i).id   := io.in.bits.id
      issue_table(i).data := io.in.bits.data
      issue_table(i).rs := io.in.bits.rs
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
        by.addr === issue_queue(i).rs(j).addr && by.valid)).asUInt
      issue_ctrl.table_sel(i)(j) := VecInit(bypass.map(by =>
        by.addr === issue_table(i).rs(j).addr && by.valid)).asUInt
      issue_ctrl.snoop(i)(j) := issue_queue(i).rs(j).valid || issue_ctrl.data_sel(i)(j).orR
    }
  }

  when (io.xcpt) {issue.count := 0.U
  }.elsewhen(io.kill.valid && issue_ctrl.kill.valid(issue_valid.asUInt)) {
    issue.count := issue_ctrl.kill.ptr
  }.otherwise {
    when(io.in.fire && !issue.inc_head) {issue.count := issue.count + 1.U}
    when(!io.in.fire && issue.inc_head) {issue.count := issue_ctrl.tail}
  }

  issue_ctrl.push.id     := io.in.bits.id
  issue_ctrl.push.rs     := io.in.bits.rs
  issue_ctrl.push.rd     := io.in.bits.rd
  issue_ctrl.push.f1     := io.in.bits.f1
  issue_ctrl.push.mem_en := io.in.bits.mem_en
  issue_ctrl.push.ls_acc := io.in.bits.mem_en
  issue_ctrl.push.tidx   := issue_ctrl.tidx
  when (io.in.fire) {issue_queue(issue_ctrl.qidx) := issue_ctrl.push}

  val id = issue_queue(issue_ctrl.fwd_ptr).id
  val rd = issue_queue(issue_ctrl.fwd_ptr).rd
  val f1 = issue_queue(issue_ctrl.fwd_ptr).f1
  io.forward.addr  := rd.addr
  io.forward.valid := rd.valid && f1 && issue_ctrl.able.orR
  issue.id := id
  issue.rd := rd
  issue.f1 := f1
  issue.tidx := issue_queue(issue_ctrl.fwd_ptr).tidx
  issue.mem_en := issue_queue(issue_ctrl.fwd_ptr).mem_en
  issue.data_sel := issue_ctrl.data_sel(issue_ctrl.fwd_ptr)

  io.issue.bits.id := issue.id
  io.issue.bits.rd := issue.rd
  io.issue.bits.f1 := issue.f1
  io.issue.bits.mem_en := issue.mem_en
  io.issue.bits.data_ok := issue.data_ok
  io.issue.bits.data_sel := issue.data_sel
  io.issue.bits.data := issue_table(issue.tidx).data
  io.issue.valid := issue.valid

}
