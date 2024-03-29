package bian

import chisel3._
import chisel3.util._
import common.CycRange

class ExeValidID(val addr_width: Int, id_width: Int)
  extends ValidID(id_width) {
  val rs = Vec(2, new ByPass(addr_width))
}

class ExeInfo(addr_width: Int, val data_width: Int)
  extends Basic(addr_width) {
  val data = Vec(2, UInt(data_width.W))
}

class ExeIssue(id_width: Int, val addr_width: Int, val data_width: Int)
  extends Issue(id_width) {
  val info = new ExeInfo(addr_width, data_width)
  def data_1: UInt = Mux(mem_en, info.sign_ext_imm, info.data(1))
}

class ExeIssueI(id_width: Int, addr_width: Int, val nCommit: Int, data_width: Int)
  extends ExeIssue(id_width, addr_width, data_width) {
  val valid = Bool() //FIXME: use some trick here
  val rs = Vec(2, new ByPass(addr_width))
  val data_sel = Vec(2, UInt(nCommit.W))
  def rs_valid(i: Int): Bool = rs(i).valid || data_sel(i).orR
  def rs_val_andR: Bool = (0 until 2).map(i => rs_valid(i)).reduce(_&&_)
}

class ExeIssueO(id_width: Int, addr_width: Int, data_width: Int)
  extends ExeIssue(id_width, addr_width, data_width) {
  val data_ok = Bool()
}

class TailInfo(val id_width: Int) extends Bundle {
  val valid = Bool()
  val ready = Bool()
  val id = UInt(id_width.W)
}

class IssueQueue(val nEntry: Int, val n: Int) extends Module with BackParam {
  val io = IO(new Bundle {
    //in check if kill or not outside
    val in = Flipped(DecoupledIO(new ExeIssueI(wOrder, wPhyAddr, nCommit, data_width)))

    val issue  = Output(Valid(new ExeIssueO(wOrder, wPhyAddr, data_width)))
    val mem_acc = Input(Bool())
    val bypass = Input(Vec(nCommit, new ByPass(wPhyAddr)))//no latched, comb logic
    val bydata = Input(Vec(nCommit, UInt(data_width.W)))  //already latched one cycle, sequential logic

    val head = Input(UInt(wOrder.W))
    val xcpt = Input(Bool())
    val kill = Input(Valid(UInt(wOrder.W)))
    val tail = Output(new TailInfo(wOrder))
    val issueable = Input(Valid(UInt(wOrder.W)))
    val limit = Input(Bool())

    val cyc = Input(UInt(data_width.W))
  })
  def wEntry: Int = log2Ceil(nEntry)
  def wCount: Int = log2Ceil(nEntry+1)

  val issue_table = Reg(Vec(nEntry, new ExeInfo(wPhyAddr, data_width)))
  val issue_valid = RegInit(VecInit(Seq.fill(nEntry){
    val w = Wire(new ExeValidID(wPhyAddr, wOrder))
    w.valid := false.B
    w.id := DontCare
    w.rs := DontCare
    w
  }))
  val issue_queue = Reg(Vec(nEntry, new IndexIssue(wOrder, nEntry)))
  val queue_valid = RegInit(VecInit(Seq.fill(nEntry)(false.B)))
  val issue = RegInit({
    val w = Wire(new Bundle {
      val count = UInt(wCount.W) /*the number of issue in the queue*/
      val tail = UInt(wEntry.W) /*the current tail of the queue*/
      val valid = Bool() /*whether output issue is valid or not*/
      val forward = Bool() /*whether the entry pop out or not*/
      val entry = new IndexIssue(wOrder, nEntry) /*the pop entry*/
      val data_ok  = Bool() /*only for the store inst, whether data is ok*/
      val data_sel  = Vec(2, UInt(nCommit.W)) /*op-data sel from the bypass data*/
      val table_sel = Vec(nEntry, Vec(2, UInt(nCommit.W))) /*table update data select of bypass data*/
      val mmacc     = Vec(nEntry, Bool())
      val snoop     = Vec(nEntry, Vec(2, new ByPass(wPhyAddr)))
    })
    w.count := 0.U
    w.valid := false.B
    w.forward := false.B
    w.tail := DontCare
    w.entry := DontCare
    w.data_ok := DontCare
    w.data_sel  := DontCare
    w.table_sel := DontCare
    w.mmacc := DontCare
    w.snoop := DontCare
    w
  })
  val issue_ctrl = Wire(new Bundle {
    val in_issue = new IndexIssue(wOrder, nEntry) /*push entry to the queue*/
    val push_queue = Bool() /*whether or not to push to the queue*/
    val entry = new ExeInfo(wPhyAddr, data_width)
    val tidx  = UInt(wEntry.W)  /*the index of table to insert the pushed entry*/
    val tidx1H = UInt(nEntry.W) /*the one hot index of table to insert the pushed entry*/
    val tb_data  = Vec(nEntry, Vec(2, UInt(data_width.W)))
    val tb_valid = UInt(nEntry.W) /*table kill info*/
    def empty: UInt = (~tb_valid).asUInt /*table empty vector*/

    val count = UInt(wCount.W) /*the number of entry in the queue*/
    val snoop = Vec(nEntry, Vec(2, Bool())) /*bypass snoop info*/
    val limit = Vec(nEntry, Bool()) /*memory inst issue limitation*/
    val ready = Vec(nEntry, Bool()) /*the data ready vector*/
    val mmacc = Vec(nEntry, Bool()) /*memory inst accelerate vector*/
    val data_sel = Vec(nEntry, Vec(2, UInt(nCommit.W))) /*data select of bypass data and also already cached data*/
    val kill = new PriorityKill(nEntry) /*kill action*/
    /*the last valid entry index of queue*/
    def tail: UInt = count - 1.U
    def ready_orR: Bool = (ready.asUInt & kill.survive).orR
    def mmacc_orR: Bool = (mmacc.asUInt & kill.survive & limit.asUInt).orR
    /*whether forward entry is valid or not*/
    def valid: Bool = mmacc_orR | ready_orR
    /*whether or not forward out of queue*/
    def forward: Bool = Mux(mmacc_orR, fwd_data_ok, ready_orR)
    /*forward index of issue in the queue*/
    def fwd_ptr: UInt = Mux(mmacc_orR, PriorityEncoder(mmacc), PriorityEncoder(ready))
    /*data ok only for store inst when its entry forward*/
    def fwd_data_ok: Bool  = PriorityMux(mmacc, snoop.map(_(1)))
    /*mmacc vector update one hot indicator*/
    def mmacc_ptr1H: UInt = PriorityEncoderOH(mmacc.asUInt & kill.survive)
  })

  io.in.ready := io.tail.ready && io.in.bits.valid
  io.tail.ready := issue_ctrl.empty.orR || issue.forward
  io.tail.valid := !queue_valid.asUInt.orR //TODO 55 is going to pop out at this cycle and 49 will be entered
  io.tail.id    := issue_queue(issue.tail).id //TODO

  io.issue.valid := issue.valid && //for branchJump logic need to add kill cancel logic
    !(io.kill.valid && CmpId(io.kill.bits, issue.entry.id, io.head, wOrder-1)) //TODO: xcpt no needed???
  io.issue.bits.id := issue.entry.id
  io.issue.bits.mem_en  := issue.entry.mem_en
  io.issue.bits.data_ok := issue.data_ok
  issue_ctrl.entry := issue_table(issue.entry.tidx)

  io.issue.bits.info.rd     := issue_ctrl.entry.rd
  io.issue.bits.info.f1     := issue_ctrl.entry.f1
  io.issue.bits.info.imm    := issue_ctrl.entry.imm
  io.issue.bits.info.branch := issue_ctrl.entry.branch

  for (i <- 0 until 2) {
    io.issue.bits.info.data(i) := Mux(issue_valid(issue.entry.tidx).rs(i).valid,
      issue_ctrl.entry.data(i),
      issue_ctrl.tb_data(issue.entry.tidx)(i))
  }

  issue.valid  := issue_ctrl.valid || (io.in.fire && io.in.bits.rs_valid(0) &&
    Mux(io.mem_acc && io.in.bits.mem_en, !io.limit, io.in.bits.rs_valid(1))) //TODO: is it too complex???

  issue.entry :=
    Mux(issue_ctrl.mmacc_orR, PriorityMux(issue_ctrl.mmacc, issue_queue),
    Mux(issue_ctrl.ready_orR, PriorityMux(issue_ctrl.ready, issue_queue),
        issue_ctrl.in_issue))

  issue.forward  := issue_ctrl.forward || (io.in.fire && !issue_ctrl.push_queue)
  issue.data_ok  := Mux(issue_ctrl.mmacc_orR, issue_ctrl.fwd_data_ok, io.in.bits.rs_valid(1))

  issue_ctrl.push_queue := !io.in.bits.rs_val_andR || issue_ctrl.mmacc_orR || (io.limit && io.in.bits.mem_en) //a piece of condition
  issue_ctrl.in_issue.id := io.in.bits.id
  issue_ctrl.in_issue.mem_en := io.in.bits.mem_en
  issue_ctrl.in_issue.tidx := issue_ctrl.tidx

  issue_ctrl.tidx     := Mux(issue.forward, issue.entry.tidx, PriorityEncoder(issue_ctrl.empty))
  issue_ctrl.tidx1H   := Mux(issue.forward, UIntToOH(issue.entry.tidx), PriorityEncoderOH(issue_ctrl.empty))
  issue_ctrl.tb_valid := VecInit(issue_valid.map(i => i.valid &&
    !(io.kill.valid && CmpId(io.kill.bits, i.id, io.head, wOrder-1)))).asUInt

  issue_ctrl.kill.cmp   := issue_queue.map(i => CmpId(io.kill.bits, i.id, io.head, wOrder-1))
  issue_ctrl.kill.kill  := io.kill.valid
  issue_ctrl.kill.valid := queue_valid.asUInt
  issue_ctrl.count := Mux(issue_ctrl.kill.use_ptr, issue_ctrl.kill.ptr, issue.count)

  when (io.xcpt) {
    issue.count := 0.U
  }.otherwise {
    when (io.in.fire) {
      when (issue_ctrl.forward) {
        issue.count := issue_ctrl.count
        issue.tail  := issue_ctrl.tail
      }.elsewhen(issue_ctrl.push_queue) {
        issue.count := issue_ctrl.count+1.U
        issue.tail  := issue_ctrl.count
      }
    }.otherwise {
      when (issue_ctrl.forward) {
        issue.count := issue_ctrl.tail
        issue.tail  := issue_ctrl.count-2.U
      }.otherwise {
        issue.count := issue_ctrl.count
        issue.tail  := issue_ctrl.tail
      }
    }
  }
  def updateQueue(i: Int): Unit = {
    for (j <- 0 until 2) issue.snoop(i)(j).valid := issue_ctrl.snoop(i)(j)
  }
  def insertQueue(i: Int): Unit = {
    issue_queue(i) := issue_ctrl.in_issue
    issue.mmacc(i) := io.mem_acc && io.in.bits.mem_en && (issue_ctrl.valid || !io.in.bits.rs_valid(0) || io.limit)
    for (j <- 0 until 2) {
      issue.snoop(i)(j).addr  := io.in.bits.rs(j).addr
      issue.snoop(i)(j).valid := io.in.bits.rs_valid(j)
    }
  }
  def stable_queue: Seq[Bool] = (0 until nEntry).map(i => i.U  <  issue_ctrl.fwd_ptr)
  def shift_queue : Seq[Bool] = (0 until nEntry).map(i => i.U  <  issue_ctrl.tail(wEntry-1,0))
  def touch_tail  : Seq[Bool] = (0 until nEntry).map(i => i.U === issue_ctrl.tail(wEntry-1,0))
  def touch_count : Seq[Bool] = (0 until nEntry).map(i => i.U === issue_ctrl.count(wEntry-1,0))

  for (i<- 0 until nEntry) {
    issue_ctrl.limit(i) := !io.issueable.valid || !CmpId(io.issueable.bits, issue_queue(i).id, io.head, wOrder-1)
    issue_ctrl.ready(i) := issue_ctrl.snoop(i).reduce(_&&_) && (issue_ctrl.limit(i) || !issue_queue(i).mem_en)
    issue_ctrl.mmacc(i) := issue_ctrl.snoop(i)(0) && (issue.mmacc(i) || (issue_ctrl.snoop(i)(1) && issue_queue(i).mem_en))

    for (j <- 0 until 2) {
      issue_ctrl.data_sel(i)(j) := VecInit(io.bypass.map(by =>
        by.addr === issue.snoop(i)(j).addr && by.valid)).asUInt
      issue_ctrl.snoop(i)(j) := issue.snoop(i)(j).valid || issue_ctrl.data_sel(i)(j).orR

      issue_ctrl.tb_data(i)(j) := (0 until nCommit).map(k => io.bydata(k) &
        Fill(data_width, issue.table_sel(i)(j)(k))).reduce(_|_)
    }
    //issue_queue valid
    when (io.xcpt) {
      queue_valid(i) := false.B
    }.elsewhen(issue_ctrl.forward) {
      when (touch_tail(i)) {
        queue_valid(i) := io.in.fire // TODO think about it carefully
      }.elsewhen(stable_queue(i)) {
        queue_valid(i) := issue_ctrl.kill.survive(i)
      }.elsewhen(shift_queue(i)) { if (i < nEntry-1)
        queue_valid(i) := issue_ctrl.kill.survive(i + 1)
      }.otherwise {
        queue_valid(i) := false.B
      }
    }.otherwise {
      when (touch_count(i) && io.in.fire) {
        queue_valid(i) := issue_ctrl.push_queue
      }.otherwise {
        queue_valid(i) := issue_ctrl.kill.survive(i)
      }
    }
    //issue_queue context
    when(issue_ctrl.forward) {
      when (touch_tail(i) && io.in.fire) {
        insertQueue(i)
      }.elsewhen(stable_queue(i)) {
        updateQueue(i)
      }.elsewhen(shift_queue(i)) { if (i < nEntry-1) {
        issue_queue(i) := issue_queue(i + 1)
        issue.mmacc(i) := issue.mmacc(i + 1)
        for (j <- 0 until 2) {
          issue.snoop(i)(j).valid := issue_ctrl.snoop(i + 1)(j)
          issue.snoop(i)(j).addr  := issue.snoop(i + 1)(j).addr
        }
      }}
    }.otherwise {
      when (touch_count(i) && io.in.fire) {
        insertQueue(i)
      }.otherwise {
        updateQueue(i)
        when (issue_ctrl.mmacc_ptr1H(i)) {
          issue.mmacc(i) := false.B
        }
      }
    }
    //issue_table valid TODO: to figure out why this modifing will lower the effeciency???
    when (io.xcpt) {
      issue_valid(i).valid := false.B
    }.elsewhen(issue_ctrl.tidx1H(i) && (io.in.fire || issue.forward)) {
      issue_valid(i).valid := io.in.fire
    }.otherwise {
      issue_valid(i).valid := issue_ctrl.tb_valid(i)
    }
    //issue_table context
    when (io.in.fire && issue_ctrl.tidx1H(i)) {
      issue_valid(i).id := io.in.bits.id
      issue_valid(i).rs := io.in.bits.rs
      issue_table(i) := io.in.bits.info
      issue.table_sel(i) := io.in.bits.data_sel
    }.otherwise {
      for (j <- 0 until 2) {
        issue.table_sel(i)(j) := VecInit(io.bypass.map(by =>
          by.addr === issue_valid(i).rs(j).addr && by.valid)).asUInt
        when(!issue_valid(i).rs(j).valid) {
          when (issue.table_sel(i)(j).orR) {
            issue_table(i).data(j) := issue_ctrl.tb_data(i)(j)
            issue_valid(i).rs(j).valid := true.B
          }
        }
      }
    }
  }
//  if (n <= ALU3) {
//    when (CycRange(io.cyc,810, 824)) {
//      //    printf(
//      //      p"in fire->${io.in.fire} " +
//      //      p"in id->${io.in.bits.id} " +
//      //      p"in mem en->${io.in.bits.mem_en} " +
//      //      p"tidx->${issue_ctrl.tidx} " +
//      //      p"kill->${io.kill} " +
//      //      p"tail ready->${io.tail.ready} " +
//      //      p"tail valid->${io.tail.valid} " +
//      //      p"tail id->${io.tail.id} " +
//      //      p"in ready->${io.in.ready}\n")
//      //      p"data_ok->${issue.data_ok} " +
//      //      p"data_sel->${issue.data_sel}\n" +
//      //      p"info->${io.issue.bits.info}\n")
//      //    printf(p"entry->${issue.entry}\n")
//      printf(
//        p"issue_$n: " +
//        p"count ${issue.count} " +
//        p"tail ${issue.tail} " +
//        p"table_valid ${issue.valid} " +
//        p"queue_valid ${RegNext(issue_ctrl.valid)} " +
//        p"table_forward ${issue.forward} " +
//        p"queue_forward ${RegNext(issue_ctrl.forward)} " +
//        p"id ${issue.entry.id} " +
////        p"ctrl: " +
////        p"snoop ${issue_ctrl.snoop} " +
////        p"mmacc ${issue_ctrl.mmacc_orR}->${issue_ctrl.mmacc}" +
//        p"kill ${io.kill.valid}->${io.kill.bits} ${issue_ctrl.tb_valid} ")
//      printf(p"\n$n io.in ${io.in.fire}->${io.in.bits.id} " +
//        p"issue_queue$n:")
//      for (j <- 0 until nEntry) printf(p" ${queue_valid(j)}->${issue_queue(j).id}")
//      printf(p" mmacc ${issue.mmacc}")
//      printf(p" issue_table$n:")
//      for (j <- 0 until nEntry) printf(p" ${issue_valid(j).valid}->${issue_valid(j).id}")
//      printf("\n")
//    }
//  }

//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}
