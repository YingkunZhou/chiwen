package bian

import chisel3._
import chisel3.util._

trait BjParam extends Pram {
  val nEntry = nBrchjr
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class BrchjrPred(val id_width: Int, data_width: Int) extends Predict(data_width) {
  val id = UInt(id_width.W)
  val pc = UInt(data_width.W)
  val cont = UInt(data_width.W)
  val jump = UInt(Jump.NUM.W)
  val branch = Bool()
  val br_type = UInt(BR_N.getWidth.W)
}

class BrjrIssueIO(val id_width: Int) extends Bundle {
  val valid = Bool()
  val id = UInt(id_width.W)
  val actual = Bool() // = 0 cont || = 1 jump
}

class BrjrIssue(id_width: Int, val wEntry: Int) extends BrjrIssueIO(id_width) {
  val branch = Bool()
  val bidx = UInt(wEntry.W)
}

class BranchJump extends Module with BjParam {
  val io = IO(new Bundle {
    val pred_i = Input(new BrchjrPred(wOrder, data_width))
    val bidx1H = Output(UInt(nEntry.W)) //alloc
    val bReady = Output(Bool())
    val inc_tail = Input(Bool())

    val issue   = Input(Vec(3, new BrjrIssueIO(wOrder)))
    val br_type = Output(Vec(3, UInt(BR_N.getWidth.W)))
    val jr_tgt  = Input(Vec(3, UInt(data_width.W)))
    val cd_link = Output(Vec(3, UInt(data_width.W)))
    val id_head = Input(UInt(wOrder.W))

    val feedback = Output(Valid(new Predict(data_width)))
    val fb_pc    = Output(UInt(data_width.W))
    val fb_type  = Output(UInt(BTBType.SZ.W))
    //TODO:
//    val ras_pop  = Output(Bool())
//    val ras_push = Output(Valid(UInt(data_width.W)))
    val xcpt = Input(Bool())
    val kill = Output(new KillInfo(wOrder, nEntry))
    //commit associate logic
    val val_mask = Output(Vec(3, Bool()))
    val b_commit = Output(Valid(UInt(wOrder.W)))
  })

  val pred_table = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(Valid(new BrchjrPred(wOrder, data_width)))
    w.valid := false.B
    w.bits := DontCare
    w
  }))
  val pred_queue = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(new BrjrIssue(wOrder, wEntry))
    w.valid  := false.B
    w.id     := DontCare
    w.actual := DontCare
    w.branch := DontCare
    w.bidx   := DontCare
    w
  }))
  val pred_count = RegInit(0.U(wCount.W))
  val pred_ctrl = Wire(new Bundle {
    val inc_head = Bool()

    val table_empty = UInt(nEntry.W)
    val table_idcam = Vec(3, UInt(nEntry.W))
    val table_bid1H = UInt(nEntry.W)

    val queue_empty = Bool()
    val queue_idcam = Vec(3, UInt(wEntry.W))
    val queue_ptr = Vec(2, UInt(wEntry.W))

    val expect = Vec(3, Bool())
    val branch = Vec(3, Bool())
    val jumprg = Vec(3, Bool())
    val actual = Vec(3, Bool())
    val b_kill = Vec(3, Bool())

    val issue_cmp = Vec(3, Bool())
    val br_oldest = Vec(3, Bool())

    val issue_id = UInt(wOrder.W)
    val issue_actual = Bool()
    val issue_kill = Bool()

    val fwd_ptr = UInt(wEntry.W)
    val fwd_issue = new BrjrIssue(wOrder, wEntry)
    val predict = Vec(2, new BrchjrPred(wOrder, data_width))

    val next_count = UInt(wEntry.W)
    val tail = UInt(wEntry.W)
    val bidx = UInt(wEntry.W)
    val buffered = Vec(nEntry, Vec(3, Bool()))
  })
  io.bReady := pred_ctrl.table_empty.orR || pred_ctrl.inc_head

  pred_ctrl.inc_head    := !pred_ctrl.queue_empty || pred_ctrl.branch.reduce(_||_)
  pred_ctrl.queue_empty := pred_queue.map(!_.valid).reduce(_&&_)
  pred_ctrl.table_empty := VecInit(pred_table.map(!_.valid)).asUInt

  io.bidx1H := Mux(pred_ctrl.inc_head, Mux(pred_ctrl.queue_empty, pred_ctrl.table_bid1H,
    UIntToOH(pred_ctrl.fwd_issue.bidx)), PriorityEncoderOH(pred_ctrl.table_empty))
  pred_ctrl.bidx := Mux(pred_ctrl.inc_head, Mux(pred_ctrl.queue_empty, OHToUInt(pred_ctrl.table_bid1H),
    pred_ctrl.fwd_issue.bidx), PriorityEncoder(pred_ctrl.table_empty))

  for (i <- 0 until nEntry) when (io.bidx1H(i)) {
    when (io.inc_tail) {
      pred_table(i).bits  := io.pred_i
    }
  }
  for (i <- 0 until nEntry) when (io.xcpt ||
    (CmpId(io.kill.id, pred_table(i).bits.id, io.id_head) && io.kill.valid)) {
      pred_table(i).valid := false.B
  }.elsewhen (io.bidx1H(i)) {
    when (io.inc_tail) {
      pred_table(i).valid := true.B
    }.elsewhen(pred_ctrl.inc_head) {
      pred_table(i).valid := false.B
    }
  }

  pred_ctrl.table_idcam := io.issue.map(i => VecInit(pred_table.map(t => t.bits.id === i.id && t.valid)).asUInt)
  pred_ctrl.queue_idcam := io.issue.map(i => PriorityEncoder(VecInit(pred_queue.map(q => q.id === i.id)).asUInt)) //FIXME
  pred_ctrl.table_bid1H := Mux1H(pred_ctrl.br_oldest, pred_ctrl.table_idcam)

  io.br_type := pred_ctrl.table_idcam.map(i => Mux1H(i, pred_table.map(_.bits.br_type)))
  pred_ctrl.expect := pred_ctrl.table_idcam.map(i => Mux1H(i, pred_table.map(_.bits.redirect)))
  for (j <- 0 until 3) {
    pred_ctrl.branch(j) := Mux1H(pred_ctrl.table_idcam(j), pred_table.map(_.bits.branch))  && io.issue(j).valid
    pred_ctrl.jumprg(j) := Mux1H(pred_ctrl.table_idcam(j), pred_table.map(!_.bits.branch)) && io.issue(j).valid
    pred_ctrl.actual(j) := pred_ctrl.branch(j) && io.issue(j).actual
    pred_ctrl.b_kill(j) := (pred_ctrl.expect(j) ^ io.issue(j).actual) && pred_ctrl.branch(j)
    io.val_mask(j) := (!(pred_ctrl.br_oldest(j) && pred_ctrl.queue_empty) && pred_ctrl.table_idcam(j).orR) || pred_ctrl.jumprg(j)
  }

  pred_ctrl.issue_cmp(0) := CmpId(io.issue(0).id, io.issue(1).id, io.id_head) && pred_ctrl.branch(0) || !pred_ctrl.branch(1)
  pred_ctrl.issue_cmp(1) := CmpId(io.issue(0).id, io.issue(2).id, io.id_head) && pred_ctrl.branch(0) || !pred_ctrl.branch(2)
  pred_ctrl.issue_cmp(2) := CmpId(io.issue(1).id, io.issue(2).id, io.id_head) && pred_ctrl.branch(1) || !pred_ctrl.branch(2)
  pred_ctrl.br_oldest(0) :=  pred_ctrl.issue_cmp(0) &&  pred_ctrl.issue_cmp(1)
  pred_ctrl.br_oldest(1) := !pred_ctrl.issue_cmp(0) &&  pred_ctrl.issue_cmp(2)
  pred_ctrl.br_oldest(2) := !pred_ctrl.issue_cmp(1) && !pred_ctrl.issue_cmp(2)

  pred_ctrl.issue_id     := Mux1H(pred_ctrl.br_oldest, io.issue.map(_.id))
  pred_ctrl.issue_actual := Mux1H(pred_ctrl.br_oldest, pred_ctrl.actual)
  pred_ctrl.issue_kill   := Mux1H(pred_ctrl.br_oldest, pred_ctrl.b_kill)

  pred_ctrl.queue_ptr(0):= Mux1H(pred_ctrl.br_oldest, pred_ctrl.queue_idcam)
  pred_ctrl.queue_ptr(1):= PriorityEncoder(pred_queue.map(_.valid))
  pred_ctrl.predict(0):= Mux1H(pred_ctrl.table_bid1H, pred_table).bits
  pred_ctrl.fwd_issue := pred_queue(pred_ctrl.queue_ptr(1))
  pred_ctrl.predict(1):= pred_table(pred_ctrl.fwd_issue.bidx).bits

  def updateQueue(i: Int): Unit = {
    when (pred_ctrl.buffered(i).reduce(_||_)) {
      pred_queue(i).valid  := true.B
      pred_queue(i).actual := (0 until 3).map(j => io.issue(j).actual && pred_ctrl.buffered(i)(j)).reduce(_||_)
    }
  }
  for (i <- 0 until nEntry) {
    when (io.xcpt) { pred_queue(i).valid  := false.B
    }.elsewhen (!pred_ctrl.inc_head) { updateQueue(i)
    }.otherwise{
      when (i.U < pred_ctrl.fwd_ptr) { updateQueue(i)
      }.elsewhen (i.U < pred_ctrl.tail) {
        if (i < nEntry-1) {
          when (io.kill.valid) {pred_queue(i).valid  := false.B
          }.otherwise {
            pred_queue(i).id := pred_queue(i + 1).id
            pred_queue(i).bidx := pred_queue(i + 1).bidx
            pred_queue(i).branch := pred_queue(i + 1).branch
            when (pred_ctrl.buffered(i + 1).reduce(_||_)) {
              pred_queue(i).valid  := true.B
              pred_queue(i).actual := (0 until 3).map(j => io.issue(j).actual && pred_ctrl.buffered(i+1)(j)).reduce(_||_)
            }.otherwise {
              pred_queue(i).valid  := pred_queue(i + 1).valid
              pred_queue(i).actual := pred_queue(i + 1).actual
            }
          }
        }
      }
    }
  }

  pred_ctrl.tail := (pred_count - 1.U)(wEntry-1,0)
  pred_ctrl.next_count := Mux(pred_ctrl.inc_head, pred_ctrl.tail, pred_count(wEntry-1,0))
  when (io.xcpt) {pred_count := 0.U
  }.elsewhen(io.kill.valid) { pred_count := pred_ctrl.fwd_ptr
  }.elsewhen (io.inc_tail) { when (!pred_ctrl.inc_head) {pred_count := pred_count + 1.U}
  }.otherwise {pred_count := pred_ctrl.next_count}

  when (pred_ctrl.inc_head || io.inc_tail) {
    pred_queue(pred_ctrl.next_count).valid  := false.B
  }
  when (io.inc_tail) {
    pred_queue(pred_ctrl.next_count).id     := io.pred_i.id
    pred_queue(pred_ctrl.next_count).branch := io.pred_i.branch
    pred_queue(pred_ctrl.next_count).bidx   := pred_ctrl.bidx
  }

  for (i <- 0 until nEntry; j <- 0 until 3) {
    pred_ctrl.buffered(i)(j) := io.issue(j).valid && io.issue(j).id === pred_queue(i).id &&
      (!(pred_ctrl.br_oldest(j) && pred_ctrl.queue_empty) || !pred_queue(i).branch)
    when (pred_ctrl.table_idcam(j)(i)) { //if is jalr
      when (pred_ctrl.jumprg(j)) {pred_table(i).bits.cont := io.jr_tgt(j)}
    }
  }

  io.cd_link := (0 until 3).map(j => Mux1H(pred_ctrl.table_idcam(j), pred_table.map(_.bits.cont)))

  pred_ctrl.fwd_ptr := Mux(pred_ctrl.queue_empty, pred_ctrl.queue_ptr(0), pred_ctrl.queue_ptr(1))
  io.fb_pc := Mux(pred_ctrl.queue_empty, pred_ctrl.predict(0).pc, pred_ctrl.predict(1).pc)
  io.fb_type := Mux(pred_ctrl.queue_empty, BTBType.branch.U,
                Mux(pred_ctrl.predict(1).branch, BTBType.branch.U,
                Mux(pred_ctrl.predict(1).jump(Jump.pop), BTBType.retn.U,
                Mux(pred_ctrl.predict(1).jump(Jump.none) ||
                    pred_ctrl.predict(1).jump(Jump.push), BTBType.jump.U, BTBType.invalid.U))))
  io.kill.valid :=
    Mux(pred_ctrl.queue_empty, pred_ctrl.issue_kill,
    Mux(pred_ctrl.fwd_issue.branch, pred_ctrl.fwd_issue.actual ^ pred_ctrl.predict(1).redirect,
        pred_ctrl.predict(1).tgt =/= pred_ctrl.predict(1).cont
    ))
  io.kill.id   := Mux(pred_ctrl.queue_empty, pred_ctrl.issue_id, pred_ctrl.fwd_issue.id)
  io.kill.bidx := Mux(pred_ctrl.queue_empty, OHToUInt(pred_ctrl.table_bid1H), pred_ctrl.fwd_issue.bidx)

  io.feedback.valid := pred_ctrl.inc_head
  io.feedback.bits.redirect := Mux(pred_ctrl.queue_empty, pred_ctrl.issue_actual,
    pred_ctrl.fwd_issue.actual || !pred_ctrl.fwd_issue.branch)
  io.feedback.bits.tgt := Mux(pred_ctrl.queue_empty,
    Mux(pred_ctrl.issue_actual, pred_ctrl.predict(0).tgt, pred_ctrl.predict(0).cont),
    Mux(pred_ctrl.fwd_issue.actual, pred_ctrl.predict(1).tgt, pred_ctrl.predict(1).cont)) //TODO: jalr need not actual to be true

  io.b_commit.valid := !pred_ctrl.queue_empty
  io.b_commit.bits  :=  pred_ctrl.fwd_issue.id
}