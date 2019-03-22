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

  val predTable = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(Valid(new BrchjrPred(wOrder, data_width)))
    w.valid := false.B
    w.bits := DontCare
    w
  }))
  val predQueue = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(new BrjrIssue(wOrder, wEntry))
    w.valid  := false.B
    w.id     := DontCare
    w.actual := DontCare
    w.branch := DontCare
    w.bidx   := DontCare
    w
  }))
  val predCount = RegInit(0.U(wCount.W))
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
    val buffered = Vec(nEntry, Vec(3, Bool()))
  })
  io.bReady := pred_ctrl.table_empty.orR || pred_ctrl.inc_head

  pred_ctrl.inc_head    := !pred_ctrl.queue_empty || pred_ctrl.branch.reduce(_||_)
  pred_ctrl.queue_empty := predQueue.map(!_.valid).reduce(_&&_)
  pred_ctrl.table_empty := VecInit(predTable.map(!_.valid)).asUInt

  io.bidx1H := Mux(pred_ctrl.inc_head, Mux(pred_ctrl.queue_empty, pred_ctrl.table_bid1H,
    UIntToOH(pred_ctrl.fwd_issue.bidx)), PriorityEncoderOH(pred_ctrl.table_empty))

  for (i <- 0 until nEntry) when (io.bidx1H(i)) {
    when (io.inc_tail) {
      predTable(i).bits  := io.pred_i
    }
  }
  for (i <- 0 until nEntry) when (io.xcpt ||
    (CmpId(io.kill.id, predTable(i).bits.id, io.id_head) && io.kill.valid)) {
      predTable(i).valid := false.B
  }.elsewhen (io.bidx1H(i)) {
    when (io.inc_tail) {
      predTable(i).valid := true.B
    }.elsewhen(pred_ctrl.inc_head) {
      predTable(i).valid := false.B
    }
  }

  pred_ctrl.next_count := Mux(pred_ctrl.inc_head, predCount - 1.U, predCount)
  when (io.xcpt) {predCount := 0.U
  }.elsewhen(io.kill.valid) { predCount := pred_ctrl.fwd_ptr
  }.elsewhen (io.inc_tail) { when (!pred_ctrl.inc_head) {predCount := predCount + 1.U}
  }.otherwise {predCount := pred_ctrl.next_count}

  when (io.inc_tail) {
    predQueue(pred_ctrl.next_count).valid := false.B
    predQueue(pred_ctrl.next_count).id := io.pred_i.id
    predQueue(pred_ctrl.next_count).branch := io.pred_i.branch
  }

  pred_ctrl.table_idcam := io.issue.map(i => VecInit(predTable.map(t => t.bits.id === i.id && t.valid)).asUInt)
  pred_ctrl.queue_idcam := io.issue.map(i => PriorityEncoder(VecInit(predQueue.map(q => q.id === i.id)).asUInt)) //FIXME
  pred_ctrl.table_bid1H := Mux1H(pred_ctrl.br_oldest, pred_ctrl.table_idcam)

  io.br_type := pred_ctrl.table_idcam.map(i => Mux1H(i, predTable.map(_.bits.br_type)))
  pred_ctrl.expect := pred_ctrl.table_idcam.map(i => Mux1H(i, predTable.map(_.bits.redirect)))
  for (j <- 0 until 3) {
    pred_ctrl.branch(j) := Mux1H(pred_ctrl.table_idcam(j), predTable.map(_.bits.branch))  && io.issue(j).valid
    pred_ctrl.jumprg(j) := Mux1H(pred_ctrl.table_idcam(j), predTable.map(!_.bits.branch)) && io.issue(j).valid
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
  pred_ctrl.queue_ptr(1):= PriorityEncoder(predQueue.map(_.valid))
  pred_ctrl.predict(0):= Mux1H(pred_ctrl.table_bid1H, predTable).bits
  pred_ctrl.fwd_issue := predQueue(pred_ctrl.queue_ptr(1))
  pred_ctrl.predict(1):= predTable(pred_ctrl.fwd_issue.bidx).bits

  for (i <- 0 until nEntry) {
    when (io.xcpt) {
      predQueue(i).valid  := true.B
    }.elsewhen (pred_ctrl.fwd_ptr < i.U && i.U < predCount) {
      if (i > 0) {
        when (io.kill.valid) {
          predQueue(i-1).valid  := false.B
        }.elsewhen (pred_ctrl.inc_head) {
          predQueue(i-1).id := predQueue(i).id
          predQueue(i-1).bidx := predQueue(i).bidx
          predQueue(i-1).branch := predQueue(i).branch
          when (pred_ctrl.buffered(i).reduce(_||_)) {
            predQueue(i-1).valid  := true.B
            predQueue(i-1).actual := (0 until 3).map(j => io.issue(j).actual && pred_ctrl.buffered(i)(j)).reduce(_||_)
          }.otherwise {
            predQueue(i-1).valid  := predQueue(i).valid
            predQueue(i-1).actual := predQueue(i).actual
          }
        }
      }
    }.elsewhen (pred_ctrl.buffered(i).reduce(_||_)) {
      predQueue(i).valid  := true.B
      predQueue(i).actual := (0 until 3).map(j => io.issue(j).actual && pred_ctrl.buffered(i)(j)).reduce(_||_)
    }

    for (j <- 0 until 3) {
      pred_ctrl.buffered(i)(j) := io.issue(j).valid && io.issue(j).id === predQueue(i).id &&
        (!(pred_ctrl.br_oldest(j) && pred_ctrl.queue_empty) || !predQueue(i).branch)
      when (pred_ctrl.table_idcam(j)(i)) { //if is jalr
        when (pred_ctrl.jumprg(j)) {predTable(i).bits.cont := io.jr_tgt(j)}
      }
    }
  }
  io.cd_link := (0 until 3).map(j => Mux1H(pred_ctrl.table_idcam(j), predTable.map(_.bits.cont)))

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