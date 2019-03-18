package myCore

import chisel3._
import chisel3.util._

trait BjParam extends Pram {
  val nEntry = nBrchjr
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
  val in = 0
  val out = 1
}

class BjEntry(val id_width: Int, data_width: Int) extends Predict(data_width) {
  val id = UInt(id_width.W)
  val cont = UInt(data_width.W)
  val jump = UInt(Jump.SZ.W)
  val branch = Bool()
  val br_type = UInt(BR_N.getWidth.W)
}

class BjIssue(val id_width: Int) extends Bundle {
  val id = UInt(id_width.W)
  val valid = Bool()
  val actual = Bool() // = 0 cont || = 1 jump
}

class BrnchJump(val data_width: Int) extends Module with BjParam {
  val io = IO(new Bundle {
    // FIXME: attention stall case
    val entry = Flipped(Decoupled(new BjEntry(wOrder, data_width))) //TODO io.entry.bits.brtype === BR_JR
    val bidx1H  = Output(UInt(nBrchjr.W)) //alloc

    val id_head = Input(UInt(wOrder.W))
    val link  = Output(UInt(data_width.W)) // TODO: only for IssueQueue not for InstQueue
    val issue = Input(Vec(3, new BjIssue(wOrder)))
    val br_type = Output(Vec(3, UInt(BR_N.getWidth.W)))
    val jrTgt = Input(Vec(3, UInt(data_width.W)))

    val flush = Input(Bool()) // TODO
    val kill  = Output(Valid(new KillInfo(wOrder, nBrchjr)))
    val feedback = Output(new Predict(data_width))
    val rollback = Output(Valid(UInt(wOrder.W)))
    // TODO: commit logic
  })

  val valid = RegInit(0.U(nEntry.W))
  val table = Reg(Vec(nEntry, new BjEntry(wOrder, data_width)))
  val update_valid = Wire(Vec(2, UInt(nEntry.W)))

  val expect = Wire(Vec(3, Bool()))
  val brnch_kill = Wire(Vec(3, Bool()))
  val isbrnch = Wire(Vec(3, Bool()))
  val bidx1H = Wire(Vec(3, Vec(nEntry, Bool())))
  val commit = Wire(Vec(3, Bool()))
  val commit_ptr1H = Wire(Vec(3, UInt(nEntry.W)))
  val bjtype = Wire(Vec(3, Bool()))

  val queue = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(new BjIssue(wOrder))
    w.valid := false.B
    w.actual := DontCare
    w.id := DontCare
    w
  }))

  io.link := Mux1H(bidx1H(2), table.map(_.contTgt)) //FIXME
  for (i <- 0 until 3) {
    bidx1H(i)     := table.map(e => e.id === io.issue(i).id && valid(i))
    expect(i)     := Mux1H(bidx1H(i), table.map(_.redirect))
    io.br_type(i) := Mux1H(bidx1H(i), table.map(_.brtype))
    bjtype(i)     := Mux1H(bidx1H(i), table.map(_.bjtype))
    isbrnch(i)    := io.issue(i).valid && bidx1H(i).reduce(_||_) && bjtype(i)
    brnch_kill(i) := (expect(i) ^ io.issue(i).actual) && isbrnch(i)
  }

  val cmp = Wire(Vec(3, Bool()))
  cmp(0) := CmpId(io.issue(0).id, io.issue(1).id, io.id_head) && isbrnch(0) || !isbrnch(1)
  cmp(1) := CmpId(io.issue(0).id, io.issue(2).id, io.id_head) && isbrnch(0) || !isbrnch(2)
  cmp(2) := CmpId(io.issue(1).id, io.issue(2).id, io.id_head) && isbrnch(1) || !isbrnch(2)
  val oldest = Wire(Vec(3, Bool())) //find oldest
  oldest(0) :=  cmp(0) &&  cmp(1)
  oldest(1) := !cmp(0) &&  cmp(2)
  oldest(2) := !cmp(1) && !cmp(2)
  val oldest_issue = Mux1H(oldest, io.issue)
  val oldest_bid1H = Mux1H(oldest, bidx1H)
  val oldest_bidx  = OHToUInt(oldest_bid1H)

  val bidxs = Reg(Vec(nEntry, UInt(wEntry.W)))
  val count = RegInit(0.U(wCount.W))
  val nEmpty: Bool = queue.map(_.valid).reduce(_||_) //some of approximate
  val pump_ptr = PriorityEncoder(queue.map(_.valid))
  val qhead = queue(pump_ptr)
  val qbidx = bidxs(pump_ptr)

  for (i <- 0 until 3) {
    commit_ptr1H := PriorityEncoderOH(VecInit(bidxs.map(_ === io.issue(i).id)).asUInt)
  }
  val forward_ptr = Mux(nEmpty, pump_ptr, OHToUInt(Mux1H(oldest, commit_ptr1H)))

  val issue_fire: Bool = nEmpty || isbrnch.reduce(_||_)

  for (i <- 0 until nEntry) {
    when (forward_ptr < i.U && i.U < count) {
      when (issue_fire) {
        bidxs(i-1) := bidxs(i)
        for (j <- 0 until 3) {
          queue(i-1).id := queue(i).id
          when (commit_ptr1H(j)(i) && (!oldest(j) || !nEmpty) && io.issue(j).valid) {
            queue(i-1).valid := true.B
            queue(i-1).actual := io.issue(j).actual
          }.otherwise {
            queue(i-1).valid := queue(i).valid
            queue(i-1).actual := queue(i).actual
          }}
      }.otherwise {
        for (j <- 0 until 3) {
          when (commit_ptr1H(j)(i) && (!oldest(j) || !nEmpty) && io.issue(j).valid) {
            queue(i).valid := true.B
            queue(i).actual := io.issue(j).actual
          }}
      }
    }
    for (j <- 0 until 3) {
      when (io.issue(j).valid && bjtype(j) && bidx1H(j)(i)) { //if is jalr
        table(i).contTgt := io.jrTgt(j)
      }
    }
  }

  when (io.entry.fire) { // rename alloc
    table(io.bidx) := io.entry
    bidxs(count) := io.bidx
    queue(count).id := io.entry.bits.id
  }

  when (io.entry.fire || issue_fire) {
    queue(count).valid := false.B
  }

  when(io.entry.fire && !issue_fire) {
    count := count + 1.U
    valid := valid | PriorityEncoderOH(~valid)
  }

  when(issue_fire && !io.entry.fire) {
    valid := valid & (~Mux(nEmpty, UIntToOH(qbidx), oldest_bid1H).asUInt).asUInt
    count := count - 1.U
  }

  io.bidx  := Mux(issue_fire, Mux(nEmpty, qbidx, oldest_bidx), PriorityEncoder(~valid))
  io.entry.ready := (~valid).asUInt.orR || issue_fire
  val entry = table(qbidx)
  val hjump = entry.tgt
  val hcont = entry.contTgt
  // kill valid also is mispredict
  io.kill.valid := Mux(nEmpty, Mux(entry.bjtype, hjump =/= hcont, qhead.actual ^ table(qbidx).redirect), Mux1H(oldest, brnch_kill))
  io.kill.bits.id   := Mux(nEmpty, qhead.id, oldest_issue.id)
  io.kill.bits.bidx := Mux(nEmpty, qbidx, oldest_bidx)

  io.feedback.redirect := Mux(nEmpty, qhead.actual, Mux1H(oldest, isbrnch))
  io.feedback.you := Mux(nEmpty, entry.you, table(oldest_bidx).you && io.issue.map(i => i.actual).reduce(_||_))
  io.feedback.typ := Mux(nEmpty, entry.typ, table(oldest_bidx).typ)
  io.feedback.tgt := Mux(nEmpty, Mux(qhead.actual, hjump, hcont), //TODO: jalr also need to care about actual
                     Mux(oldest_issue.actual, Mux1H(oldest_bid1H, table.map(_.tgt)),
                                              Mux1H(oldest_bid1H, table.map(_.contTgt))))
  io.feedback.idx := Mux(nEmpty, entry.idx, table(oldest_bidx).idx)

}