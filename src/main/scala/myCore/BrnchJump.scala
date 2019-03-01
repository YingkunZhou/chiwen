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
  val contTgt = UInt(data_width.W)
}

class BjIssue(val id_width: Int) extends Bundle {
  val id = UInt(id_width.W)
  val commit = Bool()
  val bjtype = Bool() // = 0 jalr || = 1 branch
  val actual = Bool() // = 0 cont || = 1 jump
}

class BrnchJump(val data_width: Int) extends Module with BjParam {
  val io = IO(new Bundle {
    val entry = Flipped(Decoupled(new BjEntry(wOrder, data_width)))
    val bidx  = Output(UInt(nBrchjr.W)) //alloc

    val head  = Input(UInt(wOrder.W))
    val link  = Output(Vec(3, UInt(data_width.W)))
    val issue = Input(Vec(3, new BjIssue(wOrder)))
    val jrTgt = Input(Vec(3, UInt(data_width.W)))

    val flush = Input(Bool())
    val kill  = Output(Valid(new KillInfo(wOrder, nBrchjr)))
    val feedback = Output(new Predict(data_width))
  })

  val id_head = RegNext(io.head)

  val valid = RegInit(0.U(nEntry.W))
  val table = Reg(Vec(nEntry, new BjEntry(wOrder, data_width)))
  val update_valid = Wire(2, UInt(nEntry.W))
  PriorityEncoderOH(~valid)
  io.entry.ready := (~valid).asUInt.orR
  when (io.entry.fire) {
    table(io.bidx) := io.entry
  }

  val expect = Wire(Vec(3, Bool()))
  val brnch_kill = Wire(Vec(3, Bool()))
  val isbrnch = Wire(Vec(3, Bool()))
  val bidx1H = Wire(Vec(3, Vec(nEntry, Bool())))
  for (i <- 0 until 3) {
    bidx1H(i) := table.map(e => e.id === io.issue(i).id && valid(i))
    expect(i)  := Mux1H(bidx1H(i), table.map(_.redirect))
    io.link(i) := Mux1H(bidx1H(i), table.map(_.contTgt))

    isbrnch(i) := io.issue(i).commit && io.issue(i).bjtype
    brnch_kill(i) := (expect(i) ^ io.issue(i).actual) && isbrnch(i)
  }

  val cmp = Wire(Vec(3, Bool()))
  cmp(0) := CmpId(io.issue(0).id, io.issue(1).id, id_head) && isbrnch(0) || !isbrnch(1)
  cmp(1) := CmpId(io.issue(0).id, io.issue(2).id, id_head) && isbrnch(0) || !isbrnch(2)
  cmp(2) := CmpId(io.issue(1).id, io.issue(2).id, id_head) && isbrnch(1) || !isbrnch(2)
  val oldest = Wire(Vec(3, Bool())) //find oldest
  oldest(0) :=  cmp(0) &&  cmp(1)
  oldest(1) := !cmp(0) &&  cmp(2)
  oldest(2) := !cmp(1) && !cmp(2)
  val oldest_issue = Mux1H(oldest, io.issue)
  val oldest_bid1H = Mux1H(oldest, bidx1H)
  val oldest_bidx  = OHToUInt(oldest_bid1H)

  val queue = RegInit(VecInit(Seq.fill(nEntry) {
    val w = Wire(new BjIssue(wOrder))
    w.commit := false.B
    w.actual := DontCare
    w.bjtype := DontCare
    w
  }))

  val bidxs = Reg(Vec(nEntry, UInt(wEntry.W)))
  val count = RegInit(0.U(wCount.W))
  val nEmpty: Bool = queue.map(_.commit).reduce(_||_)
  val pump_ptr = PriorityEncoder(queue.map(_.commit))
  val qhead = queue(pump_ptr)
  val qbidx = bidxs(pump_ptr)

  val commit_ptr1H = Wire(Vec(3, UInt(nEntry.W)))
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
          when (commit_ptr1H(j)(i) && (!oldest(j) || !nEmpty) && io.issue(j).commit) {
            queue(i-1) := io.issue(j) }}
      }.otherwise {
        for (j <- 0 until 3) {
          when (commit_ptr1H(j)(i) && (!oldest(j) || !nEmpty) && io.issue(j).commit) {
            queue(i) := io.issue(j) }}
      }
    }
    for (j <- 0 until 3) {
      when (io.issue(j).commit && io.issue(j).bjtype && bidx1H(j)(i)) {
        table(i).contTgt := io.jrTgt(j)
      }
    }
  }
  when (io.entry.fire) {
    bidxs(count) := io.entry.bits.id
    queue(count).commit := false.B
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

  val hjump = table(qbidx).tgt
  val hcont = table(qbidx).contTgt
  io.kill.valid := Mux(nEmpty, Mux(qhead.bjtype, hjump =/= hcont, qhead.actual ^ table(qbidx).redirect), Mux1H(oldest, brnch_kill))
  io.kill.bits.id   := Mux(nEmpty, qhead.id, oldest_issue.id)
  io.kill.bits.bidx := Mux(nEmpty, qbidx, oldest_bidx)

  io.feedback.redirect := Mux(nEmpty, qhead.actual, Mux1H(oldest, io.issue.map(i => i.actual && i.commit)))
  io.feedback.you := Mux(nEmpty, table(qbidx).you, table(oldest_bidx).you && io.issue.map(i => i.actual).reduce(_||_))
  io.feedback.typ := Mux(nEmpty, table(qbidx).typ, table(oldest_bidx).typ)
  io.feedback.tgt := Mux(nEmpty, Mux(qhead.actual, hjump, hcont), //TODO: jalr also need to care about actual
                     Mux(oldest_issue.actual, Mux1H(oldest_bid1H, table.map(_.tgt)), Mux1H(oldest, io.link)))
  io.feedback.idx := Mux(nEmpty, table(qbidx).idx, table(oldest_bidx).idx)

}