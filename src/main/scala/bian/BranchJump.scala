package bian

import chisel3._
import chisel3.util._
import common.CycRange

trait BjParam extends BackParam {
  val nEntry = nBrchjr
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class BrjrEntry(val data_width: Int) extends Bundle {
  val expect = Bool()
  val pc  = UInt(data_width.W)
  val tgt = UInt(data_width.W)
}

class BrjrIssueIO(val id_width: Int) extends Bundle {
  val valid = Bool()
  val id = UInt(id_width.W)
  val actual = Bool() // = 0 cont || = 1 jump
  val branch = Bool() //TODO: add all
}

class BrjrIssue(id_width: Int, val wEntry: Int) extends BrjrIssueIO(id_width) {
  val bidx = UInt(wEntry.W) //used to index table
}

class BranchJump extends Module with BjParam {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new BrjrEntryIO(wOrder, data_width)))
    val bid1H = Output(UInt(nEntry.W)) //alloc

    val issue = Input(Vec(3, new BrjrIssueIO(wOrder)))
    val target = Input(Vec(3, UInt(data_width.W)))
    val brtype = Output(Vec(3, UInt(BR_N.getWidth.W)))
    val cdlink = Output(Vec(3, UInt(data_width.W)))
    val mask = Output(Vec(3, Bool())) //FIXME: really need mask signal???

    val xcpt = Input(Bool())
    val kill = Output(new KillInfo(wOrder, nEntry))
    val head = Input(UInt(wOrder.W))
    val commit  = Output(Valid(UInt(wOrder.W)))

    val feedback = Output(Valid(new Predict(data_width)))
    val fb_pc   = Output(UInt(data_width.W))
    val fb_type = Output(UInt(BTBType.SZ.W))

    val cyc = Input(UInt(data_width.W))
  })

  val bj_valid = RegInit(VecInit(Seq.fill(nEntry){
    val w = Wire(Valid(UInt(wOrder.W)))
    w.valid := false.B
    w.bits := DontCare
    w
  }))
  val cd_link = Reg(Vec(nEntry, UInt(data_width.W)))
  val br_type = Reg(Vec(nEntry, UInt(BR_N.getWidth.W)))
  val bj_table = Mem(nEntry, new BrjrEntry(data_width))
  val bj_queue = Reg(Vec(nEntry, new BrjrIssue(wOrder, wEntry)))
  val bj_ctrl = Wire(new Bundle {
    val head_id = UInt(wOrder.W) /*reorder buffer head id, the first inst*/
    val issue_id = Vec(3, UInt(wOrder.W)) /*input issue id in exe stage*/
    val empty = UInt(nEntry.W) /*table empty vector*/
    val table_idcam = Vec(3, UInt(nEntry.W)) /*table id cam result*/
    def valid(i: Int): Bool = table_idcam(i).orR /*whether input issue is branch or jump inst*/
    val queue_idcam = Vec(3, UInt(nEntry.W)) /*queue id cam result*/

    val branch = Vec(3, Bool()) /*whether input issue is branch inst*/
    def branch_acc: Bool = branch.reduce(_||_) /*branch accelerate signal*/
    val wire_actual = Bool() /*the branch actual direction*/

    val pop_valid = Bool() /*whether queue is empty or not*/
    val pop_issue = new BrjrIssue(wOrder, wEntry) /*pop entry of queue*/
    val pop_qptr = UInt(wEntry.W) /*pop ptr of queue*/

    val tail = UInt(wCount.W) /*the last one position of queue*/
    val actual = Vec(nEntry, Bool()) /*the update actual bit*/
    val update = Vec(nEntry, Bool()) /*queue update vector signal*/
    val tbkill = Vec(nEntry, Bool()) /*table kill vector*/
    val tb_idx = UInt(wEntry.W)
    /*issue valid mask signal, is sequence logic not comb logic, need to push to queue first*/
    def mask(i: Int, is_branch: Bool): Bool = valid(i) && (!order(i) || pop_valid || !is_branch)

    def cmp01: Bool = CmpId(issue_id(0), issue_id(1), head_id, wOrder-1) && branch(0) || !branch(1)
    def cmp02: Bool = CmpId(issue_id(0), issue_id(2), head_id, wOrder-1) && branch(0) || !branch(2)
    def cmp12: Bool = CmpId(issue_id(1), issue_id(2), head_id, wOrder-1) && branch(1) || !branch(2)
    def order: Seq[Bool] = Seq(cmp01 && cmp02, !cmp01 && cmp12, !cmp02 && !cmp12) /*branch order*/

    val pop_entry  = new BrjrEntry(data_width) /*pop entry out of the table*/
    val push_entry = new BrjrEntry(data_width) /*push entry into the table*/
    val fwd_target = UInt(data_width.W) /*the target besides pop_entry.tgt*/
    /*entry pop out of queue and invalidate according entry of table*/
    def forward: Bool = pop_valid || branch_acc
    /*forward entry queue ptr*/
    def fwd_ptr: UInt = Mux(pop_valid, pop_qptr, OHToUInt(Mux1H(order, queue_idcam)))
    /*forward entry table idx and one hot vector*/
    def fwd_bidx: UInt  = Mux(pop_valid, pop_issue.bidx, OHToUInt(Mux1H(order, table_idcam)))
    def fwd_bid1H: UInt = Mux(pop_valid, UIntToOH(pop_issue.bidx) , Mux1H(order, table_idcam))
    /*forward inst id*/
    def fwd_id: UInt = Mux(pop_valid, pop_issue.id, Mux1H(order, issue_id))
    /*forward direction result*/
    def fwd_actual: Bool = Mux(pop_valid, pop_issue.actual || !pop_issue.branch, wire_actual)
    /*forward target*/
    def fwd_tgt: UInt = Mux(Mux(pop_valid, pop_issue.actual, wire_actual), pop_entry.tgt, fwd_target)
    /*forward kill signal*/
    def fwd_kill: Bool = Mux(pop_valid, Mux(pop_issue.branch,
      pop_issue.actual ^ pop_entry.expect,
      pop_entry.tgt =/= fwd_target),
      (wire_actual ^ pop_entry.expect) && branch_acc)
    /*forward type*/
    def fwd_type: UInt = Mux(!pop_valid || pop_issue.branch, BTBType.branch.U, BTBType.jump.U)
  })
  bj_ctrl.head_id     := io.head
  bj_ctrl.issue_id    := io.issue.map(_.id)
  bj_ctrl.empty       := VecInit(bj_valid.map(!_.valid)).asUInt
  bj_ctrl.table_idcam := io.issue.map(i => VecInit(bj_valid.map(t => t.valid && t.bits === i.id)).asUInt)
  bj_ctrl.queue_idcam := io.issue.map(i => PriorityEncoderOH(VecInit(bj_queue.map(q => q.id === i.id)).asUInt))
  bj_ctrl.pop_valid   := bj_queue.map(_.valid).reduce(_||_)
  bj_ctrl.pop_issue   := PriorityMux(bj_queue.map(_.valid), bj_queue)
  bj_ctrl.pop_qptr    := PriorityEncoder(bj_queue.map(_.valid))
  bj_ctrl.pop_entry   := bj_table(bj_ctrl.fwd_bidx)
  bj_ctrl.wire_actual := Mux1H(bj_ctrl.order, io.issue.map(_.actual))
  bj_ctrl.branch      := io.issue.map(i => i.valid && i.branch)
  bj_ctrl.fwd_target  := Mux1H(bj_ctrl.fwd_bid1H, cd_link)

  bj_ctrl.push_entry.expect := io.in.bits.redirect
  bj_ctrl.push_entry.pc     := io.in.bits.pc
  bj_ctrl.push_entry.tgt    := io.in.bits.tgt
  val bj_reg = RegInit({
    val w = Wire(new Bundle{
      val count    = UInt(wCount.W)
      val forward  = Bool()
      val fwd_kill = Bool()
      val fwd_id   = UInt(wOrder.W)
      val fwd_ptr  = UInt(wEntry.W)
      val fwd_bidx = UInt(wEntry.W)
    })
    w.count := 0.U
    w.forward  := DontCare
    w.fwd_kill := DontCare
    w.fwd_id   := DontCare
    w.fwd_ptr  := DontCare
    w.fwd_bidx := DontCare
    w
  })
  bj_reg.forward  := bj_ctrl.forward
  bj_reg.fwd_kill := bj_ctrl.fwd_kill
  bj_reg.fwd_id   := bj_ctrl.fwd_id
  bj_reg.fwd_ptr  := bj_ctrl.fwd_ptr
  bj_reg.fwd_bidx := bj_ctrl.fwd_bidx

  io.fb_pc            := bj_ctrl.pop_entry.pc
  io.fb_type          := bj_ctrl.fwd_type
  io.feedback.valid         := bj_ctrl.forward //is branch jump inst
  io.feedback.bits.redirect := bj_ctrl.fwd_actual
  io.feedback.bits.tgt      := bj_ctrl.fwd_tgt
  io.kill.valid       := bj_ctrl.fwd_kill
  io.kill.id          := bj_ctrl.fwd_id //FIXME: NOTICE: current branch jump inst not the next inst to begin killed
  io.kill.bidx        := bj_ctrl.fwd_bidx
  io.commit.valid     := bj_ctrl.pop_valid
  io.commit.bits      := bj_ctrl.pop_issue.id
  io.mask := (0 until 3).map(i => bj_ctrl.mask(i, io.issue(i).branch))
  io.brtype := bj_ctrl.table_idcam.map(i => Mux1H(i, br_type))
  io.cdlink := bj_ctrl.table_idcam.map(i => Mux1H(i, cd_link))

  io.in.ready         := bj_ctrl.empty.orR || bj_reg.forward
  io.bid1H            := Mux(bj_reg.forward, UIntToOH(bj_reg.fwd_bidx),PriorityEncoderOH(bj_ctrl.empty))

  bj_ctrl.tb_idx := Mux(bj_reg.forward, bj_reg.fwd_bidx, PriorityEncoder(bj_ctrl.empty))
  when (io.in.valid) {
    bj_table(bj_ctrl.tb_idx) := bj_ctrl.push_entry
    cd_link(bj_ctrl.tb_idx)  := io.in.bits.cont
    br_type(bj_ctrl.tb_idx)  := io.in.bits.brtype
  }

  bj_ctrl.tbkill := bj_valid.map(t => bj_reg.fwd_kill &&
    CmpId(bj_reg.fwd_id, t.bits, bj_ctrl.head_id, wOrder-1))
  for (i <- 0 until nEntry) {
    when (io.xcpt || bj_ctrl.tbkill(i)) {
      bj_valid(i).valid := false.B
    }.elsewhen (io.bid1H(i)) {
      when (io.in.valid) {
        bj_valid(i).valid := true.B
        bj_valid(i).bits := io.in.bits.id
      }.elsewhen(bj_reg.forward) {
        bj_valid(i).valid := false.B
      }
    }
  }

  bj_ctrl.tail := bj_reg.count - 1.U //no need to update bj_count using combi logic
  when (io.xcpt) {bj_reg.count := 0.U
  }.elsewhen(bj_reg.fwd_kill) {
    when (bj_ctrl.forward) { bj_reg.count := bj_reg.fwd_ptr - 1.U
    }.otherwise { bj_reg.count := bj_reg.fwd_ptr }
  }.otherwise {
    when (io.in.valid && !bj_ctrl.forward) {bj_reg.count := bj_reg.count + 1.U}
    when (!io.in.valid && bj_ctrl.forward) {bj_reg.count := bj_ctrl.tail}
  }

  def updateQueue(i: Int): Unit = {
    when (bj_ctrl.update(i)) {
      bj_queue(i).valid  := true.B
      bj_queue(i).actual := bj_ctrl.actual(i)
    }
  }
  def pushQueue(i: Int): Unit = {
    bj_queue(i).id     := io.in.bits.id
    bj_queue(i).bidx   := bj_ctrl.tb_idx
    bj_queue(i).branch := io.in.bits.branch
    bj_queue(i).valid  := false.B
  }
  def stable_queue: Seq[Bool] = (0 until nEntry).map(i => i.U  <  bj_ctrl.fwd_ptr)
  def shift_queue : Seq[Bool] = (0 until nEntry).map(i => i.U  <  bj_ctrl.tail(wEntry-1,0))
  def touch_tail  : Seq[Bool] = (0 until nEntry).map(i => i.U === bj_ctrl.tail(wEntry-1,0))
  def touch_count : Seq[Bool] = (0 until nEntry).map(i => i.U === bj_reg.count(wEntry-1,0))

  for (i <- 0 until nEntry) {
    when (bj_ctrl.forward) {
      when (touch_tail(i) && io.in.valid) {
        pushQueue(i)
      }.elsewhen (stable_queue(i)) {
        updateQueue(i)
      }.elsewhen(shift_queue(i) && !bj_ctrl.fwd_kill) { if (i < nEntry-1) {
        bj_queue(i).id     := bj_queue(i + 1).id
        bj_queue(i).bidx   := bj_queue(i + 1).bidx
        bj_queue(i).branch := bj_queue(i + 1).branch
        bj_queue(i).valid  := bj_queue(i + 1).valid
        when (bj_ctrl.update(i + 1)) {
          bj_queue(i).valid  := true.B
          bj_queue(i).actual := bj_ctrl.actual(i + 1)
        }.otherwise {
          bj_queue(i).valid  := bj_queue(i + 1).valid
          bj_queue(i).actual := bj_queue(i + 1).actual
        }
      }}.otherwise {
        bj_queue(i).valid := false.B
      }
    }.otherwise {
      when (touch_count(i) && io.in.valid) {
        pushQueue(i)
      }.otherwise {
        updateQueue(i)
      }
    }
  }

  for (i <- 0 until nEntry) {
    bj_ctrl.update(i) := (0 until 3).map(j => io.issue(j).valid && io.mask(j) && bj_ctrl.queue_idcam(j)(i)).reduce(_||_)
    bj_ctrl.actual(i) := (0 until 3).map(j => io.issue(j).valid && io.issue(j).actual && bj_ctrl.queue_idcam(j)(i)).reduce(_||_)
    for (j <- 0 until 3) {
      when (io.issue(j).valid && bj_ctrl.table_idcam(j)(i) &&
        !io.issue(j).branch) {cd_link(i) := io.target(j)}
    }
  }

  when (CycRange(io.cyc, 715, 720)) {
    printf(p"pop_valid ${bj_ctrl.pop_valid} " +
      p"pop_issue ${bj_ctrl.pop_issue}" +
      p"pop_expect ${bj_ctrl.pop_entry.expect} " +
      p"actual_vec ${bj_ctrl.actual} " +
      p"\n")
    for (i <- 0 until 3) {
      printf(
        p"issue valid ${io.issue(i).valid} " +
          p"id ${io.issue(i).id} " +
          p"branch ${io.issue(i).branch} " +
          p"actual ${io.issue(i).actual} " +
          p"brtype ${io.brtype(i)} " +
          p"hit ${bj_ctrl.valid(i)}\n")
    }
//    printf(p"output: " +
//      p"ready->${io.in.ready} " +
//      p"bid1H->${io.bid1H} " +
//      p"fb_pc->${Hexadecimal(io.fb_pc)} " +
//      p"fb_type->${io.fb_type} " +
//      p"fb_valid->${io.feedback.valid} " +
//      p"fb_redirect->${io.feedback.bits.redirect} " +
//      p"fb_tgt->${Hexadecimal(io.feedback.bits.tgt)}\n")
//
//    printf(p"output: " +
//      p"kill_val->${io.kill.valid} " +
//      p"kill_id->${io.kill.id} " +
//      p"kill_bidx->${io.kill.bidx} " +
//      p"commit_val->${io.commit.valid} " +
//      p"commit_id->${io.commit.bits}\n")
//
//    printf(p"output: mask->Vec(${io.mask(0)}, ${io.mask(1)}, ${io.mask(2)}) " +
//      p"br_type->Vec(${io.brtype(0)}, ${io.brtype(1)}, ${io.brtype(2)}) " +
//      p"cd_link->${io.cdlink(0)}" +
//      p"\n")
//    printf(p"bj_ctrl: update->${bj_ctrl.update} actual->${bj_ctrl.actual} forward->${bj_ctrl.forward} tail->${bj_ctrl.tail} " +
//      p"fwd_ptr->${bj_ctrl.fwd_ptr} fwd_kill->${bj_ctrl.fwd_kill}\n")
    printf(
      p" in_valid ${io.in.valid} " +
      p"forward ${bj_reg.forward} " +
      p"fwd_kill ${bj_reg.fwd_kill} " +
      p"fwd_id ${bj_reg.fwd_id} " +
      p"head_id ${bj_ctrl.head_id} ")
    printf(p"counter: ${bj_reg.count} table_valid:")
    for (i <- 0 until nEntry) printf(p" ${bj_valid(i).valid}->${bj_valid(i).bits}")
    printf(p" queue:")
    for (i <- 0 until nEntry) printf(p" ${bj_queue(i).valid}->${bj_queue(i).id}")
    printf("\n")
  }

//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}