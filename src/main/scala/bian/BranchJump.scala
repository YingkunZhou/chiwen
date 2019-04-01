package bian

import chisel3._
import chisel3.util._

trait BjParam extends BackParam {
  val nEntry = nBrchjr
  val wEntry = log2Ceil(nEntry)
  val wCount = log2Ceil(nEntry+1)
}

class BrjrEntryIO(val id_width: Int, data_width: Int) extends Predict(data_width) {
  val id = UInt(id_width.W)
  val pc = UInt(data_width.W)
  val cont = UInt(data_width.W)
  val jump = UInt(Jump.NUM.W)
  val branch = Bool()
  val br_type = UInt(BR_N.getWidth.W)
}

class BrjrEntry(val data_width: Int) extends Bundle {
  val expect = Bool()
  val jump = UInt(Jump.NUM.W)
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

    val target  = Input(Vec(3, UInt(data_width.W)))
    val br_type = Output(Vec(3, UInt(BR_N.getWidth.W)))
    val cd_link = Output(Vec(3, UInt(data_width.W)))

    val head_id = Input(UInt(wOrder.W))
    val xcpt = Input(Bool())
    val kill = Output(new KillInfo(wOrder, nEntry))
    val mask = Output(Vec(3, Bool())) //FIXME: really need mask signal???
    val commit = Output(Valid(UInt(wOrder.W)))

    val fb = Output(Valid(new Predict(data_width)))
    val fb_pc = Output(UInt(data_width.W))
    val fb_type = Output(UInt(BTBType.SZ.W))
    //TODO:
//    val ras_pop  = Output(Bool())
//    val ras_push = Output(Valid(UInt(data_width.W)))
  })
  val table_valid = RegInit(VecInit(Seq.fill(nEntry){
    val w = Wire(Valid(UInt(wOrder.W)))
    w.valid := false.B
    w.bits := DontCare
    w
  }))
  val cd_link = Reg(Vec(nEntry, UInt(data_width.W)))
  val br_type = Reg(Vec(nEntry, UInt(BR_N.getWidth.W)))
  val bj_table = Mem(nEntry, new BrjrEntry(data_width))
  val queue_valid = RegInit(VecInit(Seq.fill(nEntry)(false.B)))
  val bj_queue = Reg(Vec(nEntry, new BrjrIssue(wOrder, wEntry)))

  val bj_count = RegInit(0.U(wCount.W))
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
    val table_kill = Vec(nEntry, Bool()) /*table kill vector*/
    /*issue valid mask signal, is sequence logic not comb logic, need to push to queue first*/
    def mask(i: Int, is_branch: Bool): Bool = valid(i) && (!order(i) || pop_valid || !is_branch)

    def cmp01: Bool = CmpId(issue_id(0), issue_id(1), head_id) && branch(0) || !branch(1)
    def cmp02: Bool = CmpId(issue_id(0), issue_id(2), head_id) && branch(0) || !branch(2)
    def cmp12: Bool = CmpId(issue_id(1), issue_id(2), head_id) && branch(1) || !branch(2)
    def order: Seq[Bool] = Seq(cmp01 && cmp02, !cmp01 && cmp12, !cmp02 && !cmp12) /*branch order*/

    val pop_entry  = new BrjrEntry(data_width) /*pop entry out of the table*/
    val push_entry = new BrjrEntry(data_width) /*push entry into the table*/
    val fwd_target = UInt(data_width.W) /*the target besides pop_entry.tgt*/
    /*entry pop out of queue and invalidate according entry of table*/
    def forward: Bool     = pop_valid || branch_acc
    /*forward entry queue ptr*/
    def fwd_qptr: UInt    = Mux(pop_valid, pop_qptr        , OHToUInt(Mux1H(order, queue_idcam)))
    /*forward entry table idx and one hot vector*/
    def fwd_bidx: UInt    = Mux(pop_valid, pop_issue.bidx  , OHToUInt(Mux1H(order, table_idcam)))
    def fwd_bid1H: UInt   = Mux(pop_valid, UIntToOH(pop_issue.bidx) , Mux1H(order, table_idcam))
    /*forward inst id*/
    def fwd_id: UInt      = Mux(pop_valid, pop_issue.id    , Mux1H(order, issue_id))
    /*forward direction result*/
    def fwd_actual: Bool  = Mux(pop_valid, pop_issue.actual || !pop_issue.branch, wire_actual && branch_acc)
    /*forward target*/
    def fwd_tgt: UInt = Mux(Mux(pop_valid, pop_issue.actual, wire_actual), pop_entry.tgt, fwd_target)
    /*forward kill signal*/
    def fwd_kill: Bool    = Mux(pop_valid, Mux(pop_issue.branch, pop_issue.actual ^ pop_entry.expect,
      pop_entry.tgt =/= fwd_target), (wire_actual ^ pop_entry.expect) && branch_acc)
    /*forward type*/
    def fwd_type: UInt   = Mux(!pop_valid || pop_issue.branch, BTBType.branch.U,
      Mux(pop_entry.jump(Jump.none) || pop_entry.jump(Jump.push), BTBType.jump.U,
      Mux(pop_entry.jump(Jump.pop), BTBType.retn.U, BTBType.invalid.U)))
    /*insert moment alloced table idx and its one hot vector*/
    def bidx: UInt  = Mux(forward, fwd_bidx , PriorityEncoder(empty))
    def bid1H: UInt = Mux(forward, fwd_bid1H, PriorityEncoderOH(empty))
  })
  io.in.ready         := bj_ctrl.empty.orR || bj_ctrl.forward
  io.bid1H            := bj_ctrl.bid1H
  io.fb_pc            := bj_ctrl.pop_entry.pc
  io.fb_type          := bj_ctrl.fwd_type
  io.fb.valid         := bj_ctrl.forward //is branch jump inst
  io.fb.bits.redirect := bj_ctrl.fwd_actual
  io.fb.bits.tgt      := bj_ctrl.fwd_tgt
  io.kill.valid       := bj_ctrl.fwd_kill
  io.kill.id          := bj_ctrl.fwd_id
  io.kill.bidx        := bj_ctrl.fwd_bidx
  io.commit.valid     := bj_ctrl.pop_valid
  io.commit.bits      := bj_ctrl.pop_issue.id
  io.br_type := bj_ctrl.table_idcam.map(i => Mux1H(i, br_type))
  io.cd_link := bj_ctrl.table_idcam.map(i => Mux1H(i, cd_link))
  io.mask := (0 until 3).map(i => bj_ctrl.mask(i, io.issue(i).branch))

  bj_ctrl.head_id     := io.head_id
  bj_ctrl.issue_id    := io.issue.map(_.id)
  bj_ctrl.empty       := ~table_valid.asUInt
  bj_ctrl.table_idcam := io.issue.map(i => VecInit(table_valid.map(t => t.valid && t.bits === i.id)).asUInt)
  bj_ctrl.queue_idcam := io.issue.map(i => PriorityEncoder(VecInit(bj_queue.map(q => q.id === i.id)).asUInt))
  bj_ctrl.pop_valid   := queue_valid.reduce(_||_)
  bj_ctrl.pop_issue   := PriorityMux(queue_valid, bj_queue)
  bj_ctrl.pop_qptr    := PriorityEncoder(queue_valid)
  bj_ctrl.pop_entry   := bj_table(bj_ctrl.fwd_bidx)
  bj_ctrl.wire_actual := Mux1H(bj_ctrl.order, io.issue.map(_.actual))
  bj_ctrl.branch      := io.issue.map(i => i.valid && i.branch)
  bj_ctrl.fwd_target  := Mux1H(bj_ctrl.fwd_bid1H, cd_link)

  bj_ctrl.push_entry.expect := io.in.bits.redirect
  bj_ctrl.push_entry.pc     := io.in.bits.pc
  bj_ctrl.push_entry.jump   := io.in.bits.jump
  bj_ctrl.push_entry.tgt    := io.in.bits.tgt
  when (io.in.valid) {
    bj_table(bj_ctrl.bidx) := bj_ctrl.push_entry
    cd_link(bj_ctrl.bidx)  := io.in.bits.cont
    br_type(bj_ctrl.bidx)  := io.in.bits.br_type
  }

  bj_ctrl.table_kill := table_valid.map(t => bj_ctrl.fwd_kill &&
    CmpId(bj_ctrl.fwd_id, t.bits, bj_ctrl.head_id))
  for (i <- 0 until nEntry) {
    when (io.xcpt || bj_ctrl.table_kill(i)) {
      table_valid(i).valid := false.B
    }.elsewhen (bj_ctrl.bid1H(i)) {
      when (io.in.valid) {
        table_valid(i).valid := true.B
        table_valid(i).bits := io.in.bits.id
      }.elsewhen(bj_ctrl.forward) {
        table_valid(i).valid := false.B
      }
    }
  }

  bj_ctrl.tail := bj_count - 1.U
  when (io.xcpt) {bj_count := 0.U
  }.elsewhen(bj_ctrl.fwd_kill) {bj_count := bj_ctrl.fwd_qptr
  }.otherwise {
    when (io.in.valid && !bj_ctrl.forward) {bj_count := bj_count + 1.U}
    when (!io.in.valid && bj_ctrl.forward) {bj_count := bj_ctrl.tail}
  }

  def updateQueue(i: Int): Unit = {
    when (bj_ctrl.update(i)) {
      queue_valid(i) := true.B
      bj_queue(i).actual := bj_ctrl.actual(i)
    }
  }
  def stable_queue: Seq[Bool] = (0 until nEntry).map(i => i.U  <  bj_ctrl.fwd_qptr)
  def shift_queue : Seq[Bool] = (0 until nEntry).map(i => i.U  <  bj_ctrl.tail(wEntry-1,0))
  def touch_tail  : Seq[Bool] = (0 until nEntry).map(i => i.U === bj_ctrl.tail(wEntry-1,0))
  def touch_count : Seq[Bool] = (0 until nEntry).map(i => i.U === bj_count(wEntry-1,0))
  for (i <- 0 until nEntry) {
    when (bj_ctrl.forward) {
      when (touch_tail(i) && io.in.valid) {
        bj_queue(i).id     := io.in.bits.id
        bj_queue(i).bidx   := bj_ctrl.bidx
        bj_queue(i).branch := io.in.bits.branch
        bj_queue(i).valid  := false.B
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
        bj_queue(i).id     := io.in.bits.id
        bj_queue(i).bidx   := bj_ctrl.bidx
        bj_queue(i).branch := io.in.bits.branch
        bj_queue(i).valid  := false.B
      }.otherwise {
        updateQueue(i)
      }
    }
  }

  for (i <- 0 until nEntry) {
    bj_ctrl.update(i) := (0 until 3).map(j => io.issue(j).valid && io.mask(j) && bj_ctrl.queue_idcam(j)(i)).reduce(_||_)
    bj_ctrl.actual(i) := (0 until 3).map(j => io.issue(j).actual && bj_ctrl.queue_idcam(j)(i)).reduce(_||_)
    for (j <- 0 until 3) {
      when (io.issue(j).valid && bj_ctrl.table_idcam(j)(i) &&
        !io.issue(j).branch) {cd_link(i) := io.target(j)}
    }
  }
  val cnt = RegInit(0.U(32.W))
  cnt := cnt + 1.U
  printf(p"=======================cnt = $cnt=============================\n")
}