package bian

import chisel3._
import chisel3.util._
import common.{CycRange, MemPortIo}

object Above {
  def apply(num: UInt, w: Int): UInt = VecInit((0 until w).map(_.U >= num)).asUInt
}

object Below {
  def apply(num: UInt, w: Int): UInt = VecInit((0 until w).map(_.U < num)).asUInt
}

trait LsParam extends BackParam {
  val nEntry = 8
  val nLoad  = 6
  val nStore = 6
  require(isPow2(nEntry))
  require(isPow2(nLoad)  || nLoad == 6)
  require(isPow2(nStore) || nStore == 6)
  val wEntry = log2Ceil(nEntry)
  val wLoad  = log2Ceil(nLoad)
  val wStore = log2Ceil(nStore)
  val LD = 0
  val ST = 1
  require(LD == 0)
  require(ST == 1)
}

class QueuePriority(val nEntry: Int) extends Bundle {
  val const = UInt(nEntry.W)
  def valid(v: UInt): Bool = (const & v).orR
}
class QueueBackward(nEntry: Int) extends QueuePriority(nEntry) {
  def ptr(continue: Bool, head: Bool, above: UInt, below: UInt): UInt = //TODO
    Mux(continue || (above & const).orR,
      Cat( head, PriorityEncoder(above & const)),
      Cat(!head, PriorityEncoder(below & const)))
  def ptr(continue: Bool, above: UInt, below: UInt): UInt = //TODO
    PriorityEncoder(Mux(continue || (above & const).orR, above & const, below & const))
}
class QueueForward(nEntry: Int) extends QueuePriority(nEntry) {
  def ptr1H(continue: Bool, above: UInt, below: UInt): UInt =
    Reverse(PriorityEncoderOH(Reverse(
      Mux(continue || (below & const).orR, below & const, above & const)))).asUInt
}

class LsEntry(val id_width: Int, val addr_width: Int) extends Bundle {
  val id   = UInt(id_width.W)
  val typ  = UInt(MT_X.getWidth.W)
  val rd   = UInt(addr_width.W)
  val fcn  = UInt(M_X.getWidth.W)
}

class LsIssue(val id_width: Int, val data_width: Int) extends Bundle { // from ALU
  val valid = Bool()
  val id = UInt(id_width.W)
  val addr = UInt(data_width.W)
  val data = UInt(data_width.W)
  val data_ok = Bool()
}

class T2QCtrl(val n: Int) extends Bundle {
  def w: Int = log2Ceil(n)
  val inc_tail = Vec(2, Bool())
  val nxt_tail = Vec(2, UInt((w+1).W))
  val kill_ptr = UInt((w+1).W)
  val kill = new QueueBackward(n)
}

class LSQCtrl(n: Int) extends T2QCtrl(n) {
  val push = Vec(2, Bool())
  val inc_head = Vec(2, Bool())
  val nxt_head = Vec(2, UInt((w+1).W))
  val kill_head = Vec(2, Bool())
}

class StQCtrl(n: Int) extends T2QCtrl(n) {
  val inc_head = Bool()
  val nxt_head = UInt((w+1).W)
}

class LdqCtrl(n: Int) extends StQCtrl(n) {
  val head_next = new QueueBackward(n)
  val kill_head = Bool()
  val capty_gt = Vec(2, Bool()) //for accelerate
}

class H2T2RingQueue(val nEntry: Int) extends Bundle {
  def w: Int = log2Ceil(nEntry)
  val head  = Vec(2, UInt((w+1).W))
  val tail  = Vec(2, UInt((w+1).W))
  def above: UInt = Above(head(0)(w-1,0), nEntry)
  def below: UInt = Below(tail(0)(w-1,0), nEntry)
  def conti: Bool = head(0)(w) === tail(0)(w)
  def split: Bool = head(0)(w) =/= tail(0)(w)
  def valid: UInt = Mux(conti, above & below, above | below)
  def head_val0: Bool = head(0) =/= tail(0)
  def head_val1: Bool = head(1) =/= tail(0) //based on head_val0
  def tail_val0: Bool = head(0)(w-1,0) =/= tail(0)(w-1,0) || head(0)(w) === tail(0)(w)
  def tail_val1: Bool = head(0)(w-1,0) =/= tail(1)(w-1,0) //based on tail_val0
}

class LSQueue(nEntry: Int, val id_width: Int, val addr_width: Int)
  extends H2T2RingQueue(nEntry) {
  val entry = Vec(nEntry, new LsEntry(id_width, addr_width))
  def read(i: Int): LsEntry = entry(head(i)(w-1,0))
  def write(i: Int, et: LsEntry): Unit = { entry(tail(i)(w-1,0)) := et }
}

class H1T2RingQueue(val nEntry: Int) extends Bundle {
  def w: Int = log2Ceil(nEntry)
  val head  = UInt((w+1).W)
  val tail  = Vec(2, UInt((w+1).W))
  def head_val: Bool  = head =/= tail(0)
  def tail_val0: Bool = head(w-1,0) =/= tail(0)(w-1,0) || head(w) === tail(0)(w)
  def tail_val1: Bool = head(w-1,0) =/= tail(1)(w-1,0) //based on tail_val0
  def above: UInt = Above(head(w-1,0), nEntry)
  def below: UInt = Below(tail(0)(w-1,0), nEntry)
  def conti: Bool = head(w) === tail(0)(w)
  def valid: UInt = Mux(conti, above & below, above | below)
}

class LoadStoreQueue(nEntry: Int, val id_width: Int, val data_width: Int, val wPtr: Int)
  extends H1T2RingQueue(nEntry) {
  val addr_ok = Vec(nEntry, Bool())
  val data_ok = Vec(nEntry, Bool())
  val ls_addr = Vec(nEntry, UInt(data_width.W))
  val ls_id = Vec(nEntry, UInt(id_width.W))
  val ls_typ = Vec(nEntry, UInt(MT_X.getWidth.W))
  val sl_ptr = Vec(nEntry, UInt(wPtr.W))
  def ls_above: UInt = above & addr_ok.asUInt
  def ls_below: UInt = below & addr_ok.asUInt
  def ls_valid: Bool =(valid & addr_ok.asUInt).orR
  def ls_ptr: UInt = PriorityEncoder(Mux(conti || ls_above.orR, ls_above, ls_below))
  def id: UInt = ls_id(ls_ptr)
  def typ: UInt = ls_typ(ls_ptr)
  def addr: UInt = ls_addr(ls_ptr)
  def byte_en(j: Int): UInt = VecInit((0 until nEntry).map(i => ls_typ(i) === MT_W ||
    (ls_typ(i) === MT_B && ls_addr(i)(1,0) === j.U) ||
    (ls_typ(i) === MT_H && ls_addr(i)(1) === (j/2).U))).asUInt
}

class LoadQueue(val nLoad: Int, id_width: Int, val addr_width: Int, data_width: Int, val nStore: Int)
  extends LoadStoreQueue(nEntry = nLoad, id_width, data_width, wPtr = log2Ceil(nStore)+1) {
  val wbaddr = Vec(nLoad, UInt(addr_width.W))
  val unsafe = Vec(nLoad, Vec(nStore, Bool()))
  def push(i: Int, id: UInt, rd: UInt, typ: UInt, stq_ptr: UInt): Unit = {
    addr_ok(tail(i)(w-1,0)) := false.B
    data_ok(tail(i)(w-1,0)) := false.B
    ls_id(tail(i)(w-1,0))  := id
    ls_typ(tail(i)(w-1,0)) := typ
    sl_ptr(tail(i)(w-1,0)) := stq_ptr
    wbaddr(tail(i)(w-1,0)) := rd
  }
  def wb_addr: UInt = wbaddr(ls_ptr)
  def stq_ptr: UInt = sl_ptr(ls_ptr)
  def undone: UInt = VecInit((0 until nLoad).map(i => !data_ok(i) || unsafe(i).reduce(_||_))).asUInt
  def finish: Bool = !undone(head(w-1,0)) && head_val
  def addr_eq(addr: UInt, head: UInt): UInt =
    data_ok.asUInt & VecInit(unsafe.map(_(head))).asUInt &
    VecInit(ls_addr.map(_(data_width-1,2) === addr)).asUInt
}

class StoreQueue(val nStore: Int, id_width: Int, data_width: Int, val nLoad: Int)
  extends LoadStoreQueue(nEntry = nStore, id_width, data_width, wPtr = log2Ceil(nLoad)+1) {
  val data = Vec(data_width/8, Vec(nStore, UInt(8.W)))
  val arready = Vec(nStore, Bool())
  def push(i: Int, id: UInt, typ: UInt, ldq_ptr: UInt): Unit = {
    addr_ok(tail(i)(w-1,0)) := false.B
    data_ok(tail(i)(w-1,0)) := false.B
    arready(tail(i)(w-1,0)) := false.B
    ls_id(tail(i)(w-1,0))   := id
    ls_typ(tail(i)(w-1,0))  := typ
    sl_ptr(tail(i)(w-1,0))  := ldq_ptr
  }
  def ldq_ptr: UInt = sl_ptr(head(w-1,0))
  def addr_eq(addr: UInt): UInt = arready.asUInt &
    VecInit(ls_addr.map(_(data_width-1,2) === addr)).asUInt
  def head_id: UInt = ls_id(head(w-1,0))
  def head_typ: UInt = ls_typ(head(w-1,0))
  def head_addr_ok: Bool = arready(head(w-1,0))
  def head_data_ok: Bool = data_ok(head(w-1,0)) && head_val
  def head_addr: UInt = ls_addr(head(w-1,0))
  def head_data: UInt = Cat(data(3)(head(w-1,0)),data(2)(head(w-1,0)),data(1)(head(w-1,0)),data(0)(head(w-1,0)))
}

class LoadStore extends Module with LsParam {
  val io = IO(new Bundle {
    val in  = Vec(nInst, Flipped(DecoupledIO(new LsEntry(wOrder, wPhyAddr))))
    val mem_en = Input(Bool())
    val mem_first = Input(Vec(2, Bool())) //TODO: load or store within raw valid

    val issue = Input(Vec(3, new LsIssue(wOrder, data_width)))
    val issueable = Output(Valid(UInt(wOrder.W))) //TODO: have to maintain the order
    val limit = Output(Valid(UInt(wOrder.W)))
    val forward = Output(new ByPass(wPhyAddr)) //forward to inst queue

    val mem = new MemPortIo(data_width)
    val ldcommit = Output(new Commit(wOrder, wPhyAddr))
    val stcommit = Output(Valid(UInt(wOrder.W)))
    val wb_data  = Output(UInt(data_width.W))

    val xcpt = Input(Bool())
    val kill = Input(Valid(UInt(wOrder.W)))
    val head = Input(UInt(wOrder.W))
    val rollback = Output(Valid(UInt(wOrder.W))) //the role same as except
    val cyc = Input(UInt(data_width.W))
  })
  /*TODO List:
  * 1. need to accelerate rollback time???
  * 2. optimized following sequence instruction flow
  *    inst: sw      ra, 4(sp)
  *    inst: lw      t5, 4(sp)
  * 3. if full bypass load, let it go no need to wait for memory data back
  * */
  def next(ptr: UInt, n: Int, inc: Int): UInt = {
    require(inc < 3)
    if (isPow2(n)) ptr + inc.U
    else { require(n == 6)
      if (inc == 1) Mux(ptr(2,0) === (n-1).U, Cat(!ptr(3), 0.U(3.W)), ptr+1.U)
      else Mux(ptr(2).toBool, Cat(!ptr(3), 0.U(1.W), ptr(1,0)), ptr+2.U)
    }
  }
  /*=================data structure===================*/
  val queue = RegInit({
    val w = Wire(new LSQueue(nEntry = nEntry,
      id_width = wOrder, addr_width = wPhyAddr))
    w.head  := Seq(0.U, 1.U)
    w.tail  := Seq(0.U, 1.U)
    w.entry := DontCare
    w
  })
  val queue_ctrl = Wire(new LSQCtrl(nEntry))
  queue_ctrl.nxt_head := queue.head.map(_+2.U)
  queue_ctrl.nxt_tail := queue.tail.map(_+2.U)
  queue_ctrl.kill.const := VecInit(queue.entry.map(q => CmpId(io.kill.bits, q.id, io.head, wOrder-1))).asUInt
  queue_ctrl.kill_ptr := queue_ctrl.kill.ptr(
    continue = queue.conti, head = queue.head(0)(wEntry),
    above = queue.above, below = queue.below)
  queue_ctrl.kill_head := queue.head.map(h => queue_ctrl.kill.const(h(wEntry-1,0)) && io.kill.valid)
  when (io.xcpt) {
    queue.tail := queue.head
  }.elsewhen(io.kill.valid && queue_ctrl.kill.valid(queue.valid)) {
    queue.tail := Seq(queue_ctrl.kill_ptr, queue_ctrl.kill_ptr+1.U)
  }.elsewhen (queue_ctrl.inc_tail(0)) {
    when(queue_ctrl.inc_tail(1)) {
      queue.tail := queue_ctrl.nxt_tail
    }.otherwise {
      queue.tail := Seq(queue.tail(1), queue_ctrl.nxt_tail(0))
    }
  }
  when (!io.xcpt && !queue_ctrl.kill_head(0) && queue_ctrl.inc_head(0)) {
    when(!queue_ctrl.kill_head(1) && queue_ctrl.inc_head(1)) {
      queue.head := queue_ctrl.nxt_head
    }.otherwise {
      queue.head := Seq(queue.head(1), queue_ctrl.nxt_head(0))
    }
  }
  val load_queue = RegInit({
    val w = Wire(new LoadQueue(nLoad = nLoad, id_width = wOrder,
      addr_width = wPhyAddr, data_width = data_width, nStore = nStore))
    w.head := 0.U //head of queue
    w.tail := Seq(0.U, 1.U) //tail of queue
    w.addr_ok := DontCare //whether address is ok
    w.data_ok := DontCare //whether data is back
    w.ls_addr := DontCare //memory address
    w.ls_id   := DontCare //memory inst id
    w.ls_typ  := DontCare //memory inst type
    w.sl_ptr  := DontCare //the other queue pointer
    w.wbaddr  := DontCare //load inst write back regfile address
    w.unsafe  := DontCare //load inst unsafe status
    w
  })
  val load_ctrl = Wire(new LdqCtrl(nLoad))
  load_ctrl.capty_gt(0) := load_queue.tail_val0 || load_ctrl.inc_head
  load_ctrl.capty_gt(1) := Mux(load_ctrl.inc_head, load_queue.tail_val0, load_queue.tail_val1)
  load_ctrl.nxt_tail := load_queue.tail.map(next(_, nLoad, 2))
  load_ctrl.kill.const := VecInit(load_queue.ls_id.map(CmpId(io.kill.bits, _, io.head, wOrder-1))).asUInt
  load_ctrl.kill_ptr := load_ctrl.kill.ptr(
    continue = load_queue.conti, head = load_queue.head(wLoad),
    above = load_queue.above, below = load_queue.below)
  load_ctrl.kill_head := load_ctrl.kill.const(load_queue.head(wLoad-1,0)) && io.kill.valid

  when (io.xcpt) {
    load_queue.tail := Seq(load_queue.head, next(load_queue.head, nLoad, 1))
  }.elsewhen (io.kill.valid && load_ctrl.kill.valid(load_queue.valid)) {
    load_queue.tail := Seq(load_ctrl.kill_ptr, next(load_ctrl.kill_ptr, nLoad, 1))
  }.elsewhen (load_ctrl.inc_tail(0)) {
    when (load_ctrl.inc_tail(1)) {
      load_queue.tail := Seq(load_ctrl.nxt_tail(0), load_ctrl.nxt_tail(1))
    }.otherwise {
      load_queue.tail := Seq(load_queue.tail(1), load_ctrl.nxt_tail(0))
    }
  }
  when (!io.xcpt && !load_ctrl.kill_head && load_ctrl.inc_head) {
    load_queue.head := load_ctrl.nxt_head
  }

  load_ctrl.inc_head := load_queue.finish
  load_ctrl.head_next.const := load_queue.undone
  load_ctrl.nxt_head := Mux(load_ctrl.head_next.valid(load_queue.valid),
    load_ctrl.head_next.ptr(continue = load_queue.conti, head = load_queue.head(wLoad),
      above = load_queue.above, below = load_queue.below), load_queue.tail(0)) //if touch tail, means clear all

  val store_queue = RegInit({
    val w = Wire(new StoreQueue(nStore = nStore,
      id_width = wOrder, data_width = data_width, nLoad = nLoad))
    w.head  := 0.U  //same as load queue
    w.tail  := Seq(0.U, 1.U) //same as load queue
    w.addr_ok := DontCare //same as load queue
    w.data_ok := DontCare //whether write back to memory data is ready
    w.ls_addr := DontCare //same as load queue
    w.ls_id   := DontCare //same as load queue
    w.ls_typ  := DontCare //same as load queue
    w.sl_ptr  := DontCare //same as load queue
    w.arready := DontCare //same as addr_ok but don't invalidate when memory accessing
    w.data    := DontCare //write back to memory data
    w
  })
  val store_ctrl = Wire(new StQCtrl(nStore))
  store_ctrl.nxt_head := next(store_queue.head, nStore, 1)
  store_ctrl.nxt_tail := store_queue.tail.map(next(_, nStore, 2))
  store_ctrl.kill.const := VecInit(store_queue.ls_id.map(CmpId(io.kill.bits, _, io.head, wOrder-1))).asUInt
  store_ctrl.kill_ptr := store_ctrl.kill.ptr(
    continue = store_queue.conti, head = store_queue.head(wStore),
    above = store_queue.above, below = store_queue.below)
  when (io.xcpt) {
    store_queue.tail := Seq(store_queue.head, store_ctrl.nxt_head)
  }.elsewhen (io.kill.valid && store_ctrl.kill.valid(store_queue.valid)) {
    store_queue.tail := Seq(store_ctrl.kill_ptr, next(store_ctrl.kill_ptr, nStore, 1))
  }.elsewhen (store_ctrl.inc_tail(0)) {
    when (store_ctrl.inc_tail(1)) {
      store_queue.tail := store_ctrl.nxt_tail
    }.otherwise {
      store_queue.tail := Seq(store_queue.tail(1), store_ctrl.nxt_tail(0))
    }
  }
  when (store_ctrl.inc_head) {
    store_queue.head := store_ctrl.nxt_head
  }
/*==================================================*/
  /*===================rename stage===================*/
  val dec = Wire(new Bundle {
    val io_mem_fcn = Vec(2, Vec(nInst, Bool())) /*load or store io port*/
    val et_mem_fcn = Vec(2, Vec(nInst, Bool())) /*load or store queue port*/
    val mem_valid  = Vec(2, Vec(nInst, Bool())) /*load or store Mux the above two*/
    val mem_id  = Vec(nInst, UInt(wOrder.W)) /*memory inst id*/
    val mem_typ = Vec(nInst, UInt(MT_X.getWidth.W)) /*memory inst type*/
    val wbaddr  = Vec(nInst, UInt(wPhyAddr.W)) /*write back regfile address*/
    val first_fcn = Vec(2, Bool()) /*just for the first one and with in valid*/
  })
  when (load_ctrl.inc_tail(0)) {
    load_queue.push(i = 0,
      id  = Mux(dec.first_fcn(LD), dec.mem_id(0), dec.mem_id(1)),
      rd  = Mux(dec.first_fcn(LD), dec.wbaddr(0), dec.wbaddr(1)),
      typ = Mux(dec.first_fcn(LD), dec.mem_typ(0),dec.mem_typ(1)),
      stq_ptr = Mux(dec.first_fcn(LD) || !store_ctrl.inc_tail(0), store_queue.tail(0), store_queue.tail(1))
    )
    when (load_ctrl.inc_tail(1)) {
      load_queue.push(i = 1,id = dec.mem_id(1),rd = dec.wbaddr(1),typ = dec.mem_typ(1),
        stq_ptr = store_queue.tail(0))
    }
  }

  when (store_ctrl.inc_tail(0)) {
    store_queue.push(i = 0,
      id  = Mux(dec.first_fcn(ST), dec.mem_id(0), dec.mem_id(1)),
      typ = Mux(dec.first_fcn(ST), dec.mem_typ(0), dec.mem_typ(1)),
      ldq_ptr = Mux(dec.first_fcn(ST) || !load_ctrl.inc_tail(0), load_queue.tail(0), load_queue.tail(1)))
    when (store_ctrl.inc_tail(1)) {
      store_queue.push(i = 1,id = dec.mem_id(1),typ = dec.mem_typ(1),ldq_ptr = load_queue.tail(0))
    }
  }
  /* load queue capacity greater than 0 as well as
   * first is load or the second is load and the first is not rejected by store queue */
  load_ctrl.inc_tail(0)  := load_ctrl.capty_gt(0) &&
    (dec.mem_valid(LD)(0) || (dec.mem_valid(LD)(1) && !(dec.first_fcn(ST) && !store_queue.tail_val0)))
  /* store queue capacity greater than 0 as well as
   * first is store or the second is store and the first is not rejected by load queue */
  store_ctrl.inc_tail(0) := store_queue.tail_val0 &&
    (dec.mem_valid(ST)(0) || (dec.mem_valid(ST)(1) && !(dec.first_fcn(LD) && !load_ctrl.capty_gt(0))))
  /* the load capacity is greater than 1 as well as
   * both are load inst */
  load_ctrl.inc_tail(1)  := load_ctrl.capty_gt(1) && dec.mem_valid(LD).reduce(_&&_)
  /* the store capacity is greater than 1 as well as
   * both are store inst */
  store_ctrl.inc_tail(1) := store_queue.tail_val1 && dec.mem_valid(ST).reduce(_&&_)

  for (i <- 0 until 2) {
    dec.first_fcn(i)    := Mux(queue.head_val0, dec.et_mem_fcn(i)(0), io.mem_first(i)) // make effect on the second one
    dec.mem_valid(i)(0) := Mux(queue.head_val0, dec.et_mem_fcn(i)(0), dec.io_mem_fcn(i)(0))
    dec.mem_valid(i)(1) := Mux(queue.head_val0, dec.et_mem_fcn(i)(1) && queue.head_val1, dec.io_mem_fcn(i)(1))
  }
  for (i <- 0 until nInst) {
    dec.mem_id(i)  := Mux(queue.head_val0, queue.read(i).id,  io.in(i).bits.id)
    dec.wbaddr(i)  := Mux(queue.head_val0, queue.read(i).rd,  io.in(i).bits.rd)
    dec.mem_typ(i) := Mux(queue.head_val0, queue.read(i).typ, io.in(i).bits.typ)
  }
  /*================================================================================*/
  when (queue_ctrl.inc_tail(0)) {
    queue.write(i = 0, Mux(queue_ctrl.push(0), io.in(0).bits, io.in(1).bits))
    when(queue_ctrl.inc_tail(1)) {queue.write(i = 1, io.in(1).bits)}
  }

  queue_ctrl.push(0) := (queue.head_val0 && io.in(0).valid) ||
    (!load_ctrl.capty_gt(0) && dec.io_mem_fcn(LD)(0)) ||
    (!store_queue.tail_val0 && dec.io_mem_fcn(ST)(0))

  queue_ctrl.push(1) := (queue.head_val0 && io.in(1).valid) ||
    (Mux(io.mem_first(LD), !load_ctrl.capty_gt(1), !load_ctrl.capty_gt(0)) && dec.io_mem_fcn(LD)(1)) ||
    (Mux(io.mem_first(ST), !store_queue.tail_val1,!store_queue.tail_val0)&& dec.io_mem_fcn(ST)(1))

  queue_ctrl.inc_tail(0) := queue.tail_val0 && queue_ctrl.push.reduce(_||_)
  queue_ctrl.inc_tail(1) := queue.tail_val1 && queue_ctrl.push(0) && io.in(1).valid
  queue_ctrl.inc_head(0) := queue.head_val0 && (
    (dec.et_mem_fcn(LD)(0) && load_ctrl.capty_gt(0)) ||
    (dec.et_mem_fcn(ST)(0) && store_queue.tail_val0))

  queue_ctrl.inc_head(1) := queue.head_val1 && (
    (dec.et_mem_fcn(LD)(1) && Mux(dec.et_mem_fcn(LD)(0), load_ctrl.capty_gt(1), load_ctrl.capty_gt(0))) ||
    (dec.et_mem_fcn(ST)(1) && Mux(dec.et_mem_fcn(ST)(0), store_queue.tail_val1,store_queue.tail_val0)))

  dec.io_mem_fcn(LD) := io.in.map(i => i.valid && i.bits.fcn === M_XRD)
  dec.io_mem_fcn(ST) := io.in.map(i => i.valid && i.bits.fcn === M_XWR)
  for (i <- 0 until nInst) {
    dec.et_mem_fcn(LD)(i) := queue.read(i).fcn === M_XRD && !queue_ctrl.kill_head(i) //FIXED
    dec.et_mem_fcn(ST)(i) := queue.read(i).fcn === M_XWR && !queue_ctrl.kill_head(i) //FIXED
  }
/*==================================================*/
  /*==================execute stage===================*/
  val exe = Wire(new Bundle {
    val ldq_1H = Vec(3, UInt(nLoad.W))  /*load queue one hot vector*/
    val stq_1H = Vec(3, UInt(nStore.W)) /*store queue one hot vector*/
  })
  for (i <- 0 until 3) {
    exe.ldq_1H(i) := VecInit(load_queue.ls_id.map(_ === io.issue(i).id)).asUInt & load_queue.valid
    exe.stq_1H(i) := VecInit(store_queue.ls_id.map(_ === io.issue(i).id)).asUInt& store_queue.valid
    when (io.issue(i).valid) {
      for (j <- 0 until nLoad) when (exe.ldq_1H(i)(j)) {
        load_queue.ls_addr(j) := io.issue(i).addr
        load_queue.addr_ok(j) := true.B
      }
      for (j <- 0 until nStore) when (exe.stq_1H(i)(j)) {
        store_queue.addr_ok(j) := true.B
        store_queue.arready(j) := true.B
        store_queue.ls_addr(j) := io.issue(i).addr
        when (io.issue(i).data_ok) {
          store_queue.data(3)(j) := io.issue(i).data(31,24)
          store_queue.data(2)(j) := io.issue(i).data(23,16)
          store_queue.data(1)(j) := io.issue(i).data(15, 8)
          store_queue.data(0)(j) := io.issue(i).data( 7, 0)
          store_queue.data_ok(j) := true.B
        }
      }
    }
  }
/*==================================================*/
  /*===================memory stage===================*/
  val mem = Wire(new Bundle {
    val forward   = Vec(data_width/8, new QueueForward(nStore)) /*load inst forward check store queue*/
    val fwd_mux1H = Vec(data_width/8, UInt(nStore.W)) /*store2data load forward mux one hot*/
    val fwd_fcn   = UInt(M_X.getWidth.W) /*load or store inst*/
    val fwd_ptr   = UInt((wStore+1).W) /*latched forward pointer of store queue*/
    val fwd_addr  = UInt(data_width.W) /*latched forward address not include lowest two bits*/
    val fwd_type  = UInt(MT_X.getWidth.W) /*load inst type*/
    val fwd_conti = Bool() /*store queue forward part continue: is head is below tail*/
    val fwd_valid = UInt(nStore.W) /*store queue forward part valid vector*/
    val fwd_en    = Bool()
    def byte_en: UInt = VecInit((0 until data_width/8).map(i => /*data forward byte en*/
      forward(i).valid(fwd_valid) && (fwd_type === MT_W  ||
        (fwd_type(1,0) === 1.U && fwd_addr(1,0) === i.U) ||
        (fwd_type(1,0) === 2.U && fwd_addr(1,0) === (i/2).U)))).asUInt
    def fwd_below: UInt = Below(fwd_ptr(wStore-1,0), nStore) /*store queue forward back below tail vector*/

    val backward = new QueueBackward(nLoad) /*store inst backward check load queue*/
    val bwd_above = UInt(nLoad.W) /*load queue backward part above head vector*/
    val bwd_conti = Bool() /*load queue backward part continue: is head below tail*/
    val bwd_valid = UInt(nLoad.W) /*load queue backward part valid vector*/

    val store = Bool() /*store inst valid to memory port to write back data*/
    val stall = Bool() /*the outside stall signal*/
  })
  val mem_reg = RegInit({
    val w = Wire(new Bundle {
      val valid  = Vec(2, Bool()) /*load or store valid in data back stage*/
      val forward = Bool()
      val fwd_mux1H = Vec(data_width/8, UInt(nStore.W)) /*store2data load forward mux one hot*/
      val fwd_valid = UInt((data_width/8).W) /*store2load data forward valid*/
      val ld_id   = UInt(wOrder.W) /*load inst id*/
      val ld_ptr  = UInt(wLoad.W) /*load inst queue pointer*/
      val wb_addr = UInt(wPhyAddr.W) /*load inst write back address*/
      val unsafe  = Vec(nStore, Bool()) /*load inst unsafe vector*/
    })
    w.valid := Seq(false.B, false.B)
    w.forward   := DontCare
    w.fwd_mux1H := DontCare
    w.fwd_valid := DontCare
    w.forward := DontCare
    w.ld_id   := DontCare
    w.ld_ptr  := DontCare
    w.wb_addr := DontCare
    w.unsafe  := DontCare
    w
  })

  mem.fwd_fcn := Mux(!load_queue.ls_valid || (store_queue.ls_valid
    && CmpId(store_queue.id, load_queue.id, io.head, wOrder-1)), M_XWR, M_XRD)
  mem.fwd_addr  := load_queue.addr
  mem.fwd_type  := load_queue.typ
  mem.fwd_ptr   := load_queue.stq_ptr
  mem.fwd_en    := ((~mem.byte_en).asUInt | VecInit(mem.fwd_mux1H.map(
    Mux1H(_, store_queue.data_ok))).asUInt).andR //addr ok but data not ok
  mem.fwd_conti := mem.fwd_ptr(wStore) === store_queue.head(wStore)
  mem.fwd_valid := Mux(mem.fwd_conti, store_queue.above & mem.fwd_below, store_queue.above | mem.fwd_below)
  for (j <- 0 until data_width/8) {
    mem.forward(j).const := store_queue.addr_eq(mem.fwd_addr(data_width-1,2)) & store_queue.byte_en(j)
    mem.fwd_mux1H(j) := mem.forward(j).ptr1H(mem.fwd_conti, store_queue.above, mem.fwd_below)
  }

  when (!mem.stall) {
    when (mem.store) {
      mem_reg.valid(ST) := true.B
      mem_reg.valid(LD) := false.B
    }.otherwise {
      mem_reg.valid(LD) := mem.fwd_fcn === M_XRD
      mem_reg.valid(ST) := false.B
      when (mem.fwd_fcn === M_XRD) {
        mem_reg.forward   := mem.fwd_en
        mem_reg.fwd_valid := mem.byte_en
        mem_reg.fwd_mux1H := mem.fwd_mux1H
        mem_reg.ld_id   := load_queue.id
        mem_reg.ld_ptr  := load_queue.ls_ptr
        mem_reg.wb_addr := load_queue.wb_addr
        mem_reg.unsafe  := (0 until nStore).map(i => mem.fwd_valid(i) && !store_queue.arready(i))
      }
      when (io.mem.req.ready) {
        when (mem.fwd_fcn === M_XRD && mem.fwd_en) {
          load_queue.addr_ok(load_queue.ls_ptr) := false.B
        }.elsewhen(store_queue.ls_valid) {
          store_queue.addr_ok(store_queue.ls_ptr) := false.B
        }
      }
    }
  }

  mem.stall := mem_reg.valid.reduce(_||_) && !io.mem.resp.valid

  mem.bwd_above := Above(store_queue.ldq_ptr(wLoad-1,0), nLoad)
  mem.bwd_conti := store_queue.ldq_ptr(wLoad) === load_queue.tail(0)(wLoad)
  mem.bwd_valid := Mux(mem.bwd_conti, mem.bwd_above & load_queue.below, mem.bwd_above | load_queue.below)
  mem.backward.const := load_queue.addr_eq(addr = store_queue.head_addr(data_width-1,2),
    head = store_queue.head(wStore-1,0)) & (0 until data_width/8).map(j =>
    load_queue.byte_en(j) & Fill(nLoad, store_queue.head_typ === MT_W ||
      (store_queue.head_typ === MT_B && store_queue.head_addr(1,0) === j.U) ||
      (store_queue.head_typ === MT_H && store_queue.head_addr(1,0) === (j/2).U))).reduce(_|_)

  mem.store := store_queue.head_data_ok && io.head === store_queue.head_id && !mem.stall

  io.in(0).ready := queue.tail_val0
  io.in(1).ready := Mux(io.mem_en, queue.tail_val1, queue.tail_val0)
  //TODO: Does it affect timing???
  io.issueable.valid := Mux(queue_ctrl.inc_head(0), queue.head_val1, queue.head_val0)
  io.issueable.bits  := Mux(queue_ctrl.inc_head(0), queue.read(1).id, queue.read(0).id)

  io.limit.valid := queue.head_val0
  io.limit.bits := queue.read(0).id

  io.forward.valid := !mem.stall && !mem.store && mem.fwd_fcn === M_XRD && io.mem.req.ready
  io.forward.addr  := load_queue.wb_addr
  io.wb_data := Cat(
    Mux(mem_reg.fwd_valid(3), Mux1H(mem_reg.fwd_mux1H(3), store_queue.data(3)), io.mem.resp.bits.data(31,24)),
    Mux(mem_reg.fwd_valid(2), Mux1H(mem_reg.fwd_mux1H(2), store_queue.data(2)), io.mem.resp.bits.data(23,16)),
    Mux(mem_reg.fwd_valid(1), Mux1H(mem_reg.fwd_mux1H(1), store_queue.data(1)), io.mem.resp.bits.data(15, 8)),
    Mux(mem_reg.fwd_valid(0), Mux1H(mem_reg.fwd_mux1H(0), store_queue.data(0)), io.mem.resp.bits.data( 7, 0)))
  //what about load.fwd_valid.reduce(_&&_)???
  io.ldcommit.valid := io.mem.resp.valid && mem_reg.valid(LD) && mem_reg.forward
  //TODO unsafe modify at the same time
  when (io.ldcommit.valid) {
    load_queue.data_ok(mem_reg.ld_ptr) := true.B
    load_queue.unsafe(mem_reg.ld_ptr) := mem_reg.unsafe
  }
  when (store_queue.head_addr_ok) { for (i <- 0 until nLoad)
  //    when (mem.bwd_valid(i)) { it doesn't matter
    load_queue.unsafe(i)(store_queue.head(wStore-1,0)) := false.B
  }
  io.ldcommit.wb.valid := io.ldcommit.valid
  io.ldcommit.id       := mem_reg.ld_id
  io.ldcommit.wb.addr  := mem_reg.wb_addr

  io.rollback.valid := mem.backward.valid(mem.bwd_valid) && store_queue.head_addr_ok
  io.rollback.bits  := load_queue.ls_id(mem.backward.ptr(
    continue = mem.bwd_conti, above = mem.bwd_above, below = load_queue.below))

  store_ctrl.inc_head  := mem.store && io.mem.req.ready
  io.stcommit.valid := store_ctrl.inc_head
  io.stcommit.bits  := io.head

  io.mem.req.bits.data := store_queue.head_data
  io.mem.req.bits.fcn := Mux(mem.store, M_XWR, M_XRD)

  io.mem.req.valid := !mem.stall &&
    (mem.store || load_queue.ls_valid || store_queue.ls_valid)

  io.mem.req.bits.typ := Mux(mem.store, store_queue.head_typ,
    Mux(mem.fwd_fcn === M_XWR, store_queue.typ, load_queue.typ))

  io.mem.req.bits.addr:= Mux(mem.store, store_queue.head_addr,
    Mux(mem.fwd_fcn === M_XWR, store_queue.addr, load_queue.addr))

  when (CycRange(io.cyc,21966, 21975)) {
//    printf(
//      p"kill valid ${io.kill.valid} " +
//        p"kill id ${io.kill.bits} " +
//        p"kill cmp${VecInit(store_ctrl.kill.const)} " +
//        p"${store_ctrl.kill_ptr} " +
//        p"${store_ctrl.kill.valid(store_queue.valid)} " +
//        p"${VecInit(store_queue.valid)}\n")
    when (io.mem.req.valid) {
      when (io.mem.req.bits.fcn === M_XWR) {
        printf("STORE id %d [ %x %x %x]\n",
          store_queue.head_id,
          io.mem.req.bits.typ,
          io.mem.req.bits.addr,
          io.mem.req.bits.data)
      }.otherwise {
        printf("LOAD id %d [ %x %x]\n",
          Mux(mem.fwd_fcn === M_XWR, store_queue.id, load_queue.id),
          io.mem.req.bits.typ,
          io.mem.req.bits.addr)
      }
    }
    printf(p"LoadStore input: " +
      p"kill ${io.kill.valid}->${io.kill.bits} " +
      p"valid ${io.in(0).valid} ${io.in(1).valid} " +
      p"head ${io.head} " +
      p"issuable ${io.issueable.valid}->${io.issueable.bits} " +
      p"limit ${io.limit.valid}->${io.limit.bits} " +
//      p"req_ready ${io.mem.req.ready} " +
//      p"head_id ${store_queue.head_id} " +
      p"store ${mem.store} " +
      p"ld/st ${queue.head} ${queue.tail} " +
      p"data ${Hexadecimal(io.mem.resp.bits.data)} " +
      p"\n")
    printf(p"load: head ${load_queue.head} tail ${load_queue.tail} id")
    for (i <- 0 until nLoad) printf(p" ${load_queue.addr_ok(i)}->${load_queue.ls_id(i)}")
    printf("\n")
    printf(p"store: head ${store_queue.head} tail ${store_queue.tail} id")
    for (i <- 0 until nStore) printf(p" <${store_queue.arready(i)},${store_queue.data_ok(i)}>" +
      p"->${store_queue.ls_id(i)}")
    printf("\n")
  }

//  when (io.cyc === 15476.U) {
//    for (i <- 0 until nEntry) printf(p" ${queue.entry(i).id}")
//    printf(p"\nkill_valid ${queue_ctrl.kill.valid(queue.valid)} " +
//      p"kill_ptr ${queue_ctrl.kill_ptr} " +
//      p"kill_const ${queue_ctrl.kill.const}" +
//      p"kill_head ${queue_ctrl.kill_head} \n")
//  }
//  when (CycRange(io.cyc, 13870, 13880)) {
//    printf(p"${load_queue.undone(load_queue.head(wStore-1,0))} ${load_queue.unsafe(load_queue.head(wStore-1,0))}\n")
//  }
//  printf(p"output: ready->Vec(${io.in(0).ready}, ${io.in(1).ready}) isseable->${io.issueable.valid}:" +
//    p"${io.issueable.bits} forward->${io.forward.valid}:${io.forward.addr}\n" +
//    p"output: ldcommit->${io.ldcommit.valid} wb_val->${io.ldcommit.valid} id->${io.ldcommit.id} " +
//    p"wb_addr->${io.ldcommit.wb.addr} wb_data->${io.wb_data}\n" +
//    p"output: rollback->${io.rollback.valid}:${io.rollback.bits} stcommit->${io.stcommit.valid}:" +
//    p"${io.stcommit.bits}\n" +
//    p"output: id-> ${load_queue.id} req_val->${io.mem.req.valid} req_fcn->${io.mem.req.bits.fcn} req_addr->${io.mem.req.bits.addr} " +
//    p"req_typ->${io.mem.req.bits.typ} data->${io.mem.req.bits.data}\n")
//  printf(p"load queue: head->${load_queue.head} inc_head->${load_ctrl.inc_head} tail->${load_queue.tail} " +
//    p"inc_tail->${load_ctrl.inc_tail}\n")
//  printf(p"store queue: head->${store_queue.head} inc_head->${store_ctrl.inc_head} tail->${store_queue.tail} " +
//    p"inc_tail->${store_ctrl.inc_tail}\n")
//  printf(p"ldst queue: head->${queue.head} tail->${queue.tail}\n")
//  printf(p"mem ctrl: store->${mem.store} fwd_stall->${mem.fwd_stall} fwd_fcn->${mem.fwd_fcn} " +
//    p"st_lsptr->${store_queue.ls_valid}:${store_queue.ls_ptr} " +
//    p"ld_lsptr->${load_queue.ls_valid}:${load_queue.ls_ptr}\n" +
//    p"mem_reg: unsafe->${load_queue.unsafe(mem_reg.ld_ptr)} data_ok->${load_queue.data_ok(mem_reg.ld_ptr)} " +
//    p"store_addr_ok->${store_queue.head_addr_ok}\n")
//  printf(p"mem reg: fwd_mux1H->${mem_reg.fwd_mux1H} fwd_valid->${mem_reg.fwd_valid} valid->${mem_reg.valid}\n" +
//    p"ld_id->${mem_reg.ld_id} ld_typ->${mem_reg.ld_typ} ld_ptr->${mem_reg.ld_ptr} " +
//    p"wb_addr->${mem_reg.wb_addr} stq_ptr->${mem_reg.stq_ptr} unsafe->${mem_reg.unsafe}\n")
//
//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}