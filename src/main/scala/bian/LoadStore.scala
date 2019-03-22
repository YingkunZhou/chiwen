package bian

import chisel3._
import chisel3.util._
import common.MemPortIo

object Above {
  def apply(num: UInt, w: Int): UInt = VecInit((0 until w).map(_.U >= num)).asUInt
}

object Below {
  def apply(num: UInt, w: Int): UInt = VecInit((0 until w).map(_.U < num)).asUInt
}

trait LsParam extends Pram {
  val nEntry = 8
  val nLoad  = 8
  val nStore = 8
  require(isPow2(nEntry))
  require(isPow2(nLoad))
  require(isPow2(nStore))
  val wEntry = log2Ceil(nEntry)
  val wLoad  = log2Ceil(nLoad)
  val wStore = log2Ceil(nStore)
  val LD = 0
  val ST = 1
  require(LD == 0)
  require(ST == 1)
}

class QueuePriority(val nEntry: Int) extends Bundle {
  val above = UInt(nEntry.W)
  val below = UInt(nEntry.W)
  val valid = Bool()
  def ptr(continue: Bool): UInt = PriorityEncoder(Mux(continue || above.orR, above & below, above | below))
}

class StEntry(val id_width: Int) extends Bundle {
  val id   = UInt(id_width.W)
  val typ  = UInt(MT_X.getWidth.W)
}
class LdEntry(id_width: Int, val addr_width: Int)
  extends StEntry(id_width) {val rd = new ByPass(addr_width)}
class LdStEntry(id_width: Int, addr_width: Int)
  extends LdEntry(id_width, addr_width) {val fcn = UInt(M_X.getWidth.W)}
class LoadEntry(id_width: Int, addr_width: Int, val data_width: Int, val nStore: Int)
  extends LdEntry(id_width, addr_width) {
  val addr = UInt(data_width.W)
  val stq_ptr = UInt((log2Ceil(nStore)+1).W)
  val addr_ok = Bool()
  val data_ok = Bool()
  val unsafe  = Vec(nStore, Bool())
}
class StoreEntry(id_width: Int, val data_width: Int, val nLoad: Int)
  extends StEntry(id_width) {
  val addr = UInt(data_width.W)
  val data = UInt(data_width.W)
  val ldq_ptr = UInt(log2Ceil(nLoad).W)
  val addr_ok = Bool()
  val data_ok = Bool()
}

class LsIssue(val id_width: Int, val data_width: Int) extends Bundle { // from ALU
  val id = UInt(id_width.W)
  val addr = UInt(data_width.W)
  val data = UInt(data_width.W)
  val data_ok = Bool()
}

class T2QCtrl(val n: Int) extends Bundle {
  def w: Int = log2Ceil(n)
  val inc_tail = Vec(2, Bool())
  val nxt_tail = Vec(2, UInt((w+1).W))
  val kill_cmp = UInt(n.W)
  val kill_ptr = UInt((w+1).W)
  val kill = new QueuePriority(n)
}

class LSQCtrl(n: Int) extends T2QCtrl(n) {
  val inc_head = Vec(2, Bool())
  val nxt_head = Vec(2, UInt((w+1).W))
}

class StQCtrl(n: Int) extends T2QCtrl(n) {
  val inc_head = Bool()
  val nxt_head = UInt((w+1).W)
}

class LdqCtrl(n: Int) extends StQCtrl(n) {
  val head_next = new QueuePriority(n)
}

class H2T2RingQueue(val nEntry: Int) extends Bundle {
  def w: Int = log2Ceil(nEntry)
  val head  = Vec(2, UInt((w+1).W))
  val tail  = Vec(2, UInt((w+1).W))
  def above: UInt = Above(head(0)(w-1,0), nEntry)
  def below: UInt = Below(tail(0)(w-1,0), nEntry)
  def conti: Bool = head(0)(w-1,0) < tail(0)(w-1,0)
  def split: Bool = head(0)(w-1,0) >= tail(0)(w-1,0)
  def valid: UInt = (Fill(nEntry, conti) & above & below) |
    (Fill(nEntry, split && head(0) =/= tail(0)) & (above | below))
  def head_val0: Bool = head(0) =/= tail(0)
  def head_val1: Bool = head(1) =/= tail(0) //based on head_val0
  def tail_val0: Bool = head(0)(w-1,0) =/= tail(0)(w-1,0) || head(0)(w) === tail(0)(w)
  def tail_val1: Bool = head(0) =/= tail(1) //based on tail_val0
}

class H1T2RingQueue(val nEntry: Int) extends Bundle {
  def w: Int = log2Ceil(nEntry)
  val head  = UInt((w+1).W)
  val tail  = Vec(2, UInt((w+1).W))
  def above: UInt = Above(head(w-1,0), nEntry)
  def below: UInt = Below(tail(0)(w-1,0), nEntry)
  def conti: Bool = head(w-1,0) < tail(0)(w-1,0)
  def split: Bool = head(w-1,0) >= tail(0)(w-1,0)
  def valid: UInt = (Fill(nEntry, conti) & above & below) |
    (Fill(nEntry, split && head =/= tail(0)) & (above | below))
  def head_val: Bool  = head =/= tail(0)
  def tail_val0: Bool = head(w-1,0) =/= tail(0)(w-1,0) || head(w) === tail(0)(w)
  def tail_val1: Bool = head =/= tail(1) //based on tail_val0
}

class LSQueue(nEntry: Int, val id_width: Int, val addr_width: Int)
  extends H2T2RingQueue(nEntry) {
  val entry = Vec(nEntry, new LdStEntry(id_width, addr_width))
  def read(i: Int): LdStEntry = entry(head(i)(w-1,0))
  def write(i: Int, et: LdStEntry): Unit = { entry(tail(i)(w-1,0)) := et }
}

class LoadQueue(val nLoad: Int, val id_width: Int, val addr_width: Int, val data_width: Int, val nStore: Int)
  extends H1T2RingQueue(nEntry = nLoad) {
  val entry = Vec(nLoad, new LoadEntry(id_width, addr_width, data_width, nStore))
  def push(i: Int, id: UInt, rd: ByPass, typ: UInt, stq_ptr: UInt): Unit = {
    entry(tail(i)(w-1,0)).id := id
    entry(tail(i)(w-1,0)).rd := rd
    entry(tail(i)(w-1,0)).typ  := typ
    entry(tail(i)(w-1,0)).stq_ptr := stq_ptr
    entry(tail(i)(w-1,0)).addr_ok := false.B
    entry(tail(i)(w-1,0)).data_ok := false.B
  }
  def undone: UInt = VecInit(entry.map(e => !e.data_ok || e.unsafe.reduce(_||_))).asUInt
  def finish: Bool = entry(head(w-1,0)).data_ok && !entry(head(w-1,0)).unsafe.reduce(_||_)
  def addr_ok: UInt = VecInit(entry.map(e => e.addr_ok)).asUInt
  def byte_en(i: Int, j: Int): Bool = entry(i).typ === MT_W ||
    (entry(i).typ === MT_B && entry(i).addr(1,0) === j.U) ||
    (entry(i).typ === MT_H && entry(i).addr(1,0) === (j/2).U)
}

class StoreQueue(val nStore: Int, val id_width: Int, val data_width: Int, val nLoad: Int)
  extends H1T2RingQueue(nEntry = nStore) {
  val entry = Vec(nLoad, new StoreEntry(id_width, data_width, nStore))
  def push(i: Int, id: UInt, typ: UInt, ldq_ptr: UInt): Unit = {
    entry(tail(i)(w-1,0)).id   := id
    entry(tail(i)(w-1,0)).typ  := typ
    entry(tail(i)(w-1,0)).ldq_ptr := ldq_ptr
    entry(tail(i)(w-1,0)).addr_ok := false.B
    entry(tail(i)(w-1,0)).data_ok := false.B
  }
  def byte_en(i: Int, j: Int): Bool = entry(i).typ === MT_W ||
    (entry(i).typ === MT_B && entry(i).addr(1,0) === j.U) ||
    (entry(i).typ === MT_H && entry(i).addr(1,0) === (j/2).U)
  def read: StoreEntry = entry(head(w-1,0))
  def addr_ok: UInt = VecInit(entry.map(e => e.addr_ok)).asUInt
}

class LoadStore extends Module with LsParam {
  val io = IO(new Bundle {
    val entry  = Input(Vec(nInst, Valid(new LdStEntry(wOrder, wPhyAddr))))
    val mem_fcn = Input(Vec(2, Bool()))
    val lReady = Output(Vec(nInst, Bool()))

    val issue = Input(Vec(3, Valid(new LsIssue(wOrder, data_width))))
    val issueable = Output(Valid(UInt(wOrder.W)))
    val forward = Output(new ByPass(wPhyAddr)) //forward to inst queue

    val memIO = new MemPortIo(data_width)
    val ldcommit = Output(new Commit(wOrder, wPhyAddr))
    val stcommit = Output(Valid(UInt(wOrder.W)))
    val wb_data  = Output(UInt(data_width.W))

    val xcpt = Input(Bool())
    val kill = Input(Valid(UInt(wOrder.W)))
    val id_head = Input(UInt(wOrder.W))
    val rollback = Output(Valid(UInt(wOrder.W)))
  })

  def next(ptr: UInt, nEntry: Int, num: Int): UInt = {
    require(num < 3)
    if (isPow2(nEntry)) ptr + num.U
    else { require(nEntry == 6)
      if (num == 1) Mux(ptr === (nEntry-1).U, 0.U, ptr + 1.U)
      else Mux(ptr(2).toBool, Cat(0.U(1.W), ptr(1,0)), ptr+2.U)
    }
  }

  val flush: Bool = io.kill.valid || io.xcpt
  /*=================data structure===================*/
  val queue = RegInit({
    val w = Wire(new LSQueue(nEntry, wOrder, wPhyAddr))
    w.head  := Seq(0.U, 1.U)
    w.tail  := Seq(0.U, 1.U)
    w.entry := DontCare
    w
  })
  io.issueable.valid := queue.head_val0
  io.issueable.bits  := queue.read(0).id
  val queue_ctrl = Wire(new LSQCtrl(nEntry))
  queue_ctrl.nxt_head := queue.head.map(_+2.U)
  queue_ctrl.nxt_tail := queue.tail.map(_+2.U)
  queue_ctrl.kill_cmp := VecInit((0 until nEntry).map(i => CmpId(io.kill.bits, queue.entry(i).id, io.id_head))).asUInt
  queue_ctrl.kill_ptr := Cat(queue.head(0)(wEntry), queue_ctrl.kill.ptr(queue.conti))
  queue_ctrl.kill.above := queue.above & queue_ctrl.kill_cmp
  queue_ctrl.kill.below := queue.below & queue_ctrl.kill_cmp
  queue_ctrl.kill.valid := (queue.valid & queue_ctrl.kill_cmp).orR
  when (io.xcpt) {
    queue.tail := queue.head
  }.elsewhen(io.kill.valid && queue.head_val0 && queue_ctrl.kill.valid) {
    queue.tail := Seq(queue_ctrl.kill_ptr, queue_ctrl.kill_ptr+1.U)
  }.elsewhen (queue_ctrl.inc_tail(0)) {
    when(queue_ctrl.inc_tail(1)) {
      queue.tail := Seq(queue_ctrl.nxt_tail(0), queue_ctrl.nxt_tail(1))
    }.otherwise {
      queue.tail := Seq(queue.tail(1), queue_ctrl.nxt_tail(0))
    }
  }
  when (!io.xcpt && queue_ctrl.inc_head(0)) { //TODO: for branch kill
    when(queue_ctrl.inc_head(1)) {
      queue.head := Seq(queue_ctrl.nxt_head(0), queue_ctrl.nxt_head(1))
    }.otherwise {
      queue.head := Seq(queue.head(1), queue_ctrl.nxt_head(0))
    }
  }

  io.lReady(0) := queue.tail_val0 //for time saving
  io.lReady(1) := Mux(io.entry(0).valid, queue.tail_val1, queue.tail_val0) //for time saving

  val ldQueue = RegInit({
    val w = Wire(new LoadQueue(nLoad = nLoad, id_width = wOrder, addr_width = wPhyAddr, data_width = data_width, nStore = nStore))
    w.head := 0.U
    w.tail := Seq(0.U, 1.U)
    w.entry := DontCare
    w
  })
  val ldQctrl = Wire(new LdqCtrl(nLoad))
  ldQctrl.nxt_tail := ldQueue.tail.map(_ + 2.U)
  ldQctrl.kill_cmp := VecInit((0 until nLoad).map(i => CmpId(io.kill.bits, ldQueue.entry(i).id, io.id_head))).asUInt
  ldQctrl.kill_ptr := Cat(ldQueue.head(wLoad), ldQctrl.kill.ptr(ldQueue.conti))
  ldQctrl.kill.above := ldQueue.above & ldQctrl.kill_cmp
  ldQctrl.kill.below := ldQueue.above & ldQctrl.kill_cmp
  ldQctrl.kill.valid := (ldQueue.valid & ldQctrl.kill_cmp).orR
  when (io.xcpt) {
    ldQueue.tail := Seq(ldQueue.head, ldQueue.head + 1.U)
  }.elsewhen (io.kill.valid && ldQctrl.kill.valid) {
    ldQueue.tail := Seq(ldQctrl.kill_ptr, ldQctrl.kill_ptr+1.U)
  }.elsewhen (ldQctrl.inc_tail(0)) {
    when (ldQctrl.inc_tail(1)) {
      ldQueue.tail := Seq(ldQctrl.nxt_tail(0), ldQctrl.nxt_tail(1))
    }.otherwise {
      ldQueue.tail := Seq(ldQueue.tail(1), ldQctrl.nxt_tail(0))
    }
  }
  when (ldQctrl.inc_head) { ldQueue.head := ldQueue.tail(0) }

  val stQueue = RegInit({
    val w = Wire(new StoreQueue(nStore, wOrder, data_width, nLoad))
    w.head  := 0.U
    w.tail  := Seq(0.U, 1.U)
    w.entry := DontCare
    w
  })
  val stQctrl = Wire(new StQCtrl(nStore))
  stQctrl.nxt_head := stQueue.head + 1.U
  stQctrl.nxt_tail := stQueue.tail.map(_+2.U)
  stQctrl.kill_cmp := VecInit((0 until nStore).map(i => CmpId(io.kill.bits, stQueue.entry(i).id, io.id_head))).asUInt
  stQctrl.kill_ptr := Cat(stQueue.head(wStore), stQctrl.kill.ptr(stQueue.conti))
  stQctrl.kill.above := stQueue.above & stQctrl.kill_cmp
  stQctrl.kill.below := stQueue.below & stQctrl.kill_cmp
  stQctrl.kill.valid := (stQueue.valid & stQctrl.kill_cmp).orR
  when (io.xcpt) {
    stQueue.tail := Seq(stQueue.head, stQctrl.nxt_head)
  }.elsewhen (io.kill.valid && stQctrl.kill.valid) {
    stQueue.tail := Seq(stQctrl.kill_ptr, stQctrl.kill_ptr+1.U)
  }.elsewhen (stQctrl.inc_tail(0)) {
    when (stQctrl.inc_tail(1)) {
      stQueue.tail := Seq(stQctrl.nxt_tail(0), stQctrl.nxt_tail(1))
    }.otherwise {
      stQueue.tail := Seq(stQueue.tail(1), stQctrl.nxt_tail(0))
    }
  }
  when (stQctrl.inc_head) { stQueue.head := stQctrl.nxt_head }

  val dec = Wire(new Bundle {
    val io_mem_fcn  = Vec(2, Vec(nInst, Bool()))
    val et_mem_fcn  = Vec(2, Vec(nInst, Bool()))
    val valid = Vec(2, Vec(nInst, Bool()))

    val mem_id  = Vec(nInst, UInt())
    val mem_rd  = Vec(nInst, new ByPass(wPhyAddr))
    val mem_typ = Vec(nInst, UInt())
    val mem_fcn = Vec(nInst, Bool()) //just for the first one
  })

  val exe = Wire(new Bundle {
    val ldq_1H = Vec(3, UInt(nLoad.W))
    val stq_1H = Vec(3, UInt(nStore.W))
  })
  val mem = Wire(new Bundle {
    val ld = new QueuePriority(nLoad)
    val ldq_ptr = UInt(wLoad.W)
    val st = new QueuePriority(nStore)
    val stq_ptr = UInt(wStore.W)
    val wb_addr = UInt(wPhyAddr.W)
    val id   = Vec(2, UInt(wOrder.W))
    val typ  = Vec(2, UInt(MT_X.getWidth.W))
    val addr = Vec(2, UInt(data_width.W))
    val fcn  = UInt(M_X.getWidth.W)

    val tail_ptr = UInt((wStore+1).W)
    def below: UInt = Below(tail_ptr(wStore-1,0), nStore)
    val continue = Bool()
    val st_valid = UInt(nStore.W)
    val ld_unsafe = Vec(nStore, Bool())
    val st_fwd_1H = Vec(data_width/8, UInt(nStore.W))
    val st_fwdval = Vec(data_width/8, Bool())
    val load_wait = Bool()
    val load_addr = UInt((data_width-2).W)
    val load_meet = Vec(nStore, Bool())
    val stall = Bool()
  })
  val load = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val unsafe  = Vec(nStore, Bool())
      val fwd_1H  = Vec(data_width/8, UInt(nStore.W))
      val fwd_val = Vec(data_width/8, Bool())
      val typ   = UInt(MT_X.getWidth.W)
      val q_ptr = UInt(wLoad.W)
      val addr  = UInt(data_width.W)
      val wb_addr = UInt(wPhyAddr.W)
      val id = UInt(wOrder.W)
      val stq_ptr = UInt((wStore+1).W)
    })
    w.valid := false.B
    w.unsafe  := DontCare
    w.fwd_1H  := DontCare
    w.fwd_val := DontCare
    w.typ     := DontCare
    w.q_ptr   := DontCare
    w.addr    := DontCare
    w.wb_addr := DontCare
    w.id      := DontCare
    w.stq_ptr := DontCare
    w
  })
  val store = Wire(new Bundle {
    val valid = Bool()
    val entry = new StoreEntry(id_width = wOrder, data_width = data_width, nLoad = nLoad)
    def byte_wen: UInt = VecInit((0 until data_width/8).map(i =>
       entry.typ === MT_W || (entry.typ === MT_B && entry.addr(1,0) === i.U) ||
      (entry.typ === MT_H && entry.addr(1,0) === (i/2).U))).asUInt
    def above: UInt = Above(entry.ldq_ptr(wLoad-1,0), nLoad)
    val continue = Bool()
    val ld_valid = UInt(nLoad.W)
    val unsafe_ld = UInt(nLoad.W)
    val fwd_load = Vec(data_width/8, Bool())
    val rollback = new QueuePriority(nLoad)
  })
  store.entry := stQueue.read
/*==================================================*/
  /*===================rename stage===================*/
  when (ldQctrl.inc_tail(0)) {
    ldQueue.push(i = 0, stq_ptr = stQueue.tail(0),
      id  = Mux(dec.valid(LD)(0), dec.mem_id(0), dec.mem_id(1)),
      rd  = Mux(dec.valid(LD)(0), dec.mem_rd(0), dec.mem_rd(1)),
      typ = Mux(dec.valid(LD)(0), dec.mem_typ(0),dec.mem_typ(1)))
    when (ldQctrl.inc_tail(1)) {
      ldQueue.push(i = 1, id = dec.mem_id(1), rd = dec.mem_rd(1), typ = dec.mem_typ(1),
        stq_ptr = Mux(stQctrl.inc_tail(0), stQueue.tail(1), stQueue.tail(0)))
    }
  }

  when (stQctrl.inc_tail(0)) {
    stQueue.push(i = 0, ldq_ptr = ldQueue.tail(0),
      id  = Mux(dec.valid(ST)(0), dec.mem_id(0), dec.mem_id(1)),
      typ = Mux(dec.valid(ST)(0), dec.mem_typ(0), dec.mem_typ(1)))
    when (stQctrl.inc_tail(1)) {
      stQueue.push(i = 1, id = dec.mem_id(1), typ = dec.mem_typ(1),
        ldq_ptr = Mux(ldQctrl.inc_tail(0), ldQueue.tail(1), ldQueue.tail(0)))
    }
  }

  val push = Wire(Vec(4, Bool()))
  push(0) := (dec.io_mem_fcn(LD)(0) && !ldQueue.tail_val0) ||
    (dec.io_mem_fcn(ST)(0) && !stQueue.tail_val0) ||
    (queue.head_val0 && io.entry(0).valid)
  push(1) := dec.io_mem_fcn(LD)(1)  && !ldQueue.tail_val1 && (io.mem_fcn(LD) || !ldQueue.tail_val0)
  push(2) := dec.io_mem_fcn(ST)(1)  && !stQueue.tail_val1 && (io.mem_fcn(ST) || !stQueue.tail_val0)
  push(3) := queue.head_val0 && io.entry(1).valid

  when (queue_ctrl.inc_tail(0)) {
    queue.write(0, Mux(push(0), io.entry(0).bits, io.entry(1).bits))
    when(queue_ctrl.inc_tail(1)) {queue.write(1, io.entry(1).bits)}
  }

  ldQctrl.inc_tail(0) := ldQueue.tail_val0 &&(dec.valid(LD)(0) || (dec.valid(LD)(1) && !(dec.mem_fcn(ST) && !stQueue.tail_val0)))
  ldQctrl.inc_tail(1) := ldQueue.tail_val1 && dec.valid(LD).reduce(_&&_)
  stQctrl.inc_tail(0) := stQueue.tail_val0 &&(dec.valid(ST)(0) || (dec.valid(ST)(1) && !(dec.mem_fcn(LD) && !ldQueue.tail_val0)))
  stQctrl.inc_tail(1) := stQueue.tail_val1 && dec.valid(ST).reduce(_&&_)

  queue_ctrl.inc_tail(0) := queue.tail_val0 && push.reduce(_||_)
  queue_ctrl.inc_tail(1) := push(0) && io.entry(1).valid && queue.tail_val1

  queue_ctrl.inc_head(0) := queue.head_val0 && (
    (dec.et_mem_fcn(LD)(0) && ldQueue.tail_val0) ||
    (dec.et_mem_fcn(ST)(0) && stQueue.tail_val0))

  queue_ctrl.inc_head(1) := queue.head_val1 && (
    (dec.et_mem_fcn(LD)(1) && (ldQueue.tail_val1 || (!dec.et_mem_fcn(LD)(0) && ldQueue.tail_val0))) ||
    (dec.et_mem_fcn(ST)(1) && (stQueue.tail_val1 || (!dec.et_mem_fcn(ST)(0) && stQueue.tail_val0)))
  )

  dec.mem_fcn(LD) := Mux(queue.head_val0, dec.et_mem_fcn(LD)(0), io.mem_fcn(LD))
  dec.mem_fcn(ST) := Mux(queue.head_val0, dec.et_mem_fcn(ST)(0), io.mem_fcn(ST))
  for (i <- 0 until 2) {
    dec.valid(i)(0) := Mux(queue.head_val0, dec.et_mem_fcn(i)(0), dec.io_mem_fcn(i)(0))
    dec.valid(i)(1) := Mux(queue.head_val0, dec.et_mem_fcn(i)(1) && queue.head_val1, dec.io_mem_fcn(i)(1))
  }

  dec.io_mem_fcn(LD) := io.entry.map(i => i.valid && i.bits.fcn === M_XRD)
  dec.io_mem_fcn(ST) := io.entry.map(i => i.valid && i.bits.fcn === M_XWR)
  for (i <- 0 until nInst) {
    dec.et_mem_fcn(LD)(i) := queue.read(i).fcn === M_XRD
    dec.et_mem_fcn(ST)(i) := queue.read(i).fcn === M_XWR
    dec.mem_id(i)   := Mux(queue.head_val0, queue.read(i).id,  io.entry(i).bits.id)
    dec.mem_rd(i)   := Mux(queue.head_val0, queue.read(i).rd,  io.entry(i).bits.rd)
    dec.mem_typ(i)  := Mux(queue.head_val0, queue.read(i).typ, io.entry(i).bits.typ)
  }

/*==================================================*/
  /*==================execute stage===================*/
  for (i <- 0 until 3) {
    exe.ldq_1H(i) := VecInit(ldQueue.entry.map(_.id === io.issue(i).bits.id)).asUInt & ldQueue.valid
    exe.stq_1H(i) := VecInit(stQueue.entry.map(_.id === io.issue(i).bits.id)).asUInt & stQueue.valid
    when (io.issue(i).valid) {
      for (j <- 0 until nLoad) when (exe.ldq_1H(i)(j)) {
          ldQueue.entry(j).addr_ok := true.B
          ldQueue.entry(j).addr := io.issue(i).bits.addr
        }
      for (j <- 0 until nStore) when (exe.stq_1H(i)(j)) {
          stQueue.entry(j).addr_ok := true.B
          stQueue.entry(j).addr := io.issue(i).bits.addr
          stQueue.entry(j).data_ok := io.issue(i).bits.data_ok
          stQueue.entry(j).data := io.issue(i).bits.data
        }
    }
  }
/*==================================================*/
  /*===================memory stage===================*/
  mem.ld.above := ldQueue.above & ldQueue.addr_ok
  mem.ld.below := ldQueue.below & ldQueue.addr_ok
  mem.ldq_ptr  := mem.ld.ptr(ldQueue.conti)
  mem.ld.valid := ldQueue.head_val && (ldQueue.addr_ok & ldQueue.valid).orR
  mem.st.above := stQueue.above & stQueue.addr_ok
  mem.st.below := stQueue.below & stQueue.addr_ok
  mem.stq_ptr  := mem.st.ptr(stQueue.conti)
  mem.st.valid := stQueue.head_val && (stQueue.addr_ok & stQueue.valid).orR

  mem.id(LD)   := ldQueue.entry(mem.ldq_ptr).id
  mem.typ(LD)  := ldQueue.entry(mem.ldq_ptr).typ
  mem.addr(LD) := ldQueue.entry(mem.ldq_ptr).addr
  mem.wb_addr  := ldQueue.entry(mem.ldq_ptr).rd.addr
  mem.id(ST)   := stQueue.entry(mem.stq_ptr).id
  mem.typ(ST)  := stQueue.entry(mem.stq_ptr).typ
  mem.addr(ST) := stQueue.entry(mem.stq_ptr).addr
  mem.fcn := Mux(!mem.ld.valid || (mem.st.valid && CmpId(mem.id(ST), mem.id(LD), io.id_head)), M_XWR, M_XRD)

  when (io.memIO.req.valid) {
    when (mem.fcn === M_XRD) {
      ldQueue.entry(mem.ldq_ptr).addr_ok := false.B
    }.otherwise {
      stQueue.entry(mem.stq_ptr).addr_ok := false.B
    }
  }
  io.forward.valid := !mem.stall && !store.valid && mem.fcn === M_XRD
  io.forward.addr  := mem.wb_addr
  val stq_ptr = ldQueue.entry(mem.ldq_ptr).stq_ptr
  when (!mem.stall && !store.valid) {
    load.valid := mem.fcn === M_XRD
    when (mem.fcn === M_XRD) {
      load.q_ptr := mem.ldq_ptr
      load.typ   := mem.typ(LD)
      load.addr  := mem.addr(LD)
      load.wb_addr := mem.wb_addr
      load.id      := mem.id(LD)
      load.stq_ptr := stq_ptr
    }
    when (mem.fcn === M_XRD || mem.load_wait) { // if ld_wait, continue to update these nodes
      load.unsafe  := mem.ld_unsafe
      load.fwd_1H  := mem.st_fwd_1H
      load.fwd_val := mem.st_fwdval
    }
  }

  mem.load_addr := Mux(mem.load_wait, load.addr(data_width-1,2), mem.addr(LD)(data_width-1,2))
  mem.load_meet := (0 until nStore).map(i => stQueue.entry(i).addr(data_width-1,2) === mem.load_addr)
  mem.tail_ptr  := Mux(mem.load_wait, load.stq_ptr, stq_ptr)

  mem.continue := mem.tail_ptr(wStore-1,0) > stQueue.head(wStore-1,0)
  mem.st_valid := Mux(mem.continue, stQueue.above & mem.below, stQueue.above | mem.below)
  val forward = Wire(new Bundle {
    val byte_en = Vec(data_width/8, UInt(nStore.W))
    val above = Vec(data_width/8, UInt(nStore.W))
    val below = Vec(data_width/8, UInt(nStore.W))
    val valid = Bool()
  })
  forward.valid := mem.tail_ptr =/= stQueue.head
  mem.ld_unsafe := (0 until nStore).map(i => mem.st_valid(i) && !stQueue.addr_ok(i) && forward.valid)

  for (j <- 0 until data_width/8) {
    forward.byte_en(j) := VecInit((0 until nStore).map(i =>
      stQueue.byte_en(i,j) && mem.load_meet(i) && stQueue.entry(i).addr_ok)).asUInt
    forward.above(j) := stQueue.above & forward.byte_en(j)
    forward.below(j) := mem.below & forward.byte_en(j)
    mem.st_fwdval(j) := (forward.byte_en(j) & mem.st_valid).orR && forward.valid
    mem.st_fwd_1H(j) := Reverse(PriorityEncoderOH(Reverse(
      Mux(mem.continue || forward.below(j).orR, forward.below(j), forward.above(j))))).asUInt
  }
  /*==================================================*/
  /*===================load===================*/
  val store_valid = RegInit(false.B)
  when (!mem.stall) { store_valid := store.valid }
  mem.stall := (load.valid && (!io.memIO.resp.valid || mem.load_wait)) ||
    (store_valid && !io.memIO.resp.valid)
  store.fwd_load := (0 until data_width/8).map(i => load.fwd_val(i) && (load.typ === MT_W ||
    (load.typ(1,0) === 1.U && load.addr(1,0) === i.U) ||
    (load.typ(1,0) === 2.U && load.addr(1,0) === (i/2).U)))

  mem.load_wait := (0 until data_width/8).map(i =>
    store.fwd_load(i) && !Mux1H(load.fwd_1H(i), stQueue.entry.map(_.data_ok))).reduce(_||_)
  io.wb_data := Cat(
    Mux(store.fwd_load(3), Mux1H(load.fwd_1H(3), stQueue.entry.map(_.data))(31,24), io.memIO.resp.bits.data(31,24)),
    Mux(store.fwd_load(2), Mux1H(load.fwd_1H(2), stQueue.entry.map(_.data))(23,16), io.memIO.resp.bits.data(23,16)),
    Mux(store.fwd_load(1), Mux1H(load.fwd_1H(1), stQueue.entry.map(_.data))(15, 8), io.memIO.resp.bits.data(15, 8)),
    Mux(store.fwd_load(0), Mux1H(load.fwd_1H(0), stQueue.entry.map(_.data))( 7, 0), io.memIO.resp.bits.data( 7, 0)))

  when (io.ldcommit.valid) {ldQueue.entry(load.q_ptr).data_ok := true.B}

  io.ldcommit.valid := (io.memIO.resp.valid || load.fwd_val.reduce(_&&_)) && !mem.load_wait && load.valid
  io.ldcommit.id := load.id
  io.ldcommit.wb.valid := true.B
  io.ldcommit.wb.addr := load.wb_addr
  /*==================================================*/
  /*===================store===================*/
  store.unsafe_ld := VecInit((0 until nLoad).map(i =>
    ldQueue.entry(i).unsafe(stQueue.head) && ldQueue.entry(i).data_ok &&
    ldQueue.entry(i).addr(data_width-1,2) === store.entry.addr(data_width-1,2) &&
    (0 until data_width/8).map(j => store.byte_wen(j) && ldQueue.byte_en(i,j)).reduce(_||_))).asUInt

  store.continue := store.entry.ldq_ptr(wLoad-1,0) < ldQueue.tail(0)(wLoad-1,0)
  store.ld_valid := Mux(store.continue, store.above & ldQueue.below, store.above | ldQueue.below)
  store.rollback.valid := (store.ld_valid & store.unsafe_ld).orR && store.entry.ldq_ptr =/= ldQueue.tail(0)
  store.rollback.above := store.above & store.unsafe_ld
  store.rollback.below := ldQueue.below & store.unsafe_ld
  io.rollback.valid := store.rollback.valid && store.entry.data_ok
  io.rollback.bits  := ldQueue.entry(store.rollback.ptr(store.continue)).id

  for (i <- 0 until nLoad) {
    when (io.ldcommit.valid && i.U === load.q_ptr) {
      ldQueue.entry(i).unsafe := load.unsafe
    }.elsewhen (store.ld_valid(i) && store.entry.data_ok) {
      ldQueue.entry(i).unsafe(stQueue.head(wStore-1,0)) := false.B
    }
  }

  ldQctrl.inc_head := ldQueue.head_val && ldQueue.finish
  ldQctrl.head_next.above := ldQueue.above & ldQueue.undone
  ldQctrl.head_next.below := ldQueue.below & ldQueue.undone
  ldQctrl.head_next.valid := (ldQueue.valid & ldQueue.undone).orR
  ldQctrl.nxt_head := Mux(ldQctrl.head_next.valid,
    Cat(ldQueue.tail(0)(wLoad), ldQctrl.head_next.ptr(ldQueue.conti)),
    ldQueue.tail(0))
  store.valid := stQueue.head_val && (store.entry.data_ok && io.id_head === io.stcommit.bits)
  stQctrl.inc_head  := store.valid && !mem.stall
  io.stcommit.valid := stQctrl.inc_head
  io.stcommit.bits  := store.entry.id

  io.memIO.req.valid := !mem.stall && (stQctrl.inc_head || mem.ld.valid || mem.st.valid || mem.load_wait)
  io.memIO.req.bits.fcn  := Mux(stQctrl.inc_head, M_XWR, M_XRD)
  io.memIO.req.bits.typ  := Mux(stQctrl.inc_head, store.entry.typ,
    Mux(mem.load_wait, load.typ,  Mux(mem.fcn === M_XWR, mem.typ(ST), mem.typ(LD))))
  io.memIO.req.bits.addr := Mux(stQctrl.inc_head, store.entry.addr,
    Mux(mem.load_wait, load.addr, Mux(mem.fcn === M_XWR, mem.addr(ST), mem.addr(LD))))
  io.memIO.req.bits.data := store.entry.data
}
