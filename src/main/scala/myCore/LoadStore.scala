package myCore

import chisel3._
import chisel3.util._
import common.MemPortIo

trait LsParam extends Pram {
  val nEntry = 8
  val nLoad  = 8 //6
  val nStore = 8 //6
  val wEntry = log2Ceil(nEntry)
  val wLoad  = log2Ceil(nLoad)
  val wStore = log2Ceil(nStore)
  val LD = 0
  val ST = 1
  val ABOVE = 0
  val BELOW = 1
}

class QueuePriority(val nEntry: Int) extends Bundle {
  val segment = Vec(2, UInt(nEntry.W))
  val index = UInt(log2Ceil(nEntry).W)
  val valid   = Bool()
}

class LsEntry(val id_width: Int, val addr_width: Int) extends Bundle {
  val id   = UInt(id_width.W)
  val typ  = UInt(MT_X.getWidth.W)
  val fcn  = UInt(M_X.getWidth.W)
  val rd   = new ByPass(addr_width)
}

//  val valid = Bool()  // TODO: mem_en & valid
class LsIssue(val id_width: Int, val data_width: Int) extends Bundle { // from ALU
  val id = UInt(id_width.W)
  val addr = UInt(data_width.W)
  val data = UInt(data_width.W)
  val dataOK = Bool()
  val fcn = UInt(M_X.getWidth.W)
}

class LSqueueCtrl(val n: Int) extends Bundle with LsParam {
  val capty_gt = Vec(nInst, Bool())
  val inc_tail = Vec(nInst, Bool())
  val nxt_head = UInt(wLoad.W)
  val nxt_tail = Vec(nInst, UInt(wLoad.W))
  val push_0   = Bool()
  val segment  = Vec(2, UInt(n.W))
  val valid    = UInt(n.W)
  val above   = Bool()
  val ntEmpty = Bool()
}

class LdqCtrl(n: Int) extends LSqueueCtrl(n) with LsParam {
  val inc_head = UInt(2.W) //01: pop-1, 10: pop-2, 11: pop-many
}

class StqCtrl(n: Int) extends LSqueueCtrl(n) with LsParam {
  val inc_head = Bool()
}

class LoadStore(val data_width: Int) extends Module with LsParam {
  val io = IO(new Bundle {
    val entry = Flipped(Vec(nInst, Decoupled(new LsEntry(wOrder, wPhyAddr))))
    val load0 = Input(Bool())  //the first one, rename stage
    val store0 = Input(Bool()) //the first one, rename stage

    val issue = Input(Vec(3, Valid(new LsIssue(wOrder, data_width)))) // the 3 is not ready to issue immediately
    val issueable = Output(Valid(UInt(wOrder.W)))

    val memIO = new MemPortIo(data_width)
    val ldcommit = Output(new Commit(wOrder, wPhyAddr))
    val stcommit = Output(Valid(UInt(wOrder.W)))
    val wb_data = Output(UInt(data_width.W))

    val flush = Input(Bool())
    val id_head = Input(UInt(wOrder.W))
    val id_tail = Input(UInt(wOrder.W))
    // TODO: forward to inst queue
//    val kill  = Input() TODO
  })

  def next(ptr: UInt, nEntry: Int, num: Int): UInt = {
    require(num < 3)
    if (isPow2(nEntry)) ptr + num.U
    else { require(nEntry == 6)
      if (num == 1) Mux(ptr === (nEntry-1).U, 0.U, ptr + 1.U)
      else Mux(ptr(2).toBool, Cat(0.U(1.W), ptr(1,0)), ptr+2.U)
    }
  }
  def above(num: UInt, w: Int): UInt = VecInit((0 until w).map(_.U >= num)).asUInt
  def below(num: UInt, w: Int): UInt = VecInit((0 until w).map(_.U < num)).asUInt

  /*=================data structure===================*/
  val queue = RegInit({
    val w = Wire(new Bundle {
      val full = Bool()
      val head = Vec(nInst, UInt(wEntry.W))
      val tail = Vec(nInst, UInt(wEntry.W))
      val entry = Vec(nEntry, new LsEntry(wOrder, wPhyAddr))
    })
    w.entry := DontCare
    w.full := false.B
    w.head := VecInit(Seq(0.U, 1.U))
    w.tail := VecInit(Seq(0.U, 1.U))
    w
  })
  val queue_ctrl = Wire(new Bundle {
    val head_val = Vec(nInst, Bool())
    val capty_gt = Vec(nInst, Bool())
    val inc_head = Vec(nInst, Bool())
    val inc_tail = Vec(nInst, Bool())
    val nxt_head = Vec(nInst, UInt(wEntry.W))
    val nxt_tail = Vec(nInst, UInt(wEntry.W))
    val push_0   = Bool()
    val entry_0  = new LsEntry(wOrder, wPhyAddr)
  })
  queue_ctrl.head_val(0) := queue.full || queue.head(0) =/= queue.tail(0)
  queue_ctrl.head_val(1) := queue.head(1) =/= queue.tail(0) //based on head_val(0)
  queue_ctrl.capty_gt(0) := !queue.full
  queue_ctrl.capty_gt(1) := queue.tail(1) =/= queue.head(0) //based on capty_gt(0)
  queue_ctrl.nxt_head := queue.head.map(next(_, nEntry, 2))
  queue_ctrl.nxt_tail := queue.tail.map(next(_, nEntry, 2))

  io.entry(0).ready := queue_ctrl.capty_gt(0) //for time saving
  io.entry(1).ready := Mux(io.entry(0).valid, queue_ctrl.capty_gt(1), queue_ctrl.capty_gt(0)) //for time saving

  val ldQueue = RegInit({
    val w = Wire(new Bundle {
      val full = Bool()
      val head = UInt(wLoad.W)
      val tail = Vec(nInst, UInt(wLoad.W))

      val id   = Vec(nLoad, UInt(wOrder.W))
      val typ  = Vec(nLoad, UInt(MT_X.getWidth.W))
      val rd   = Vec(nLoad, new ByPass(wPhyAddr))
      val stQid = Vec(nLoad, UInt(wStore.W))
      val addr = Vec(nLoad, UInt(data_width.W))
      val addrOK = Vec(nLoad, Bool())
      val commit = Vec(nLoad, Bool())
      val unsafe = Vec(nLoad, Vec(nStore, Bool()))
    })
    w.full := false.B
    w.head := 0.U
    w.tail := VecInit(Seq(0.U, 1.U))

    w.id     := DontCare
    w.typ    := DontCare
    w.stQid  := DontCare
    w.addr   := DontCare
    w.addrOK := DontCare
    w.commit := DontCare
    w.unsafe := DontCare
    w
  })
  val ldQctrl = Wire(new LdqCtrl(nLoad))
  //to find first dataok but unsafe index, and move head to it
//  ldQctrl.inc_head := ;
  ldQctrl.nxt_head := ldQctrl.valid & ((~ldQueue.commit.asUInt).asUInt | ldQueue.unsafe.asUInt)
  ldQctrl.nxt_tail := ldQueue.tail.map(next(_, nLoad, 2))
  ldQctrl.capty_gt(0) := !ldQueue.full  || ldQctrl.inc_head =/= 0.U
  ldQctrl.capty_gt(1) := (!ldQueue.full && (ldQctrl.inc_head =/= 0.U || ldQueue.tail(1) =/= ldQueue.head)) || ldQctrl.inc_head > 1.U
  ldQctrl.segment(ABOVE) := above(ldQueue.head, nLoad)
  ldQctrl.segment(BELOW) := below(ldQueue.tail(0), nLoad)
  ldQctrl.above := ldQueue.head < ldQueue.tail(0)
  ldQctrl.ntEmpty := ldQueue.head=/=ldQueue.tail(0) || ldQueue.full
  ldQctrl.valid := Mux(ldQctrl.above, ldQctrl.segment.reduce(_&_), Fill(nLoad, ldQctrl.ntEmpty) & ldQctrl.segment.reduce(_|_))

  val stQueue = RegInit({
    val w = Wire(new Bundle {
      val full = Bool()
      val head = UInt(wStore.W)
      val tail = Vec(nInst, UInt(wStore.W))

      val id     = Vec(nStore, UInt(wOrder.W))
      val typ    = Vec(nStore, UInt(MT_X.getWidth.W))
      val ldQid  = Vec(nStore, UInt(wLoad.W))
      val addr   = Vec(nStore, UInt(data_width.W))
      val data   = Vec(nStore, UInt(data_width.W))
      val addrOK = Vec(nStore, Bool())
      val dataOK = Vec(nStore, Bool())
      val wb_mem = Vec(nStore, Bool())
    })
    w.full := false.B
    w.head := 0.U
    w.tail := VecInit(Seq(0.U, 1.U))

    w.id   := DontCare
    w.typ  := DontCare
    w.ldQid:= DontCare
    w.addr := DontCare
    w.data := DontCare
    w.addrOK := DontCare
    w.dataOK := DontCare
    w.wb_mem := DontCare
    w
  })
  val stQctrl = Wire(new StqCtrl(nStore))
  stQctrl.nxt_head := next(stQueue.head, nLoad, 1)
  stQctrl.nxt_tail := stQueue.tail.map(next(_, nLoad, 2))
  stQctrl.capty_gt(0) := !stQueue.full || stQctrl.inc_head
  stQctrl.capty_gt(1) := !stQueue.full &&(stQctrl.inc_head || stQueue.tail(1) =/= stQueue.head)
  stQctrl.segment(ABOVE) := above(stQueue.head, nStore)
  stQctrl.segment(BELOW) := below(stQueue.tail(0), nStore)
  stQctrl.above   := stQueue.head < stQueue.tail(0)
  stQctrl.ntEmpty := stQueue.head =/= stQueue.tail(0) || stQueue.full
  stQctrl.valid   := Mux(stQctrl.above, stQctrl.segment.reduce(_&_), Fill(nStore, stQctrl.ntEmpty) & stQctrl.segment.reduce(_|_))
  /*==================================================*/
  /*===================rename stage===================*/
  val dec = Wire(new Bundle {
    val io_load  = Vec(nInst, Bool())
    val io_store = Vec(nInst, Bool())
    val et_load  = Vec(nInst, Bool())
    val et_store = Vec(nInst, Bool())
    val io1ready = Vec(2, Bool())
    val et1ready = Vec(2, Bool())

    val ld_valid = Vec(nInst, Bool())
    val st_valid = Vec(nInst, Bool())
    val ls_id    = Vec(nInst, UInt())
    val ls_typ   = Vec(nInst, UInt())
    val ls_rd    = Vec(nInst, new ByPass(wPhyAddr))
    val ls_valid0 = Vec(nInst, Bool())
  })
  dec.et_load(0)  := queue.entry(queue.head(0)).fcn === M_XRD
  dec.et_store(0) := queue.entry(queue.head(0)).fcn === M_XWR
  dec.et_load(1)  := queue.entry(queue.head(1)).fcn === M_XRD && queue_ctrl.head_val(1)
  dec.et_store(1) := queue.entry(queue.head(1)).fcn === M_XWR && queue_ctrl.head_val(1)
  dec.io_load  := io.entry.map(i => i.valid && i.bits.fcn === M_XRD)
  dec.io_store := io.entry.map(i => i.valid && i.bits.fcn === M_XWR)
  dec.ld_valid := (0 until nInst).map(i => Mux(queue_ctrl.head_val(0), dec.et_load(i),  dec.io_load(i)))
  dec.st_valid := (0 until nInst).map(i => Mux(queue_ctrl.head_val(0), dec.et_store(i), dec.io_store(i)))

  dec.ls_valid0(LD) := Mux(queue_ctrl.head_val(0), dec.et_load, io.load0)
  dec.ls_valid0(ST) := Mux(queue_ctrl.head_val(0), dec.et_store, io.store0)
  ldQctrl.push_0 := dec.ld_valid(0) && ldQctrl.capty_gt(0)
  stQctrl.push_0 := dec.st_valid(0) && stQctrl.capty_gt(0)
  ldQctrl.inc_tail(0) := ldQctrl.push_0 || (dec.ld_valid(1) && ldQctrl.capty_gt(0) && (!dec.ls_valid0(LD) || stQctrl.capty_gt(0)))
  ldQctrl.inc_tail(1) := ldQctrl.push_0 &&  dec.ld_valid(1) && ldQctrl.capty_gt(1)
  stQctrl.inc_tail(0) := stQctrl.push_0 || (dec.st_valid(1) && stQctrl.capty_gt(0) && (!dec.ls_valid0(ST) || ldQctrl.capty_gt(0)))
  stQctrl.inc_tail(1) := stQctrl.push_0 &&  dec.st_valid(1) && stQctrl.capty_gt(1)

  for (i <- 0 until nInst) {
    dec.ls_id(i)  := Mux(queue_ctrl.head_val(0), queue.entry(queue.head(i)).id, io.entry(i).bits.id)
    dec.ls_typ(i) := Mux(queue_ctrl.head_val(0), queue.entry(queue.head(i)).typ, io.entry(i).bits.typ)
    dec.ls_rd(i)  := Mux(queue_ctrl.head_val(0), queue.entry(queue.head(i)).rd, io.entry(i).bits.rd)
  }
  when (ldQctrl.inc_tail(0)) {
    ldQueue.id(ldQueue.head(0))   := Mux(ldQctrl.push_0, dec.ls_id(0), dec.ls_id(1))
    ldQueue.rd(ldQueue.head(0))   := Mux(ldQctrl.push_0, dec.ls_rd(0), dec.ls_rd(1))
    ldQueue.typ(ldQueue.head(0))  := Mux(ldQctrl.push_0, dec.ls_typ(0), dec.ls_typ(1))
    ldQueue.addrOK(ldQueue.head(0)) := false.B
    ldQueue.commit(ldQueue.head(0)) := false.B
    ldQueue.stQid(ldQueue.head(0)) := stQueue.tail(0)
    when (ldQctrl.inc_tail(1)) {
      ldQueue.id(ldQueue.head(1))   := dec.ls_id(1)
      ldQueue.typ(ldQueue.head(1))  := dec.ls_typ(1)
      ldQueue.addrOK(ldQueue.head(1)) := false.B
      ldQueue.commit(ldQueue.head(1)) := false.B
      ldQueue.stQid(ldQueue.head(1)) := Mux(stQctrl.inc_tail(0), stQueue.tail(1), stQueue.tail(0))
      ldQueue.tail(0) := ldQctrl.nxt_tail(0)
      ldQueue.tail(1) := ldQctrl.nxt_tail(1)
    }.otherwise {
      ldQueue.tail(0) := ldQueue.tail(1)
      ldQueue.tail(1) := ldQctrl.nxt_tail(0)
    }
  }
  when (stQctrl.inc_tail(0)) {
    stQueue.id(stQueue.head(0))   := Mux(stQctrl.push_0, dec.ls_id(0), dec.ls_id(1))
    stQueue.typ(stQueue.head(0))  := Mux(stQctrl.push_0, dec.ls_typ(0), dec.ls_typ(1))
    stQueue.ldQid(stQueue.head(0)) := ldQueue.tail(0)
    stQueue.addrOK(stQueue.head(0)) := false.B
    stQueue.dataOK(stQueue.head(0)) := false.B
    when (stQctrl.inc_tail(1)) {
      stQueue.id(stQueue.head(1))   := dec.ls_id(1)
      stQueue.typ(stQueue.head(1))  := dec.ls_typ(1)
      stQueue.ldQid(stQueue.head(1)) := Mux(ldQctrl.inc_tail(0), ldQueue.tail(1), ldQueue.tail(0))
      stQueue.addrOK(stQueue.head(1)) := false.B
      stQueue.dataOK(stQueue.head(1)) := false.B
      stQueue.tail(0) := stQctrl.nxt_tail(0)
      stQueue.tail(1) := stQctrl.nxt_tail(1)
    }.otherwise {
      stQueue.tail(0) := stQueue.tail(1)
      stQueue.tail(1) := stQctrl.nxt_tail(0)
    }
  }
  when (ldQctrl.inc_tail(0)) {
    when (ldQctrl.inc_tail(1)) {
      when (ldQctrl.inc_head(1)) {
        when (ldQctrl.inc_head(0)) {ldQueue.full := false.B}
      }.elsewhen(ldQctrl.inc_head(0)) {
        ldQueue.full := ldQctrl.nxt_tail(0) === ldQctrl.nxt_head
      }.otherwise {
        ldQueue.full := ldQctrl.nxt_tail(0) === ldQueue.head
      }
    }.otherwise {
      when (ldQctrl.inc_head(1)) {ldQueue.full := false.B
      }.elsewhen(!ldQctrl.inc_head(0)) {
        ldQueue.full := ldQueue.tail(1) === ldQueue.head
      }
    }
  }.elsewhen(ldQctrl.inc_head.orR) {ldQueue.full := false.B}
  when (stQctrl.inc_tail(0)) {
    when (stQctrl.inc_tail(1)) {
      when (stQctrl.inc_head) {
        stQueue.full := stQctrl.nxt_tail(0) === stQctrl.nxt_head
      }.otherwise {
        stQueue.full := stQctrl.nxt_tail(0) === stQueue.head
      }
    }.elsewhen(!stQctrl.inc_head) {
      stQueue.full := stQueue.tail(1) === stQueue.head
    }
  }.elsewhen(stQctrl.inc_head){ stQueue.full := false.B}

  when (ldQctrl.inc_head.orR) { ldQueue.head := ldQctrl.nxt_head }
  when (stQctrl.inc_head)     { stQueue.head := stQctrl.nxt_head }

  dec.io1ready(LD) := ldQctrl.capty_gt(1) || (!io.load0  && ldQctrl.capty_gt(0))
  dec.io1ready(ST) := stQctrl.capty_gt(1) || (!io.store0 && stQctrl.capty_gt(0))
  queue_ctrl.push_0 := (dec.io_load(0) && !ldQctrl.capty_gt(0))   ||
                       (dec.io_store(0) && !stQctrl.capty_gt(0))  ||
                       (queue_ctrl.head_val(0) && io.entry(0).valid)

  queue_ctrl.inc_tail(0) := queue_ctrl.capty_gt(0) && (queue_ctrl.push_0 ||
                            (dec.io_load(1)  && !dec.io1ready(LD)) ||
                            (dec.io_store(1) && !dec.io1ready(ST)) ||
                            (queue_ctrl.head_val(0) && io.entry(1).valid))

  queue_ctrl.inc_tail(1) := queue_ctrl.push_0 && io.entry(1).valid && queue_ctrl.capty_gt(1)
  queue_ctrl.entry_0 := Mux(queue_ctrl.push_0, io.entry(0).bits, io.entry(1).bits)

  dec.et1ready(LD) := ldQctrl.capty_gt(1) || (!dec.et_load(0)  && ldQctrl.capty_gt(0))
  dec.et1ready(ST) := stQctrl.capty_gt(1) || (!dec.et_store(0) && stQctrl.capty_gt(0))
  queue_ctrl.inc_head(0) := ((dec.et_load(0) && ldQctrl.capty_gt(0)) || (dec.et_store(0) && stQctrl.capty_gt(0))) && queue_ctrl.head_val(0)
  queue_ctrl.inc_head(1) := ((dec.et_load(1) && dec.et1ready(LD)) || (dec.et_store(1) && dec.et1ready(ST))) && queue_ctrl.head_val(1)

  when(queue_ctrl.inc_tail(0)) {
    queue.entry(queue.tail(0)) := queue_ctrl.entry_0
    when (queue_ctrl.inc_tail(1)) {
      queue.entry(queue.tail(1)) := io.entry(1).bits
      queue.tail(0) := queue_ctrl.nxt_tail(0)
      queue.tail(1) := queue_ctrl.nxt_tail(1)
    }.otherwise {
      queue.tail(0) := queue.tail(1)
      queue.tail(1) := queue_ctrl.nxt_tail(0)
    }
  }
  when(queue_ctrl.inc_head(0)) {
    when(queue_ctrl.inc_head(1)) {
      queue.head(0) := queue_ctrl.nxt_head(0)
      queue.head(1) := queue_ctrl.nxt_head(1)
    }.otherwise {
      queue.head(0) := queue.head(1)
      queue.head(1) := queue_ctrl.nxt_head(0)
    }
  }
  when (queue_ctrl.inc_tail(0)) {
    when (queue_ctrl.inc_tail(1)) {
      when (queue_ctrl.inc_head(0)) {
        when(queue_ctrl.inc_head(1)) {
          queue.full := queue_ctrl.nxt_tail(0) === queue.head(1)
        }.otherwise { queue.full := queue_ctrl.nxt_tail(0) === queue.head(0) }
      }
    }.otherwise {
      when (queue_ctrl.inc_head(0)) {
        when(queue_ctrl.inc_head(1)) {queue.full := false.B}
      }.otherwise {
        queue.full := queue.head(1) === queue.head(0)
      }
    }
  }.elsewhen(queue_ctrl.inc_head(0)) {queue.full := false.B}
  /*==================================================*/
  /*==================execute stage===================*/
  val exe = Wire(new Bundle {
    val ldidx = Vec(3, UInt(wLoad.W))
    val stidx = Vec(3, UInt(wStore.W))
  })
  for (i <- 0 until 3) {
    exe.ldidx(i) := OHToUInt(VecInit(ldQueue.id.map(_ === io.issue(i).bits.id)).asUInt & ldQctrl.valid)
    exe.stidx(i) := OHToUInt(VecInit(stQueue.id.map(_ === io.issue(i).bits.id)).asUInt & stQctrl.valid)
    when (io.issue(i).valid) {
      when (io.issue(i).bits.fcn === M_XRD) {
        ldQueue.addrOK(exe.ldidx(i)) := true.B
        ldQueue.addr(exe.ldidx(i))   := io.issue(i).bits.addr
      }.otherwise {
        stQueue.addrOK(exe.stidx(i)) := true.B
        stQueue.addr(exe.stidx(i))   := io.issue(i).bits.addr
        stQueue.dataOK(exe.stidx(i)) := io.issue(i).bits.dataOK
        stQueue.data(exe.stidx(i))   := io.issue(i).bits.data
      }
    }
  }
  /*==================================================*/
  /*===================memory stage===================*/
  val mem = Wire(new Bundle {
    val ld = new QueuePriority(nLoad)
    val st = new QueuePriority(nStore)
    val typ  = UInt(MT_X.getWidth.W)
    val fcn  = UInt(M_X.getWidth.W)
    val addr = UInt(data_width.W)
    val above = Bool()
    val forward = Vec(data_width/2, Vec(2, Vec(nStore, Bool())))
    val unsafe = Vec(nStore, Bool())
  })

  mem.ld.segment(ABOVE) := ldQctrl.segment(ABOVE) & ldQueue.addrOK.asUInt
  mem.ld.segment(BELOW) := ldQctrl.segment(BELOW) & ldQueue.addrOK.asUInt
  mem.ld.index := Mux(ldQctrl.above || mem.ld.segment(ABOVE).orR,
    PriorityEncoder(mem.ld.segment(ABOVE)), PriorityEncoder(mem.ld.segment(BELOW))) //backward pick
  mem.ld.valid  := (ldQueue.addrOK.asUInt & ldQctrl.valid).orR
  mem.st.segment(ABOVE) := stQctrl.segment(ABOVE) & stQueue.addrOK.asUInt
  mem.st.segment(BELOW) := stQctrl.segment(BELOW) & stQueue.addrOK.asUInt
  mem.st.index := Mux(stQctrl.above || mem.st.segment(ABOVE).orR,
    PriorityEncoder(mem.st.segment(ABOVE)), PriorityEncoder(mem.st.segment(BELOW))) //backward pick
  mem.st.valid  := (stQueue.addrOK.asUInt & stQctrl.valid).orR

  mem.fcn  := (!mem.ld.valid || (mem.st.valid &&
    CmpId(stQueue.id(mem.st.index), ldQueue.id(mem.ld.index), io.id_head))).toUInt
  mem.typ  := Mux(mem.fcn === M_XWR, stQueue.typ(mem.st.index), ldQueue.typ(mem.ld.index))
  val ld_addr = ldQueue.addr(mem.ld.index)
  mem.addr := Mux(mem.fcn === M_XWR, stQueue.addr(mem.st.index), ld_addr)

  when (io.memIO.req.valid) { //FIXME: it is not right
    when (mem.fcn === M_XRD) {
      ldQueue.addrOK(mem.ld.index) := false.B
    }.otherwise {
      stQueue.addrOK(mem.st.index) := false.B
    }
  }

  io.memIO.req.valid     := (mem.ld.valid || mem.st.valid) && (wb_reg.fcn === M_XWR || io.memIO.resp.valid) //&& !frwd.stall FIXME
  io.memIO.req.bits.fcn  := mem.fcn
  io.memIO.req.bits.typ  := mem.typ
  io.memIO.req.bits.addr := mem.addr
  io.memIO.req.bits.data := stQueue.data(mem.st.index)

  //compare address
  val base_equal = stQueue.addr.map(_(data_width-1,2) === ld_addr(data_width-1,2))
  val byte_forward = Wire(Vec(data_width/8, Vec(nStore, Bool())))
  for (i <- 0 until data_width/8) {
    for (j <- 0 until nStore) {
      byte_forward(i)(j) := base_equal(j) && (stQueue.typ(j) === MT_W ||
        (stQueue.typ(j) === MT_B && stQueue.addr(j)(1,0) === i.U) ||
        (stQueue.typ(j) === MT_H && stQueue.addr(j)(1,0) === (i/2).U))
    }
    mem.forward(i)(ABOVE) := byte_forward(i).asUInt & above(stQueue.head, nStore) & stQueue.addrOK.asUInt
    mem.forward(i)(BELOW) := byte_forward(i).asUInt & below(ldQueue.stQid(mem.ld.index), nStore) & stQueue.addrOK.asUInt
  }

  mem.above := ldQueue.stQid(mem.ld.index) > stQueue.head
  mem.unsafe:= Mux(mem.above, above(stQueue.head, nStore) & below(ldQueue.stQid(mem.ld.index), nStore),
    above(stQueue.head, nStore) | below(ldQueue.stQid(mem.ld.index), nStore)) | (~stQueue.addrOK.asUInt).asUInt

  /*==================================================*/
  /*===================load wtback stage===================*/
  val wb_reg = RegInit({
    val w = Wire(new Bundle {
      val fcn = UInt(M_X.getWidth.W) // very important
      val typ = UInt()
      val above   = Bool()
      val forward = Vec(data_width/2, Vec(2, UInt(nStore.W)))
      val offset  = UInt(2.W)
      val unsafe = Vec(nStore, Bool()) //for load
      val ldQidx = UInt(wLoad.W) //for store
    })
    w.fcn  := M_XWR
    w.above   := DontCare
    w.forward := DontCare
    w.offset  := DontCare
    w.unsafe  := DontCare
    w.ldQidx  := DontCare
    w
  })

  val wb = Wire(new Bundle {
    val data_valid = Bool()
  })
  val frwd = Wire(new Bundle {
    val index = Vec(data_width/8, UInt(wLoad.W))
    val bytes = Vec(data_width/8, UInt(8.W))
    val valid = Vec(data_width/8, Bool())
    val ready = Vec(data_width/8, Bool())
//    val stall = Bool()
  })

//  frwd.stall := (frwd.valid.asUInt() & frwd.notOK.asUInt).orR
  for (i <- 0 until data_width/8) {
    frwd.index(i) := Mux(wb_reg.above || wb_reg.forward(i)(BELOW).orR,
      Reverse(PriorityEncoderOH(Reverse(wb_reg.forward(i)(BELOW)))),
      Reverse(PriorityEncoderOH(Reverse(wb_reg.forward(i)(BELOW))))) //forward pick
    frwd.bytes(i) := Mux1H(frwd.index(i), stQueue.data)(8*(i+1), 8*i)
    frwd.valid(i) := Mux(wb_reg.above, wb_reg.forward(i).reduce(_&_).orR, wb_reg.forward(i).reduce(_|_).orR)
    frwd.ready(i) := Mux1H(frwd.index(i), stQueue.dataOK) || !(wb_reg.typ === MT_W ||
      (wb_reg.typ(1,0) === 1.U && wb_reg.offset === i.U) ||
      (wb_reg.typ(1,0) === 2.U && wb_reg.offset === (i/2).U)) && wb_reg.fcn === M_XRD
  }

  when (wb.data_valid) {
    ldQueue.commit(wb_reg.ldQidx) := true.B
    ldQueue.unsafe(wb_reg.ldQidx) := wb_reg.unsafe
  }

  io.stcommit.valid := stQueue.addrOK(stQueue.head) && stQueue.dataOK(stQueue.head)
  io.stcommit.bits := stQueue.id(stQueue.head)
  io.ldcommit.valid := io.memIO.resp.valid || frwd.valid.asUInt.andR
  io.ldcommit.id := ldQueue.id(wb_reg.ldQidx)
  io.ldcommit.wb := ldQueue.rd(wb_reg.ldQidx)

  io.wb_data := Cat(
    Mux(frwd.valid(3), frwd.bytes(3), io.memIO.resp.bits(31,24)),
    Mux(frwd.valid(2), frwd.bytes(2), io.memIO.resp.bits(23,16)),
    Mux(frwd.valid(1), frwd.bytes(1), io.memIO.resp.bits(15, 8)),
    Mux(frwd.valid(0), frwd.bytes(0), io.memIO.resp.bits( 7, 0))
  )

  /*==================================================*/
  /*===================store wtback stage===================*/

}
