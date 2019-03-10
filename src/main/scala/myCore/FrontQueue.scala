package myCore

import chisel3._
import chisel3.util._
import common.CPUConfig

trait Pram {
  val nPhyAddr = 64
  val nOrder   = 32
  require(isPow2(nPhyAddr))
  require(isPow2(nOrder))
  val wPhyAddr = log2Ceil(nPhyAddr)
  val wPhyCnt  = log2Ceil(nPhyAddr+1)
  val wOrder   = log2Ceil(nOrder)
  val nCommit  = 4
  val nBrchjr  = 4
  val nInst    = 2
}

trait FrontParam extends Pram {
  val nEntry  = 8
  val wEntry = log2Ceil(nEntry)
  val head = 0
  val tail = 1
}

class FrontInfo(implicit val conf: CPUConfig) extends Bundle {
  val valid = Bool()
  val inst  = UInt(conf.inst_width.W)
  val pc    = UInt(conf.data_width.W)
}

class FrontTotal(implicit conf: CPUConfig) extends FrontInfo {
  val pred = new Predict(conf.data_width)
}

class FrontBuffer(val nEntry: Int)
                 (implicit val conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val raddr = Input(UInt(log2Ceil(nEntry).W))
    val rdata = Output(new FrontInfo)
    val waddr = Input(Valid(UInt(log2Ceil(nEntry).W)))
    val wdata = Input(new FrontInfo)
  })
  val buffer = Mem(nEntry, new FrontInfo)
  io.rdata := buffer(io.raddr)
  when (io.waddr.valid) { buffer(io.waddr.bits) := io.wdata }
}

class RePredict(val data_width: Int) extends Bundle {
  val redirct = Bool()
  val tgt = UInt(data_width.W)
}

class FrontQueue(implicit val conf: CPUConfig) extends Module with FrontParam {
  val io = IO(new Bundle {
    val forward = Output(Bool())
    val flush   = Input(Bool()) //from pipeline
    val dec_bj  = Input(Vec(nInst, Bool())) //from dec stage
    val cancel  = Input(Valid(new RePredict(conf.data_width))) //rename stage(just after dec stage) cancels dec stage
    val ready   = Input(Vec(nInst, Bool()))
    val in  = Input(Vec(nInst, new FrontTotal))
    val out = Output(Vec(nInst, new FrontInfo))
    val pred = Output(new Predict(conf.data_width))
  })

  def next(ptr: UInt, nEntry: Int): UInt = {
    if (isPow2(nEntry)) ptr + 1.U
    else Mux(ptr === (nEntry-1).U, 0.U, ptr + 1.U)
  }
  def prev(ptr: UInt, nEntry: Int): UInt = {
    if (isPow2(nEntry)) ptr - 1.U
    else Mux(ptr === 0.U, (nEntry-1).U, ptr - 1.U)
  }

  val ptr    = RegInit(VecInit(Seq.fill(2)(0.U(wEntry.W))))
  val ptrnxt = ptr.map(i => next(i, nEntry))
  val ptrInc = Wire(Vec(2, Bool()))
  val full   = RegInit(false.B)
  val empty = !full && ptr(head) === ptr(tail)

  val queue  = Array.fill(nInst)(Module(new FrontBuffer(nEntry)).io)
  val preds  = Mem(nEntry, new Predict(conf.data_width))
  val cancel = RegInit(VecInit(Seq.fill(nEntry)(false.B)))

  val reg_front = RegInit(VecInit(Seq.fill(nInst) {
    val w = Wire(new FrontInfo)
    w.valid := false.B
    w.inst := DontCare
    w.pc := DontCare
    w
  }))
  val reg_pred  = Reg(new Predict(conf.data_width))

  val dec_valid = Wire(Vec(nInst, Bool()))
  val dec_bj  = (0 until nInst).map(i => io.dec_bj(i) && dec_valid(i))
  val forward = (0 until nInst).map(i => !reg_front(i).valid || io.ready(i))
  val segment: Bool = dec_bj.reduce(_&&_)
  val pump: Bool = forward.reduce(_&&_)

  val mask = RegInit(true.B)
  when (forward(0)) { mask := segment }

  dec_valid(0) := io.in(0).valid && mask
  dec_valid(1) := io.in(1).valid

  when (io.flush) {
    reg_front(0).valid := false.B
  }.elsewhen (forward(0)) {
    when (!forward(1) || io.cancel.valid) {
      reg_front(0).valid := false.B
    }.elsewhen (empty) {
      reg_front(0).valid := dec_valid(0)
    }.otherwise {
      reg_front(0).valid := queue(0).rdata.valid
    }
  }

  when (io.flush) {
    reg_front(1).valid := false.B
  }.elsewhen (forward(1)) {
    when (io.cancel.valid) {
      reg_front(1).valid := false.B
    }.elsewhen (empty) {
      reg_front(1).valid := dec_valid(1) && !segment
    }.otherwise {
      reg_front(1).valid := queue(1).rdata.valid
    }
  }

  //read part
  when (pump) {
    for (i <- 0 until nInst) {
      when(empty) {
        reg_front(i).pc   := io.in(i).pc
        reg_front(i).inst := io.in(i).inst
      }.otherwise {
        reg_front(i).pc   := queue(i).rdata.pc
        reg_front(i).inst := queue(i).rdata.inst
      }
    }
    when (empty) {
      when (dec_bj(0))      { reg_pred := io.in(0).pred
      }.elsewhen(dec_bj(1)) { reg_pred := io.in(1).pred }
    }.otherwise{ reg_pred := preds(ptr(head))  }
  }

  val prev_tail = prev(ptr(tail), nEntry)
  val sel_0 = RegInit(true.B)
  cancel(prev_tail) := sel_0 && io.cancel.valid
  preds(prev_tail).tgt := io.cancel.bits.tgt //the jump addr, TODO: only for branch not for jalr

  when (ptrInc(tail)) {
    when (dec_bj(0)) { preds(ptr(tail)) := io.in(0).pred
    }.elsewhen(dec_bj(1)) { preds(ptr(tail)) := io.in(1).pred }
    sel_0 := dec_bj(0)
  }
  when (io.cancel.valid) {
    preds(prev_tail).redirect := io.cancel.bits.redirct
  }

  for (i <- 0 until nInst) {
    queue(i).raddr       := ptr(head)
    queue(i).waddr.bits  := ptr(tail)
    queue(i).waddr.valid := ptrInc(tail)
    queue(i).wdata.valid := dec_valid(i)
    queue(i).wdata.pc    := io.in(i).pc
    queue(i).wdata.inst  := io.in(i).inst

    when(io.flush)        { ptr(i) := 0.U
    }.elsewhen(ptrInc(i)) { ptr(i) := ptrnxt(i) }

    io.out(i).inst  := reg_front(i).inst
    io.out(i).pc    := reg_front(i).pc
    io.out(i).valid := reg_front(i).valid
  }

  io.pred := reg_pred

  ptrInc(head) := pump && !empty
  ptrInc(tail) := dec_valid.reduce(_||_) && !io.cancel.valid && ((empty && !pump) || (full  &&  pump) || (!full && !empty))
  io.forward := (!full || pump) && !segment

  when (io.flush) {
    full := false.B
  }.otherwise {
    when (ptrInc(head) && !ptrInc(tail)) { full := false.B }
    when (ptrInc(tail) && !ptrInc(head) && ptrnxt(tail) === ptr(head)) { full := true.B }
  }
}