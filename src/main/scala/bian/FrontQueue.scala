package bian

import chisel3._
import chisel3.util.{Cat, Valid, isPow2, log2Ceil}
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

trait FrontParam {
  val nEntry  = 8
  val wEntry = log2Ceil(nEntry)
  val head = 0
  val tail = 1
}

class PredictInfo(val data_width: Int) extends Bundle {
  val bj_sel = Vec(2, Bool()) //determine pick which btb
  val split  = Bool()
  val pred   = new Predict(data_width)
  val branch = Bool()
  val jump   = UInt(Jump.NUM.W)
}

class FrontQueue(implicit val conf: CPUConfig) extends Module with FrontParam {
  val io = IO(new Bundle {
    val xcpt = Input(Valid(UInt(conf.data_width.W)))
    val kill = Input(Valid(UInt(conf.data_width.W)))
    val inst_i  = Input(Vec(2, Valid(UInt(conf.inst_width.W)))) //valid & !split
    val info_i  = Input(new PredictInfo(conf.data_width))
    val inst_o  = Output(Vec(2, Valid(UInt(conf.inst_width.W))))
    val info_o  = Output(new PredictInfo(conf.data_width))
    val forward = Output(Bool())
    val ready = Input(Vec(conf.nInst, Bool()))
    val pc = Output(Vec(2, UInt(conf.data_width.W)))
  })

  def next(ptr: UInt, nEntry: Int): UInt = {
    if (isPow2(nEntry)) ptr + 1.U
    else Mux(ptr === (nEntry-1).U, 0.U, ptr + 1.U)
  }

  val ptr  = RegInit(VecInit(Seq.fill(2)(0.U(wEntry.W))))
  val full = RegInit(false.B)
  val flush = io.xcpt.valid || io.kill.valid
  val ptr_inc  = Wire(Vec(2, Bool()))
  val ptr_next = ptr.map(i => next(i, nEntry))
  val inst = RegInit(VecInit(Seq.fill(conf.nInst) {
    val w = Wire(Valid(UInt(conf.inst_width.W)))
    w.valid := false.B
    w.bits := DontCare
    w
  }))

  io.inst_o(0).valid := inst(0).valid
  io.inst_o(1).valid := inst(1).valid && !(io.info_o.split && io.info_o.bj_sel(0) && inst(0).valid)
  for (i <- 0 until conf.nInst) io.inst_o(i).bits  := inst(i).bits

  val instQueue = Mem(nEntry, Vec(conf.nInst, Valid(UInt(conf.inst_width.W))))
  val infoQueue = Mem(nEntry, new PredictInfo(conf.data_width))

  val headNtail = ptr(head) =/= ptr(tail)
  val out_valid = io.inst_o.map(_.valid).reduce(_||_)
  val in_valid = io.inst_i.map(_.valid).reduce(_||_)
  val out_fire = (0 until conf.nInst).map(i => !io.inst_o(i).valid || io.ready(i)).reduce(_||_)
  io.forward := !full || out_fire

  ptr_inc(head) := out_fire  && (full || headNtail)
  ptr_inc(tail) := out_valid && (Mux(full, out_fire, !out_fire) || headNtail)

  when(flush) {ptr(tail) := 0.U
  }.elsewhen (ptr_inc(tail)) {ptr(tail) := ptr_next(tail)}
  when(flush) {ptr(head) := 0.U
  }.elsewhen (ptr_inc(head)) {ptr(head) := ptr_next(head)}

  when (flush) {
    for (i <- 0 until conf.nInst) inst(i).valid := false.B
  }.elsewhen(inst(1).valid && !io.ready(1) && io.ready(0)) {
    inst(0).valid := false.B
  }.elsewhen(ptr_inc(head)) {
    inst := instQueue(ptr(head))
  }.elsewhen(out_fire && out_valid) {
    inst := io.inst_i
  }

  when(flush) { full := false.B
  }.otherwise {
    when(ptr_inc(head) && !ptr_inc(tail)) {
      full := false.B
    }
    when(ptr_inc(tail) && !ptr_inc(head)) {
      full := ptr_next(tail) === ptr(head)
    }
  }

  val pred_ctrl = RegInit({
    val w = Wire(new Bundle {
      val empty = Bool()
      val ptr = Vec(2, UInt(wEntry.W))
      val valid = Bool()
    })
    w.empty := true.B
    w.valid := false.B
    w.ptr(head) := 0.U
    w.ptr(tail) := 0.U
    w
  })
  val pred_ptr_inc  = Wire(Vec(2, Bool()))
  val pred_ptr_next = pred_ctrl.ptr.map(i => next(i, nEntry))
  when(flush) {
    pred_ctrl.valid := false.B
  }.elsewhen(!full) {
    pred_ctrl.valid := in_valid
  }.elsewhen(in_valid) {
    pred_ctrl.valid := true.B
  }

  val info = infoQueue(pred_ctrl.ptr(head))
  io.info_o := Mux(pred_ctrl.empty, io.info_i, info)

  pred_ptr_inc(tail) :=  pred_ctrl.valid && (headNtail ||
    Mux(pred_ctrl.empty, !out_fire, out_fire && pred_ctrl.ptr(head) === pred_ctrl.ptr(tail)))
  pred_ptr_inc(head) := !pred_ctrl.empty && out_fire

  when(flush) {
    pred_ctrl.ptr(tail) := 0.U
  }.elsewhen (pred_ptr_inc(tail)) {
    pred_ctrl.ptr(tail) := pred_ptr_next(tail)
  }
  when(flush) {
    pred_ctrl.ptr(tail) := 0.U
  }.elsewhen (pred_ptr_inc(head)) {
    pred_ctrl.ptr(head) := pred_ptr_next(head)
  }

  when(flush) {pred_ctrl.empty := true.B
  }.otherwise {
    when (pred_ptr_inc(tail) && !pred_ptr_inc(head)) {
      pred_ctrl.empty := false.B
    }
    when (pred_ptr_inc(head) && !pred_ptr_inc(tail)) {
      pred_ctrl.empty := pred_ptr_next(head) === pred_ctrl.ptr(tail)
    }
  }

  val pc = RegInit(START_ADDR)
  io.pc(0) := Cat(pc(conf.inst_width-1, conf.pcLSB+1), 0.U(1.W), 0.U(conf.pcLSB))
  val split = !pred_ctrl.empty && info.bj_sel(0) && inst(0).valid && info.pred.redirect
  io.pc(1) := Mux(split, info.pred.tgt, Cat(pc(conf.inst_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB)))

  when(io.xcpt.valid) {
    pc := io.xcpt.bits
  }.elsewhen(io.kill.valid) {
    pc := io.kill.bits
  }.elsewhen(out_fire && out_valid) {
    when (split && io.inst_o(1).valid) {
      pc := info.pred.tgt + 4.U
    }.elsewhen(io.info_o.pred.redirect &&
      (0 until conf.nInst).map(i => io.info_o.bj_sel(i) && io.inst_o(i).valid).reduce(_||_)) {
      pc := io.info_o.pred.tgt
    }.otherwise {
      pc := pc + 8.U
    }
  }

  //cached
  when(ptr_inc(tail)) {instQueue(ptr(tail)) := io.inst_i}
  when(pred_ptr_inc(tail)) {infoQueue(pred_ctrl.ptr(tail)) := io.info_i}
}