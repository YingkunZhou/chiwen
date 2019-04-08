package bian

import chisel3._
import chisel3.util._
import common.CPUConfig

trait FrontParam {
  val nEntry  = 16
  require(isPow2(nEntry))
  val wEntry = log2Ceil(nEntry)
  val H = 0
  val T = 1
}

object RingQNext {
  def apply(ptr: UInt, nEntry: Int): UInt = {
    val w = log2Ceil(nEntry)
    if (isPow2(w)) ptr + 1.U
    else Mux(ptr(w-1,0) === (nEntry-1).U, Cat(~ptr(w), 0.U(w.W)), ptr + 1.U)
  }
}

object RingNfull {
  def apply(ptr: Vec[UInt], w: Int): Bool =
    ptr(0)(w-1,0) =/= ptr(1)(w-1,0) || ptr(0)(w) === ptr(1)(w)
}

class FrontIO(implicit conf: CPUConfig) extends Bundle {
  val inst = Vec(conf.nInst, Flipped(ValidIO(UInt(conf.inst_width.W))))
  val pred = Input(new PredictInfo(conf.data_width))
  val pc_split = Input(Bool())
  val inst_split = Input(Bool())
}

class FrontQueue(implicit val conf: CPUConfig) extends Module with FrontParam {
  val io = IO(new Bundle {
    val xcpt = Input(Valid(UInt(conf.data_width.W)))
    val kill = Input(Valid(UInt(conf.data_width.W)))
    val in  = new FrontIO
    val forward = Output(Bool())

    val inst = Vec(conf.nInst, DecoupledIO(UInt(conf.inst_width.W)))
    val pred = Output(new PredictInfo(conf.data_width))
    val pc = Output(Vec(conf.nInst, UInt(conf.data_width.W)))
  })

  val pred_ctrl = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val pc = UInt(conf.data_width.W)
      val ptr = Vec(2, UInt((wEntry+1).W))
      def next_ptr(i: Int): UInt = RingQNext(ptr(i), nEntry)
      val cancel = Vec(nEntry, Bool()) /*rename stage cancel*/
      def Cancel: Bool = cancel(next_ptr(H)(wEntry-1,0))
      val inst_split = Vec(nEntry, Bool()) /*dec stage split cancle*/
      def split_inst(i: Bool): Bool = Mux(nEmpty, inst_split(ptr(H)(wEntry-1,0)), i)
      val pc_split   = Vec(nEntry, Bool()) /*pc can be split rather than even and then odd*/
      def split_pc: Bool = pc_split(ptr(H)(wEntry-1,0)) && nEmpty

      def empty: Bool  = ptr(H) === ptr(T)
      def nEmpty: Bool = ptr(H) =/= ptr(T)
      def cancel_i(i: Bool): Unit = {cancel(ptr(T)(wEntry-1,0)) := i}
      def inst_split_i(i : Bool): Unit = {inst_split(ptr(T)(wEntry-1,0)) := i}
      def pc_split_i(i: Bool): Unit = {pc_split(ptr(T)(wEntry-1,0)) := i}
    })
    w.valid := false.B
    w.pc  := START_ADDR
    w.ptr := Seq.fill(2)(0.U((wEntry+1).W))
    w.cancel := DontCare
    w.pc_split   := DontCare
    w
  })
  val inst_ready = io.inst.map(i => !i.valid || i.ready).reduce(_||_)
  val inst_valid = RegInit(VecInit(Seq.fill(conf.nInst)(false.B)))
  when(io.xcpt.valid) {
    pred_ctrl.pc := io.xcpt.bits
  }.elsewhen(io.kill.valid) {
    pred_ctrl.pc := io.kill.bits
  }.elsewhen(inst_ready && io.inst.map(_.valid).reduce(_||_)) {
    when (io.pred.redirect && !pred_ctrl.split_pc) {
      pred_ctrl.pc := io.pred.tgt
    }.elsewhen (inst_valid(1)) {
      pred_ctrl.pc := io.pc(1) + 4.U
    }.elsewhen(inst_valid(0)) {
      pred_ctrl.pc := Cat(pred_ctrl.pc(conf.inst_width-1,
        conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
    }
  }

  val inst = Reg(Vec(conf.nInst, UInt(conf.nInst.W)))
  val inst_ptr   = RegInit(VecInit(Seq.fill(2)(0.U((wEntry+1).W))))
  val inst_inc   = Wire(Vec(2, Bool()))
  val instQueue  = Mem(nEntry, Vec(conf.nInst, UInt(conf.inst_width.W)))
  val inst_vals  = Reg(Vec(nEntry, Vec(conf.nInst, Bool())))
  val head_insts = instQueue(inst_ptr(H)(wEntry-1,0))
  val head_valid = inst_vals(inst_ptr(H)(wEntry-1,0))
  val inst_next  = inst_ptr.map(RingQNext(_, nEntry))
  val queue_valid = inst_ptr(H) =/= inst_ptr(T)
  val input_valid = io.in.inst.map(_.valid).reduce(_||_)
  val flush = io.xcpt.valid || io.kill.valid

  io.forward := RingNfull(inst_ptr, wEntry)
  for (i <- 0 until conf.nInst) {
    io.inst(i).bits := inst(i)
  }
  io.inst(0).valid := inst_valid(0)
  io.inst(1).valid := inst_valid(1) &&
    !(inst_valid(0) && pred_ctrl.split_inst(io.in.inst_split))

  inst_inc(H) := inst_ready && queue_valid
  inst_inc(T) := input_valid && io.forward && (!inst_ready || queue_valid)
  when(inst_inc(T)) {
    instQueue(inst_ptr(T)(wEntry-1,0)) :=
      Seq(io.in.inst(0).bits, io.in.inst(1).bits)
    inst_vals(inst_ptr(T)(wEntry-1,0)) :=
      Seq(io.in.inst(0).valid, io.in.inst(1).valid)
  }

  when (flush) {
    for (i <- 0 until conf.nInst)
    inst_valid(i) := false.B
  }.elsewhen(inst_ready) {
    when (queue_valid) {
      inst_valid(0) := head_valid(0)
      inst_valid(1) := head_valid(1) && !pred_ctrl.Cancel
    }.elsewhen(input_valid) {
      for (i <- 0 until conf.nInst)
      inst_valid(i) := io.in.inst(i).valid
    }
  }.otherwise {
    when(io.inst(0).ready) {
      inst_valid(0) := false.B
    }
    when(io.pred.split) {
      inst_valid(1) := false.B
    }
  }

  for (i <- 0 until conf.nInst) {
    when (inst_ready) {
      when (queue_valid) {
        inst(i) := head_insts(i)
      }.elsewhen(input_valid) {
        inst(i) := io.in.inst(i).bits
      }
    }
  }

  when(flush) {
    inst_ptr(T) := inst_ptr(H)
  }.elsewhen (inst_inc(T)) {
    inst_ptr(T) := inst_next(T)
  }
  when (!flush && inst_inc(H)) {
    inst_ptr(H) := inst_next(H)
  }

  val pred_inc  = Wire(Vec(2, Bool()))
  val predQueue = Mem(nEntry, new PredictInfo(conf.data_width))
  val head_pred = predQueue(pred_ctrl.ptr(H)(wEntry-1,0))

  pred_inc(H) := inst_ready && pred_ctrl.nEmpty
  pred_inc(T) := pred_ctrl.valid && RingNfull(pred_ctrl.ptr, wEntry) && (!inst_ready || pred_ctrl.nEmpty)
  when(pred_inc(T)) {
    predQueue(pred_ctrl.ptr(T)(wEntry-1,0)) := io.in.pred
    pred_ctrl.cancel_i(io.in.pred.split)
    pred_ctrl.inst_split_i(io.in.inst_split)
    pred_ctrl.pc_split_i(io.in.pc_split && !io.in.pred.split)
  }

  when(flush) {
    pred_ctrl.valid := false.B
  }.elsewhen(input_valid && io.forward) {
    pred_ctrl.valid := true.B
  }.elsewhen(pred_inc(T)) {
    pred_ctrl.valid := false.B
  }

  when(flush) {
    pred_ctrl.ptr(T) := pred_ctrl.ptr(H)
  }.elsewhen (pred_inc(T)) {
    pred_ctrl.ptr(T) := pred_ctrl.next_ptr(T)
  }
  when(!flush && pred_inc(H)) {
    pred_ctrl.ptr(H) := pred_ctrl.next_ptr(H)
  }

  io.pred.redirect := Mux(pred_ctrl.nEmpty, head_pred.redirect, io.in.pred.redirect)
  io.pred.tgt      := Mux(pred_ctrl.nEmpty, head_pred.tgt, io.in.pred.tgt)
  io.pred.branch   := Mux(pred_ctrl.nEmpty, head_pred.branch, io.in.pred.branch)
  io.pred.rectify  := Mux(pred_ctrl.nEmpty, head_pred.rectify, io.in.pred.rectify)
  io.pred.is_jal   := Mux(pred_ctrl.nEmpty, head_pred.is_jal, io.in.pred.is_jal)
  io.pred.split    := pred_ctrl.empty && io.in.pred.split
  for (i <- 0 until conf.nInst) {
    io.pred.brchjr(i) := Mux(pred_ctrl.nEmpty,
      head_pred.brchjr(i) && !head_pred.is_jal, io.in.pred.brchjr(i))
  }

  io.pc(0) :=
    Cat(pred_ctrl.pc(conf.inst_width-1, conf.pcLSB+1), 0.U(1.W), 0.U(conf.pcLSB.W))
  io.pc(1) := Mux(pred_ctrl.split_pc, head_pred.tgt,
    Cat(pred_ctrl.pc(conf.inst_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W)))

//  printf(p"out_fire $out_fire out_valid $out_valid in_valid $in_valid inst0 ${io.inst_o(0).valid}:${io.inst_o(0).bits} inst1 " +
//    p"${io.inst_o(1).valid}:${io.inst_o(1).bits}\n")
//  printf(p"inst [head ptr ${inst_ptr(H)} inc ${inst_inc(H)} | tail ptr ${inst_ptr(T)} inc ${inst_inc(T)}]\n" +
//         p"pred [head ptr ${pred.ptr(H)} inc ${pred_inc(H)} | tail ptr ${pred.ptr(T)} inc ${pred_inc(T)}]\n")
//  printf(p"io [pc0 ${io.pc(0)} inst0 ${io.inst_o(0).valid}:${io.inst_o(0).bits} | pc1 ${io.pc(1)} inst1 ${io.inst_o(1).valid}:${io.inst_o(1).bits}] " +
//    p"io.pred: [tgt ${io.pred_o.tgt}] redirect ${io.pred_o.redirect} brchjr ${io.pred_o.brchjr} io.split: ${io.split}\n")
//  printf(p"pred_valid ${pred.valid}  pc $pc redirect $pc_split pred_tgt ${predt.tgt}\n")
//
//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}