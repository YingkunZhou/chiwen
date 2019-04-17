package bian

import chisel3._
import chisel3.util._
import common.{CPUConfig, CycRange}

trait FrontParam {
  val nEntry  = 2
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
  val pc_split   = Input(Bool())
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
    val cyc = Input(UInt(conf.data_width.W))
  })

  val pred_ctrl = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val pc = UInt(conf.data_width.W)
      val ptr = Vec(2, UInt((wEntry+1).W))
      def next_ptr(i: Int): UInt = RingQNext(ptr(i), nEntry)
      /*pc can be split rather than even and then odd*/
      val pc_split = Vec(nEntry, Bool())
      def split_pc: Bool = pc_split(ptr(H)(wEntry-1,0)) && nEmpty
      def split_in(i: Bool): Unit = {pc_split(ptr(T)(wEntry-1,0)) := i}

      def empty: Bool  = ptr(H) === ptr(T)
      def nEmpty: Bool = ptr(H) =/= ptr(T)
    })
    w.valid := false.B
    w.pc  := START_ADDR
    w.ptr := Seq.fill(2)(0.U((wEntry+1).W))
    w.pc_split   := DontCare
    w
  })
  val inst_ready = (!io.inst(0).valid || io.inst(0).ready) &&
    (!io.inst(1).valid || io.inst(1).ready || io.pred.split)
  val inst_valid = RegInit(VecInit(Seq.fill(conf.nInst)(false.B)))
  when(io.xcpt.valid) {
    pred_ctrl.pc := io.xcpt.bits
  }.elsewhen(io.kill.valid) {
    pred_ctrl.pc := io.kill.bits
  }.elsewhen(inst_ready && io.inst.map(_.valid).reduce(_||_)) {
    when (io.pred.redirect && !pred_ctrl.split_pc) {
      pred_ctrl.pc := io.pred.tgt
    }.elsewhen (io.inst(1).valid) {
      pred_ctrl.pc := io.pc(1) + 4.U
    }.elsewhen(io.inst(0).valid) {
      pred_ctrl.pc := Cat(pred_ctrl.pc(conf.inst_width-1,
        conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
    }
  }

  val inst_bits  = Reg(Vec(conf.nInst, UInt(conf.data_width.W)))
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
      inst_valid := head_valid
    }.otherwise {
      for (i <- 0 until conf.nInst)
      inst_valid(i) := io.in.inst(i).valid
    }
  }.elsewhen(io.inst(0).ready) {
    inst_valid(0) := false.B
  }
  for (i <- 0 until conf.nInst) {
    when (inst_ready) {
      when (queue_valid) {
        inst_bits(i) := head_insts(i)
      }.elsewhen(input_valid) { //FIXME: is it add time latency???
        inst_bits(i) := io.in.inst(i).bits
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

  val predQueue = Mem(nEntry, new PredictInfo(conf.data_width))
  val head_pred = predQueue(pred_ctrl.ptr(H)(wEntry-1,0))
  val pred_inc  = Wire(Vec(2, Bool()))
  pred_inc(H) := inst_ready && pred_ctrl.nEmpty
  pred_inc(T) := pred_ctrl.valid && (!inst_ready || pred_ctrl.nEmpty) &&
    RingNfull(pred_ctrl.ptr, wEntry)
  val tail_pred = Wire(new PredictInfo(conf.data_width))
  tail_pred := io.in.pred
  tail_pred.split := io.in.inst_split || io.in.pred.split
  when(pred_inc(T)) {
    predQueue(pred_ctrl.ptr(T)(wEntry-1,0)) := tail_pred
    pred_ctrl.split_in(io.in.pc_split && !io.in.pred.split)
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

  io.forward := RingNfull(inst_ptr, wEntry)
  io.pc(0) :=
    Cat(pred_ctrl.pc(conf.inst_width-1, conf.pcLSB+1), 0.U(1.W), 0.U(conf.pcLSB.W))
  io.pc(1) := Mux(pred_ctrl.split_pc, head_pred.tgt,
    Cat(pred_ctrl.pc(conf.inst_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W)))

  io.inst(0).valid := inst_valid(0)
  io.inst(1).valid := inst_valid(1) && !(inst_valid(0) &&
    Mux(pred_ctrl.nEmpty, head_pred.split, io.in.inst_split))

  for (i <- 0 until conf.nInst) {
    io.inst(i).bits := inst_bits(i)
    io.pred.brchjr(i) := Mux(pred_ctrl.nEmpty, head_pred.Brchjr(i), io.in.pred.brchjr(i))
  }
  io.pred.redirect    := Mux(pred_ctrl.nEmpty, head_pred.redirect,  io.in.pred.redirect)
  io.pred.tgt         := Mux(pred_ctrl.nEmpty, head_pred.tgt,       io.in.pred.tgt)
  io.pred.branch      := Mux(pred_ctrl.nEmpty, head_pred.branch,    io.in.pred.branch)
  io.pred.rectify     := Mux(pred_ctrl.nEmpty, head_pred.rectify,   io.in.pred.rectify)
  io.pred.is_jal := pred_ctrl.empty && io.in.pred.is_jal
  io.pred.split  := pred_ctrl.empty && io.in.pred.split

//  when (CycRange(io.cyc, 185766, 185766)) {
//    printf(p"FroneQueue: " +
//      p"head ${inst_ptr(H)} tail ${inst_ptr(T)} " +
////      p"output redirect ${io.pred.redirect} " +
//      p"split ${io.in.pred.split} " +
//      p"valid $inst_valid " +
//      p"nEmpty ${pred_ctrl.nEmpty} " +
//      p"head split ${head_pred.split}"
//      "xcpt %x:%x kill %x:%x " +
//      io.xcpt.valid,
//      io.xcpt.bits,
//      io.kill.valid,
//      io.kill.bits,
//      + p"\n")
//    printf("input val0 %x inst: DASM(%x) val1 %x inst1: DASM(%x)\n",
//      io.in.inst(0).valid,
//      io.in.inst(0).bits,
//      io.in.inst(1).valid,
//      io.in.inst(1).bits
//    )
//    printf(
//      p"inst_ready $inst_ready " +
////      p"${inst_valid(0)} ${io.inst(0).valid}:pc ${Hexadecimal(io.pc(0))} | " +
////      p"${inst_valid(1)} ${io.inst(1).valid}:pc ${Hexadecimal(io.pc(1))} " +
//      p"split ${io.in.inst_split} \n" +
//      p"tgt ${Hexadecimal(io.pred.tgt)} " +
//      p"pred redirect ${io.pred.redirect} " +
//      p"brchjr ${io.pred.brchjr} " +
//      p"rectify ${io.pred.rectify} " +
//      p"branch ${io.pred.branch} " +
//      p"is_jal ${io.pred.is_jal} " +
//      p"split ${io.pred.split} " +
//      p"forward ${io.forward} \n")
//    printf(
//      p"inst=>[head ptr ${inst_ptr(H)} inc ${inst_inc(H)} | " +
//      p"tail ptr ${inst_ptr(T)} inc ${inst_inc(T)}] " +
//      p"pred=>[head ptr ${pred_ctrl.ptr(H)} inc ${pred_inc(H)} | " +
//      p"tail ptr ${pred_ctrl.ptr(T)} inc ${pred_inc(T)}]\n")
//    printf(
//      p"pred_ctrl_valid ${pred_ctrl.valid} " +
//      p"pc ${Hexadecimal(pred_ctrl.pc)} " +
//      p"pc_split ${pred_ctrl.split_pc}\n")
//  }

//
//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}