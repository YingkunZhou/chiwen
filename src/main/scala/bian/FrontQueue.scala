package bian

import chisel3._
import chisel3.util.{Cat, Valid, isPow2, log2Ceil}
import common.CPUConfig

trait Pram {
  val nPhyAddr = 60
  val nOrder   = 32
  require(isPow2(nOrder))
  val data_width = 32
  val wPhyAddr = log2Ceil(nPhyAddr)
  val wOrder   = log2Ceil(nOrder)
  val nCommit  = 4
  val nBrchjr  = 4
  val nInst    = 2
}

trait FrontParam {
  val nEntry  = 16
  require(isPow2(nEntry))
  val wEntry = log2Ceil(nEntry)
  val H = 0
  val T = 1
}

class PredictInfo(data_width: Int) extends Predict(data_width) {
  val brchjr = Vec(2, Bool()) //determine pick which btb
  val branch = Bool()
  val jump   = UInt(Jump.NUM.W)
}

class FrontQueue(implicit val conf: CPUConfig) extends Module with FrontParam {
  val io = IO(new Bundle {
    val xcpt = Input(Valid(UInt(conf.data_width.W)))
    val kill = Input(Valid(UInt(conf.data_width.W)))

    val inst_i  = Input(Vec(conf.nInst, Valid(UInt(conf.inst_width.W))))
    val pred_i  = Input(new PredictInfo(conf.data_width))
    val split_i = Input(Bool())

    val inst_o  = Output(Vec(conf.nInst, Valid(UInt(conf.inst_width.W))))
    val pred_o  = Output(new PredictInfo(conf.data_width))
    val split_o = Output(Bool())
    val pc = Output(Vec(conf.nInst, UInt(conf.data_width.W)))

    val forward = Output(Bool())
    val ready = Input(Vec(conf.nInst, Bool()))
  })

//  def next(ptr: UInt, nEntry: Int): UInt = {
//    if (isPow2(nEntry)) ptr + 1.U
//    else Mux(ptr === (nEntry-1).U, 0.U, ptr + 1.U)
//  }

  val flush: Bool = io.xcpt.valid || io.kill.valid
  val out_fire:  Bool = (!io.inst_o(0).valid || io.ready(0)) &&
    (!io.inst_o(1).valid || io.ready(1) || io.split_o)
  val out_valid: Bool = io.inst_o.map(_.valid).reduce(_||_)
  val in_valid:  Bool = io.inst_i.map(_.valid).reduce(_||_)

  val instQueue = Mem(nEntry, Vec(conf.nInst, Valid(UInt(conf.inst_width.W))))
  val instQ_ptr = RegInit(VecInit(Seq.fill(2)(0.U((wEntry+1).W))))
  val predQueue = Mem(nEntry, new PredictInfo(conf.data_width))
  val predQ_ptr = RegInit(VecInit(Seq.fill(2)(0.U((wEntry+1).W))))
  val inst = RegInit(VecInit(Seq.fill(conf.nInst) {
    val w = Wire(Valid(UInt(conf.inst_width.W)))
    w.valid := false.B
    w.bits := DontCare
    w
  }))
  io.inst_o := inst

  def Nfull(head: UInt, tail: UInt, w: Int): Bool =
    head(w-1,0) =/= tail(w-1,0) || head(w) === tail(w)
  io.forward := Nfull(instQ_ptr(H), instQ_ptr(T), wEntry)

  val instQ_inc = Wire(Vec(2, Bool()))
  val instQ_empty: Bool = instQ_ptr(H) === instQ_ptr(T)
  instQ_inc(H) := out_fire && !instQ_empty
  instQ_inc(T) := in_valid && io.forward && !(out_fire && instQ_empty)

  when(flush) { instQ_ptr(T) := instQ_ptr(H)
  }.elsewhen (instQ_inc(T)) { instQ_ptr(T) := instQ_ptr(T) + 1.U }

  when (!flush && instQ_inc(H)) {instQ_ptr(H) := instQ_ptr(H) + 1.U}

  when (flush) {
    for (i <- 0 until conf.nInst)
      inst(i).valid := false.B
  }.elsewhen(instQ_inc(H)) {
    inst := instQueue(instQ_ptr(H)(wEntry-1,0))
  }.elsewhen(out_fire && in_valid) {
    inst := io.inst_i
  }.elsewhen(io.ready(0)) {
    inst(0).valid := false.B
  }

  val predQ_inc = Wire(Vec(2, Bool()))
  val predQ_empty: Bool = predQ_ptr(H) === predQ_ptr(T)
  val predQ_ready: Bool = Nfull(predQ_ptr(H), predQ_ptr(T), wEntry)
  val pred_valid = RegInit(false.B)

  predQ_inc(T) := pred_valid && predQ_ready && !(out_fire && predQ_empty)
  predQ_inc(H) := out_fire && !predQ_empty

  when(flush) {pred_valid := false.B
  }.elsewhen(in_valid && io.forward) {pred_valid := true.B
  }.elsewhen(predQ_inc(T)) {pred_valid := false.B}


  when(flush) {predQ_ptr(T) := predQ_ptr(H)
  }.elsewhen (predQ_inc(T)) {predQ_ptr(T) := predQ_ptr(T) + 1.U}

  when(!flush && predQ_inc(H)) {predQ_ptr(H) := predQ_ptr(H) + 1.U}

  val pred = predQueue(predQ_ptr(H)(wEntry-1,0))
  io.pred_o  := Mux(predQ_empty, io.pred_i , pred)
  val splitQueue = Reg(Vec(nEntry, Bool()))
  val split = splitQueue(predQ_ptr(H)(wEntry-1,0))
  io.split_o := Mux(predQ_empty, io.split_i, split)

  val pc = RegInit(START_ADDR)
  val redirect: Bool = !predQ_empty && pred.brchjr(0) && inst(0).valid && pred.redirect
  io.pc(0) := Cat(pc(conf.inst_width-1, conf.pcLSB+1), 0.U(1.W), 0.U(conf.pcLSB))
  io.pc(1) := Mux(redirect, pred.tgt, Cat(pc(conf.inst_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB)))

  when(io.xcpt.valid) {
    pc := io.xcpt.bits
  }.elsewhen(io.kill.valid) {
    pc := io.kill.bits
  }.elsewhen(out_fire && out_valid) {
    when (redirect && !split && inst(1).valid) {
      pc := pred.tgt + 4.U
    }.elsewhen(io.pred_o.redirect &&
      (0 until conf.nInst).map(i => io.pred_o.brchjr(i) && inst(i).valid).reduce(_||_)) {
      pc := io.pred_o.tgt
    }.otherwise {
      pc := pc + 8.U
    }
  }
  // buffered
  when(instQ_inc(T)) {
    instQueue(instQ_ptr(T)(wEntry-1,0)) := io.inst_i
  }
  when(predQ_inc(T)) {
    predQueue(predQ_ptr(T)(wEntry-1,0)) := io.pred_i
    splitQueue(predQ_ptr(T)(wEntry-1,0)) := io.split_i
  }

//  printf(p"ptr $ptr full $full flush $flush ptr_inc $ptr_inc [ptr_next ${ptr_next(0)} ${ptr_next(1)}] " +
//    p"[inst ${inst(0).valid}: ${inst(0).bits} ${inst(1).valid}: ${inst(1).bits}] ready ${io.ready}\n" +
//    p"[inst ${io.inst_i(0).valid}: ${io.inst_i(0).bits} ${io.inst_i(1).valid}: ${io.inst_i(1).bits}] " +
//    p"out_valid $out_valid out_fire $out_fire in_valid $in_valid forward ${io.forward} pc ${Hexadecimal(pc)}\n" +
//    p"ptr ${pred_ctrl.ptr} empty ${pred_ctrl.empty} valid ${pred_ctrl.valid} " +
//    p"ptr_inc $pred_ptr_inc ptr_next ${pred_ptr_next(0)} ${pred_ptr_next(1)}\n\n")
}