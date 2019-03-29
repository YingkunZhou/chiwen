package bian

import chisel3._
import chisel3.util.{Cat, Valid, isPow2, log2Ceil}
import common.CPUConfig

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
    val inst_split = Input(Bool())
    val pc_split   = Input(Bool())

    val inst_o  = Output(Vec(conf.nInst, Valid(UInt(conf.inst_width.W))))
    val pred_o  = Output(new PredictInfo(conf.data_width))
    val split_o = Output(Bool())
    val pc = Output(Vec(conf.nInst, UInt(conf.data_width.W)))

    val forward = Output(Bool())
    val ready = Input(Vec(conf.nInst, Bool()))
  })

  def next(ptr: UInt, nEntry: Int): UInt = {
    if (isPow2(nEntry)) ptr + 1.U
    else Mux(ptr === (nEntry-1).U, Cat(1.U(1.W), 0.U(log2Ceil(nEntry).W)), ptr + 1.U)
  }

  val flush = io.xcpt.valid || io.kill.valid
  val out_fire = (!io.inst_o(0).valid || io.ready(0)) && (!io.inst_o(1).valid || io.ready(1) || io.split_o)
  val out_valid = io.inst_o.map(_.valid).reduce(_||_)
  val in_valid = io.inst_i.map(_.valid).reduce(_||_)

  def Nfull(ptr: Vec[UInt], w: Int): Bool = ptr(H)(w-1,0) =/= ptr(T)(w-1,0) || ptr(H)(w) === ptr(T)(w)

  val inst = RegInit(VecInit(Seq.fill(conf.nInst) {
    val w = Wire(Valid(UInt(conf.inst_width.W)))
    w.valid := false.B
    w.bits := DontCare
    w
  }))
  io.inst_o := inst

  val instQ_ptr = RegInit(VecInit(Seq.fill(2)(0.U((wEntry+1).W))))
  val instQ_nEmpty = instQ_ptr(H) =/= instQ_ptr(T)
  val inst_inc = Wire(Vec(2, Bool()))
  inst_inc(H) := out_fire && instQ_nEmpty
  inst_inc(T) := in_valid && io.forward && (!out_fire || instQ_nEmpty)
  io.forward := Nfull(instQ_ptr, wEntry)

  val instQueue = Mem(nEntry, Vec(conf.nInst, Valid(UInt(conf.inst_width.W))))
  when (flush) {
    for (i <- 0 until conf.nInst)
      inst(i).valid := false.B
  }.elsewhen(inst_inc(H)) {
    inst := instQueue(instQ_ptr(H)(wEntry-1,0))
  }.elsewhen(out_fire) { //FIXME: is it lose energy???
    inst := io.inst_i
  }.elsewhen(io.ready(0)) {
    inst(0).valid := false.B
  }

  when(inst_inc(T)) {instQueue(instQ_ptr(T)(wEntry-1,0)) := io.inst_i}

  when(flush) { instQ_ptr(T) := instQ_ptr(H)
  }.elsewhen (inst_inc(T)) { instQ_ptr(T) := instQ_ptr(T) + 1.U }

  when (!flush && inst_inc(H)) {instQ_ptr(H) := instQ_ptr(H) + 1.U}

  val pred_ctrl = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val ptr = Vec(2, UInt((wEntry+1).W))
      val inst_split = Vec(nEntry, Bool())
      val pc_split   = Vec(nEntry, Bool())
      def nEmpty: Bool = ptr(H) =/= ptr(T)
      def empty: Bool  = ptr(H) === ptr(T)
    })
    w.valid := false.B
    w.ptr := Seq.fill(2)(0.U((wEntry+1).W))
    w.inst_split := DontCare
    w.pc_split := DontCare
    w
  })

  val pred_inc = Wire(Vec(2, Bool()))
  pred_inc(H) := out_fire && pred_ctrl.nEmpty
  pred_inc(T) := pred_ctrl.valid && Nfull(pred_ctrl.ptr, wEntry) && (!out_fire || pred_ctrl.nEmpty)

  when(flush) {pred_ctrl.valid := false.B
  }.elsewhen(in_valid && io.forward) {pred_ctrl.valid := true.B
  }.elsewhen(pred_inc(T)) {pred_ctrl.valid := false.B}

  val predQueue = Mem(nEntry, new PredictInfo(conf.data_width))
  when(pred_inc(T)) {
    predQueue(pred_ctrl.ptr(T)(wEntry-1,0)) := io.pred_i
    pred_ctrl.inst_split(pred_ctrl.ptr(T)(wEntry-1,0)) := io.inst_split
    pred_ctrl.pc_split(pred_ctrl.ptr(T)(wEntry-1,0)) := io.pc_split && !io.inst_split
  }

  when(flush) {pred_ctrl.ptr(T) := pred_ctrl.ptr(H)
  }.elsewhen (pred_inc(T)) {pred_ctrl.ptr(T) := pred_ctrl.ptr(T) + 1.U}

  when(!flush && pred_inc(H)) {pred_ctrl.ptr(H) := pred_ctrl.ptr(H) + 1.U}

  val pc = RegInit(START_ADDR)
  val pred = predQueue(pred_ctrl.ptr(H)(wEntry-1,0))
  val pc_split: Bool = pred_ctrl.nEmpty && pred_ctrl.pc_split(pred_ctrl.ptr(H)(wEntry-1,0))
  io.pred_o  := Mux(pred_ctrl.nEmpty, pred, io.pred_i)
  io.split_o := Mux(pred_ctrl.nEmpty, pred_ctrl.inst_split(pred_ctrl.ptr(H)(wEntry-1,0)), io.inst_split)
  io.pc(0) :=
    Cat(pc(conf.inst_width-1, conf.pcLSB+1), 0.U(1.W), 0.U(conf.pcLSB.W))
  io.pc(1) := Mux(pc_split, pred.tgt,
    Cat(pc(conf.inst_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W)))

  when(io.xcpt.valid) {
    pc := io.xcpt.bits
  }.elsewhen(io.kill.valid) {
    pc := io.kill.bits
  }.elsewhen(out_fire && out_valid) {
    when (io.pred_o.redirect && !pc_split) {
      pc := io.pred_o.tgt
    }.elsewhen (inst(1).valid) {
      pc := io.pc(1) + 4.U
    }.elsewhen(inst(0).valid) {
      pc := Cat(pc(conf.inst_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
    }
  }

//  printf(p"out_fire $out_fire out_valid $out_valid in_valid $in_valid inst0 ${io.inst_o(0).valid}:${io.inst_o(0).bits} inst1 " +
//    p"${io.inst_o(1).valid}:${io.inst_o(1).bits}\n")
//  printf(p"inst [head ptr ${instQ_ptr(H)} inc ${inst_inc(H)} | tail ptr ${instQ_ptr(T)} inc ${inst_inc(T)}]\n" +
//         p"pred [head ptr ${pred.ptr(H)} inc ${pred_inc(H)} | tail ptr ${pred.ptr(T)} inc ${pred_inc(T)}]\n")
//  printf(p"io [pc0 ${io.pc(0)} inst0 ${io.inst_o(0).valid}:${io.inst_o(0).bits} | pc1 ${io.pc(1)} inst1 ${io.inst_o(1).valid}:${io.inst_o(1).bits}] " +
//    p"io.pred: [tgt ${io.pred_o.tgt}] redirect ${io.pred_o.redirect} brchjr ${io.pred_o.brchjr} io.split: ${io.split_o}\n")
//  printf(p"pred_valid ${pred.valid}  pc $pc redirect $pc_split pred_tgt ${predt.tgt}\n")
//
//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}