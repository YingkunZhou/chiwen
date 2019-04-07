package bian

import chisel3._
import chisel3.util._
import common.CPUConfig

trait BTBParams {
  val nLow: Int = 64
  val nHigh: Int = 4
  require(nLow == 64)
  require(nHigh == 4)
  val nRAS: Int = 8
  val OFF_MSB: Int = 11
  val OFF_LSB: Int = 2
  val wHcount: Int = 2
}

object BTBType {
  val branch  = 0
  val jump    = 1
//  val retn    = 3
  val NUM = jump + 1
  val SZ = log2Ceil(NUM)
}

class BTB(implicit conf: CPUConfig) extends Module with BTBParams { //todo: you or meiyou, that's a question
  val io = IO(new Bundle{
    val if_pc = Input(UInt(conf.data_width.W)) // 8 Bytes aligned
    val fb_pc = Input(UInt(conf.data_width.W))
    val fb_type = Input(UInt(BTBType.SZ.W))

    val predict  = Output(Vec(conf.nInst, new Predict(conf.data_width)))
    val split    = Output(Bool())
    val feedBack = Input(Valid(new Predict(conf.data_width)))

    val cyc = Input(UInt(conf.data_width.W))
  })

  val btb = RegInit({
    val w = Wire(new Bundle {
      val low_val  = Vec(nLow,  Bool())
      val high_val = Vec(nHigh, Bool())
      val high_idx = Vec(nLow,  UInt(log2Ceil(nHigh).W))
      val bj_type  = Vec(nLow,  UInt(BTBType.SZ.W))
      val h_count  = Vec(nLow,  UInt(wHcount.W))
      val pc_low   = Vec(nLow,  UInt((OFF_MSB + 1 - OFF_LSB).W))
      val tg_low   = Vec(nLow,  UInt((OFF_MSB + 1 - OFF_LSB).W))
      val pc_high  = Vec(nHigh, UInt((conf.data_width-OFF_MSB-1).W))
      val tg_high  = Vec(nHigh, UInt((conf.data_width-OFF_MSB-1).W))
    })
    w.low_val  := Seq.fill(nLow)(false.B)
    w.high_val := Seq.fill(nHigh)(false.B)
    w.high_idx := DontCare
    w.bj_type  := DontCare
    w.h_count  := DontCare
    w.pc_low   := DontCare
    w.tg_low   := DontCare
    w.pc_high  := DontCare
    w.tg_high  := DontCare
    w
  })
  val predict = Wire(new Bundle {
    val high_val = UInt(nHigh.W)
    def high_idx: UInt = OHToUInt(high_val)
    val high_equ = UInt(log2Ceil(nLow).W)
    val pc_high  = UInt((conf.data_width-OFF_MSB-1).W)
    val tg_high  = UInt((conf.data_width-OFF_MSB-1).W)

    val pc_low   = Vec(conf.nInst, UInt((OFF_MSB + 1 - OFF_LSB).W))
    val tg_low   = Vec(conf.nInst, UInt((OFF_MSB + 1 - OFF_LSB).W))
    val low_val  = Vec(conf.nInst, UInt(nLow.W))
    val h_count  = Vec(conf.nInst, UInt(wHcount.W))
    val bj_type  = Vec(conf.nInst, UInt(BTBType.SZ.W))
    val jump_tgt = Vec(conf.nInst, UInt(conf.data_width.W))
    val cont_tgt = Vec(conf.nInst, UInt(conf.data_width.W))
//    val tgt_cand = Vec(conf.nInst, Vec(BTBType.NUM, UInt(conf.data_width.W)))
  })
  predict.pc_high  := io.if_pc(conf.xprlen-1, OFF_MSB+1)
  predict.tg_high  := Mux1H(predict.high_val, btb.tg_high)
  predict.high_val := VecInit(btb.pc_high.map(_ === predict.pc_high)).asUInt & btb.high_val.asUInt
  predict.high_equ := VecInit(btb.high_idx.map(predict.high_idx === _)).asUInt //FIXME
  for (i <- 0 until conf.nInst) {
    predict.pc_low(i)  := Cat(io.if_pc(OFF_MSB, OFF_LSB+1), i.U(1.W))

    predict.low_val(i) := VecInit(btb.pc_low.map(_ === predict.pc_low(i))).asUInt &
      predict.high_equ & btb.low_val.asUInt
    predict.tg_low(i)  := Mux1H(predict.low_val(i), btb.tg_low)
    predict.jump_tgt(i):= Cat(predict.tg_high, predict.tg_low(i), 0.U(OFF_LSB.W))

    predict.bj_type(i) := Mux1H(predict.low_val(i), btb.bj_type)
    predict.h_count(i) := Mux1H(predict.low_val(i), btb.h_count)

    io.predict(i).redirect := predict.low_val(i).orR &&
      (predict.bj_type(i) =/= BTBType.branch.U || predict.h_count(i)(1).toBool)
    io.predict(i).tgt := Mux(io.predict(i).redirect, predict.jump_tgt(i), predict.cont_tgt(i))
  }
  predict.cont_tgt(0) := Cat(io.if_pc(conf.data_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
  predict.cont_tgt(1) := predict.cont_tgt(0) + 4.U
  io.split := io.predict(0).redirect && !io.if_pc(OFF_LSB).toBool

  val feedback_wire = Wire(new Bundle {
    val high_val = UInt(nHigh.W)
    val pc_high  = UInt((conf.data_width-OFF_MSB-1).W)
    val tgt_high_neq = Bool()

    val high_exist   = Bool()
    val high_insert  = Bool()
    val high_replace = Bool()
    val high_idx = UInt(log2Ceil(nHigh).W)

    val pc_low  = UInt((OFF_MSB + 1 - OFF_LSB).W)
    val tg_low  = UInt((OFF_MSB + 1 - OFF_LSB).W)
    val high_eq = UInt(nLow.W)
    val low_inval = Vec(nLow, Bool())
    val low_val   = UInt(nLow.W)

    val exist = Bool()
    val low_idx = UInt(log2Ceil(nLow).W)
    val h_count = UInt(wHcount.W)
    val bj_type = UInt(BTBType.SZ.W)
  })

  val feedback_reg = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val redirect = Bool()
      val btb_type = UInt(BTBType.SZ.W)
      val high_idx = UInt(log2Ceil(nHigh).W)
      val low_val  = UInt(nLow.W)
      val pc  = UInt(conf.data_width.W)
      val tgt = UInt(conf.data_width.W)
    })
    w.valid := false.B
    w.redirect := false.B
    w.btb_type := DontCare
    w.high_idx := DontCare
    w.low_val  := DontCare
    w.pc  := DontCare
    w.tgt := DontCare
    w
  })
  val lru = Module(new LRU(nHigh)).io
  lru.newest.bits := Mux(feedback_wire.high_exist, feedback_wire.high_idx,
    Mux(feedback_wire.high_insert, PriorityEncoder(btb.high_val.map(!_)), lru.oldest))
  lru.newest.valid := io.feedBack.valid
  val lfsr6 = RegInit(1.U(log2Ceil(nLow).W))
  lfsr6 := Cat(lfsr6(1)^lfsr6(0), lfsr6(log2Ceil(nLow)-1,1))

  feedback_wire.pc_high  := io.fb_pc(conf.data_width-1, OFF_MSB+1)
  feedback_wire.high_val := VecInit(btb.pc_high.map(_ === feedback_wire.pc_high)).asUInt & btb.high_val.asUInt
  feedback_wire.high_idx := OHToUInt(feedback_wire.high_val)

  feedback_wire.pc_low := io.fb_pc(OFF_MSB, OFF_LSB)
  feedback_wire.tg_low := io.feedBack.bits.tgt(OFF_MSB, OFF_LSB)

  feedback_wire.high_exist   := feedback_wire.high_val.orR
  feedback_wire.high_insert  := btb.high_val.map(!_).reduce(_||_)
  feedback_wire.high_replace := !feedback_wire.high_insert && !feedback_wire.high_exist
  feedback_wire.tgt_high_neq := Mux1H(feedback_wire.high_val, btb.tg_high) =/=
    io.feedBack.bits.tgt(conf.data_width-1, OFF_MSB+1)
  feedback_wire.low_inval := (0 until nLow).map(i =>
    (feedback_wire.high_replace && btb.high_idx(i) === lru.oldest) ||
    (feedback_wire.tgt_high_neq && btb.high_idx(i) === feedback_wire.high_idx && feedback_wire.high_exist))

  feedback_wire.high_eq := VecInit(btb.high_idx.map(feedback_wire.high_idx === _)).asUInt
  feedback_wire.low_val := VecInit(btb.pc_low.map(_ === feedback_wire.pc_low)).asUInt & feedback_wire.high_eq & btb.low_val.asUInt
  feedback_wire.exist   := feedback_reg.low_val.orR
  feedback_wire.low_idx := Mux(feedback_wire.exist, OHToUInt(feedback_reg.low_val),
                           Mux(btb.high_val.map(!_).reduce(_||_), PriorityEncoder(btb.high_val.map(!_)), lfsr6))

  feedback_wire.h_count  := Mux1H(feedback_reg.low_val, btb.h_count)
  feedback_wire.bj_type  := Mux1H(feedback_reg.low_val, btb.bj_type)

  feedback_reg.valid := io.feedBack.valid
  feedback_reg.pc  := io.fb_pc
  feedback_reg.tgt := io.feedBack.bits.tgt
  feedback_reg.btb_type := io.fb_type
  feedback_reg.redirect := io.feedBack.bits.redirect
  feedback_reg.high_idx := lru.newest.bits
  feedback_reg.low_val  := feedback_wire.low_val

  when (io.feedBack.bits.redirect) {
    for (i <- 0 until nLow)
    when (feedback_wire.low_inval(i)) {
      btb.low_val(i) := false.B //TODO: has some risk unable to invalidate all invalid entry
    }
  }

  when (feedback_reg.redirect) {
    // high part
    btb.high_val(feedback_reg.high_idx) := true.B
    btb.tg_high(feedback_reg.high_idx)  := feedback_reg.tgt(conf.data_width-1, OFF_MSB+1)
    btb.pc_high(feedback_reg.high_idx)  := feedback_reg.pc(conf.data_width-1, OFF_MSB+1)
    // low part
    btb.tg_low(feedback_wire.low_idx)   := feedback_reg.tgt(OFF_MSB, OFF_LSB)
    btb.pc_low(feedback_wire.low_idx)   := feedback_reg.pc(OFF_MSB, OFF_LSB)
    btb.high_idx(feedback_wire.low_idx) := feedback_reg.high_idx
    btb.bj_type(feedback_wire.low_idx)  := feedback_reg.btb_type
  }

  when (feedback_reg.redirect) {
    btb.low_val(feedback_wire.low_idx)  := true.B
  }.elsewhen(feedback_reg.valid && feedback_wire.exist && feedback_reg.btb_type =/= BTBType.branch.U) {
    btb.low_val(feedback_wire.low_val) := false.B
  }

  when (feedback_reg.valid && feedback_wire.exist) {
    when (feedback_reg.redirect) { when (feedback_wire.h_count =/= 3.U) {
      btb.h_count(feedback_wire.low_idx) := feedback_wire.h_count + 1.U }
    }.elsewhen(feedback_wire.h_count =/= 0.U) {
      btb.h_count(feedback_wire.low_idx) := feedback_wire.h_count - 1.U
    }
  }.elsewhen(feedback_reg.redirect) {
    btb.h_count(feedback_wire.low_idx) := 2.U
  }
//  when ((feedback_reg.valid && feedback_wire.exist && feedback_wire.bj_type === BTBType.invalid.U) ||
//    (feedback_wire.bj_type =/= BTBType.invalid.U && feedback_reg.redirect)) {
//    printf("===============btb fault!!!================\n")
//  }
}
