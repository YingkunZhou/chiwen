package fuxi

import chisel3._
import chisel3.util._
import common.CPUConfig

trait BTBParams {
  val nLow: Int = 64
  require(nLow == 64 || nLow == 32)
  val nHigh: Int = 4
  require(nHigh == 4)
  //  val nRAS    : Int = 8
  val OFF_MSB: Int = 13
  val OFF_LSB: Int = 2
  val wHcount: Int = 2
}

object BTBType {
  //  val invalid = 0
  //  val retn    = 1
  //  val branch  = 2
  //  val jump    = 3
  val branch  = 0
  val jump    = 1
  val NUM = jump + 1
  val SZ = log2Ceil(NUM)
}

class Predict(val data_width: Int) extends Bundle with BTBParams {
  val redirect = Bool() // = 0 cont || = 1 jump
  val tgt = UInt(data_width.W)
}

class BTB(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    // pc stage inquire
    // val raspeek  = Input(UInt(conf.xprlen.W)) // used for return type
    val if_pc = Input(UInt(conf.xprlen.W))
    val predict  = Output(Vec(conf.nInst, new Predict(conf.xprlen)))
    val split    = Output(Bool())

    val fb_pc = Input(UInt(conf.xprlen.W))
    val fb_type = Input(UInt(BTBType.SZ.W))
    val feedBack = Input(Valid(new Predict(conf.xprlen)))

    val cyc = Input(UInt(conf.xprlen.W))
  })

  val btb = RegInit({
    val w = Wire(new Bundle {
      val low_val  = Vec(nLow,  Bool())
      val pc_low   = Vec(nLow,  UInt((OFF_MSB + 1 - OFF_LSB).W))
      val tg_low   = Vec(nLow,  UInt((OFF_MSB + 1 - OFF_LSB).W))
      val high_idx = Vec(nLow,  UInt(log2Ceil(nHigh).W))
      val bj_type  = Vec(nLow,  UInt(BTBType.SZ.W))
      val h_count  = Vec(nLow,  UInt(wHcount.W))

      val high_val = Vec(nHigh, Bool())
      val pc_high  = Vec(nHigh, UInt((conf.data_width-OFF_MSB-1).W))
      val tg_high  = Vec(nHigh, UInt((conf.data_width-OFF_MSB-1).W))
    })
    w.low_val  := Seq.fill(nLow)(false.B)
    w.pc_low   := DontCare
    w.tg_low   := DontCare
    w.high_idx := DontCare
    w.bj_type  := DontCare
    w.h_count  := DontCare

    w.high_val := Seq.fill(nHigh)(false.B)
    w.pc_high  := DontCare
    w.tg_high  := DontCare
    w
  })

  val predict = Wire(new Bundle {
    val high_val = UInt(nHigh.W)
    def high_idx: UInt = OHToUInt(high_val)
    val high_equ = UInt(nLow.W)
    val pc_high  = UInt((conf.data_width-OFF_MSB-1).W)
    val tg_high  = UInt((conf.data_width-OFF_MSB-1).W)

    val low_val  = Vec(conf.nInst, UInt(nLow.W))
    val pc_low   = Vec(conf.nInst, UInt((OFF_MSB + 1 - OFF_LSB).W))
    val tg_low   = Vec(conf.nInst, UInt((OFF_MSB + 1 - OFF_LSB).W))

    val jump_tgt = Vec(conf.nInst, UInt(conf.data_width.W))
    val cont_tgt = Vec(conf.nInst, UInt(conf.data_width.W))
  })

  predict.pc_high  := io.if_pc(conf.data_width-1, OFF_MSB+1)
  predict.tg_high  := Mux1H(predict.high_val, btb.tg_high)
  predict.high_val := VecInit(btb.pc_high.map(_ === predict.pc_high)).asUInt & btb.high_val.asUInt
  predict.high_equ := VecInit(btb.high_idx.map(_ === predict.high_idx)).asUInt
  predict.cont_tgt(0) := Cat(io.if_pc(conf.data_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
  predict.cont_tgt(1) := predict.cont_tgt(0) + 4.U
  io.split := io.predict(0).redirect && !io.if_pc(OFF_LSB).toBool
  for (i <- 0 until conf.nInst) {
    predict.pc_low(i)  := Cat(io.if_pc(OFF_MSB, OFF_LSB+1), i.U(1.W))
    predict.low_val(i) := VecInit(btb.pc_low.map(_ === predict.pc_low(i))).asUInt &
      btb.low_val.asUInt & predict.high_equ
    predict.tg_low(i)  := Mux1H(predict.low_val(i), btb.tg_low)

    predict.jump_tgt(i) := Cat(predict.tg_high, predict.tg_low(i), 0.U(OFF_LSB.W))
    io.predict(i).redirect := predict.low_val(i).orR &&
      (Mux1H(predict.low_val(i), btb.bj_type) =/= BTBType.branch.U ||
       Mux1H(predict.low_val(i), btb.h_count)(1).toBool)
    io.predict(i).tgt := Mux(io.predict(i).redirect, predict.jump_tgt(i), predict.cont_tgt(i))
  }

  val fb_wire = Wire(new Bundle {
    val high_val = UInt(nHigh.W)
    def high_idx: UInt = OHToUInt(high_val)
    val pc_high  = UInt((conf.data_width-OFF_MSB-1).W)
    val pc_high_equ = UInt(nLow.W)
    val tg_high_neq = Bool()

    val high_exist  = Bool()
    val high_insert = Bool()
    def high_replace: Bool = !high_exist && !high_insert

    val low_val = UInt(nLow.W)
    val pc_low  = UInt((OFF_MSB + 1 - OFF_LSB).W)
    val low_inval = Vec(nLow, Bool())

    val low_idx = UInt(log2Ceil(nLow).W)
    val h_count = UInt(wHcount.W)
  })
  val fb_reg = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val redirect = Bool()
      val btb_type = UInt(BTBType.SZ.W)
      val high_idx = UInt(log2Ceil(nHigh).W)
      val low_val  = UInt(nLow.W)
      val pc  = UInt(conf.data_width.W)
      val tgt = UInt(conf.data_width.W)
      val lfsr = UInt(log2Ceil(nLow).W)//used for replaced
      def lfsr_next: UInt = {
        val i = log2Ceil(nLow)
        if (i == 5) Cat(lfsr(0) ^ lfsr(2), lfsr(i-1,1))
        else Cat(lfsr(0) ^ lfsr(1), lfsr(i-1,1))
      }
      def low_exist: Bool = low_val.orR

    })
    w.valid := false.B
    w.redirect := false.B
    w.lfsr := 1.U
    w.btb_type := DontCare
    w.high_idx := DontCare
    w.low_val  := DontCare
    w.pc  := DontCare
    w.tgt := DontCare
    w
  })

  val lru = Module(new LRU(nHigh)).io
  lru.newest.bits := Mux(fb_wire.high_exist, fb_wire.high_idx,
    Mux(fb_wire.high_insert, PriorityEncoder(btb.high_val.map(!_)), lru.oldest))
  lru.newest.valid := io.feedBack.valid

  fb_wire.pc_high  := io.fb_pc(conf.data_width-1, OFF_MSB+1)
  fb_wire.high_val := VecInit(btb.pc_high.map(_ === fb_wire.pc_high)).asUInt & btb.high_val.asUInt
  fb_wire.pc_high_equ := VecInit(btb.high_idx.map(_ === fb_wire.high_idx)).asUInt
  fb_wire.tg_high_neq := Mux1H(fb_wire.high_val, btb.tg_high) =/= io.feedBack.bits.tgt(conf.data_width-1, OFF_MSB+1)

  fb_wire.high_exist  := fb_wire.high_val.orR
  fb_wire.high_insert := btb.high_val.map(!_).reduce(_||_)

  fb_wire.low_inval := (0 until nLow).map(i =>
    (fb_wire.high_replace && btb.high_idx(i) === lru.oldest) ||
      (fb_wire.high_exist   && fb_wire.pc_high_equ(i) && fb_wire.tg_high_neq))
  fb_wire.pc_low  := io.fb_pc(OFF_MSB, OFF_LSB)
  fb_wire.low_val := VecInit(btb.pc_low.map(_ === fb_wire.pc_low)).asUInt & fb_wire.pc_high_equ & btb.low_val.asUInt

  fb_reg.low_val  := fb_wire.low_val
  fb_reg.valid    := io.feedBack.valid
  fb_reg.pc       := io.fb_pc
  fb_reg.tgt      := io.feedBack.bits.tgt
  fb_reg.btb_type := io.fb_type
  fb_reg.redirect := io.feedBack.bits.redirect
  fb_reg.high_idx := lru.newest.bits
  fb_reg.lfsr     := fb_reg.lfsr_next

  fb_wire.h_count := Mux1H(fb_reg.low_val, btb.h_count)
  fb_wire.low_idx := Mux(fb_reg.low_exist, OHToUInt(fb_reg.low_val),
    Mux(btb.low_val.reduce(_&&_), fb_reg.lfsr, PriorityEncoder(btb.low_val.map(!_))))

  // Think about two sequential redirect insts feed back, may cause some btb fault
  when (io.feedBack.bits.redirect) {
    for (i <- 0 until nLow) {
      when (fb_wire.low_inval(i)) { //TODO: has some risk unable to invalidate all invalid entry
        btb.low_val(i) := false.B
      }
    }
  }
  when (fb_reg.redirect) {
    // high part
    btb.high_val(fb_reg.high_idx) := true.B
    btb.tg_high(fb_reg.high_idx)  := fb_reg.tgt(conf.data_width-1, OFF_MSB+1)
    btb.pc_high(fb_reg.high_idx)  := fb_reg.pc(conf.data_width-1, OFF_MSB+1)
    // low part
    btb.tg_low(fb_wire.low_idx)   := fb_reg.tgt(OFF_MSB, OFF_LSB)
    btb.pc_low(fb_wire.low_idx)   := fb_reg.pc(OFF_MSB, OFF_LSB)
    btb.high_idx(fb_wire.low_idx) := fb_reg.high_idx
    btb.bj_type(fb_wire.low_idx)  := fb_reg.btb_type
  }

  when (fb_reg.redirect) {
    btb.low_val(fb_wire.low_idx)  := true.B
  }.elsewhen(fb_reg.valid && fb_reg.low_exist) {
    btb.low_val(fb_wire.low_val)  := fb_reg.btb_type === BTBType.branch.U
  }

  when (fb_reg.valid && fb_reg.low_exist) {
    when (fb_reg.redirect) {
      when (fb_wire.h_count =/= 3.U) {
        btb.h_count(fb_wire.low_idx) := fb_wire.h_count + 1.U
      }}.otherwise {
      when (fb_wire.h_count =/= 0.U) {
        btb.h_count(fb_wire.low_idx) := fb_wire.h_count - 1.U
      }}
  }.elsewhen(fb_reg.redirect) {
    btb.h_count(fb_wire.low_idx) := 2.U
  }

  //  when (CycRange(io.cyc, 900, 910)) {
  //    printf(p"cyc = ${io.cyc}\n" +
  //      p"pred_tgt ${Hexadecimal(io.predict.tgt)} " +
  //      p"pred_redirct ${io.predict.redirect} " +
  //      p"fb_valid ${io.feedBack.valid} " +
  //      p"fb_pc ${Hexadecimal(io.fb_pc)} " +
  //      p"fb_type ${io.fb_type} " +
  //      p"fb_redirect ${io.feedBack.bits.redirect} " +
  //      p"fb_tgt ${Hexadecimal(io.feedBack.bits.tgt)}\n")
  //    printf(p"predict: high_val ${predict.high_val} " +
  //      p"low_val ${Hexadecimal(predict.low_val)} " +
  //      p"type ${predict.bj_type} " +
  //      p"count ${predict.h_count}\n")
  //    printf(p"fb_wire high_val ${fb_wire.high_val} " +
  //      p"tgt_high_neq ${fb_wire.tg_high_neq} " +
  //      p"high exist ${fb_wire.high_exist} " +
  //      p"high insert ${fb_wire.high_insert}\n" +
  //      p"low invalid ${Hexadecimal(fb_wire.low_inval.asUInt)} " +
  //      p"low valid ${Hexadecimal(fb_wire.low_val)}\n")
  //    printf(p"fb_reg valid ${fb_reg.valid} " +
  //      p"redirect ${fb_reg.redirect} " +
  //      p"type ${fb_reg.btb_type} " +
  //      p"high_idx ${fb_reg.btb_type} " +
  //      p"low valid ${Hexadecimal(fb_reg.low_val)} " +
  //      p"low idx ${fb_wire.low_idx}\n")
  //  }

}
