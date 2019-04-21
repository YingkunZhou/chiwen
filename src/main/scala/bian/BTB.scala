package bian

import chisel3._
import chisel3.util._
import common.{CPUConfig, CycRange}

trait BTBParams {
  val nEntry: Int = 64
  val wEntry: Int = log2Ceil(nEntry)
  require(nEntry == 64 || nEntry == 32)
  val nRAS   : Int = 8
  val OFF_MSB: Int = 13
  val OFF_LSB: Int = 2
  val wHcount: Int = 2
  val wHistory: Int = 10
  val nBHT: Int = 1024
}

object BTBType {
  val invalid = 0
  val retn    = 1
  val branch  = 2
  val jump    = 3
  val NUM = jump + 1
  val SZ = log2Ceil(NUM)
}

class Predict(val data_width: Int) extends Bundle with BTBParams {
  val redirect = Bool() // = 0 cont || = 1 jump
  val history = UInt(wHistory.W)
  val diff = Bool()
  val tgt = UInt(data_width.W)
}

class PredictVal(data_width: Int) extends Predict(data_width) {
  val valid = Bool()
}

class PredictInner(val data_width: Int) extends Bundle with BTBParams {
  val lookup = UInt(nEntry.W)
  def valid: Bool = lookup.orR
  val h_count = UInt(wHcount.W)
  val gshare  = UInt(wHcount.W)
  val gshxor  = UInt(log2Ceil(nBHT).W)
  val select  = Bool()
  val btb_type = UInt(BTBType.SZ.W)
  def branch: Bool = btb_type === BTBType.branch.U
  def taken: Bool  = Mux(select, gshare(1), h_count(1))
  def diff: Bool   = gshare(1) ^ h_count(1) // && !select
  def redirect: Bool = Mux(btb_type === BTBType.branch.U, taken, btb_type =/= BTBType.invalid.U)
  val jump_tgt = UInt(data_width.W)
  val cont_tgt = UInt(data_width.W)
}

class BTB(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    val if_pc    = Input(Valid(UInt(conf.data_width.W)))
    val predict  = Output(Vec(conf.nInst, new PredictVal(conf.data_width)))
    val split    = Output(Bool())

    val branch   = Input(Bool())
    val retn     = Input(Bool())
    val call     = Input(Valid(UInt(conf.data_width.W)))
    val peek     = Output(UInt(conf.data_width.W))

    val fb_pc    = Input(Valid(UInt(conf.data_width.W)))
    val fb_type  = Input(UInt(BTBType.SZ.W))
    val feedBack = Input(new PredictVal(conf.data_width))

    val cyc = Input(UInt(conf.data_width.W))
  })
  def HcntInc(i: UInt): UInt = Cat(i.orR , i(1) || !i(0))
  def SelInc(i: UInt): UInt = Cat(1.U(1.W) , i(1))
  def HcntDec(i: UInt): UInt = Cat(i.andR, i(1) && !i(0))
  def SelDec(i: UInt): UInt = Cat(0.U(1.W), i(1))
  val gb_history = RegInit(0.U(wHistory.W))
  val bht = Mem(nBHT, UInt(wHcount.W))
  val arb = Mem(nBHT, Bool())
  val ras = Module(new RAS(nRAS)).io
  ras.pop  := io.retn
  ras.push := io.call
  val btb = RegInit({
    val w = Wire(new Bundle {
      val valid = Vec(nEntry, Bool())
      val pc = Vec(nEntry, UInt((conf.data_width-conf.pcLSB).W))
      val tgt = Vec(nEntry, UInt((conf.data_width-conf.pcLSB).W))
      val bj_type  = Vec(nEntry,  UInt(BTBType.SZ.W))
      val h_count  = Vec(nEntry,  UInt(wHcount.W))
    })
    w.valid   := Seq.fill(nEntry)(false.B)
    w.pc      := DontCare
    w.tgt     := DontCare
    w.bj_type := DontCare
    w.h_count := DontCare
    w
  })

  val predict = Wire(Vec(conf.nInst, new PredictInner(conf.data_width)))
  predict(0).cont_tgt := Cat(io.if_pc.bits(conf.data_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
  predict(1).cont_tgt := predict(0).cont_tgt + 4.U
  io.split := io.predict(0).redirect && !io.if_pc.bits(OFF_LSB).toBool
  val if_pc = Wire(Vec(conf.nInst, UInt((conf.data_width-conf.pcLSB).W)))
  for (i <- 0 until conf.nInst) {
    if_pc(i) := Cat(io.if_pc.bits(conf.data_width-1,conf.pcLSB+1),i.U(1.W))
    predict(i).lookup   := VecInit(btb.pc.map(_ === if_pc(i))).asUInt & btb.valid.asUInt
    predict(i).jump_tgt := Cat(Mux1H(predict(i).lookup, btb.tgt), 0.U(conf.pcLSB.W))
    predict(i).btb_type := Mux1H(predict(i).lookup, btb.bj_type)
    predict(i).h_count  := Mux1H(predict(i).lookup, btb.h_count)
    //lose some accurate
    predict(i).gshxor   := gb_history(9,0) ^ if_pc(i)(9,0) // ^ gb_history(19,10)
    predict(i).select   := arb(predict(i).gshxor)
    predict(i).gshare   := bht(predict(i).gshxor)

    io.predict(i).valid    := predict(i).valid
    io.predict(i).redirect := predict(i).redirect
    io.predict(i).diff     := predict(i).diff
    io.predict(i).history  := gb_history //lose some accurate
    io.predict(i).tgt      := Mux(predict(i).btb_type === BTBType.retn.U, ras.peek,
      Mux(predict(i).redirect, predict(i).jump_tgt, predict(i).cont_tgt))
  }
  io.peek := ras.peek

  val unpredict = Reg(Bool())
  val b_predict = predict.map(_.branch).reduce(_||_)
  when (!io.fb_pc.valid && io.if_pc.valid) {unpredict := !b_predict}
  val b_taken = Mux(predict(0).branch, predict(0).taken, predict(1).taken)
  val b_shift = io.if_pc.valid && b_predict
  val l_shift = Cat(gb_history(wHistory-2,0), 0.U(1.W))

  when (io.fb_pc.valid) {
    gb_history := Mux(io.fb_type =/= BTBType.branch.U, io.feedBack.history,
      Cat(io.feedBack.history(wHistory-2,0), io.feedBack.redirect))
  }.elsewhen(io.branch && unpredict) {
    gb_history := Mux(b_shift, Cat(l_shift(wHistory-2,0), b_taken), l_shift)
  }.elsewhen(b_shift) {
    gb_history := Cat(gb_history(wHistory-2,0), b_taken)
  }

  // latch first
  val fb_reg = RegInit({
    val w = Wire(new Bundle {
      val valid  = Bool()
      val punish = Bool()
      val redirect = Bool()
      val btb_type = UInt(BTBType.SZ.W)
      val pc   = UInt((conf.data_width-conf.pcLSB).W)
      val tgt  = UInt((conf.data_width-conf.pcLSB).W)
      val diff = Bool()
      val gshxor = UInt(log2Ceil(nBHT).W)
      val lfsr = UInt(wEntry.W)//used for replaced
      def lfsr_next: UInt = {
        if (wEntry == 5) Cat(lfsr(0) ^ lfsr(2), lfsr(wEntry-1,1))
        else Cat(lfsr(0) ^ lfsr(1), lfsr(wEntry-1,1))
      }
    })
    w.valid  := false.B
    w.punish := false.B
    w.lfsr   := 1.U
    w.pc     := DontCare
    w.tgt    := DontCare
    w.diff   := DontCare
    w.gshxor := DontCare
    w.redirect := DontCare
    w.btb_type := DontCare
    w
  })
  fb_reg.valid    := io.feedBack.valid
  fb_reg.punish   := io.fb_pc.valid
  fb_reg.pc       := io.fb_pc.bits(conf.data_width-1, conf.pcLSB)
  fb_reg.tgt      := io.feedBack.tgt(conf.data_width-1, conf.pcLSB)
  fb_reg.btb_type := io.fb_type
  fb_reg.redirect := io.feedBack.redirect
  fb_reg.diff     := io.feedBack.diff
  fb_reg.gshxor   := io.feedBack.history(9,0) ^ io.fb_pc.bits(11, conf.pcLSB)// ^ io.feedBack.history(19,10)
  fb_reg.lfsr     := fb_reg.lfsr_next

  val feedback = Wire(new Bundle {
    val lookup = UInt(nEntry.W)
    def valid: Bool = lookup.orR
    val h_count = UInt(wHcount.W)
    val gshare  = UInt(wHcount.W)
    val select  = Bool()
    val idx = UInt(wEntry.W)
  })
  feedback.lookup  := VecInit(btb.pc.map(_ === fb_reg.pc)).asUInt & btb.valid.asUInt
  feedback.h_count := Mux1H(feedback.lookup, btb.h_count)
  feedback.gshare  := bht(fb_reg.gshxor)
  feedback.idx     := Mux(feedback.valid, OHToUInt(feedback.lookup),
    Mux(btb.valid.asUInt.andR, fb_reg.lfsr, PriorityEncoder(~btb.valid.asUInt)))
  feedback.select  := arb(fb_reg.gshxor)

  when (fb_reg.valid) {
    when (fb_reg.redirect) {
      btb.valid(feedback.idx)  := true.B
    }.elsewhen(feedback.valid) {
      btb.valid(feedback.idx)  := fb_reg.btb_type === BTBType.branch.U
    }
    when (fb_reg.redirect) {
      btb.pc(feedback.idx)      := fb_reg.pc
      btb.tgt(feedback.idx)     := fb_reg.tgt
      btb.bj_type(feedback.idx) := fb_reg.btb_type
    }
    //update bht and btb parallel
    when (fb_reg.btb_type === BTBType.branch.U) {
      when (feedback.valid) {
        when (fb_reg.redirect) {
          btb.h_count(feedback.idx) := HcntInc(feedback.h_count)
          bht(fb_reg.gshxor) := HcntInc(feedback.gshare)
        }.otherwise {
          btb.h_count(feedback.idx) := HcntDec(feedback.h_count)
          bht(fb_reg.gshxor) := HcntDec(feedback.gshare)
        }
      }.elsewhen(fb_reg.redirect) {
        btb.h_count(feedback.idx) := 2.U
        bht(fb_reg.gshxor) := 2.U
      }
    }
  }

  when (fb_reg.punish && feedback.valid) {
    when (fb_reg.diff && !feedback.select) {
      arb(fb_reg.gshxor) := true.B
    }.otherwise {
      arb(fb_reg.gshxor) := false.B
    }
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
