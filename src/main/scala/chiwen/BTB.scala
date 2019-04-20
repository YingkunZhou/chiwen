package chiwen

import chisel3._
import chisel3.util._
import common.{CPUConfig, CycRange}

trait BTBParams {
  val nLow: Int = 64
  require(nLow == 64 || nLow == 32)
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

class BHTshift extends Bundle {
  val forward = Bool()
  val branch = Bool()
  val redirect = Bool()
}

class BTB(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    // pc stage inquire
    val if_pc = Input(UInt(conf.xprlen.W))
    val predict = Output(new Predict(conf.xprlen))
    //dec from inst decoder
    val branch   = Input(Bool())
    val forward  = Input(Bool())
    val ras_pop  = Input(Bool())
    val ras_push = Input(Valid(UInt(conf.data_width.W)))
    val fb_pc    = Input(UInt(conf.xprlen.W))
    val fb_miss  = Input(Bool())
    val fb_type  = Input(UInt(BTBType.SZ.W))
    val feedBack = Input(new PredictVal(conf.xprlen))

    val cyc = Input(UInt(conf.xprlen.W))
  })

  def HcntInc(i: UInt): UInt = Cat(i.orR , i(1) || !i(0))
  def SelInc(i: UInt): UInt = Cat(1.U(1.W) , i(1))
  def HcntDec(i: UInt): UInt = Cat(i.andR, i(1) && !i(0))
  def SelDec(i: UInt): UInt = Cat(0.U(1.W), i(1))
  val gb_history = RegInit(0.U(wHistory.W))
  val bht = Mem(nBHT, UInt(wHcount.W))
  val arb = Mem(nBHT, Bool())
  val ras = Module(new RAS(nRAS)).io
  ras.pop  := io.ras_pop
  ras.push := io.ras_push
  val btb = RegInit({
    val w = Wire(new Bundle {
      val valid = Vec(nLow, Bool())
      val pc = Vec(nLow, UInt(conf.data_width.W))
      val tgt = Vec(nLow, UInt(conf.data_width.W))
      val bj_type  = Vec(nLow,  UInt(BTBType.SZ.W))
      val h_count  = Vec(nLow,  UInt(wHcount.W))
    })
    w.valid   := Seq.fill(nLow)(false.B)
    w.pc      := DontCare
    w.tgt     := DontCare
    w.bj_type := DontCare
    w.h_count := DontCare
    w
  })

  val predict = Wire(new Bundle {
    val lookup = UInt(nLow.W)
    def valid: Bool = lookup.orR
    val h_count = UInt(wHcount.W)
    val gshare  = UInt(wHcount.W)
    val select  = Bool()
    val bj_type = UInt(BTBType.SZ.W)
    def taken: Bool = Mux(select, gshare(1), h_count(1))
    def diff: Bool  = gshare(1) ^ h_count(1) // && !select
    def branch: Bool = valid && bj_type === BTBType.branch.U
    def redirect: Bool = valid && (bj_type =/= BTBType.branch.U || taken)
    val jump_tgt = UInt(conf.data_width.W)
    val cont_tgt = UInt(conf.data_width.W)
  })
  predict.cont_tgt := io.if_pc + 4.U
  predict.lookup := VecInit(btb.pc.map(_ === io.if_pc)).asUInt & btb.valid.asUInt
  predict.jump_tgt := Mux1H(predict.lookup, btb.tgt)
  predict.bj_type  := Mux1H(predict.lookup, btb.bj_type)
  predict.h_count  := Mux1H(predict.lookup, btb.h_count)
  val gshare_xor = gb_history(9,0) ^ io.if_pc(11, conf.pcLSB)// ^ gb_history(19,10)
  predict.select   := arb(gshare_xor)
  predict.gshare   := bht(gshare_xor)
  val shift_reg  = Reg(Bool())
  val shift_wire = shift_reg && io.branch
  when (io.fb_miss) {
    when (io.fb_type === BTBType.branch.U) {
      gb_history := Cat(io.feedBack.history(wHistory-2,0), io.feedBack.redirect)
    }.otherwise {
      gb_history := io.feedBack.history
    }
  }.elsewhen(io.forward) {
    shift_reg := !predict.branch
    when (shift_wire ) {
      gb_history := Cat(gb_history(wHistory-2,0), 0.U(1.W))
    }.elsewhen(predict.branch) {
      gb_history := Cat(gb_history(wHistory-2,0), predict.taken)
    }
  }
  io.predict.redirect := predict.redirect
  io.predict.diff     := predict.diff
  io.predict.history  := gb_history
  io.predict.tgt := Mux(predict.bj_type === BTBType.retn.U, ras.peek,
    Mux(predict.redirect, predict.jump_tgt, predict.cont_tgt))

  val fb_reg = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val miss = Bool()
      val redirect = Bool()
      val btb_type = UInt(BTBType.SZ.W)
      val pc  = UInt(conf.data_width.W)
      val tgt = UInt(conf.data_width.W)
      val diff = Bool()
      val gshare = UInt(log2Ceil(nBHT).W)
      val lfsr = UInt(log2Ceil(nLow).W)//used for replaced
      def lfsr_next: UInt = {
        val i = log2Ceil(nLow)
        if (i == 5) Cat(lfsr(0) ^ lfsr(2), lfsr(i-1,1))
        else Cat(lfsr(0) ^ lfsr(1), lfsr(i-1,1))
      }
    })
    w.valid := false.B
    w.redirect := false.B
    w.miss := false.B
    w.lfsr := 1.U
    w.btb_type := DontCare
    w.pc  := DontCare
    w.tgt := DontCare
    w.diff := DontCare
    w.gshare := DontCare
    w
  })
  fb_reg.valid    := io.feedBack.valid
  fb_reg.pc       := io.fb_pc
  fb_reg.tgt      := io.feedBack.tgt
  fb_reg.btb_type := io.fb_type
  fb_reg.miss     := io.fb_miss
  fb_reg.redirect := io.feedBack.redirect
  fb_reg.diff     := io.feedBack.diff
  fb_reg.gshare   := io.feedBack.history(9,0) ^ io.fb_pc(11, conf.pcLSB)// ^ io.feedBack.history(19,10)
  fb_reg.lfsr     := fb_reg.lfsr_next

  val feedback = Wire(new Bundle {
    val lookup = UInt(nLow.W)
    def exist: Bool = lookup.orR
    val h_count  = UInt(wHcount.W)
    val gshare   = UInt(wHcount.W)
    val idx = UInt(log2Ceil(nLow).W)
  })
  feedback.lookup  := VecInit(btb.pc.map(_ === fb_reg.pc)).asUInt & btb.valid.asUInt
  feedback.h_count := Mux1H(feedback.lookup, btb.h_count)
  feedback.gshare  := bht(fb_reg.gshare)
  feedback.idx := Mux(feedback.exist, OHToUInt(feedback.lookup),
    Mux(btb.valid.asUInt.andR, fb_reg.lfsr, PriorityEncoder(~btb.valid.asUInt)))

  when (fb_reg.valid) {
    when (fb_reg.redirect) {
      btb.valid(feedback.idx)  := true.B
    }.elsewhen(feedback.exist) {
      btb.valid(feedback.idx)  := fb_reg.btb_type === BTBType.branch.U
    }
    when (fb_reg.redirect) {
      btb.pc(feedback.idx)      := fb_reg.pc
      btb.tgt(feedback.idx)     := fb_reg.tgt
      btb.bj_type(feedback.idx) := fb_reg.btb_type
    }
    //update bht and btb parallel
    when (fb_reg.btb_type === BTBType.branch.U) {
      when (feedback.exist) {
        when (fb_reg.redirect) {
          btb.h_count(feedback.idx) := HcntInc(feedback.h_count)
          bht(fb_reg.gshare) := HcntInc(feedback.gshare)
        }.otherwise {
          btb.h_count(feedback.idx) := HcntDec(feedback.h_count)
          bht(fb_reg.gshare) := HcntDec(feedback.gshare)
        }
      }.elsewhen(fb_reg.redirect) {
        btb.h_count(feedback.idx) := 2.U
        bht(fb_reg.gshare) := 2.U
      }
    }
  }

  val gshare_sel = arb(fb_reg.gshare)
  when (fb_reg.miss && feedback.exist) {
    when (fb_reg.diff && !gshare_sel) {
      arb(fb_reg.gshare) := true.B
    }.otherwise {
      arb(fb_reg.gshare) := false.B
    }
  }

  when (predict.valid /*&& io.if_pc.bits === "h80005920".U*/) {
    printf("BTB: Cyc= %d pc %x redirect %x index %d select %d gshare %d hcount %d bht idx %x"
    , io.cyc
    , io.if_pc
    , io.predict.redirect
    , OHToUInt(predict.lookup)
    , predict.select
    , predict.gshare
    , predict.h_count
    , gshare_xor
    )
//    for (i <- 0 until 10) printf(p" ${gb_history(i)}")
    printf("\n")
//    when (CycRange(io.cyc, 773, 857)) {
//      when (fb_reg.valid) {
//        printf(p"${fb_reg.redirect}" +
//          p" ${fb_reg.low_exist} " +
//          p"${fb_reg.low_idx(btb.low_val.asUInt)} " +
//          p"${btb.low_val}\n")
//      }
//    }
  }


//  when (CycRange(io.cyc, 635, 638)) {
//    printf(p"DEBUG:" +
//      p" ${io.predict.diff}" +
//      p" ${fb_reg.miss}" +
//      p" ${fb_reg.low_exist}" +
//      p" ${fb_reg.diff}" +
//      p" ${fb_wire.low_idx}" +
//      p"\n")
//  }
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
