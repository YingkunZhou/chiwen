package chiwen

import chisel3._
import chisel3.util._
import common.{AxiIO, CPUConfig, Str}

import scala.math.pow

trait ICCParams { // FIXME: the last two bits of pc must be 00
  val nLine   : Int = 16
  val wLine   : Int = log2Ceil(nLine)
  val wOffset : Int = 8
  val nOffset : Int = pow(2, wOffset).toInt
  val wTAG    : Int = 18
  val TAG_LSB : Int = 14
  require(2 + wLine + wOffset + wTAG == 32)
}

class CoreIO(val data_width: Int) extends Bundle {
  val pc        = Input(Valid(UInt(data_width.W)))
  val inst      = Output(Valid(UInt(data_width.W)))
  val ready     = Output(Bool())
}

object Latch {
  def apply(in: Bool, wait: Bool): Bool = {
    val in_latch = RegInit(false.B)
    when (wait) { in_latch := false.B
    }.elsewhen(in) {in_latch := true.B}
    in || in_latch
  }
}

object ShakeHand {
  def apply(in: Bool, wait: Bool): Bool = {
    val in_latch = RegInit(false.B)
    when (wait) { in_latch := false.B
    }.elsewhen(in) {in_latch := true.B}
    (in || in_latch) && wait
  }
}

class Icache(implicit conf: CPUConfig) extends Module with ICCParams {
  val io = IO(new Bundle{
    val core   = new CoreIO(conf.xprlen)
    val axi    = new AxiIO(conf.xprlen)
    val cyc = Input(UInt(conf.xprlen.W))
  })
  io.axi.ar.burst := "b01".U       //incr mode
  io.axi.ar.size  := "b010".U      //4 bytes/transfer
  io.axi.ar.len   := "b00001111".U  //16 transfer/burst
  io.axi.ar.id    := conf.iccRd
  val rvalid: Bool = io.axi.r.valid && io.axi.r.id === conf.iccRd
  val rlast : Bool = io.axi.r.last  && io.axi.r.id === conf.iccRd

  val pc_valid = RegInit(false.B) // pulse signal
  val pc = Reg(UInt(conf.xprlen.W))
  val pc_accept: Bool = io.core.ready && io.core.pc.valid
  pc_valid := pc_accept
  when (pc_accept) { pc := io.core.pc.bits } // Notice: if io.core.pc is invalid then pc is unchageable
  val sLookUp :: sBurst :: sWriteBack :: Nil = Enum(3)
  val state = RegInit(sLookUp)

  val wb_buffer = Reg(Vec(nLine, UInt(conf.xprlen.W)))
  val wb_addr = Reg(UInt((conf.xprlen - wLine - conf.pcLSB).W))
  val icache = Module(new CaheCore(wLines = wLine, wOffset = wOffset, wTag = wTAG))
  icache.io.wen     := state === sWriteBack
  icache.io.addr    := Mux(state === sWriteBack, Cat(wb_addr, 0.U((conf.pcLSB + wLine).W)), io.core.pc.bits)
  icache.io.wdata   := wb_buffer
  icache.io.wstatus := state === sWriteBack
  icache.io.cyc     := io.cyc

  val cache_hit: Bool= pc_valid && icache.io.rvalid // one cycle
  val line_hit: Bool = pc(conf.xprlen-1,conf.pcLSB+wLine) === wb_addr
  val miss: Bool     = pc_valid && !icache.io.rvalid
  val pc_miss: Bool  = miss && (state === sLookUp || !line_hit)
  //FIXME: there are some trick in here, think about it, what if replaced cache line is now you read,
  // but not worry, because here we suppose inst in immutable
  val pc_double_miss: Bool = Latch(miss && state =/= sLookUp && !line_hit, state === sWriteBack)
  io.axi.ar.valid := Latch(pc_miss, io.axi.ar.ready)
  io.axi.ar.addr  := Cat(pc(conf.xprlen-1,conf.pcLSB+wLine), 0.U((conf.pcLSB + wLine).W))
  io.core.ready   := state =/= sWriteBack

  switch (state) {
    is (sLookUp) {
      when (pc_miss) { state := sBurst
      }.otherwise { state := sLookUp }
    }
    is (sBurst) {
      when (rlast && rvalid) { state := sWriteBack
      }.otherwise { state := sBurst }
    }
    is (sWriteBack) {
      when (pc_double_miss) { state := sBurst
      }.otherwise { state := sLookUp}
    }
  }

  when ((state === sLookUp    && pc_miss) ||
        (state === sWriteBack && pc_double_miss))
  { wb_addr := pc(conf.xprlen-1,conf.pcLSB+wLine) }

  val cnt = RegInit(0.U(wLine.W))
  val wb_buf_valid = RegInit(VecInit(Seq.fill(nLine)(false.B)))
  when (state === sWriteBack) {
    for (i <- 0 until nLine) {
      wb_buf_valid(i) := false.B
    }
    when (rvalid) {
      cnt := cnt + 1.U(wLine.W)
      wb_buf_valid(cnt) := true.B
    }
  }.elsewhen(rvalid) {
    cnt := cnt + 1.U(wLine.W)
    wb_buf_valid(cnt) := true.B
  }

  when (rvalid) { wb_buffer(cnt) := io.axi.r.data }

  val wait_inst_back: Bool  =
    (state =/= sLookUp    && pc_valid && line_hit) ||
    (state === sLookUp    && pc_miss) ||
    (state === sWriteBack && pc_double_miss)

  val line_idx    = pc(conf.pcLSB+wLine-1, conf.pcLSB)
  val buffer_inst = Mux(line_hit, wb_buffer(line_idx), icache.io.rdata)
  val buf_inst_valid: Bool= ShakeHand(wait_inst_back, wb_buf_valid(line_idx) && line_hit) || cache_hit
  io.core.inst.valid := Mux(state === sLookUp, cache_hit, buf_inst_valid)
  io.core.inst.bits  := Mux(state === sLookUp, icache.io.rdata, buffer_inst)

//  when (io.cyc < 120.U && io.cyc > 100.U) {
//    printf("Icache: state = %c pc = %x valid = %x inst = %x\n"
//      , MuxCase(Str("L"), Array(
//        (state === sBurst) -> Str("B"),
//        (state === sWriteBack) -> Str("W")
//      ))
//      , pc
//      , io.core.inst.valid
//      , io.core.inst.bits
//    )
//  }
}
