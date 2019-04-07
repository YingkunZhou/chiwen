package bian

import chisel3._
import chisel3.util._
import common.{CPUConfig, Str}

object LatchData {
  def apply(in: Bool, data: UInt, deflt: UInt = 0.U): UInt = {
    val data_latch = RegInit(deflt)
    when (in) { data_latch := data }
    Mux(in, data, data_latch)
  }
}

class FetchInst(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val cyc = Input(UInt(conf.xprlen.W))
    val mem = new AxiIO(conf.xprlen)

    val if_btb     = Input(Vec(2, new Predict(conf.xprlen)))
    val dec_btb    = Output(Vec(2, new Predict(conf.xprlen)))
    val pc         = Input(UInt(conf.data_width.W))
    val pc_split   = Input(Bool())
    val pc_forward = Output(Bool())
    val forward    = Input(Vec(2, Bool()))
    val if_kill    = Input(Bool()) // from dec and downflow
    val dec_kill   = Input(Bool()) // from exe and downflow
    val inst       = Output(Vec(2, Valid(UInt(conf.xprlen.W))))
    val dec_pc     = Output(Vec(2, UInt(conf.xprlen.W)))
    val inst_split = Input(Bool())
  })

  val pc = Wire(UInt(conf.xprlen.W))
  val pc_valid = Wire(Bool())
  val icache = Module(new Icache())
  icache.io.cyc := io.cyc
  icache.io.axi.r <> io.mem.r
  icache.io.axi.ar.ready := io.mem.ar.ready
  icache.io.core.pc.bits := pc
  icache.io.core.pc.valid := conf.use_cc.B & pc_valid
  io.mem.ar.id    := Mux(conf.use_cc.B, icache.io.axi.ar.id, conf.incRd)
  io.mem.ar.valid := Mux(conf.use_cc.B, icache.io.axi.ar.valid, pc_valid)
  io.mem.ar.addr  := Mux(conf.use_cc.B, icache.io.axi.ar.addr, pc)
  io.mem.ar.burst := Mux(conf.use_cc.B, icache.io.axi.ar.burst, "b01".U)
  io.mem.ar.len   := Mux(conf.use_cc.B, icache.io.axi.ar.len, 0.U)
  io.mem.ar.size  := Mux(conf.use_cc.B, icache.io.axi.ar.size, "b010".U)
  val addr_ready: Bool = Mux(conf.use_cc.B, icache.io.core.ready, !icache.io.axi.ar.valid && io.mem.ar.ready)

  val inst_odd   = RegInit(false.B)
  val inst_kill  = RegInit(false.B)
  val inst_split = RegInit(false.B)
  val inst_valid = Wire(Vec(2, Bool()))
  inst_valid(0) := Mux(io.mem.r.id === conf.incRd, io.mem.r.valid, icache.io.core.inst(0).valid) && !inst_odd
  inst_valid(1) := Mux(io.mem.r.id === conf.incRd, io.mem.r.valid && inst_odd, icache.io.core.inst(1).valid)

  val sWtAddrOK :: sWtInstOK :: sWtForward :: Nil = Enum(3)
  val state = RegInit(sWtAddrOK)
  val inst_valid_orR: Bool = inst_valid.reduce(_||_)
  val if_forward: Bool = io.forward(1) || inst_kill || Mux(inst_split, io.pc(conf.pcLSB).toBool, !inst_valid(1))
  switch (state) {
    is (sWtAddrOK) {
      when (addr_ready) { state := sWtInstOK }
    }
    is (sWtInstOK) {
     when(inst_valid_orR) {
        when(if_forward) {
          state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
        }.elsewhen (io.dec_kill) { state := sWtAddrOK
        }.otherwise { state := sWtForward }
      }
    }
    is (sWtForward) {
      when(io.forward(1)) {
        state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
      }.elsewhen (io.dec_kill) { state := sWtAddrOK }
    }
  }

  when (pc_valid) { inst_kill := io.if_kill //very important logic
  }.elsewhen(inst_valid_orR) { inst_kill := false.B
  }.elsewhen(io.dec_kill && state === sWtInstOK) { inst_kill := true.B
  }
  val pc_odd_reg = RegInit(false.B)
  val state_WtForward = RegInit(false.B)

  pc_valid := state === sWtAddrOK ||
    (inst_valid_orR && if_forward) ||
    (state === sWtForward && io.forward(1))

  io.pc_forward := io.if_kill || addr_ready && (
    (state === sWtAddrOK  && !pc_odd_reg)   ||
    (state === sWtForward && io.forward(1)) ||
    ((inst_kill || (inst_split  && io.pc(conf.pcLSB).toBool) ||
    (io.forward(1) && (inst_valid(1) || inst_split))) && inst_valid_orR))

  val maintain = RegInit(true.B)
  io.inst(0).valid := ( inst_valid(0) && !inst_kill) || state_WtForward
  io.inst(1).valid := ((inst_valid(1) && !inst_kill) || (state === sWtForward && maintain)) && !inst_split

  val pc_odd_wire: Bool = inst_valid(0) && !inst_valid(1) && !inst_split && !inst_kill
  when (addr_ready) { pc_odd_reg := false.B
  }.elsewhen(pc_odd_wire) { pc_odd_reg := true.B
  }
  val pc_odd = Wire(Vec(conf.nInst, Bool()))
  pc_odd(0) := pc_odd(1) || io.pc(conf.pcLSB).toBool
  pc_odd(1) := pc_odd_reg || pc_odd_wire

  pc := Mux(pc_odd(1), io.dec_pc(1), io.pc)

  when (io.dec_kill || io.forward(0)) { state_WtForward := false.B
  }.elsewhen(inst_valid(0) && !inst_kill) { state_WtForward := true.B
  } //FIXME: wonder why inst_kill don't make sence
  when (io.dec_kill || io.forward(1)) { maintain := true.B
  }.elsewhen(state === sWtForward && io.inst_split) { maintain := false.B
  }

  /*=======================dec part==============================*/
  val reg_pc   = Reg(Vec(2, UInt(conf.data_width.W)))
  val reg_pred = Reg(Vec(2, new Predict(conf.xprlen)))
  when (pc_valid) {
    inst_odd   :=  pc_odd(0)
    inst_split := !pc_odd(0) && io.pc_split
    for (i <- 0 until conf.nInst) {
      when (!pc_odd(i)) { reg_pc(i) := Cat(io.pc(conf.xprlen-1, conf.pcLSB+1), i.U(1.W), 0.U(conf.pcLSB.W)); reg_pred(i) := io.if_btb(i) }
    }
  }
  val inst = Wire(Vec(2, UInt(conf.xprlen.W)))
  for (i <- 0 until 2) {
    inst(i) := Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst(i).bits, io.mem.r.data)
    io.dec_btb(i)   := reg_pred(i)
    io.dec_pc(i)    := reg_pc(i)
    io.inst(i).bits := LatchData(inst_valid(i), inst(i), BUBBLE)
  }

//  when (io.cyc >= 993.U && io.cyc <= 998.U) {
//    printf("FetchInst: state = %c [dec_pc %x %x] [inst %x %x] [valid %x %x] inst_kill %x pc_fwd %x\n"
//      , MuxCase(Str("A"), Array(
//          (state === sWtInstOK) -> Str("I"),
//          (state === sWtForward) -> Str("F")
//        ))
//      , io.dec_pc(0)
//      , io.dec_pc(1)
//      , io.inst(0).bits
//      , io.inst(1).bits
//      , io.inst(0).valid
//      , io.inst(1).valid
//      , inst_kill
//      , io.pc_forward
//    )
//    printf(p"DEBUG io.forward ${io.forward} inst_valids $inst_valids inst_odd $inst_odd " +
//      p" inst_split $inst_split dec_kill ${io.dec_kill} state_wtForward $state_WtForward" +
//      p" btb_tg = ${Hexadecimal(io.dec_btb(1).tgt)} ${Hexadecimal(io.dec_btb(1).tgt)}\n")
//  }
}
