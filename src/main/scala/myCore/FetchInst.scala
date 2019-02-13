package myCore

import chisel3._
import chisel3.util._
import common.{CPUConfig, Str}

object Pulse {
  def apply(in: Bool, forward: Bool): Bool = {
    val in_latch = RegInit(true.B)
    when (in && !forward) { in_latch := false.B
    }.elsewhen(forward) {in_latch := true.B }
    in && in_latch
  }
}

object LatchData {
  def apply(valid: Bool, data: UInt): UInt = {
    val data_latch = Reg(UInt())
    when (valid) { data_latch := data }
    Mux(valid, data, data_latch)
  }
}

class FetchInst(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val cyc = Input(UInt(conf.xprlen.W))
    val mem = new AxiIO(conf.xprlen)

    val if_btb     = Input(new Predict(conf.xprlen))
    val dec_btb    = Output(new Predict(conf.xprlen))
    val pc         = Input(UInt(conf.xprlen.W))
    val pc_forward = Output(Bool())
    val forward    = Input(Bool())
    val if_kill    = Input(Bool()) // from dec and downflow
    val dec_kill   = Input(Bool()) // from exe and downflow
    val inst       = Output(UInt(conf.xprlen.W))
    val inst_valid = Output(Bool())
    val dec_pc     = Output(UInt(conf.xprlen.W))
  })

  val if_kill:  Bool = Pulse(io.if_kill, io.forward)
  val dec_kill: Bool = Pulse(io.dec_kill, io.forward)

  val sWtAddrOK :: sWtInstOK :: sWtForward :: Nil = Enum(3)
  val state = RegInit(sWtAddrOK)
  val pc_valid = Wire(Bool())
  val pc       = Wire(UInt(conf.xprlen.W))
  val icache = Module(new Icache())
  icache.io.cyc := io.cyc
  icache.io.axi.r <> io.mem.r
  icache.io.axi.ar.ready := io.mem.ar.ready
  icache.io.core.pc := pc
  icache.io.core.pc_valid := conf.use_cc.B & pc_valid
  io.mem.ar.id    := Mux(conf.use_cc.B, icache.io.axi.ar.id, conf.incRd)
  io.mem.ar.valid := Mux(conf.use_cc.B, icache.io.axi.ar.valid, pc_valid)
  io.mem.ar.addr  := Mux(conf.use_cc.B, icache.io.axi.ar.addr, pc)
  io.mem.ar.burst := Mux(conf.use_cc.B, icache.io.axi.ar.burst, "b01".U)
  io.mem.ar.len   := Mux(conf.use_cc.B, icache.io.axi.ar.len, 0.U)
  io.mem.ar.size  := Mux(conf.use_cc.B, icache.io.axi.ar.size, "b010".U)
  val addr_ready: Bool = Mux(conf.use_cc.B, icache.io.core.ready, !icache.io.axi.ar.valid && io.mem.ar.ready)

  val inst_valid: Bool = Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst_valid, io.mem.r.valid)
  val inst: UInt       = Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst, io.mem.r.data)

  val inst_kill = RegInit(false.B)
  when ((state === sWtInstOK && inst_valid) || state === sWtForward) { inst_kill := false.B
  }.elsewhen(io.if_kill) { inst_kill := true.B }

  switch (state) {
    is (sWtAddrOK) {
      when (addr_ready)         { state := sWtInstOK
      }
    }
    is (sWtInstOK) {
      when (inst_valid) {
        when (dec_kill)         { state := sWtAddrOK
        }.elsewhen(inst_kill ||
          io.forward)           { state := Mux(!addr_ready || if_kill, sWtAddrOK, sWtInstOK)
        }.otherwise             { state := sWtForward
        }
      }
    }
    is (sWtForward) {
      when (dec_kill)           { state := sWtAddrOK
      }.elsewhen(io.forward)    { state := Mux(!addr_ready || if_kill, sWtAddrOK, sWtInstOK)
      }
    }
  }

  val kill_pc = Reg(UInt(conf.xprlen.W))
  when (io.if_kill) { kill_pc := io.pc }

  val pc_kill: Bool = RegInit(false.B)
  when(state === sWtAddrOK) {
    when(addr_ready)    { pc_kill := false.B
    }.elsewhen(if_kill) { pc_kill := true.B }
  }

  pc := Mux(pc_kill, kill_pc, io.pc)
  /*========================pc part============================*/
  io.pc_forward := if_kill || addr_ready && (
    (state === sWtAddrOK  && !pc_kill)   ||
    (state === sWtInstOK  && inst_valid  && (io.forward || inst_kill)) ||
    (state === sWtForward && io.forward)
  )

  pc_valid := !if_kill && (
    (state === sWtInstOK  && inst_valid   && (io.forward || inst_kill)) ||
    (state === sWtForward && io.forward)) ||
     state === sWtAddrOK

  io.inst_valid := ((state === sWtInstOK && inst_valid && !inst_kill) ||
                     state === sWtForward) && !dec_kill

  io.inst := LatchData(inst_valid, inst)
  val reg_pred = Reg(new Predict(conf.xprlen))
  val reg_pc   = Reg(UInt(conf.xprlen.W))

  when(pc_valid) { //enter into dec stage wait for inst come back
    reg_pred := io.if_btb
    reg_pc   := io.pc
  }

  io.dec_btb.Tp  := Mux(io.inst_valid, reg_pred.Tp, CFIType.invalid.U)
  io.dec_btb.Tg  := reg_pred.Tg
  io.dec_btb.Sel := reg_pred.Sel
  io.dec_pc      := reg_pc

//  printf("%c, pc = %x, valid = %x, frwd = %x, redirect = %x, inst = %x, valid = %x, frwd = %x, dec_pc = %x, memReady = %x, memValid = %x\n"
//    , MuxCase(Str("A"), Array(
//      (state === sWtInstOK) -> Str("I"),
//      (state === sWtForward) -> Str("F")
//    ))
//    ,io.pc
//    ,pc_valid
//    ,io.pc_forward
//    ,io.redirect
//    ,io.inst
//    ,io.inst_valid
//    ,io.forward
//    ,io.dec_pc
////    ,addr_ready
//    ,io.mem.r.id
////    ,inst_valid
//    ,io.mem.r.valid
//  )
}
