package myCore

import chisel3._
import chisel3.util._
import common.{CPUConfig, Str}

object LatchData {
  def apply(valid: Bool, data: UInt, init: UInt = 0.U): UInt = {
    val data_latch = RegInit(init)
    when (valid) { data_latch := data }
    Mux(valid, data, data_latch)
  }
}

class PredIO(val addr_width: Int) extends Bundle with BTBParams {
  val Tp  = Input(UInt(CFIType.SZ.W))
  val Tg  = Input(UInt(addr_width.W))
  val Sel = Input(UInt(log2Ceil(nEntries).W))
}

class FetchInst(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val if_pred    = new PredIO(conf.xprlen)
    val dec_pred   = Flipped(new PredIO(conf.xprlen))

    val pc         = Input(UInt(conf.xprlen.W))
    val pc_forward = Output(Bool())

    val mem        = new AxiIO(conf.xprlen)
    val forward    = Input(Bool())
    val redirect   = Input(Bool())

    val inst       = Output(UInt(conf.xprlen.W))
    val inst_valid = Output(Bool())
    val dec_pc     = Output(UInt(conf.xprlen.W))
    val cyc = Input(UInt(conf.xprlen.W))

  })

  val sWtAddrOK :: sWtInstOK :: sWtForward :: Nil = Enum(3)
  val state = RegInit(sWtAddrOK)
  val use_cc: Bool   = true.B
  val pc_valid = Wire(Bool())
  val pc       = Wire(UInt(conf.xprlen.W))
  val icache = Module(new Icache())
  icache.io.cyc := io.cyc
  icache.io.axi.r <> io.mem.r
  icache.io.axi.ar.ready := io.mem.ar.ready
  icache.io.core.pc := pc
  icache.io.core.pc_valid := use_cc & pc_valid
  io.mem.ar.id    := Mux(use_cc, icache.io.axi.ar.id, conf.incRd)
  io.mem.ar.valid := Mux(use_cc, icache.io.axi.ar.valid, pc_valid)
  io.mem.ar.addr  := Mux(use_cc, icache.io.axi.ar.addr, pc)
  io.mem.ar.burst := Mux(use_cc, icache.io.axi.ar.burst, "b01".U)
  io.mem.ar.len   := Mux(use_cc, icache.io.axi.ar.len, 0.U)
  io.mem.ar.size  := Mux(use_cc, icache.io.axi.ar.size, "b010".U)

  val addr_ready: Bool = Mux(use_cc, icache.io.core.ready, !icache.io.axi.ar.valid && io.mem.ar.ready)
  val inst_valid: Bool = Mux(io.mem.r.id === conf.incRd ,io.mem.r.valid, icache.io.core.inst_valid)
  val inst: UInt       = Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst, io.mem.r.data)

  val kill = RegInit(false.B)
  when ((state === sWtInstOK && inst_valid) || state === sWtForward) { kill := false.B
  }.elsewhen(io.redirect) {kill := true.B}

  switch (state) {
    is (sWtAddrOK) {
      when (addr_ready)              { state := sWtInstOK
      }
    }
    is (sWtInstOK) {
      when (inst_valid) {
        when (io.redirect)           { state := sWtAddrOK
        }.elsewhen(kill||io.forward) { state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
        }.otherwise                  { state := sWtForward
        }
      }
    }
    is (sWtForward) {
      when (io.redirect)             { state := sWtAddrOK
      }.elsewhen(io.forward)         { state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
      }
    }
  }

  val maintain_pc = Reg(UInt(conf.xprlen.W))
  when (io.redirect) { maintain_pc := io.pc }

  val maintain: Bool = RegInit(false.B)
  when(state === sWtAddrOK) {
    when(addr_ready)        { maintain := false.B
    }.elsewhen(io.redirect) { maintain := true.B
    }
  }
  val cond_WtAddrOK: Bool  = state === sWtAddrOK
  val cond_WtInstOK: Bool  = state === sWtInstOK  && inst_valid && (io.forward || kill)
  val cond_WtForward: Bool = state === sWtForward && io.forward
  pc_valid := cond_WtAddrOK || ((cond_WtInstOK || cond_WtForward) && !io.redirect)
  pc := Mux(maintain, maintain_pc, io.pc)
  io.pc_forward := (((cond_WtAddrOK && !maintain) || cond_WtInstOK || cond_WtForward) && addr_ready) || io.redirect

  io.inst := LatchData(inst_valid, inst, BUBBLE)
  io.inst_valid := ((state === sWtInstOK   && inst_valid) ||
                     state === sWtForward) && !kill

  val reg_PredTp  = Reg(UInt(CFIType.SZ.W))
  val reg_predTg  = Reg(UInt(conf.xprlen.W))
  val reg_PredSel = Reg(UInt(log2Ceil(nEntries).W))
  val reg_pc      = RegInit(START_ADDR)

  when((cond_WtAddrOK || cond_WtInstOK || cond_WtForward) && addr_ready) { //enter into dec stage wait for inst come back
    reg_PredTp  := io.if_pred.Tp
    reg_predTg  := io.if_pred.Tg
    reg_PredSel := io.if_pred.Sel
    reg_pc      := io.pc
  }

  io.dec_pred.Tp  := Mux(io.inst_valid, reg_PredTp, CFIType.invalid.U(CFIType.SZ.W))
  io.dec_pred.Tg  := reg_predTg
  io.dec_pred.Sel := reg_PredSel
  io.dec_pc       := reg_pc

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
