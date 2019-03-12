package chiwen

import chisel3._
import chisel3.util._
import common.{CPUConfig, Str}

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
    val if_pc      = Input(UInt(conf.xprlen.W))
    val dec_pc     = Output(UInt(conf.xprlen.W))
    val if_kill    = Input(Bool()) // from dec and downflow
    val dec_kill   = Input(Bool()) // from exe and downflow

    val pc_forward = Output(Bool())
    val forward    = Input(Bool())
    val inst       = Output(Valid(UInt(conf.xprlen.W)))
  })

  val sWtAddrOK :: sWtInstOK :: sWtForward :: Nil = Enum(3)
  val state = RegInit(sWtAddrOK)
  val pc_valid = Wire(Bool())
  val icache = Module(new Icache())
  icache.io.cyc := io.cyc
  icache.io.axi.r <> io.mem.r
  icache.io.axi.ar.ready := io.mem.ar.ready
  icache.io.core.pc.bits := io.if_pc
  icache.io.core.pc.valid := conf.use_cc.B & pc_valid
  io.mem.ar.id    := Mux(conf.use_cc.B, icache.io.axi.ar.id, conf.incRd)
  io.mem.ar.valid := Mux(conf.use_cc.B, icache.io.axi.ar.valid, pc_valid)
  io.mem.ar.addr  := Mux(conf.use_cc.B, icache.io.axi.ar.addr, io.if_pc)
  io.mem.ar.burst := Mux(conf.use_cc.B, icache.io.axi.ar.burst, "b01".U)
  io.mem.ar.len   := Mux(conf.use_cc.B, icache.io.axi.ar.len, 0.U)
  io.mem.ar.size  := Mux(conf.use_cc.B, icache.io.axi.ar.size, "b010".U)
  val addr_ready: Bool = Mux(conf.use_cc.B, icache.io.core.ready, !icache.io.axi.ar.valid && io.mem.ar.ready)
  val inst = Wire(Valid(UInt(conf.xprlen.W)))
  inst.valid := Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst.valid, io.mem.r.valid)
  inst.bits  := Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst.bits, io.mem.r.data)

  /*========================FSM part============================*/
  val inst_kill = RegInit(false.B)
  switch (state) {
    is (sWtAddrOK) {
      when(addr_ready) { state := sWtInstOK }
    }
    is (sWtInstOK) {
      when(inst.valid) {
        when(io.forward || inst_kill) {
          state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
        }.elsewhen(io.dec_kill) { state := sWtAddrOK
        }.otherwise { state := sWtForward }
      }
    }
    is (sWtForward) {
      when(io.forward) {
        state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
      }.elsewhen(io.dec_kill) { state := sWtAddrOK }
    }
  }
  when (pc_valid) {
    inst_kill := io.if_kill
  }.elsewhen(inst.valid) {
    inst_kill := false.B
  }.elsewhen(io.dec_kill && state === sWtInstOK) {
    inst_kill := true.B
  }

  io.inst.valid := (inst.valid && !inst_kill)|| state === sWtForward
  pc_valid := state === sWtAddrOK ||
    (inst.valid && (io.forward || inst_kill)) ||
    (state === sWtForward && io.forward)
  io.pc_forward := io.if_kill || (addr_ready && pc_valid)

  val reg_pred = Reg(new Predict(conf.xprlen))
  val reg_pc   = Reg(UInt(conf.xprlen.W))

  when(pc_valid) { //enter into dec stage wait for inst come back
    reg_pred := io.if_btb
    reg_pc   := io.if_pc
  }

  io.inst.bits := LatchData(inst.valid, inst.bits)
  io.dec_btb := reg_pred
  io.dec_pc  := reg_pc

  if (conf.verbose) {
    when (io.cyc > 16427.U && io.cyc < 16475.U) {
      printf("FetchInst: %c "
        , MuxCase(Str("A"), Array(
          (state === sWtInstOK) -> Str("I"),
          (state === sWtForward) -> Str("F")
        ))
      )
      printf(p"${io.cyc}")
      printf(p" pc_valid $pc_valid pc ${Hexadecimal(io.if_pc)} pc_forward ${io.pc_forward}")
      printf(p" dec_kill ${io.dec_kill} inst_kill $inst_kill inst_valid ${inst.valid} forward ${io.forward}\n")
      printf(p"pc = ${Hexadecimal(io.if_pc)} valid = $pc_valid pc_fwd = ${io.pc_forward} if_kill = ${io.if_kill}, fwd = ${io.forward}\n")
    }
  }
}
