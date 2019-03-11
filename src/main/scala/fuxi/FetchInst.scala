package fuxi

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
    val pc         = Input(UInt(conf.xprlen.W))
    val pc_split   = Input(Bool())
    val pc_forward = Output(Bool())
    val forward    = Input(Bool())
    val if_kill    = Input(Bool()) // from dec and downflow
    val dec_kill   = Input(Bool()) // from exe and downflow
    val inst       = Output(Vec(2, Valid(UInt(conf.xprlen.W))))
    val dec_pc     = Output(Vec(2, UInt(conf.xprlen.W)))
  })

  val sWtAddrOK :: sWtInstOK :: sWtForward :: Nil = Enum(3)
  val state = RegInit(sWtAddrOK)
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
  inst_valid(0) := Mux(io.mem.r.id === conf.incRd, io.mem.r.valid && !inst_odd, icache.io.core.inst(0).valid)
  inst_valid(1) := Mux(io.mem.r.id === conf.incRd, io.mem.r.valid &&  inst_odd, icache.io.core.inst(1).valid)
  val valid_mux: Bool = Mux(inst_odd, inst_valid(1), inst_valid(0))
  val next_odd: Bool  = !inst_valid(1) && Mux(inst_split, io.pc(conf.pcLSB).toBool, !inst_split) //based on valid_orR
  switch (state) {
    is (sWtAddrOK) {
      when (addr_ready) { state := sWtInstOK }
    }
    is (sWtInstOK) {
     when(valid_mux) {
        when(io.forward || inst_kill || next_odd) {
          state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
        }.elsewhen (io.dec_kill) { state := sWtAddrOK
        }.otherwise { state := sWtForward }
      }
    }
    is (sWtForward) {
      when(io.forward) {
        state := Mux(addr_ready, sWtInstOK, sWtAddrOK)
      }.elsewhen (io.dec_kill) { state := sWtAddrOK }
    }
  }

  when (pc_valid) {
    inst_kill := io.if_kill
  }.elsewhen(valid_mux) {
    inst_kill := false.B
  }.elsewhen(io.dec_kill && state === sWtInstOK) {
    inst_kill := true.B
  }

  pc_valid := state === sWtAddrOK ||
    (valid_mux && (io.forward || inst_kill || next_odd)) || //sWtInstOK
    (state === sWtForward && io.forward)

  val pc_odd_reg  = RegInit(false.B)
  io.pc_forward := io.if_kill || addr_ready && (
    (state === sWtForward && io.forward) ||
      (!pc_odd_reg && state === sWtAddrOK) || Mux(inst_odd,
      inst_valid(1) && (inst_kill || io.forward),
      inst_valid(0) && (inst_kill ||(io.forward && (inst_valid(1) || inst_split))))
    )

  val pc_odd_wire = inst_valid(0) && !inst_valid(1) && !inst_split && !inst_kill
  when (addr_ready) { pc_odd_reg := false.B
  }.elsewhen(pc_odd_wire) { pc_odd_reg := true.B }

  val pc_odd = pc_odd_reg || pc_odd_wire
  pc := Mux(pc_odd, Cat(io.dec_pc(0)(conf.xprlen-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W)), io.pc)

  val state_WtForward = RegInit(false.B)
  when (io.dec_kill || io.forward || inst_kill) { state_WtForward := false.B
  }.elsewhen(inst_valid(0) && !inst_odd) { state_WtForward := true.B  }

  io.inst(0).valid := ( inst_valid(0) && !inst_kill  && !inst_odd) || state_WtForward
  io.inst(1).valid := ((inst_valid(1) && !inst_kill) || state === sWtForward) && !inst_split

  /*=======================dec part==============================*/
  val reg_pred = Reg(Vec(2, new Predict(conf.xprlen)))
  val reg_pc   = RegInit(VecInit(Seq.fill(2)(START_ADDR)))
  when (pc_valid) {
    inst_odd   := pc_odd || io.pc(conf.pcLSB).toBool
    inst_split := !pc_odd && !io.pc(conf.pcLSB).toBool && io.pc_split
    for (i <- 0 until 2) {
      when (!pc_odd) {
        reg_pc(i) := Cat(io.pc(conf.xprlen-1, conf.pcLSB+1), i.U(1.W), 0.U(conf.pcLSB.W))
        reg_pred(i) := io.if_btb(i)
      }
    }
  }
  val inst = Wire(Vec(2, UInt(conf.xprlen.W)))
  for (i <- 0 until 2) {
    inst(i) := Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst(i).bits, io.mem.r.data)
    io.dec_btb(i)   := reg_pred(i)
    io.dec_pc(i)    := reg_pc(i)
    io.inst(i).bits := LatchData(inst_valid(i), inst(i), BUBBLE)
  }

//  when (io.cyc >= 132.U && io.cyc <= 134.U) {
//    printf("FetchInst: state = %c pc = %x io.pc = %x [inst %x %x] [valid %x %x] inst_kill %x forward %x pc_fwd %x\n"
//      , MuxCase(Str("A"), Array(
//          (state === sWtInstOK) -> Str("I"),
//          (state === sWtForward) -> Str("F")
//        ))
//      , pc
//      , io.pc
//      , io.inst(0).bits
//      , io.inst(1).bits
//      , io.inst(0).valid
//      , io.inst(1).valid
//      , inst_kill
//      , io.forward
//      , io.pc_forward
//    )
//    printf(p"DEBUG: inst_valid $inst_valid inst_split $inst_split dec_kill ${io.dec_kill} btb_tg = ${Hexadecimal(io.dec_btb(1).Tg)} btb_tp = ${io.dec_btb(1).Tp}\n")
//  }
}
