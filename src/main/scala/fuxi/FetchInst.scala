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
  val valid_orR: Bool = inst_valid.asUInt.orR
  val valid_and: Bool = inst_valid.asUInt.andR
  val next_odd:  Bool = !inst_odd && !inst_valid(1) && ((!inst_split && !inst_kill) || (inst_split && io.pc(conf.pcLSB).toBool))

  switch (state) {
    is (sWtAddrOK) {
      when (addr_ready)           { state := sWtInstOK }
    }
    is (sWtInstOK) {
      when (valid_orR) {
        when (io.dec_kill)        { state := sWtAddrOK
        }.elsewhen(io.forward ||
          inst_kill || next_odd)  { state := Mux(!addr_ready || io.if_kill, sWtAddrOK, sWtInstOK)
        }.otherwise               { state := sWtForward }
      }
    }
    is (sWtForward) {
      when (io.dec_kill)          { state := sWtAddrOK
      }.elsewhen(io.forward)      { state := Mux(!addr_ready || io.if_kill, sWtAddrOK, sWtInstOK) }
    }
  }

  when ((state === sWtInstOK && valid_orR) || state === sWtForward) { inst_kill := false.B
  }.elsewhen(io.if_kill) { inst_kill := true.B } // FIXME: if_kill > dec_kill

  val state_WtForward = RegInit(false.B)
  when (io.dec_kill || io.forward || inst_kill) { state_WtForward := false.B
  }.elsewhen(state === sWtInstOK && inst_valid(0)) { state_WtForward := true.B  }

  val kill_pc = Reg(UInt(conf.xprlen.W))
  when (io.if_kill) { kill_pc := pc }

  val pc_kill: Bool = RegInit(false.B)
  when(state === sWtAddrOK) {
    when(addr_ready)       { pc_kill := false.B
    }.elsewhen(io.if_kill) { pc_kill := true.B }
  }

  val pc_odd: Bool = Latch(state === sWtInstOK && !inst_odd && inst_valid(0) && !inst_valid(1) && !inst_split && !inst_kill,
                           addr_ready, !(io.if_kill || inst_kill))

  val flip_pc = Mux(pc_odd, Cat(io.dec_pc(0)(conf.xprlen-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W)), io.pc)
  pc := Mux(pc_kill, kill_pc, flip_pc)
  /*========================pc part============================*/
  io.pc_forward := io.if_kill || addr_ready &&((state === sWtForward && io.forward) ||
                                    !pc_odd &&((state === sWtAddrOK  && !pc_kill)   ||
                                 (valid_orR &&  state === sWtInstOK  && (io.forward || inst_kill))))


  pc_valid := state === sWtAddrOK || !io.if_kill && ((state === sWtForward && io.forward) ||
    (state === sWtInstOK  && valid_orR && (io.forward || inst_kill || next_odd)))

  when(pc_valid) {
    inst_odd    := pc(conf.pcLSB)
    inst_split  := io.pc_split
  }

  val reg_pred = Reg(Vec(2, new Predict(conf.xprlen)))
  val reg_pc   = RegInit(VecInit(Seq.fill(2)(START_ADDR)))
  when (pc_valid) {
    when (io.pc(conf.pcLSB).toBool) {
      reg_pc(1)   := io.pc
      reg_pred(1) := io.if_btb(1)
    }.elsewhen(!pc_odd) {
      for (i <- 0 until 2) {
        reg_pred(i) := io.if_btb(i)
        reg_pc(i)   := Cat(io.pc(conf.xprlen-1, conf.pcLSB+1), i.U(1.W), 0.U(conf.pcLSB.W))
      }
    }
  }
  /*=======================dec part==============================*/
  io.inst(0).valid := ((state === sWtInstOK && inst_valid(0) && !inst_kill) || state_WtForward) && !io.dec_kill
  io.inst(1).valid := ((state === sWtInstOK && inst_valid(1) && !inst_kill) || state === sWtForward) && !inst_split && !io.dec_kill

  val inst = Wire(Vec(2, UInt(conf.xprlen.W)))
  for (i <- 0 until 2) {
    inst(i) := Mux(io.mem.r.id === conf.iccRd, icache.io.core.inst(i).bits, io.mem.r.data)
    io.dec_btb(i)   := reg_pred(i)
    io.dec_pc(i)    := reg_pc(i)
    io.inst(i).bits := LatchData(inst_valid(i), inst(i), BUBBLE)
  }

  when (io.cyc >= 142.U && io.cyc <= 145.U) {
    printf("FetchInst: state = %c pc = %x io.pc = %x [inst %x %x] [valid %x %x] inst_kill %x forward %x pc_fwd %x\n"
      , MuxCase(Str("A"), Array(
          (state === sWtInstOK) -> Str("I"),
          (state === sWtForward) -> Str("F")
        ))
      , pc
      , io.pc
      , io.inst(0).bits
      , io.inst(1).bits
      , io.inst(0).valid
      , io.inst(1).valid
      , inst_kill
      , io.forward
      , io.pc_forward
    )
    printf(p"FetchiInst: wtForward = $state_WtForward\n")
  }

when (io.cyc === 142.U) {
  printf(p"DEBUG: inst_valid ${inst_valid} inst_kill ${inst_kill} if_kill ${io.if_kill}\n")
}
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
