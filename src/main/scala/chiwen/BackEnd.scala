package chiwen

import chisel3._
import chisel3.util._
import common._

object Stage {
  val DEC = 0
  val EXE = 1
  val MEM = 2
  val Num: Int = MEM + 1
}

class WbCrtl extends Bundle {
  val rf_wen = Bool()
}

class MemCrtl extends WbCrtl {
  val mem_en  = Bool()
  val csr_cmd = UInt(CSR.SZ)
  val illegal = Bool()
}

class ExeCrtl extends MemCrtl {
  val br_type = UInt(BR_N.getWidth.W)
  val branch  = Bool()
  val jump    = UInt(Jump.NUM.W)
  val btb_you = Bool()
}

class DecCrtl extends Bundle {
  val rs1_oen = Bool()
  val rs2_oen = Bool()
}

class Wb extends Bundle {
  val rf_wen = Bool()
  val wbaddr = UInt(5.W)
}

class Mem(implicit val conf: CPUConfig) extends Wb {
  val mem_en   = Bool()
  val csr_cmd  = UInt(CSR.SZ)
  val illegal  = Bool()
  val pc       = UInt(conf.xprlen.W)
  val inst     = UInt(conf.xprlen.W)
  val rs2_data = UInt(conf.xprlen.W)
  val wb_sel   = UInt(WB_X.getWidth.W)
  val mem_fcn  = UInt(M_X.getWidth.W)
  val mem_typ  = UInt(MT_X.getWidth.W)
}

class Exe(val nEntries : Int)(implicit conf: CPUConfig) extends Mem {
  val br_type  = UInt(BR_N.getWidth.W)
  val branch   = Bool()
  val jump     = UInt(Jump.NUM.W)
  val btb      = new Predict(conf.xprlen)
  val op1_data = UInt(conf.xprlen.W)
  val op2_data = UInt(conf.xprlen.W)
  val alu_fun  = UInt(ALU_X.getWidth.W)
}

object Pulse {
  def apply(in: Bool, forward: Bool): Bool = {
    val in_latch = RegInit(true.B)
    when (forward) { in_latch := true.B
    }.elsewhen(in) { in_latch := false.B}
    in && in_latch
  }
}

class BackEnd(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val mem  = new MemPortIo(conf.xprlen)
    val cyc   = Output(UInt(conf.xprlen.W))
    val front = Flipped(new InterfaceIO(conf.xprlen))
  })

  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)
  // Decode Stage ===========================================================================================================================================
  // ========================================================================================================================================================
  val stall     = Wire(Vec(Stage.Num, Bool()))
  val dec       = Module(new InstDecoder()).io
  val dec_wire  = Wire(new DecCrtl)
  val dec_valid = Wire(Bool())
  val exe       = Reg(new Exe(nEntries))
  val exe_valid = RegInit(false.B)
  val exe_wire  = Wire(new ExeCrtl)
  val mem       = Reg(new Mem())
  val mem_valid = RegInit(false.B)
  val mem_wire  = Wire(new MemCrtl)
  val wb        = Reg(new Wb())
  val wb_valid  = RegInit(false.B)
  val wb_wire   = Wire(new WbCrtl)

  val exe_wbdata = Wire(UInt(conf.xprlen.W))
  val mem_wbdata = Wire(UInt(conf.xprlen.W))
  val wb_wbdata  = Reg(UInt(conf.xprlen.W))

  dec.inst         := io.front.inst.bits
  dec_valid        := io.front.inst.valid
  dec_wire.rs1_oen := dec_valid && dec.cinfo.rs1_oen
  dec_wire.rs2_oen := dec_valid && dec.cinfo.rs2_oen

  // Bypass Muxes
  val dec_op1_data  = Wire(UInt(conf.xprlen.W))
  val dec_op2_data  = Wire(UInt(conf.xprlen.W))
  val dec_rs1_data  = Wire(UInt(conf.xprlen.W))
  val dec_rs2_data  = Wire(UInt(conf.xprlen.W))

  // Register File
  val regfile = Module(new Regfile())
  val rf_rs1_data = Wire(UInt())
  val rf_rs2_data = Wire(UInt())
  regfile.io.rs1_addr := dec.cinfo.rs1_addr
  regfile.io.rs2_addr := dec.cinfo.rs2_addr
  rf_rs1_data := regfile.io.rs1_data
  rf_rs2_data := regfile.io.rs2_data
  // roll the OP1 mux into the bypass mux logic
  dec_rs1_data := MuxCase(rf_rs1_data, Array(
    ((exe.wbaddr === dec.cinfo.rs1_addr) && exe_wire.rf_wen) -> exe_wbdata,
    ((mem.wbaddr === dec.cinfo.rs1_addr) && mem_wire.rf_wen) -> mem_wbdata,
    (( wb.wbaddr === dec.cinfo.rs1_addr) &&  wb_wire.rf_wen) -> wb_wbdata
  ))

  dec_rs2_data := MuxCase(rf_rs2_data, Array(
    ((exe.wbaddr === dec.cinfo.rs2_addr) && exe_wire.rf_wen) -> exe_wbdata,
    ((mem.wbaddr === dec.cinfo.rs2_addr) && mem_wire.rf_wen) -> mem_wbdata,
    (( wb.wbaddr === dec.cinfo.rs2_addr) &&  wb_wire.rf_wen) -> wb_wbdata
  ))

  dec_op1_data := MuxCase(dec_rs1_data, Array(
    (dec.cinfo.op1_sel === OP1_IMZ)-> dec.dinfo.imm_z,
    (dec.cinfo.op1_sel === OP1_PC) -> io.front.pc))

  dec_op2_data := MuxCase(dec_rs2_data, Array(
    (dec.cinfo.op2_sel === OP2_ITYPE)  -> dec.dinfo.imm_i,
    (dec.cinfo.op2_sel === OP2_STYPE)  -> dec.dinfo.imm_s,
    (dec.cinfo.op2_sel === OP2_SBTYPE) -> dec.dinfo.imm_sb,
    (dec.cinfo.op2_sel === OP2_UTYPE)  -> dec.dinfo.imm_u,
    (dec.cinfo.op2_sel === OP2_UJTYPE) -> dec.dinfo.imm_uj))

  when ((stall(Stage.DEC) && !stall(Stage.EXE) && !stall(Stage.MEM)) || io.front.xcpt.valid) {
    // (kill exe stage) insert NOP (bubble) into Execute stage on front-end stall (e.g., hazard clearing)
    exe_valid := false.B
  }.elsewhen(!stall(Stage.EXE) && !stall(Stage.MEM)) {
    exe_valid    := dec_valid && !io.front.kill
    exe.rf_wen   := dec.cinfo.rf_wen
    exe.mem_en   := dec.cinfo.mem_en
    // convert CSR instructions with raddr1 == 0 to read-only CSR commands
    exe.csr_cmd  := Mux((dec.cinfo.csr_cmd === CSR.S || dec.cinfo.csr_cmd === CSR.C) &&
      dec.cinfo.rs1_addr === 0.U, CSR.R, dec.cinfo.csr_cmd)
    exe.illegal  := dec.cinfo.illegal
    exe.br_type  := dec.cinfo.br_type
    exe.pc       := io.front.pc
    exe.op1_data := dec_op1_data
    exe.op2_data := dec_op2_data
    exe.rs2_data := dec_rs2_data
    exe.inst     := dec.inst
    exe.alu_fun  := dec.cinfo.alu_fun
    exe.wb_sel   := dec.cinfo.wb_sel
    exe.wbaddr   := dec.cinfo.wbaddr
    exe.mem_fcn  := dec.cinfo.mem_fcn
    exe.mem_typ  := dec.cinfo.mem_typ
    exe.branch   := dec.cinfo.is_branch
    exe.jump     := io.front.jump
    exe.btb      := io.front.pred
  }
  // Execute Stage ==========================================================================================================================================
  //=========================================================================================================================================================
  exe_wire.rf_wen  := exe.rf_wen && exe_valid && exe.wbaddr =/= 0.U
  exe_wire.mem_en  := exe.mem_en && exe_valid
  exe_wire.csr_cmd := Mux(exe_valid, exe.csr_cmd, CSR.N)
  exe_wire.br_type := Mux(exe_valid, exe.br_type, BR_N)
  exe_wire.branch  := exe_valid && exe.branch
  exe_wire.jump    := Mux(exe_valid, exe.jump, 0.U)
  exe_wire.illegal := exe.illegal && exe_valid
  exe_wire.btb_you := exe.btb.you && exe_valid

  val alu = Module(new ALU()).io
  alu.op1          := exe.op1_data
  alu.op2          := exe.op2_data
  alu.rs2_data     := exe.rs2_data
  alu.pc           := exe.pc
  alu.ctrl.fun     := exe.alu_fun
  alu.ctrl.br_type := exe_wire.br_type
  alu.ctrl.wb_sel  := exe.wb_sel

  exe_wbdata := alu.result

  io.front.ras_pop  := Pulse(exe_wire.jump(Jump.pop).toBool,  !stall(Stage.MEM))
  io.front.ras_push := Pulse(exe_wire.jump(Jump.push).toBool, !stall(Stage.MEM))
  io.front.ras_tgt  := alu.target.conti

  io.front.feedBack.you := Pulse(exe_wire.btb_you, !stall(Stage.MEM))
  io.front.feedBack.idx := exe.btb.idx
  io.front.feedBack.redirect := Pulse(alu.ctrl.pc_sel === PC_BRJMP || alu.ctrl.pc_sel === PC_JALR, !stall(Stage.MEM))
  io.front.fb_pc := exe.pc
  val bj_type =
    Mux(exe_wire.branch,                                      CFIType.branch.U,
    Mux(exe_wire.jump(Jump.pop),                              CFIType.retn.U,
    Mux(exe_wire.jump(Jump.none) || exe_wire.jump(Jump.push), CFIType.jump.U,
                                                              CFIType.invalid.U
    )))
  val next_pc =
    Mux(alu.ctrl.pc_sel === PC_BRJMP, alu.target.brjmp,
    Mux(alu.ctrl.pc_sel === PC_JALR,  alu.target.jpreg,
                                      alu.target.conti
    ))
  io.front.feedBack.typ := bj_type
  io.front.feedBack.tgt := next_pc

  val mispredict = Wire(Bool())
  if (conf.hasBTB) mispredict := next_pc =/= exe.btb.tgt && exe_valid
  else mispredict := alu.ctrl.pc_sel =/= PC_4 && exe_valid

  val mem_reg_exe_out = Reg(UInt(conf.xprlen.W))
  val mem_reg_jpnpc   = RegInit(0.U(conf.xprlen.W))
  when (!stall(Stage.EXE) && stall(Stage.MEM) || io.front.xcpt.valid) {
    mem_valid    := false.B
    mem_reg_jpnpc:= 0.U
  } .elsewhen (!stall(Stage.MEM)) {
    mem_valid    := exe_valid
    mem.rf_wen   := exe.rf_wen
    mem.mem_en   := exe.mem_en
    mem.csr_cmd  := exe.csr_cmd
    mem.illegal  := exe.illegal
    mem.pc       := exe.pc
    mem.inst     := exe.inst
    mem.wb_sel   := exe.wb_sel
    mem.wbaddr   := exe.wbaddr
    mem.rs2_data := exe.rs2_data
    mem.mem_fcn  := exe.mem_fcn
    mem.mem_typ  := exe.mem_typ

    mem_reg_exe_out := exe_wbdata
    mem_reg_jpnpc:= (Fill(conf.xprlen, alu.ctrl.pc_sel === PC_BRJMP) & alu.target.brjmp) |
                    (Fill(conf.xprlen, alu.ctrl.pc_sel === PC_JALR)  & alu.target.jpreg)
  }
  // Memory Stage ============================================================================================================================================
  //==========================================================================================================================================================
  mem_wire.rf_wen  := mem.rf_wen && mem_valid && mem.wbaddr =/= 0.U
  mem_wire.mem_en  := mem.mem_en && mem_valid
  mem_wire.csr_cmd := Mux(mem_valid, mem.csr_cmd, CSR.N)
  mem_wire.illegal := mem.illegal && mem_valid
  // Control Status Registers
  csr.io := DontCare
  csr.io.rw.addr  := mem.inst(CSR_ADDR_MSB,CSR_ADDR_LSB)
  csr.io.rw.wdata := mem_reg_exe_out
  csr.io.rw.cmd   := mem_wire.csr_cmd
  csr.io.pc       := mem.pc

  val ls_addr_ma_valid = MuxLookup(mem.mem_typ(1,0) ,false.B, Array(
    2.U -> mem_reg_exe_out(0),
    3.U -> mem_reg_exe_out(1,0).orR
  ))
  val ma_jump: Bool    = mem_reg_jpnpc(1,0).orR
  val ma_load: Bool    = mem_wire.mem_en && mem.mem_fcn === M_XRD && ls_addr_ma_valid
  val ma_store: Bool   = mem_wire.mem_en && mem.mem_fcn === M_XWR && ls_addr_ma_valid
  csr.io.xcpt  := ma_load || ma_store || ma_jump || mem_wire.illegal
  csr.io.cause := MuxCase(0.U, Array(
    ma_jump    -> Causes.misaligned_fetch.U,
    ma_load    -> Causes.misaligned_load.U,
    ma_store   -> Causes.misaligned_store.U,
    mem_wire.illegal -> Causes.illegal_instruction.U
  ))
  csr.io.tval  := MuxCase(0.U, Array(
    ma_jump    -> mem_reg_jpnpc,
    ma_load    -> mem_reg_exe_out,
    ma_store   -> mem_reg_exe_out,
    mem_wire.illegal -> mem.inst
  ))
  io.front.xcpt.valid := ma_jump || ma_load || ma_store || mem_wire.illegal || csr.io.eret
  io.front.xcpt.bits  := csr.io.evec

  // datapath to data memory outputs =============================
  io.mem.req.valid     := mem_wire.mem_en && !ma_store && !ma_load
  io.mem.req.bits.addr := mem_reg_exe_out
  io.mem.req.bits.fcn  := mem.mem_fcn
  io.mem.req.bits.typ  := mem.mem_typ
  io.mem.req.bits.data := mem.rs2_data
  //===============================================================

  // WB Mux
  mem_wbdata := MuxCase(mem_reg_exe_out, Array( // default is wb_alu and wb_pc4
    // (mem_reg_wb_sel === WB_ALU) -> mem_reg_alu_out,
    // (mem_reg_wb_sel === WB_PC4) -> mem_reg_alu_out,
    (mem.wb_sel === WB_MEM) -> io.mem.resp.bits.data,
    (mem.wb_sel === WB_CSR) -> csr.io.rw.rdata))

  when (stall(Stage.MEM) || io.front.xcpt.valid) {
    wb_valid  := false.B
  } .otherwise {
    wb_valid  := mem_valid
    wb.rf_wen := mem.rf_wen
    wb.wbaddr := mem.wbaddr
    wb_wbdata := mem_wbdata
  }
  // Writeback Stage ===========================================================================================================================================
  //============================================================================================================================================================
  wb_wire.rf_wen   := wb.rf_wen && wb_valid && wb.wbaddr =/= 0.U
  regfile.io.waddr := wb.wbaddr
  regfile.io.wdata := wb_wbdata
  regfile.io.wen   := wb_wire.rf_wen

  csr.io.retire    := wb_valid //FIXME
  // Add your own uarch counters here!
  csr.io.counters.foreach(_.inc := false.B)
  //control pipeline signals====================================================================================================================================
  //============================================================================================================================================================
  val exe_load_inst: Bool = exe_wire.mem_en && exe.mem_fcn === M_XRD

  stall(Stage.DEC) :=
    (exe_load_inst && exe.wbaddr === dec.cinfo.rs1_addr && dec.cinfo.rs1_addr =/= 0.U && dec_wire.rs1_oen) ||
    (exe_load_inst && exe.wbaddr === dec.cinfo.rs2_addr && dec.cinfo.rs2_addr =/= 0.U && dec_wire.rs2_oen) ||
    exe_wire.csr_cmd =/= CSR.N

  stall(Stage.EXE) := false.B
  stall(Stage.MEM) := mem_wire.mem_en && !io.mem.resp.valid

  io.front.kill := Pulse(mispredict, forward = !stall(Stage.MEM))
  io.front.forward  := !stall.asUInt.orR

  // Printout
  printf("Core: Cyc= %d WB[ %x %x %x] (%x, %x, %x) %c %c %c ExeInst: DASM(%x)\n"
    , io.cyc
    , wb_wire.rf_wen
    , wb.wbaddr
    , wb_wbdata
    , io.front.pc
    , exe.pc
    , mem.pc
    , Mux(stall(Stage.MEM), Str("F"),             //FREEZE-> F
      Mux(stall(Stage.DEC), Str("S"), Str(" ")))  //STALL->S
    , Mux(alu.ctrl.pc_sel === 1.U, Str("B"),    //BJ -> B
      Mux(alu.ctrl.pc_sel === 2.U, Str("J"),    //JR -> J
      Mux(alu.ctrl.pc_sel === 3.U, Str("E"),    //EX -> E
      Mux(alu.ctrl.pc_sel === 0.U, Str(" "), Str("?")))))
    , Mux(csr.io.illegal, Str("X"), Str(" "))
    , Mux(io.front.xcpt.valid || !exe_valid, BUBBLE, exe.inst)
  )

  if (conf.verbose) {
    when (io.cyc > 16427.U && io.cyc < 16475.U) {
      printf(p"Debug: target = ${Hexadecimal(io.front.feedBack.tgt)} dec_kill = ${io.front.kill}\n")
    }
  }
}
