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
  val btbTp   = UInt(CFIType.SZ.W)
}

class DecCrtl extends Bundle {
  val jump    = UInt(Jump.NUM.W)
  val branch  = Bool()
  val btbTp   = UInt(CFIType.SZ.W)
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
  val xcpt      = Wire(Valid(UInt(conf.xprlen.W)))
  val stall     = Wire(Vec(Stage.Num, Bool()))
  val dec       = Module(new InstDecoder())
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

  dec.io.inst      := io.front.inst.bits
  dec_valid        := io.front.inst.valid
  dec_wire.btbTp   := Mux(dec_valid, io.front.pred.Tp, CFIType.invalid.U)
  dec_wire.jump    := Mux(dec_valid, dec.io.cinfo.cfi_jump, 0.U)
  dec_wire.branch  := dec_valid && dec.io.cinfo.cfi_branch
  dec_wire.rs1_oen := dec_valid && dec.io.cinfo.rs1_oen
  dec_wire.rs2_oen := dec_valid && dec.io.cinfo.rs2_oen

  // Bypass Muxes
  val dec_op1_data  = Wire(UInt(conf.xprlen.W))
  val dec_op2_data  = Wire(UInt(conf.xprlen.W))
  val dec_rs1_data  = Wire(UInt(conf.xprlen.W))
  val dec_rs2_data  = Wire(UInt(conf.xprlen.W))

  // Register File
  val regfile = Module(new Regfile())
  val rf_rs1_data = Wire(UInt())
  val rf_rs2_data = Wire(UInt())
  regfile.io.rs1_addr := dec.io.cinfo.rs1_addr
  regfile.io.rs2_addr := dec.io.cinfo.rs2_addr
  rf_rs1_data := regfile.io.rs1_data
  rf_rs2_data := regfile.io.rs2_data
  // roll the OP1 mux into the bypass mux logic
  dec_rs1_data := MuxCase(rf_rs1_data, Array(
    ((exe.wbaddr === dec.io.cinfo.rs1_addr) && exe_wire.rf_wen) -> exe_wbdata,
    ((mem.wbaddr === dec.io.cinfo.rs1_addr) && mem_wire.rf_wen) -> mem_wbdata,
    (( wb.wbaddr === dec.io.cinfo.rs1_addr) &&  wb_wire.rf_wen) -> wb_wbdata
  ))

  dec_rs2_data := MuxCase(rf_rs2_data, Array(
    ((exe.wbaddr === dec.io.cinfo.rs2_addr) && exe_wire.rf_wen) -> exe_wbdata,
    ((mem.wbaddr === dec.io.cinfo.rs2_addr) && mem_wire.rf_wen) -> mem_wbdata,
    (( wb.wbaddr === dec.io.cinfo.rs2_addr) &&  wb_wire.rf_wen) -> wb_wbdata
  ))

  dec_op1_data := MuxCase(dec_rs1_data, Array(
    (dec.io.cinfo.op1_sel === OP1_IMZ)-> dec.io.dinfo.imm_z,
    (dec.io.cinfo.op1_sel === OP1_PC) -> io.front.pc))

  dec_op2_data := MuxCase(dec_rs2_data, Array(
    (dec.io.cinfo.op2_sel === OP2_ITYPE)  -> dec.io.dinfo.imm_i,
    (dec.io.cinfo.op2_sel === OP2_STYPE)  -> dec.io.dinfo.imm_s,
    (dec.io.cinfo.op2_sel === OP2_SBTYPE) -> dec.io.dinfo.imm_sb,
    (dec.io.cinfo.op2_sel === OP2_UTYPE)  -> dec.io.dinfo.imm_u,
    (dec.io.cinfo.op2_sel === OP2_UJTYPE) -> dec.io.dinfo.imm_uj))

  val if_mispredict = Wire(Bool())
  if (!conf.hasBTB) if_mispredict := false.B
  else if_mispredict := dec_wire.jump(Jump.pop) && (io.front.rasIO.peek =/= io.front.pred.Tg || dec_wire.btbTp =/= CFIType.retn.U)

  when ((stall(Stage.DEC) && !stall(Stage.EXE) && !stall(Stage.MEM)) || xcpt.valid) {
    // (kill exe stage) insert NOP (bubble) into Execute stage on front-end stall (e.g., hazard clearing)
    exe_valid := false.B
  }.elsewhen(!stall(Stage.EXE) && !stall(Stage.MEM)) {
    exe_valid    := dec_valid
    exe.rf_wen   := dec.io.cinfo.rf_wen
    exe.mem_en   := dec.io.cinfo.mem_en
    // convert CSR instructions with raddr1 == 0 to read-only CSR commands
    exe.csr_cmd  := Mux((dec.io.cinfo.csr_cmd === CSR.S || dec.io.cinfo.csr_cmd === CSR.C) &&
      dec.io.cinfo.rs1_addr === 0.U, CSR.R, dec.io.cinfo.csr_cmd)
    exe.illegal  := dec.io.cinfo.illegal
    exe.br_type  := dec.io.cinfo.br_type
    exe.pc       := io.front.pc
    exe.op1_data := dec_op1_data
    exe.op2_data := dec_op2_data
    exe.rs2_data := dec_rs2_data
    exe.inst     := dec.io.inst
    exe.alu_fun  := dec.io.cinfo.alu_fun
    exe.wb_sel   := dec.io.cinfo.wb_sel
    exe.wbaddr   := dec.io.cinfo.wbaddr
    exe.mem_fcn  := dec.io.cinfo.mem_fcn
    exe.mem_typ  := dec.io.cinfo.mem_typ
    exe.branch   := dec.io.cinfo.cfi_branch
    exe.jump     := dec.io.cinfo.cfi_jump
    exe.btb.Tp   := io.front.pred.Tp
    exe.btb.Tg   := Mux(if_mispredict, io.front.rasIO.peek, io.front.pred.Tg)
    exe.btb.Sel  := io.front.pred.Sel
  }
  // Execute Stage ==========================================================================================================================================
  //=========================================================================================================================================================
  exe_wire.rf_wen  := exe.rf_wen && exe_valid && exe.wbaddr =/= 0.U
  exe_wire.mem_en  := exe.mem_en && exe_valid
  exe_wire.csr_cmd := Mux(exe_valid, exe.csr_cmd, CSR.N)
  exe_wire.br_type := Mux(exe_valid, exe.br_type, BR_N)
  exe_wire.branch  := exe_valid && exe.branch
  exe_wire.jump    := Mux(exe_valid, exe.jump, 0.U)
  exe_wire.btbTp   := Mux(exe_valid, exe.btb.Tp, CFIType.invalid.U)
  exe_wire.illegal := exe.illegal && exe_valid

  val alu = Module(new ALU())
  alu.io.alu_op1      := exe.op1_data
  alu.io.alu_op2      := exe.op2_data
  alu.io.rs2_data     := exe.rs2_data
  alu.io.pc           := exe.pc
  alu.io.ctrl.fun     := exe.alu_fun
  alu.io.ctrl.br_type := exe_wire.br_type
  alu.io.ctrl.wb_sel  := exe.wb_sel

  exe_wbdata := alu.io.alu_result

  io.front.rasIO.pop := exe_wire.jump(Jump.pop).toBool
  io.front.rasIO.push.valid := exe_wire.jump(Jump.push).toBool
  io.front.rasIO.push.bits  := alu.io.target.conti

  io.front.feedBack.sel.valid := exe_wire.btbTp =/= CFIType.invalid.U
  io.front.feedBack.sel.bits  := exe.btb.Sel
  io.front.feedBack.redirect  := alu.io.ctrl.pc_sel === PC_BRJMP || alu.io.ctrl.pc_sel === PC_JALR
  io.front.feedBack.pc        := exe.pc
  val fb_cfiType =
    Mux(exe_wire.branch,                                      CFIType.branch.U,
    Mux(exe_wire.jump(Jump.pop),                              CFIType.retn.U,
    Mux(exe_wire.jump(Jump.none) || exe_wire.jump(Jump.push), CFIType.jump.U,
                                                              CFIType.invalid.U
    )))
  io.front.feedBack.cfiType := fb_cfiType
  val fb_tg =
    Mux(alu.io.ctrl.pc_sel === PC_BRJMP, alu.io.target.brjmp,
    Mux(alu.io.ctrl.pc_sel === PC_JALR,  alu.io.target.jpreg,
                                         alu.io.target.conti
    ))
  io.front.feedBack.target := fb_tg

  val dec_mispredict = Wire(Bool())
  if (conf.hasBTB) dec_mispredict := fb_tg =/= exe.btb.Tg && exe_valid
  else dec_mispredict := alu.io.ctrl.pc_sel =/= PC_4

  val mem_reg_exe_out = Reg(UInt(conf.xprlen.W))
  val mem_reg_jpnpc   = RegInit(0.U(conf.xprlen.W))
  when (!stall(Stage.EXE) && stall(Stage.MEM) || xcpt.valid) {
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
    mem_reg_jpnpc:= (Fill(conf.xprlen, alu.io.ctrl.pc_sel === PC_BRJMP) & alu.io.target.brjmp) |
                    (Fill(conf.xprlen, alu.io.ctrl.pc_sel === PC_JALR)  & alu.io.target.jpreg)
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
    mem_wire.illegal -> Causes.illegal_instruction.U,
    ma_load    -> Causes.misaligned_load.U,
    ma_store   -> Causes.misaligned_store.U
  ))
  csr.io.tval  := MuxCase(0.U, Array(
    ma_jump    -> mem_reg_jpnpc,
    mem_wire.illegal -> mem.inst,
    ma_load    -> mem_reg_exe_out,
    ma_store   -> mem_reg_exe_out
  ))
  xcpt.valid := ma_jump || ma_load || ma_store || mem_wire.illegal || csr.io.eret
  xcpt.bits  := csr.io.evec

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

  when (stall(Stage.MEM) || xcpt.valid) {
    wb_valid := false.B
  } .otherwise {
    wb_valid := mem_valid
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
    (exe_load_inst && exe.wbaddr === dec.io.cinfo.rs1_addr && dec.io.cinfo.rs1_addr =/= 0.U && dec_wire.rs1_oen) ||
    (exe_load_inst && exe.wbaddr === dec.io.cinfo.rs2_addr && dec.io.cinfo.rs1_addr =/= 0.U && dec_wire.rs2_oen) ||
    exe_wire.csr_cmd =/= CSR.N

  stall(Stage.EXE) := false.B
  // stall(mem_stall) full pipeline on no response from memory
  stall(Stage.MEM) := mem_wire.mem_en && !io.mem.resp.valid

  io.front.dec_kill := dec_mispredict
  io.front.if_kill  := if_mispredict || io.front.dec_kill
  io.front.xcpt     := xcpt
  io.front.forward  := !stall.asUInt.orR

  // Printout
  printf("Core: Cyc= %d WB[ %x %x: 0x%x] (0x%x, 0x%x, 0x%x, 0x%x) %c %c %c ExeInst: DASM(%x)\n"
    , io.cyc
    , wb_wire.rf_wen
    , wb.wbaddr
    , wb_wbdata
    , io.front.pc
    , exe.pc
    , mem.pc
    , RegNext(mem.pc)
    , Mux(stall(Stage.MEM), Str("F"),             //FREEZE-> F
      Mux(stall(Stage.DEC), Str("S"), Str(" ")))  //STALL->S
    , Mux(alu.io.ctrl.pc_sel === 1.U, Str("B"),    //BJ -> B
      Mux(alu.io.ctrl.pc_sel === 2.U, Str("J"),    //JR -> J
      Mux(alu.io.ctrl.pc_sel === 3.U, Str("E"),    //EX -> E
      Mux(alu.io.ctrl.pc_sel === 0.U, Str(" "), Str("?")))))
    , Mux(csr.io.illegal, Str("X"), Str(" "))
    , Mux(xcpt.valid || !exe_valid, BUBBLE, exe.inst)
  )
  if (conf.verbose) {
    when (io.cyc > 16427.U && io.cyc < 16475.U) {
      printf(p"Debug: target = ${Hexadecimal(io.front.feedBack.target)} dec_kill = ${io.front.dec_kill}\n")
    }
  }
}
