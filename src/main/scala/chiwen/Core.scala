package chiwen
import chisel3._
import chisel3.util._
import common._
import chisel3.util.ListLookup
import common.Instructions._
import myCore._

object Jump {
  val none = 0
  val push = 1
  val pop  = 2
  val NUM  = pop + 1
}

class Core(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val imem = new AxiIO(conf.xprlen)
    val dmem = new MemPortIo(conf.xprlen)
    val cyc  = Output(UInt(conf.xprlen.W))
  })
  io := DontCare
  val inst = Module(new InstDecoder())
  val alu = Module(new ALU())
  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)
  val pred = Module(new Predictor())
  pred.io.cyc := io.cyc
  val ras  = Module(new RAS(nRAS))
  val fechi = Module(new FetchInst())
  fechi.io.cyc := io.cyc

  val if_reg_pc           = RegInit(START_ADDR)
  val dec_pc              = fechi.io.dec_pc
  val exe_reg_pc          = RegInit(START_ADDR)
  val mem_reg_pc          = RegInit(START_ADDR)
  val dec_inst            = fechi.io.inst
  val exe_reg_inst        = RegInit(BUBBLE)
  val mem_reg_inst        = RegInit(BUBBLE)
  val dec_inst_valid      = fechi.io.inst_valid
  val exe_reg_inst_valid  = RegInit(false.B)
  val mem_reg_inst_valid  = RegInit(false.B)

  // Instruction Fetch Stage
  val mem_xcpt_target     = Wire(UInt(conf.xprlen.W))
  val fencei              = Wire(Bool())
  val xcpt                = Wire(Bool())
  val dec_stall           = Wire(Bool())
  val mem_stall           = Wire(Bool())
  val if_kill             = Wire(Bool())
  val dec_kill            = Wire(Bool())
  val if_mispredict       = Wire(Bool())
  val dec_mispredict      = Wire(Bool())

  val dec_reg_predTp  = fechi.io.dec_pred.Tp
  val dec_reg_predTg  = fechi.io.dec_pred.Tg
  val dec_reg_predSel = fechi.io.dec_pred.Sel
  val dec_cfi_branch: Bool = inst.io.cinfo.cfi_branch
  val dec_cfi_jump: UInt   = inst.io.cinfo.cfi_jump

  val exe_reg_branch  = RegInit(false.B)
  val exe_reg_jump    = RegInit(0.U(Jump.NUM.W))
  val exe_reg_predTp  = RegInit(CFIType.invalid.U(CFIType.SZ.W))
  val exe_reg_predTg  = Reg(UInt(conf.xprlen.W))
  val exe_reg_predSel = Reg(UInt(log2Ceil(nEntries).W))

  when ((dec_stall && !mem_stall) || xcpt) {
     exe_reg_predTp     := CFIType.invalid.U(CFIType.SZ.W)
     exe_reg_branch     := false.B
     exe_reg_jump       := 0.U(Jump.NUM.W)
  }.elsewhen(!mem_stall) {
    when (dec_kill || !dec_inst_valid) {
       exe_reg_predTp     := CFIType.invalid.U(CFIType.SZ.W)
       exe_reg_branch     := false.B
       exe_reg_jump       := 0.U(Jump.NUM.W)
    }.otherwise {
      exe_reg_predTp        := dec_reg_predTp
      exe_reg_branch        := dec_cfi_branch
      exe_reg_jump          := dec_cfi_jump
      exe_reg_predTg        := Mux(if_mispredict, ras.io.peek, dec_reg_predTg)
      exe_reg_predSel       := dec_reg_predSel
    }
  }

  ras.io.pop        := exe_reg_jump(Jump.pop).toBool
  ras.io.push.valid := exe_reg_jump(Jump.push).toBool
  ras.io.push.bits  := alu.io.target.conti

  pred.io.pc                 := if_reg_pc
  pred.io.peekRAS            := ras.io.peek
  pred.io.feedBack.sel.valid := exe_reg_predTp =/= CFIType.invalid.U
  pred.io.feedBack.sel.bits  := exe_reg_predSel
  pred.io.feedBack.redirect  := alu.io.ctrl.pc_sel === PC_BRJMP || alu.io.ctrl.pc_sel === PC_JALR
  pred.io.feedBack.pc        := exe_reg_pc
  pred.io.feedBack.cfiType   := // include branch and jump type info
     Mux(exe_reg_branch,                                         CFIType.branch.U,
     Mux(exe_reg_jump(Jump.pop),                                 CFIType.retn.U,
     Mux(exe_reg_jump(Jump.none) || exe_reg_jump(Jump.push),     CFIType.jump.U,
                                                                 CFIType.invalid.U)))
  pred.io.feedBack.target    :=
     Mux(alu.io.ctrl.pc_sel === PC_BRJMP, alu.io.target.brjmp,
     Mux(alu.io.ctrl.pc_sel === PC_JALR,  alu.io.target.jpreg,
                                          alu.io.target.conti))

  val if_pc_next = Wire(UInt(conf.xprlen.W))
  if (conf.hasbrJPredictor) {
    val tg_mispredict = pred.io.feedBack.target  =/= exe_reg_predTg && pred.io.feedBack.cfiType =/= CFIType.invalid.U

    if_mispredict  := ((dec_cfi_jump(Jump.pop) && (ras.io.peek =/= dec_reg_predTg ||
                        dec_reg_predTp =/= CFIType.retn.U)) && dec_inst_valid) ||
                       tg_mispredict

    dec_mispredict :=  tg_mispredict

    if_pc_next :=
       Mux(xcpt,                    mem_xcpt_target,
       Mux(dec_mispredict,          pred.io.feedBack.target,
       Mux(if_mispredict,           ras.io.peek,
       /*Mux(alu.io.ctrl.pc_sel === PC_4,*/ pred.io.target.bits)))
  } else {
    if_mispredict  := alu.io.ctrl.pc_sel =/= PC_4
    dec_mispredict := alu.io.ctrl.pc_sel =/= PC_4
    val if_pc_plus: UInt = if_reg_pc + conf.pcInc.asUInt(conf.xprlen.W)
    if_pc_next :=
      Mux(xcpt,                             mem_xcpt_target,
      Mux(alu.io.ctrl.pc_sel === PC_BRJMP,  alu.io.target.brjmp,
      Mux(alu.io.ctrl.pc_sel === PC_JALR,   alu.io.target.jpreg,
      /*Mux(alu.io.ctrl.pc_sel === PC_4,*/  if_pc_plus)))
  }

  // for a fencei, refetch the if_pc (assuming no stall, no branch, and no exception)
  when (fencei && alu.io.ctrl.pc_sel === PC_4 && !dec_stall /*&& !mem_stall*/ && !xcpt) {
    if_pc_next := if_reg_pc
  }

  when (fechi.io.pc_forward) {
    if_reg_pc := if_pc_next
  }

  fechi.io.pc          := if_reg_pc
  fechi.io.if_pred.Tp  := pred.io.target.cifType
  fechi.io.if_pred.Tg  := pred.io.target.bits
  fechi.io.if_pred.Sel := pred.io.target.sel
  fechi.io.redirect    := if_kill || xcpt
  fechi.io.forward     := !dec_stall && !mem_stall
  fechi.io.mem         <> io.imem

  // Decode Stage ===========================================================================================================================================
  // ========================================================================================================================================================  
  inst.io.inst.bits  := dec_inst
  inst.io.inst.valid := dec_inst_valid

   // Bypass Muxes
  val mem_wbdata    = Wire(UInt(conf.xprlen.W))
  val wb_reg_wbdata = Reg(UInt(conf.xprlen.W))

  val dec_op1_data  = Wire(UInt(conf.xprlen.W))
  val dec_op2_data  = Wire(UInt(conf.xprlen.W))
  val dec_rs2_data  = Wire(UInt(conf.xprlen.W))

  val exe_reg_wbaddr = Reg(UInt(5.W))
  val mem_reg_wbaddr = Reg(UInt(5.W))
  val wb_reg_wbaddr  = Reg(UInt(5.W))
  val exe_reg_rf_wen = RegInit(false.B)
  val mem_reg_rf_wen = RegInit(false.B)
  val wb_reg_rf_wen  = RegInit(false.B)

  // Register File
  val regfile = Module(new RegisterFile())
  regfile.io.rs1_addr := inst.io.cinfo.rs1_addr
  regfile.io.rs2_addr := inst.io.cinfo.rs2_addr
  val rf_rs1_data: UInt = regfile.io.rs1_data
  val rf_rs2_data: UInt = regfile.io.rs2_data
  regfile.io.waddr := wb_reg_wbaddr
  regfile.io.wdata := wb_reg_wbdata
  regfile.io.wen   := wb_reg_rf_wen

  // roll the OP1 mux into the bypass mux logic
  dec_op1_data := MuxCase(rf_rs1_data, Array(
                       (inst.io.cinfo.op1_sel === OP1_IMZ)-> inst.io.dinfo.imm_z,
                       (inst.io.cinfo.op1_sel === OP1_PC) -> dec_pc,
                       ((exe_reg_wbaddr === inst.io.cinfo.rs1_addr) && (inst.io.cinfo.rs1_addr =/= 0.U) && exe_reg_rf_wen) -> alu.io.alu_result,
                       ((mem_reg_wbaddr === inst.io.cinfo.rs1_addr) && (inst.io.cinfo.rs1_addr =/= 0.U) && mem_reg_rf_wen) -> mem_wbdata,
                       ((wb_reg_wbaddr  === inst.io.cinfo.rs1_addr) && (inst.io.cinfo.rs1_addr =/= 0.U) &&  wb_reg_rf_wen) -> wb_reg_wbdata))

  dec_rs2_data := MuxCase(rf_rs2_data, Array(
                       ((exe_reg_wbaddr === inst.io.cinfo.rs2_addr) && (inst.io.cinfo.rs2_addr =/= 0.U) && exe_reg_rf_wen) -> alu.io.alu_result,
                       ((mem_reg_wbaddr === inst.io.cinfo.rs2_addr) && (inst.io.cinfo.rs2_addr =/= 0.U) && mem_reg_rf_wen) -> mem_wbdata,
                       ((wb_reg_wbaddr  === inst.io.cinfo.rs2_addr) && (inst.io.cinfo.rs2_addr =/= 0.U) &&  wb_reg_rf_wen) -> wb_reg_wbdata))

  dec_op2_data := MuxCase(dec_rs2_data, Array(
                       (inst.io.cinfo.op2_sel === OP2_ITYPE)  -> inst.io.dinfo.imm_i,
                       (inst.io.cinfo.op2_sel === OP2_STYPE)  -> inst.io.dinfo.imm_s,
                       (inst.io.cinfo.op2_sel === OP2_SBTYPE) -> inst.io.dinfo.imm_sb,
                       (inst.io.cinfo.op2_sel === OP2_UTYPE)  -> inst.io.dinfo.imm_u,
                       (inst.io.cinfo.op2_sel === OP2_UJTYPE) -> inst.io.dinfo.imm_uj))

  val exe_reg_mem_en   = RegInit(false.B)
  val exe_reg_csr_cmd  = RegInit(CSR.N)
  val exe_reg_is_csr   = RegInit(false.B)
  val exe_reg_illegal  = RegInit(false.B)
  val exe_reg_br_type  = RegInit(BR_N)
  val exe_reg_op1_data = Reg(UInt(conf.xprlen.W))
  val exe_reg_op2_data = Reg(UInt(conf.xprlen.W))
  val exe_reg_rs2_data = Reg(UInt(conf.xprlen.W))
  val exe_reg_alu_fun  = Reg(UInt())
  val exe_reg_wb_sel   = Reg(UInt())
  val exe_reg_mem_fcn  = Reg(UInt())
  val exe_reg_mem_typ  = Reg(UInt())
  // we need to stall IF while fencei goes through DEC and EXE, as there may
  // be a store we need to wait to clear in MEM.
  val exe_reg_fencei = RegInit(false.B)
  // critcal signals
  def flush_dec_exe = {
    exe_reg_rf_wen   := false.B
    exe_reg_mem_en   := false.B
    exe_reg_csr_cmd  := CSR.N
    exe_reg_is_csr   := false.B
    exe_reg_illegal  := false.B
    exe_reg_br_type  := BR_N
    exe_reg_inst_valid := false.B
    exe_reg_inst     := BUBBLE
    exe_reg_fencei   := false.B
  }

  def forward_dec_exe = {
    exe_reg_rf_wen   := inst.io.cinfo.rf_wen // FIXME: inorder to debug
    exe_reg_mem_en   := inst.io.cinfo.mem_en
    // convert CSR instructions with raddr1 == 0 to read-only CSR commands
    exe_reg_csr_cmd  := Mux((inst.io.cinfo.csr_cmd === CSR.S || inst.io.cinfo.csr_cmd === CSR.C) && inst.io.cinfo.rs1_addr === 0.U, CSR.R, inst.io.cinfo.csr_cmd)
    exe_reg_is_csr   := inst.io.cinfo.csr_cmd =/= CSR.N && inst.io.cinfo.csr_cmd =/= CSR.I
    exe_reg_illegal  := inst.io.cinfo.illegal
    exe_reg_br_type  := inst.io.cinfo.br_type
    exe_reg_inst_valid := true.B
    exe_reg_inst     := dec_inst
    exe_reg_fencei   := inst.io.cinfo.fencei
  }

  when ((dec_stall && !mem_stall) || xcpt) {
    // (kill exe stage) insert NOP (bubble) into Execute stage on front-end stall (e.g., hazard clearing)
    flush_dec_exe
  }.elsewhen(!mem_stall) {
    when (dec_kill || !dec_inst_valid) { flush_dec_exe }
   .otherwise { forward_dec_exe }
     // data signals
    exe_reg_pc            := dec_pc
    exe_reg_op1_data      := dec_op1_data
    exe_reg_op2_data      := dec_op2_data
    exe_reg_rs2_data      := dec_rs2_data
    // exe_reg_op2_sel       := dec_op2_sel
    exe_reg_alu_fun       := inst.io.cinfo.alu_fun
    // exe_reg_rf_wen
    exe_reg_wb_sel        := inst.io.cinfo.wb_sel
    exe_reg_wbaddr        := inst.io.cinfo.wbaddr
    // exe_reg_mem_en
    exe_reg_mem_fcn       := inst.io.cinfo.mem_fcn
    exe_reg_mem_typ       := inst.io.cinfo.mem_typ
    // exe_reg_csr_cmd
  }

  fencei := inst.io.cinfo.fencei || exe_reg_fencei
  // Execute Stage ==========================================================================================================================================
  //=========================================================================================================================================================
  alu.io.alu_op1  := exe_reg_op1_data
  alu.io.alu_op2  := exe_reg_op2_data
  alu.io.rs2_data := exe_reg_rs2_data
  alu.io.pc       := exe_reg_pc
  alu.io.ctrl.fun := exe_reg_alu_fun
  alu.io.ctrl.br_type := exe_reg_br_type
  alu.io.ctrl.wb_sel  := exe_reg_wb_sel

  val mem_reg_mem_en   = RegInit(false.B)
  val mem_reg_csr_cmd  = RegInit(CSR.N)
  val mem_reg_illegal  = RegInit(false.B)
  val mem_reg_jpnpc    = RegInit(0.U(conf.xprlen.W))
  val mem_reg_alu_out  = Reg(UInt(conf.xprlen.W))
  val mem_reg_rs2_data = Reg(UInt(conf.xprlen.W))
  val mem_reg_wb_sel   = Reg(UInt())
  val mem_reg_mem_fcn  = Reg(UInt())
  val mem_reg_mem_typ  = Reg(UInt())
  when (xcpt) {
    mem_reg_rf_wen        := false.B
    mem_reg_mem_en        := false.B
    mem_reg_csr_cmd       := CSR.N
    mem_reg_illegal       := false.B
    mem_reg_jpnpc         := 0.U
    mem_reg_inst_valid    := false.B
  } .elsewhen (!mem_stall) {
    mem_reg_rf_wen        := exe_reg_rf_wen
    mem_reg_mem_en        := exe_reg_mem_en
    mem_reg_csr_cmd       := exe_reg_csr_cmd
    mem_reg_illegal       := exe_reg_illegal
    mem_reg_jpnpc         := (Fill(conf.xprlen, alu.io.ctrl.pc_sel === PC_BRJMP) & alu.io.target.brjmp) |
                             (Fill(conf.xprlen, alu.io.ctrl.pc_sel === PC_JALR)  & alu.io.target.jpreg)

    mem_reg_inst_valid    := exe_reg_inst_valid

    mem_reg_pc            := exe_reg_pc
    mem_reg_inst          := exe_reg_inst
    mem_reg_alu_out       := alu.io.alu_result
    mem_reg_wb_sel        := exe_reg_wb_sel
    mem_reg_wbaddr        := exe_reg_wbaddr

    mem_reg_rs2_data      := exe_reg_rs2_data
    mem_reg_mem_fcn       := exe_reg_mem_fcn
    mem_reg_mem_typ       := exe_reg_mem_typ
  }

  // Memory Stage ============================================================================================================================================
  //==========================================================================================================================================================

  // Control Status Registers
  csr.io := DontCare
  csr.io.rw.addr  := mem_reg_inst(CSR_ADDR_MSB,CSR_ADDR_LSB)
  csr.io.rw.wdata := mem_reg_alu_out
  csr.io.rw.cmd   := mem_reg_csr_cmd
  csr.io.retire   := RegNext(mem_reg_inst_valid)
  csr.io.pc       := mem_reg_pc
  // Add your own uarch counters here!
  csr.io.counters.foreach(_.inc := false.B)

  val ls_addr_ma_valid = MuxLookup(mem_reg_mem_typ(1,0) ,false.B, Array( 2.U -> mem_reg_alu_out(0), 3.U -> mem_reg_alu_out(1,0).orR ))
  val ma_jump: Bool          = mem_reg_jpnpc(1,0).orR
  val ma_load: Bool          = mem_reg_mem_en && mem_reg_mem_fcn === M_XRD && ls_addr_ma_valid
  val ma_store: Bool         = mem_reg_mem_en && mem_reg_mem_fcn === M_XWR && ls_addr_ma_valid
  csr.io.xcpt  := ma_load || ma_store || ma_jump || mem_reg_illegal
  csr.io.cause := MuxCase(0.U, Array(
                   ma_jump            -> Causes.misaligned_fetch.U,
                   mem_reg_illegal    -> Causes.illegal_instruction.U,
                   ma_load            -> Causes.misaligned_load.U,
                   ma_store           -> Causes.misaligned_store.U ))
  csr.io.tval  := MuxCase(0.U, Array(
                   ma_jump         -> mem_reg_jpnpc,
                   mem_reg_illegal -> mem_reg_inst,
                   ma_load         -> mem_reg_alu_out,
                   ma_store        -> mem_reg_alu_out ))

  xcpt := ma_jump  || ma_load || ma_store || mem_reg_illegal || csr.io.eret
  mem_xcpt_target := csr.io.evec

  // datapath to data memory outputs =============================
  io.dmem.req.valid     := mem_reg_mem_en && !ma_store && !ma_load
  io.dmem.req.bits.addr := mem_reg_alu_out.asUInt
  io.dmem.req.bits.fcn  := mem_reg_mem_fcn
  io.dmem.req.bits.typ  := mem_reg_mem_typ
  io.dmem.req.bits.data := mem_reg_rs2_data
  //===============================================================

  // WB Mux
  mem_wbdata := MuxCase(mem_reg_alu_out, Array( // default is wb_alu and wb_pc4
                // (mem_reg_wb_sel === WB_ALU) -> mem_reg_alu_out,
                // (mem_reg_wb_sel === WB_PC4) -> mem_reg_alu_out,
                (mem_reg_wb_sel === WB_MEM) -> io.dmem.resp.bits.data,
                (mem_reg_wb_sel === WB_CSR) -> csr.io.rw.rdata ))

  // Writeback Stage ===========================================================================================================================================
  //============================================================================================================================================================
  when (!mem_stall) {
    wb_reg_wbaddr        := mem_reg_wbaddr
    wb_reg_wbdata        := mem_wbdata
    wb_reg_rf_wen        := Mux(xcpt, false.B, mem_reg_rf_wen)
  } .otherwise {
    wb_reg_rf_wen        := false.B
  }


  //control pipeline signals
  val exe_inst_is_load: Bool = exe_reg_mem_en && (exe_reg_mem_fcn === M_XRD)

  dec_stall := ((exe_inst_is_load && exe_reg_wbaddr === inst.io.cinfo.rs1_addr && exe_reg_wbaddr =/= 0.U && inst.io.cinfo.rs1_oen) ||
                (exe_inst_is_load && exe_reg_wbaddr === inst.io.cinfo.rs2_addr && exe_reg_wbaddr =/= 0.U && inst.io.cinfo.rs2_oen)) &&
                dec_inst_valid || exe_reg_is_csr

  // stall(mem_stall) full pipeline on no response from memory
  mem_stall := mem_reg_mem_en && !io.dmem.resp.valid

  if_kill  := if_mispredict || fencei
  dec_kill := dec_mispredict

  // Printout
  printf("Core: Cyc= %d WB[ %x %x: 0x%x] (0x%x, 0x%x, 0x%x, 0x%x, 0x%x) %c %c %c ExeInst: DASM(%x)\n"
    , io.cyc
    , wb_reg_rf_wen
    , wb_reg_wbaddr
    , wb_reg_wbdata
    , if_reg_pc
    , dec_pc
    , exe_reg_pc
    , mem_reg_pc
    , RegNext(mem_reg_pc)
    , Mux(mem_stall, Str("F"),             //FREEZE-> F 
      Mux(dec_stall, Str("S"), Str(" ")))  //STALL->S
    , Mux(alu.io.ctrl.pc_sel === 1.U, Str("B"),    //BJ -> B
      Mux(alu.io.ctrl.pc_sel === 2.U, Str("J"),    //JR -> J
      Mux(alu.io.ctrl.pc_sel === 3.U, Str("E"),    //EX -> E
      Mux(alu.io.ctrl.pc_sel === 0.U, Str(" "), Str("?")))))
    , Mux(csr.io.illegal, Str("X"), Str(" "))
    , Mux(xcpt, BUBBLE, exe_reg_inst)
    )
}