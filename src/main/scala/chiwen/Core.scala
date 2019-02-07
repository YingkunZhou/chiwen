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
  val if_reg_pc           = RegInit(START_ADDR)
  val dec_reg_pc          = Wire(UInt(conf.xprlen.W))
  val exe_reg_pc          = RegInit(START_ADDR)
  val mem_reg_pc          = RegInit(START_ADDR)
  val dec_reg_inst        = Wire(UInt(conf.xprlen.W))
  val exe_reg_inst        = RegInit(BUBBLE)
  val mem_reg_inst        = RegInit(BUBBLE)
  val dec_reg_inst_valid  = Wire(Bool())
  val exe_reg_inst_valid  = RegInit(false.B)
  val mem_reg_inst_valid  = RegInit(false.B)

  // Instruction Fetch Stage
  val if_pc_next          = Wire(UInt(conf.xprlen.W))
  val exe_brjmp_target    = Wire(UInt(conf.xprlen.W))
  val exe_jpreg_target    = Wire(UInt(conf.xprlen.W))
  val mem_xcpt_target     = Wire(UInt(conf.xprlen.W))
  val fencei              = Wire(Bool())
  val xcpt                = Wire(Bool())
  val dec_stall           = Wire(Bool())
  val mem_stall           = Wire(Bool())
  val if_kill             = Wire(Bool())
  val dec_kill            = Wire(Bool())
  val exe_pc_sel          = Wire(UInt(2.W)) // include branch and jump type taken info
  val if_mispredict       = Wire(Bool())
  val dec_mispredict      = Wire(Bool())

  val dec_rs1_addr = dec_reg_inst(RS1_MSB, RS1_LSB)
  val dec_rs2_addr = dec_reg_inst(RS2_MSB, RS2_LSB)
  val dec_wbaddr   = dec_reg_inst(RD_MSB , RD_LSB)

  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)
  val pred = Module(new Predictor())
  val ras  = Module(new RAS(nRAS))
  pred.io.cyc := io.cyc
  pred.io.pc        := if_reg_pc
  pred.io.peekRAS   := ras.io.peek
  val dec_reg_predTp  = Wire(UInt(CFIType.SZ.W))
  val dec_reg_predTg  = Wire(UInt(conf.xprlen.W))
  val dec_reg_predSel = Wire(UInt(log2Ceil(nEntries).W))

  val func = dec_reg_inst(6,0)
  val dec_cfi_branch: Bool = func === "b1100011".U
  def link(addr: UInt): Bool = addr === 1.U || addr === 5.U
  /*
  * A JAL instruction should push the return address onto
  * a return-address stack (RAS) only when rd=x1/x5
  * JALR instructions should push/pop a RAS as shown in the Table:
  *   rd    |   rs1    |    rs1 = rd    |   RAS action
  * !link   |  !link   |        -       |   none
  * !link   |   link   |        -       |   pop
  *  link   |  !link   |        -       |   push
  *  link   |   link   |        0       |   push and pop
  *  link   |   link   |        1       |   push
  */
  val cfiType_jal: Bool  = func === "b1101111".U
  val cfiType_jalr: Bool = func === "b1100111".U
  val cifType_jump       = Wire(Vec(Jump.NUM, Bool()))
  val pop_push: Bool     =  cfiType_jalr &&  link(dec_wbaddr) && link(dec_rs1_addr)  && dec_wbaddr =/= dec_rs1_addr
  cifType_jump(Jump.none) := (cfiType_jalr && !link(dec_wbaddr) && !link(dec_rs1_addr))  || //case 1
                             (cfiType_jal  && !link(dec_wbaddr))    // case 2

  cifType_jump(Jump.push) := (cfiType_jal  &&  link(dec_wbaddr))||  // case 1
                             (cfiType_jalr &&  link(dec_wbaddr) && !link(dec_rs1_addr)) || // case 2
                             (cfiType_jalr &&  link(dec_wbaddr) &&  link(dec_rs1_addr)  && dec_wbaddr === dec_rs1_addr) || //case 3
                              pop_push // case 4

  cifType_jump(Jump.pop)  :=  cfiType_jalr && !link(dec_wbaddr) &&  link(dec_rs1_addr)  || //case 1
                              pop_push // case 2

  val dec_cfi_jump: UInt = cifType_jump.asUInt

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
    when (dec_kill || !dec_reg_inst_valid) {
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

  val exe_pc_plus4: UInt     = (exe_reg_pc + 4.U)(conf.xprlen-1,0) // FIXME: can forward if_pc_plus4 for time saver

  ras.io.pop        := exe_reg_jump(Jump.pop).toBool
  ras.io.push.valid := exe_reg_jump(Jump.push).toBool
  ras.io.push.bits  := exe_pc_plus4

  pred.io.feedBack.sel.valid := exe_reg_predTp =/= CFIType.invalid.U
  pred.io.feedBack.sel.bits  := exe_reg_predSel
  pred.io.feedBack.redirect  := exe_pc_sel === PC_BRJMP || exe_pc_sel === PC_JALR
  pred.io.feedBack.pc        := exe_reg_pc
  pred.io.feedBack.cfiType   := // include branch and jump type info
     Mux(exe_reg_branch,                                         CFIType.branch.U,
     Mux(exe_reg_jump(Jump.pop),                                 CFIType.retn.U,
     Mux(exe_reg_jump(Jump.none) || exe_reg_jump(Jump.push),     CFIType.jump.U,
                                                                 CFIType.invalid.U)))
  pred.io.feedBack.target    :=
     Mux(exe_pc_sel === PC_BRJMP, exe_brjmp_target,
     Mux(exe_pc_sel === PC_JALR,  exe_jpreg_target,
                                  exe_pc_plus4))
  if (conf.hasbrJPredictor) {
    val tg_mispredict = pred.io.feedBack.target  =/= exe_reg_predTg && pred.io.feedBack.cfiType =/= CFIType.invalid.U

    if_mispredict  := ((dec_cfi_jump(Jump.pop) && (ras.io.peek =/= dec_reg_predTg ||
                        dec_reg_predTp =/= CFIType.retn.U)) && dec_reg_inst_valid) ||
                       tg_mispredict

    dec_mispredict :=  tg_mispredict

    if_pc_next :=
       Mux(exe_pc_sel === PC_EXC,   mem_xcpt_target,
       Mux(dec_mispredict,          pred.io.feedBack.target,
       Mux(if_mispredict,           ras.io.peek,
       /*Mux(exe_pc_sel === PC_4,*/ pred.io.target.bits)))
//    when (io.cyc === 45280.U || io.cyc === 45300.U || io.cyc === 45298.U) {
//      printf("target_type: %x, target_addr: %x, target_sel: %d, sel_valid: %x, sel_bits: %d, redirect: %x, pc: %x, cfiType: %x, target: %x\n"
//      , pred.io.target.cifType
//      , pred.io.target.bits
//      , pred.io.target.sel
//      , pred.io.feedBack.sel.valid
//      , pred.io.feedBack.sel.bits
//      , pred.io.feedBack.redirect
//      , pred.io.feedBack.pc
//      , pred.io.feedBack.cfiType
//      , pred.io.feedBack.target
//      )
//    }
  } else {
    if_mispredict  := exe_pc_sel =/= PC_4
    dec_mispredict := exe_pc_sel =/= PC_4
    val if_pc_plus4: UInt = if_reg_pc + 4.asUInt(conf.xprlen.W)
    if_pc_next :=
      Mux(exe_pc_sel === PC_EXC,    mem_xcpt_target,
      Mux(exe_pc_sel === PC_BRJMP,  exe_brjmp_target,
      Mux(exe_pc_sel === PC_JALR,   exe_jpreg_target,
      /*Mux(exe_pc_sel === PC_4,*/  if_pc_plus4)))
  }


  // for a fencei, refetch the if_pc (assuming no stall, no branch, and no exception)
  when (fencei && exe_pc_sel === PC_4 && !dec_stall /*&& !mem_stall*/ && !xcpt) {
    if_pc_next := if_reg_pc
  }

  val fechi = Module(new FetchInst())
  fechi.io.cyc := io.cyc
  fechi.io.pc          := if_reg_pc
  fechi.io.if_pred.Tp  := pred.io.target.cifType
  fechi.io.if_pred.Tg  := pred.io.target.bits
  fechi.io.if_pred.Sel := pred.io.target.sel
  fechi.io.redirect    := if_kill || xcpt
  fechi.io.forward     := !dec_stall && !mem_stall
  dec_reg_inst_valid   := fechi.io.inst_valid
  dec_reg_inst         := fechi.io.inst
  dec_reg_pc           := fechi.io.dec_pc
  dec_reg_predTp       := fechi.io.dec_pred.Tp
  dec_reg_predTg       := fechi.io.dec_pred.Tg
  dec_reg_predSel      := fechi.io.dec_pred.Sel
  fechi.io.mem         <> io.imem

  when (fechi.io.pc_forward) {
    if_reg_pc := if_pc_next
  }

  // Decode Stage ===========================================================================================================================================
  // ========================================================================================================================================================  

  val dec_signals =
    ListLookup(dec_reg_inst,
                     List(N, BR_N  , OP1_X , OP2_X    , OEN_0, OEN_0, ALU_X   , WB_X  ,  REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
      Array(       /* val  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | fence.i */
        /* inst | type |   sel |    sel    |  oen |  oen |   fcn   |  sel  | wen  |  en  |  wr  | type | cmd |         */
        LW     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, CSR.N, N),
        LB     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_B, CSR.N, N),
        LBU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_BU,CSR.N, N),
        LH     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_H, CSR.N, N),
        LHU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_HU,CSR.N, N),
        SW     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_W, CSR.N, N),
        SB     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_B, CSR.N, N),
        SH     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_H, CSR.N, N),

        AUIPC  -> List(Y, BR_N  , OP1_PC , OP2_UTYPE , OEN_0, OEN_0, ALU_ADD   ,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N),
        LUI    -> List(Y, BR_N  , OP1_X  , OP2_UTYPE , OEN_0, OEN_0, ALU_COPY_2,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N),

        ADDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        ANDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        ORI    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        XORI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SLTI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SLTIU  -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SLLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SRAI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SRLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),

        SLL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        ADD    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SUB    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SUB , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SLT    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SLTU   -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        AND    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        OR     -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        XOR    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SRA    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        SRL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),

        JAL    -> List(Y, BR_J  , OP1_RS1, OP2_UJTYPE, OEN_0, OEN_0, ALU_X   , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        JALR   -> List(Y, BR_JR , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
        BEQ    -> List(Y, BR_EQ , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
        BNE    -> List(Y, BR_NE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
        BGE    -> List(Y, BR_GE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
        BGEU   -> List(Y, BR_GEU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
        BLT    -> List(Y, BR_LT , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
        BLTU   -> List(Y, BR_LTU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),

        CSRRWI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N),
        CSRRSI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N),
        CSRRW  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N),
        CSRRS  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N),
        CSRRC  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N),
        CSRRCI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N),

        ECALL  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
        MRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
        DRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
        EBREAK -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
        WFI    -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N), // implemented as a NOP

        FENCE_I-> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, Y),
        // kill pipeline and refetch instructions since the pipeline will be holding stall instructions.
        FENCE  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_1, M_X  , MT_X, CSR.N, N)
        // we are already sequentially consistent, so no need to honor the fence instruction
      ))

  // Put these control signals in variables
  val (dec_val_inst: Bool) :: dec_br_type :: dec_op1_sel :: dec_op2_sel :: (dec_rs1_oen: Bool) :: (dec_rs2_oen: Bool) :: dec0 = dec_signals
  val dec_alu_fun :: dec_wb_sel :: (dec_rf_wen: Bool) :: (dec_mem_en: Bool) :: dec_mem_fcn :: dec_mem_typ :: dec_csr_cmd :: (dec_fencei: Bool) :: Nil = dec0

  val dec_illegal: Bool = !dec_val_inst && dec_reg_inst_valid //illegal instruction

  // we need to stall IF while fencei goes through DEC and EXE, as there may
  // be a store we need to wait to clear in MEM.
  val exe_reg_fencei = RegInit(false.B)
  fencei := (dec_fencei && dec_reg_inst_valid) || exe_reg_fencei

  // immediates
  val imm_itype  = dec_reg_inst(31,20)
  val imm_stype  = Cat(dec_reg_inst(31,25), dec_reg_inst(11,7))
  val imm_sbtype = Cat(dec_reg_inst(31), dec_reg_inst(7), dec_reg_inst(30, 25), dec_reg_inst(11,8))
  val imm_utype  = dec_reg_inst(31, 12)
  val imm_ujtype = Cat(dec_reg_inst(31), dec_reg_inst(19,12), dec_reg_inst(20), dec_reg_inst(30,21))
  val imm_z = Cat(Fill(27,0.U), dec_reg_inst(19,15))

  // sign-extend immediates
  val imm_itype_sext  = Cat(Fill(20,imm_itype(11)), imm_itype)
  val imm_stype_sext  = Cat(Fill(20,imm_stype(11)), imm_stype)
  val imm_sbtype_sext = Cat(Fill(19,imm_sbtype(11)), imm_sbtype, 0.U)
  val imm_utype_sext  = Cat(imm_utype, Fill(12,0.U))
  val imm_ujtype_sext = Cat(Fill(11,imm_ujtype(19)), imm_ujtype, 0.U)

   // Bypass Muxes
  val exe_wbdata   = Wire(UInt(conf.xprlen.W))
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
  regfile.io.rs1_addr := dec_rs1_addr
  regfile.io.rs2_addr := dec_rs2_addr
  val rf_rs1_data = regfile.io.rs1_data
  val rf_rs2_data = regfile.io.rs2_data
  regfile.io.waddr := wb_reg_wbaddr
  regfile.io.wdata := wb_reg_wbdata
  regfile.io.wen   := wb_reg_rf_wen

  // roll the OP1 mux into the bypass mux logic
  dec_op1_data := MuxCase(rf_rs1_data, Array(
                       (dec_op1_sel === OP1_IMZ) -> imm_z,
                       (dec_op1_sel === OP1_PC) -> dec_reg_pc,
                       ((exe_reg_wbaddr === dec_rs1_addr) && (dec_rs1_addr =/= 0.U) && exe_reg_rf_wen) -> exe_wbdata,
                       ((mem_reg_wbaddr === dec_rs1_addr) && (dec_rs1_addr =/= 0.U) && mem_reg_rf_wen) -> mem_wbdata,
                       ((wb_reg_wbaddr  === dec_rs1_addr) && (dec_rs1_addr =/= 0.U) &&  wb_reg_rf_wen) -> wb_reg_wbdata))

  dec_rs2_data := MuxCase(rf_rs2_data, Array(
                       ((exe_reg_wbaddr === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && exe_reg_rf_wen) -> exe_wbdata,
                       ((mem_reg_wbaddr === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) && mem_reg_rf_wen) -> mem_wbdata,
                       ((wb_reg_wbaddr  === dec_rs2_addr) && (dec_rs2_addr =/= 0.U) &&  wb_reg_rf_wen) -> wb_reg_wbdata))

  dec_op2_data := MuxCase(dec_rs2_data, Array(
                       (dec_op2_sel === OP2_ITYPE)  -> imm_itype_sext,
                       (dec_op2_sel === OP2_STYPE)  -> imm_stype_sext,
                       (dec_op2_sel === OP2_SBTYPE) -> imm_sbtype_sext,
                       (dec_op2_sel === OP2_UTYPE)  -> imm_utype_sext,
                       (dec_op2_sel === OP2_UJTYPE) -> imm_ujtype_sext))


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
    exe_reg_rf_wen   := dec_rf_wen // FIXME: inorder to debug
    exe_reg_mem_en   := dec_mem_en
    // convert CSR instructions with raddr1 == 0 to read-only CSR commands
    exe_reg_csr_cmd  := Mux((dec_csr_cmd === CSR.S || dec_csr_cmd === CSR.C) && dec_rs1_addr === 0.U, CSR.R, dec_csr_cmd)
    exe_reg_is_csr   := dec_csr_cmd =/= CSR.N && dec_csr_cmd =/= CSR.I
    exe_reg_illegal  := dec_illegal
    exe_reg_br_type  := dec_br_type
    exe_reg_inst_valid := true.B
    exe_reg_inst     := dec_reg_inst
    exe_reg_fencei   := dec_fencei
  }

  when ((dec_stall && !mem_stall) || xcpt) {
    // (kill exe stage)
    // insert NOP (bubble) into Execute stage on front-end stall (e.g., hazard clearing)
    flush_dec_exe
  }.elsewhen(!mem_stall) {
    when (dec_kill || !dec_reg_inst_valid) { flush_dec_exe }
   .otherwise { forward_dec_exe }
     // data signals
    exe_reg_pc            := dec_reg_pc

    exe_reg_op1_data      := dec_op1_data
    exe_reg_op2_data      := dec_op2_data
    exe_reg_rs2_data      := dec_rs2_data
    // exe_reg_op2_sel       := dec_op2_sel
    exe_reg_alu_fun       := dec_alu_fun
    // exe_reg_rf_wen
    exe_reg_wb_sel        := dec_wb_sel
    exe_reg_wbaddr        := dec_wbaddr
    // exe_reg_mem_en
    exe_reg_mem_fcn       := dec_mem_fcn
    exe_reg_mem_typ       := dec_mem_typ
    // exe_reg_csr_cmd
  }

  // Execute Stage ==========================================================================================================================================
  //=========================================================================================================================================================

  val exe_alu_op1: UInt   = exe_reg_op1_data.asUInt
  val exe_alu_op2: UInt   = exe_reg_op2_data.asUInt
  // ALU
  val alu_shamt: UInt     = exe_alu_op2(4,0).asUInt
  val exe_adder_out: UInt = (exe_alu_op1 + exe_alu_op2)(conf.xprlen-1,0)

  //only for debug purposes right now until debug() works
  val exe_alu_out = Wire(UInt(conf.xprlen.W))
  exe_alu_out := MuxCase(0.U, Array(   // FIXME: why default is exe_reg_inst
                (exe_reg_alu_fun === ALU_ADD)   -> exe_adder_out,
                (exe_reg_alu_fun === ALU_SUB)   -> (exe_alu_op1 - exe_alu_op2).asUInt,
                (exe_reg_alu_fun === ALU_AND)   -> (exe_alu_op1 & exe_alu_op2).asUInt,
                (exe_reg_alu_fun === ALU_OR)    -> (exe_alu_op1 | exe_alu_op2).asUInt,
                (exe_reg_alu_fun === ALU_XOR)   -> (exe_alu_op1 ^ exe_alu_op2).asUInt,
                (exe_reg_alu_fun === ALU_SLT)   -> (exe_alu_op1.asSInt < exe_alu_op2.asSInt).asUInt,
                (exe_reg_alu_fun === ALU_SLTU)  -> (exe_alu_op1 < exe_alu_op2).asUInt,
                (exe_reg_alu_fun === ALU_SLL)   -> (exe_alu_op1 << alu_shamt)(conf.xprlen-1, 0).asUInt,
                (exe_reg_alu_fun === ALU_SRA)   -> (exe_alu_op1.asSInt >> alu_shamt).asUInt,
                (exe_reg_alu_fun === ALU_SRL)   -> (exe_alu_op1 >> alu_shamt).asUInt,
                (exe_reg_alu_fun === ALU_COPY_1)->  exe_alu_op1,
                (exe_reg_alu_fun === ALU_COPY_2)->  exe_alu_op2))

  // Branch/Jump Target Calculation
  exe_brjmp_target    := exe_reg_pc + exe_alu_op2
  exe_jpreg_target    := Cat(exe_alu_out(31,1), 0.U(1.W))
  exe_wbdata := Mux(exe_reg_wb_sel === WB_PC4, exe_pc_plus4, exe_alu_out)

  val exe_br_eq: Bool  = exe_reg_op1_data === exe_reg_rs2_data
  val exe_br_lt: Bool  = exe_reg_op1_data.asSInt < exe_reg_rs2_data.asSInt
  val exe_br_ltu: Bool = exe_reg_op1_data.asUInt < exe_reg_rs2_data.asUInt

  // Branch Logic
  exe_pc_sel := Mux(xcpt                      , PC_EXC,
                Mux(exe_reg_br_type === BR_N  , PC_4,
                Mux(exe_reg_br_type === BR_NE , Mux(!exe_br_eq,  PC_BRJMP, PC_4),
                Mux(exe_reg_br_type === BR_EQ , Mux( exe_br_eq,  PC_BRJMP, PC_4),
                Mux(exe_reg_br_type === BR_GE , Mux(!exe_br_lt,  PC_BRJMP, PC_4),
                Mux(exe_reg_br_type === BR_GEU, Mux(!exe_br_ltu, PC_BRJMP, PC_4),
                Mux(exe_reg_br_type === BR_LT , Mux( exe_br_lt,  PC_BRJMP, PC_4),
                Mux(exe_reg_br_type === BR_LTU, Mux( exe_br_ltu, PC_BRJMP, PC_4),
                Mux(exe_reg_br_type === BR_J  , PC_BRJMP,
                Mux(exe_reg_br_type === BR_JR , PC_JALR,
                                                PC_4 ))))))))))

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
    mem_reg_jpnpc         :=  Mux(exe_pc_sel === PC_BRJMP, exe_brjmp_target,
                              Mux(exe_pc_sel === PC_JALR,  exe_jpreg_target, 0.U))
    mem_reg_inst_valid    := exe_reg_inst_valid

    mem_reg_pc            := exe_reg_pc
    mem_reg_inst          := exe_reg_inst
    mem_reg_alu_out       := exe_wbdata
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

  dec_stall := ((exe_inst_is_load && exe_reg_wbaddr === dec_rs1_addr && exe_reg_wbaddr =/= 0.U && dec_rs1_oen) ||
                (exe_inst_is_load && exe_reg_wbaddr === dec_rs2_addr && exe_reg_wbaddr =/= 0.U && dec_rs2_oen)) &&
                dec_reg_inst_valid || exe_reg_is_csr

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
    , dec_reg_pc
    , exe_reg_pc
    , mem_reg_pc
    , RegNext(mem_reg_pc)
    , Mux(mem_stall, Str("F"),             //FREEZE-> F 
      Mux(dec_stall, Str("S"), Str(" ")))  //STALL->S
    , Mux(exe_pc_sel === 1.U, Str("B"),    //BJ -> B
      Mux(exe_pc_sel === 2.U, Str("J"),    //JR -> J
      Mux(exe_pc_sel === 3.U, Str("E"),    //EX -> E
      Mux(exe_pc_sel === 0.U, Str(" "), Str("?")))))
    , Mux(csr.io.illegal, Str("X"), Str(" "))
    , Mux(xcpt, BUBBLE, exe_reg_inst)
    )
}