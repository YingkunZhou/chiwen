package myCore
import chisel3._
import chisel3.util._
import chiwen._
import common.{CPUConfig, CSR}
import common.Instructions._

object Jump {
  val none = 0
  val push = 1
  val pop  = 2
  val NUM  = pop + 1
}

class CtrlInfo extends Bundle {
  val br_type = UInt(BR_N.getWidth.W)
  val op1_sel = UInt(OP1_X.getWidth.W)
  val op2_sel = UInt(OP2_X.getWidth.W)
  val rs1_oen = Bool()
  val rs2_oen = Bool()
  val alu_fun = UInt(ALU_X.getWidth.W)
  val wb_sel  = UInt(WB_X.getWidth.W)
  val rf_wen  = Bool()
  val mem_en  = Bool()
  val mem_fcn = UInt(M_X.getWidth.W)
  val mem_typ = UInt(MT_X.getWidth.W)
  val csr_cmd = UInt(CSR.N.getWidth.W)
  val illegal = Bool()
  val fencei  = Bool()
  val cfi_branch = Bool()
  val cfi_jump = UInt(Jump.NUM.W)
  val rs1_addr = UInt(5.W)
  val rs2_addr = UInt(5.W)
  val wbaddr   = UInt(5.W)
}

class DataInfo(val data_width: Int) extends Bundle {
  val imm_i    = UInt(data_width.W)
  val imm_s    = UInt(data_width.W)
  val imm_sb   = UInt(data_width.W)
  val imm_u    = UInt(data_width.W)
  val imm_uj   = UInt(data_width.W)
  val imm_z    = UInt(data_width.W)
}

class InstDecoder(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val inst = Flipped(new ValidIO(UInt(conf.xprlen.W)))
    val cinfo = Output(new CtrlInfo())
    val dinfo = Output(new DataInfo(conf.xprlen))
  })

  val signals =
    ListLookup(io.inst.bits,
                 List(N, BR_N  , OP1_X , OP2_X    , OEN_0, OEN_0, ALU_X   , WB_X  ,  REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
      Array(   /* val  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | fence.i */
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
  val (val_inst: Bool) :: br_type :: op1_sel :: op2_sel :: (rs1_oen: Bool) :: (rs2_oen: Bool) :: sig = signals
  val alu_fun :: wb_sel :: (rf_wen: Bool) :: (mem_en: Bool) :: mem_fcn :: mem_typ :: csr_cmd :: (fencei: Bool) :: Nil = sig
  io.cinfo.br_type := br_type
  io.cinfo.op1_sel := op1_sel
  io.cinfo.op2_sel := op2_sel
  io.cinfo.rs1_oen := rs1_oen
  io.cinfo.rs2_oen := rs2_oen
  io.cinfo.alu_fun := alu_fun
  io.cinfo.wb_sel  := wb_sel
  io.cinfo.rf_wen  := rf_wen
  io.cinfo.mem_en  := mem_en
  io.cinfo.mem_fcn := mem_fcn
  io.cinfo.mem_typ := mem_typ
  io.cinfo.csr_cmd := csr_cmd
  io.cinfo.illegal := !val_inst && io.inst.valid //illegal instruction
  io.cinfo.fencei  := fencei && io.inst.valid

  io.cinfo.rs1_addr := io.inst.bits(RS1_MSB, RS1_LSB)
  io.cinfo.rs2_addr := io.inst.bits(RS2_MSB, RS2_LSB)
  io.cinfo.wbaddr   := io.inst.bits(RD_MSB , RD_LSB)

  val func = io.inst.bits(6,0)
  io.cinfo.cfi_branch := func === "b1100011".U
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
  val pop_push: Bool     =  cfiType_jalr &&  link(io.cinfo.wbaddr) && link(io.cinfo.rs1_addr)  && io.cinfo.wbaddr =/= io.cinfo.rs1_addr
  cifType_jump(Jump.none) := (cfiType_jalr && !link(io.cinfo.wbaddr) && !link(io.cinfo.rs1_addr))  || //case 1
    (cfiType_jal  && !link(io.cinfo.wbaddr))    // case 2

  cifType_jump(Jump.push) := (cfiType_jal  &&  link(io.cinfo.wbaddr))||  // case 1
    (cfiType_jalr &&  link(io.cinfo.wbaddr) && !link(io.cinfo.rs1_addr)) || // case 2
    (cfiType_jalr &&  link(io.cinfo.wbaddr) &&  link(io.cinfo.rs1_addr)  && io.cinfo.wbaddr === io.cinfo.rs1_addr) || //case 3
    pop_push // case 4

  cifType_jump(Jump.pop)  :=  cfiType_jalr && !link(io.cinfo.wbaddr) &&  link(io.cinfo.rs1_addr)  || //case 1
    pop_push // case 2

  io.cinfo.cfi_jump := cifType_jump.asUInt
  // immediates
  val imm_itype  = io.inst.bits(31,20)
  val imm_stype  = Cat(io.inst.bits(31,25), io.inst.bits(11,7))
  val imm_sbtype = Cat(io.inst.bits(31), io.inst.bits(7), io.inst.bits(30, 25), io.inst.bits(11,8))
  val imm_utype  = io.inst.bits(31, 12)
  val imm_ujtype = Cat(io.inst.bits(31), io.inst.bits(19,12), io.inst.bits(20), io.inst.bits(30,21))

  // sign-extend immediates
  io.dinfo.imm_i  := Cat(Fill(20,imm_itype(11)), imm_itype)
  io.dinfo.imm_s  := Cat(Fill(20,imm_stype(11)), imm_stype)
  io.dinfo.imm_sb := Cat(Fill(19,imm_sbtype(11)), imm_sbtype, 0.U)
  io.dinfo.imm_u  := Cat(imm_utype, Fill(12,0.U))
  io.dinfo.imm_uj := Cat(Fill(11,imm_ujtype(19)), imm_ujtype, 0.U)
  io.dinfo.imm_z  := Cat(Fill(27,0.U), io.inst.bits(19,15))

}
