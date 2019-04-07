package bian

import chisel3._
import chisel3.util._
import common.{CPUConfig, CSR}
import common.Instructions._

class OpCode extends Bundle {
  val alu_fun  = UInt(ALU_X.getWidth.W)
  val wb_sel   = UInt(WB_X.getWidth.W)
  val cycle    = UInt(CYC_X.getWidth.W)
}

class InstDecoder(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(conf.inst_width.W))
    val illegal = Output(Bool())
    val privil  = Output(Bool())
    val fencei  = Output(Bool())
    val order   = Output(Bool())
    val csr_cmd = Output(UInt(CSR.SZ))
    val br_type = Output(UInt(BR_N.getWidth.W))
    val mem_en  = Output(Bool())
    val mem_fcn = Output(UInt(M_X.getWidth.W))
    val mem_typ = Output(UInt(MT_X.getWidth.W))
    val imm     = Output(UInt(12.W))
    val imm7_0  = Output(UInt(8.W))
    val imm_z   = Output(UInt(conf.data_width.W))
    val op1_sel = Output(UInt(OP1_X.getWidth.W))
    val op2_sel = Output(UInt(OP22_X.getWidth.W))
    val op  = Output(new OpCode())
    val rs  = Output(Vec(2, new ByPass(5)))
    val rd  = Output(new ByPass(5))
  })

  val signals =
    ListLookup(io.inst,
                  List(N, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, Y , CYC_X),
       Array(   /* val  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | f.i|ord| cycle*/
                /* inst | type |   sel |    sel    |  oen |  oen |   fcn   |  sel  | wen  |  en  |  wr  | type | cmd |    |ord|      */
        LW     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, CSR.N, N, N , CYC_VAL),
        LB     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_B, CSR.N, N, N , CYC_VAL),
        LBU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_BU,CSR.N, N, N , CYC_VAL),
        LH     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_H, CSR.N, N, N , CYC_VAL),
        LHU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_HU,CSR.N, N, N , CYC_VAL),
        SW     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_W, CSR.N, N, N , CYC_VAL),
        SB     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_B, CSR.N, N, N , CYC_VAL),
        SH     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_H, CSR.N, N, N , CYC_VAL),

        AUIPC  -> List(Y, BR_N  , OP1_PC , OP2_UTYPE , OEN_0, OEN_0, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        LUI    -> List(Y, BR_N  , OP1_X  , OP2_UTYPE , OEN_0, OEN_0, ALU_COPY_2,WB_ALU,REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),

        ADDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        ANDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        ORI    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        XORI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SLTI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SLTIU  -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SLLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SRAI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SRLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),

        SLL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        ADD    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SUB    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SUB , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SLT    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SLTU   -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        AND    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        OR     -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        XOR    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SRA    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        SRL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),

        JALR   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        JAL    -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        BEQ    -> List(Y, BR_EQ , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        BNE    -> List(Y, BR_NE , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        BGE    -> List(Y, BR_GE , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        BGEU   -> List(Y, BR_GEU, OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        BLT    -> List(Y, BR_LT , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),
        BLTU   -> List(Y, BR_LTU, OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N , CYC_1),

        CSRRWI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N, Y , CYC_X),
        CSRRSI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N, Y , CYC_X),
        CSRRCI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N, Y , CYC_X),
        CSRRW  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N, Y , CYC_X),
        CSRRS  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N, Y , CYC_X),
        CSRRC  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N, Y , CYC_X),

        ECALL  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y , CYC_X),
        MRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y , CYC_X),
        DRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y , CYC_X),
        EBREAK -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y , CYC_X),
        WFI    -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, Y , CYC_X),

        FENCE_I-> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, Y, Y , CYC_X),
        // kill pipeline and refetch instructions since the pipeline will be holding stall instructions.
        FENCE  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_1, M_X  , MT_X, CSR.N, N, Y , CYC_X)
        // we are already sequentially consistent, so no need to honor the fence instruction
      ))

  // Put these control signals in variables
  val (val_inst: Bool) :: br_type :: op1_sel :: op2_sel :: (rs1_oen: Bool) :: (rs2_oen: Bool) :: sig1 = signals
  val alu_fun :: wb_sel :: (rd_wen: Bool) :: (mem_en: Bool) :: mem_fcn :: mem_typ :: sig2 = sig1
  val csr_cmd :: (fencei: Bool) :: (order: Bool) :: cycle :: Nil = sig2
  io.op1_sel := Mux(io.rs(0).addr === 0.U, OP1_X, op1_sel)
  io.op2_sel := Mux(io.rs(1).addr === 0.U, OP2_X, op2_sel(1,0)) // for space saving, and change OP2_STYPE to OP22_RS2
  io.op.alu_fun := alu_fun
  io.op.wb_sel  := wb_sel
  io.op.cycle   := cycle
  io.br_type := br_type
  io.mem_en  := mem_en
  io.mem_fcn := mem_fcn
  io.mem_typ := mem_typ
  io.csr_cmd := Mux((csr_cmd === CSR.S || csr_cmd === CSR.C) &&
    io.inst(RS1_MSB, RS1_LSB) === 0.U, CSR.R, csr_cmd)

  io.privil := io.inst(6,2) === "b11100".U || io.inst(6,2) === "b00011".U
  io.illegal := !val_inst //illegal instruction
  io.order   := order
  io.fencei  := fencei
  io.rs(0).addr  := io.inst(RS1_MSB, RS1_LSB)
  io.rs(1).addr  := io.inst(RS2_MSB, RS2_LSB)
  io.rd.addr     := io.inst(RD_MSB , RD_LSB)
  io.rs(0).valid := !rs1_oen || io.rs(0).addr === 0.U
  io.rs(1).valid := !rs2_oen || io.rs(1).addr === 0.U
  io.rd.valid    := rd_wen   && io.rd.addr =/= 0.U

  // immediates
  val imm_itype  = io.inst(31,20)
  val imm_utype  = io.inst(31,12)
  val imm_stype  = Cat(io.inst(31,25), io.inst(11,7))

  // sign-extend immediates
  io.imm    := Mux(op2_sel === OP2_STYPE, imm_stype, imm_itype)
  io.imm7_0 := imm_utype(7,0)
  io.imm_z  := Cat(Fill(27, 0.U(1.W)), io.inst(19,15))
}
