package myCore

import chisel3._
import chisel3.util._
import common.{CPUConfig, CSR}
import common.Instructions._

object Jump {
  val none = 0
  val push = 1
  val pop  = 2
  val NUM: Int  = pop + 1
  val SZ: Int = log2Ceil(NUM)
}

object Fast {
  val auipc = 0
  val lui = 1
  val jal = 2
  val NUM: Int = jal + 1
}

class InnerOp extends Bundle {
  val op1_sel  = UInt(OP1_X.getWidth.W)
  val op2_sel  = UInt(OP2_X.getWidth.W)
  val alu_fun  = UInt(ALU_X.getWidth.W)
  val wb_sel   = UInt(WB_X.getWidth.W)
  val mem_en   = Bool()
  val mem_fcn  = UInt(M_X.getWidth.W)
  val mem_typ  = UInt(MT_X.getWidth.W)
  val cycle    = UInt(CYC_X.getWidth.W)
}

class InstDecoder(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(conf.inst_width.W))
    val illegal = Output(Bool())
    val jump    = Output(UInt(Jump.SZ.W))
    val brtype  = Output(UInt(BR_N.getWidth.W))
    val op    = Output(new InnerOp())
    val imm   = Output(UInt(12.W))
    val imm_z = Output(UInt(5.W))
    val imm_l = Output(UInt(8.W))
    val fix1  = Output(Bool())
    val rs    = Output(Vec(2, Valid(UInt(5.W))))
    val rd    = Output(Valid(UInt(5.W)))
  })

  val signals =
    ListLookup(io.inst,
                    List(N, BR_N  , OP1_X , OP2_X    , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X ,CSR.N,   N, Y, CYC_X),
         Array(   /* ill  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | fence.i */
                  /* inst | type |   sel |    sel    |  oen |  oen |   fcn   |  sel  | wen  |  en  |  wr  | type | cmd |  order  */
          LW     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, CSR.N, N, N, CYC_VAL),
          LB     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_B, CSR.N, N, N, CYC_VAL),
          LBU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_BU,CSR.N, N, N, CYC_VAL),
          LH     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_H, CSR.N, N, N, CYC_VAL),
          LHU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_HU,CSR.N, N, N, CYC_VAL),
          SW     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_W, CSR.N, N, N, CYC_VAL),
          SB     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_B, CSR.N, N, N, CYC_VAL),
          SH     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_H, CSR.N, N, N, CYC_VAL),

          AUIPC  -> List(Y, BR_N  , OP1_PC , OP2_UTYPE , OEN_0, OEN_0, ALU_ADD   ,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N, N, CYC_1),
          LUI    -> List(Y, BR_N  , OP1_X  , OP2_UTYPE , OEN_0, OEN_0, ALU_COPY_2,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N, N, CYC_1),

          ADDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          ANDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          ORI    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          XORI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SLTI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SLTIU  -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SLLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SRAI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SRLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),

          SLL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          ADD    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SUB    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SUB , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SLT    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SLTU   -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          AND    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          OR     -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          XOR    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SRA    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          SRL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),

          JAL    -> List(Y, BR_N  , OP1_RS1, OP2_UJTYPE, OEN_0, OEN_0, ALU_X   , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          JALR   -> List(Y, BR_JR , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          BEQ    -> List(Y, BR_EQ , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          BNE    -> List(Y, BR_NE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          BGE    -> List(Y, BR_GE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          BGEU   -> List(Y, BR_GEU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          BLT    -> List(Y, BR_LT , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),
          BLTU   -> List(Y, BR_LTU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, N, CYC_1),

          //order for mtc0 but out of order for mfc0
          CSRRWI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N, Y, CYC_X),
          CSRRSI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N, Y, CYC_X),
          CSRRW  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N, Y, CYC_X),
          CSRRS  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N, Y, CYC_X),
          CSRRC  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N, Y, CYC_X),
          CSRRCI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N, Y, CYC_X),

          ECALL  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y, CYC_X),
          MRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y, CYC_X),
          DRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y, CYC_X),
          EBREAK -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, Y, CYC_X),
          WFI    -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, Y, CYC_1), // implemented as a NOP

          FENCE_I-> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, Y, Y, CYC_X),
          // kill pipeline and refetch instructions since the pipeline will be holding stall instructions.
          FENCE  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_1, M_X  , MT_X, CSR.N, N, Y, CYC_X)
          // we are already sequentially consistent, so no need to honor the fence instruction
        ))

  // Put these control signals in variables
  val (val_inst: Bool) :: br_type :: op1_sel :: op2_sel :: (rs1_ren: Bool) :: (rs2_ren: Bool) :: sig1 = signals
  val alu_fun :: wb_sel :: (rd_wen: Bool) :: (mem_en: Bool) :: mem_fcn :: mem_typ :: sig2 = sig1
  val csr_cmd :: (fencei: Bool) :: (order: Bool) :: cycle :: Nil = sig2
  io.op.op1_sel := op1_sel
  io.op.op2_sel := op2_sel(1,0)
  io.op.alu_fun := alu_fun
  io.op.wb_sel  := wb_sel
  io.op.mem_en  := mem_en
  io.op.mem_fcn := mem_fcn
  io.op.mem_typ := mem_typ
  io.op.cycle   := cycle
  io.fix1   := cycle === CYC_1
  io.brtype := br_type
  io.rs(0).bits  := io.inst(RS1_MSB, RS1_LSB)
  io.rs(0).bits  := io.inst(RS2_MSB, RS2_LSB)
  io.rs(1).valid := rs1_ren
  io.rs(1).valid := rs2_ren
  io.rd.bits  := io.inst(RD_MSB , RD_LSB)
  io.rd.valid := rd_wen
  io.illegal  := !val_inst //illegal instruction

  val func = io.inst(6,0)
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
  val jalr: Bool = func === "b1100111".U
  val pop_push: Bool = jalr &&  link(io.rd.bits) && link(io.rs(1).bits)  && io.rd.bits =/= io.rs(1).bits
  val jump  = Wire(Vec(Jump.NUM, Bool()))
  jump(Jump.none) := jalr && !link(io.rd.bits) && !link(io.rs(1).bits)
//    (jal  && !link(io.rd.bits))    // case 2

  jump(Jump.push) :=
    (jalr &&  link(io.rd.bits) && !link(io.rs(1).bits)) || pop_push ||
    (jalr &&  link(io.rd.bits) &&  link(io.rs(1).bits)  && io.rd.bits === io.rs(1).bits)
  //    (jal  &&  link(io.rd.bits))||  // case 1

  jump(Jump.pop) := (jalr && !link(io.rd.bits) && link(io.rs(1).bits)) || pop_push
  io.jump := OHToUInt(jump)
  // immediates
  val imm_itype  = io.inst(31,20)
  val imm_stype  = Cat(io.inst(31,25), io.inst(11,7))
  val imm_sbtype = Cat(io.inst(31), io.inst(7), io.inst(30, 25), io.inst(11,8))
  val imm_utype  = io.inst(31, 12) // the front is same as imm_itype
  val imm_ujtype = Cat(io.inst(31), io.inst(19,12), io.inst(20), io.inst(30,21))
  io.imm := MuxCase(imm_itype, Array(
    (op2_sel === OP2_STYPE)  -> imm_stype,
    (op2_sel === OP2_SBTYPE) -> imm_sbtype,
    (op2_sel === OP2_UJTYPE) -> imm_ujtype(19, 8)))

  io.imm_z := io.inst(19,15)
  io.imm_l := Mux(op2_sel === OP2_UJTYPE, imm_ujtype(7, 0), imm_utype(7, 0))

}
