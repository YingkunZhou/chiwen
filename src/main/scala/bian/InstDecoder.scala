//package bian
//
//import chisel3._
//import chisel3.util._
//import common.{CPUConfig, CSR}
//import common.Instructions._
//
//object Jump {
//  val none = 0
//  val push = 1
//  val pop  = 2
//  val NUM: Int  = pop + 1
//  val SZ: Int = log2Ceil(NUM)
//}
//
//object Fast {
//  val auipc = 0
//  val lui = 1
//  val jal = 2
//  val NUM: Int = jal + 1
//}
//
//class InnerInst extends Bundle { // 15 bits total
//  val op1_sel  = UInt(OP1_X.getWidth.W) //2
//  val op2_sel  = UInt(OP2_X.getWidth.W) //2
//  val alu_fun  = UInt(ALU_X.getWidth.W) //4
//  val wb_sel   = UInt(WB_X.getWidth.W)  //2
//  val mem_en   = Bool()                 //1
//  val mem_fcn  = UInt(M_X.getWidth.W)   //1
//  val mem_typ  = UInt(MT_X.getWidth.W)  //3
//}
//
//class FastInfo(val data_width: Int) extends Bundle {
//  val imm = UInt(data_width.W)
//  val op = Vec(Fast.NUM, Bool())
//}
//
//class InstDecoder(implicit conf: CPUConfig) extends Module {
//  val io = IO(new Bundle {
//    val rawinst = Input(UInt(conf.inst_width.W))
//    val inst    = Output(new InnerInst())
//    val illegal = Output(Bool())
//    val jump    = Output(UInt(Jump.SZ.W))
//    val brtype  = Output(UInt(BR_N.getWidth.W))
//    val fast = Output(new FastInfo(conf.data_width))
//    val imm  = Output(UInt(12.W))
//    val rs   = Output(Vec(2, Valid(UInt(5.W))))
//    val rd   = Output(Valid(UInt(5.W)))
//  })
//
//  val signals =
//    ListLookup(io.rawinst,
//                  List(N, BR_N  , OP1_X , OP2_X    , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X ,CSR.N, N),
//        Array(   /* ill  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | fence.i */
//                 /* inst | type |   sel |    sel    |  oen |  oen |   fcn   |  sel  | wen  |  en  |  wr  | type | cmd |         */
//        LW     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, CSR.N, N),
//        LB     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_B, CSR.N, N),
//        LBU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_BU,CSR.N, N),
//        LH     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_H, CSR.N, N),
//        LHU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_HU,CSR.N, N),
//        SW     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_W, CSR.N, N),
//        SB     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_B, CSR.N, N),
//        SH     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_H, CSR.N, N),
//
//        AUIPC  -> List(Y, BR_N  , OP1_PC , OP2_UTYPE , OEN_0, OEN_0, ALU_X   , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N), //fast
//        LUI    -> List(Y, BR_N  , OP1_X  , OP2_UTYPE , OEN_0, OEN_0, ALU_X   , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N), //fast
//
//        ADDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        ANDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        ORI    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        XORI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SLTI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SLTIU  -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SLLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SRAI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SRLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//
//        SLL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        ADD    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SUB    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SUB , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SLT    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SLTU   -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        AND    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        OR     -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        XOR    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SRA    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        SRL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//
//        JAL    -> List(Y, BR_N  , OP1_X  , OP2_UJTYPE, OEN_0, OEN_0, ALU_X   , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N), //quick redirect, fast
//        JALR   -> List(Y, BR_JR , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N),
//        BEQ    -> List(Y, BR_EQ , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
//        BNE    -> List(Y, BR_NE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
//        BGE    -> List(Y, BR_GE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
//        BGEU   -> List(Y, BR_GEU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
//        BLT    -> List(Y, BR_LT , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
//        BLTU   -> List(Y, BR_LTU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
//
//        CSRRWI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N),
//        CSRRSI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N),
//        CSRRW  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N),
//        CSRRS  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N),
//        CSRRC  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N),
//        CSRRCI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N),
//
//        ECALL  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
//        MRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
//        DRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
//        EBREAK -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N),
//        WFI    -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N), // implemented as a NOP
//
//        FENCE_I-> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, Y),
//        // kill pipeline and refetch instructions since the pipeline will be holding stall instructions.
//        FENCE  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_1, M_X  , MT_X, CSR.N, N)
//        // we are already sequentially consistent, so no need to honor the fence instruction
//      ))
//
//  // Put these control signals in variables
//  val (val_inst: Bool) :: br_type :: op1_sel :: op2_sel :: (rs1_oen: Bool) :: (rs2_oen: Bool) :: sig = signals
//  val alu_fun :: wb_sel :: (rf_wen: Bool) :: (mem_en: Bool) :: mem_fcn :: mem_typ :: csr_cmd :: (fencei: Bool) :: Nil = sig
//  io.inst.op1_sel := op1_sel
//  io.inst.op2_sel := op2_sel(1,0)
//  io.inst.alu_fun := alu_fun
//  io.inst.wb_sel  := wb_sel
//  io.inst.mem_en  := mem_en
//  io.inst.mem_fcn := mem_fcn
//  io.inst.mem_typ := mem_typ
//
//  io.brtype := br_type
//  io.rs(0).bits  := io.rawinst(RS1_MSB, RS1_LSB)
//  io.rs(1).valid := rs1_oen
//  io.rs(0).bits  := io.rawinst(RS2_MSB, RS2_LSB)
//  io.rs(1).valid := rs2_oen
//  io.rd.bits  := io.rawinst(RD_MSB , RD_LSB)
//  io.rd.valid := rf_wen
//  io.illegal  := !val_inst //illegal instruction
//
//  val func = io.rawinst(6,0)
//  def link(addr: UInt): Bool = addr === 1.U || addr === 5.U
//  /*
//  * A JAL instruction should push the return address onto
//  * a return-address stack (RAS) only when rd=x1/x5
//  * JALR instructions should push/pop a RAS as shown in the Table:
//  *   rd    |   rs1    |    rs1 = rd    |   RAS action
//  * !link   |  !link   |        -       |   none
//  * !link   |   link   |        -       |   pop
//  *  link   |  !link   |        -       |   push
//  *  link   |   link   |        0       |   push and pop
//  *  link   |   link   |        1       |   push
//  */
//  val jalr: Bool = func === "b1100111".U
//  val pop_push: Bool = jalr &&  link(io.rd.bits) && link(io.rs(1).bits)  && io.rd.bits =/= io.rs(1).bits
//  val jump  = Wire(Vec(Jump.NUM, Bool()))
//  jump(Jump.none) := jalr && !link(io.rd.bits) && !link(io.rs(1).bits)
//  //    (jal  && !link(io.rd.bits))    // case 2
//
//  jump(Jump.push) :=
//    (jalr &&  link(io.rd.bits) && !link(io.rs(1).bits)) || pop_push ||
//      (jalr &&  link(io.rd.bits) &&  link(io.rs(1).bits)  && io.rd.bits === io.rs(1).bits)
//  //    (jal  &&  link(io.rd.bits))||  // case 1
//
//  jump(Jump.pop) := (jalr && !link(io.rd.bits) && link(io.rs(1).bits)) || pop_push
//  io.jump := OHToUInt(jump)
//  // immediates
//  val imm_itype  = io.rawinst(31,20)
//  val imm_stype  = Cat(io.rawinst(31,25), io.rawinst(11,7))
//  val imm_sbtype = Cat(io.rawinst(31), io.rawinst(7), io.rawinst(30, 25), io.rawinst(11,8))
//  val imm_utype  = io.rawinst(31, 12)
//  val imm_ujtype = Cat(io.rawinst(31), io.rawinst(19,12), io.rawinst(20), io.rawinst(30,21))
//  io.imm := MuxCase(imm_itype, Array(
//    (op2_sel === OP2_STYPE)  -> imm_stype,
//    (op2_sel === OP2_SBTYPE) -> imm_sbtype))
//
//  io.fast.op(Fast.auipc)= func === "b0110111".U
//  io.fast.op(Fast.lui)  = func === "b0010111".U
//  io.fast.op(Fast.jal)  = func === "b1101111".U
//  io.fast.imm := Mux(op2_sel === OP2_UJTYPE, Cat(Fill(11,imm_ujtype(19)), imm_ujtype, 0.U), Cat(imm_utype, Fill(12,0.U)))
//}
