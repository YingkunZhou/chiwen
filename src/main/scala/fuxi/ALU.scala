package fuxi

import chisel3._
import chisel3.util.{Cat, MuxCase}
import common.CPUConfig

class Target(val addr_width: Int) extends Bundle {
  val brjmp = UInt(addr_width.W)
  val jpreg = UInt(addr_width.W)
  val conti = UInt(addr_width.W)
}

class CtrlIO extends Bundle {
  val fun      = Input(UInt(ALU_X.getWidth.W))
  val br_type  = Input(UInt(BR_N.getWidth.W))
  val wb_sel   = Input(UInt(WB_X.getWidth.W))
  val pc_sel   = Output(UInt(PC_4.getWidth.W))
}

class ALU(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val op1  = Input(UInt(conf.xprlen.W))
    val op2  = Input(UInt(conf.xprlen.W))
    val pc   = Input(UInt(conf.xprlen.W))
    val ctrl = new CtrlIO()
    val rs2_data = Input(UInt(conf.xprlen.W))
    val result   = Output(UInt(conf.xprlen.W))
    val target   = Output(new Target(conf.xprlen))
  })

  // ALU
  val alu_shamt: UInt  = io.op2(4,0).asUInt
  val add_result: UInt = (io.op1 + io.op2)(conf.xprlen-1,0)

  //only for debug purposes right now until debug() works
  val result = Wire(UInt(conf.xprlen.W))
  result := MuxCase(0.U, Array(   // FIXME: why default is exe_reg_inst
    (io.ctrl.fun === ALU_ADD)   -> add_result,
    (io.ctrl.fun === ALU_SUB)   -> (io.op1 - io.op2).asUInt,
    (io.ctrl.fun === ALU_AND)   -> (io.op1 & io.op2).asUInt,
    (io.ctrl.fun === ALU_OR)    -> (io.op1 | io.op2).asUInt,
    (io.ctrl.fun === ALU_XOR)   -> (io.op1 ^ io.op2).asUInt,
    (io.ctrl.fun === ALU_SLT)   -> (io.op1.asSInt < io.op2.asSInt).asUInt,
    (io.ctrl.fun === ALU_SLTU)  -> (io.op1 < io.op2).asUInt,
    (io.ctrl.fun === ALU_SLL)   -> (io.op1 << alu_shamt)(conf.xprlen-1, 0).asUInt,
    (io.ctrl.fun === ALU_SRA)   -> (io.op1.asSInt >> alu_shamt).asUInt,
    (io.ctrl.fun === ALU_SRL)   -> (io.op1 >> alu_shamt).asUInt,
    (io.ctrl.fun === ALU_COPY_1)->  io.op1,
    (io.ctrl.fun === ALU_COPY_2)->  io.op2))

  // Branch/Jump Target Calculation
  io.target.brjmp    := io.pc + io.op2
  io.target.jpreg    := Cat(add_result(conf.xprlen-1,1), 0.U(1.W))
  io.target.conti    := io.pc + 4.U
  io.result := Mux(io.ctrl.wb_sel === WB_PC4, io.target.conti, result)

  val br_eq: Bool  = io.op1 === io.rs2_data
  val br_lt: Bool  = io.op1.asSInt < io.rs2_data.asSInt
  val br_ltu: Bool = io.op1.asUInt < io.rs2_data.asUInt

  // Branch Logic
  io.ctrl.pc_sel :=
    Mux(io.ctrl.br_type === BR_N  , PC_4,
    Mux(io.ctrl.br_type === BR_NE , Mux(!br_eq,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_EQ , Mux( br_eq,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_GE , Mux(!br_lt,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_GEU, Mux(!br_ltu, PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_LT , Mux( br_lt,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_LTU, Mux( br_ltu, PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_J  , PC_BRJMP,
    Mux(io.ctrl.br_type === BR_JR , PC_JALR,
    PC_4 )))))))))
}
