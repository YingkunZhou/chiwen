//**************************************************************************
// RISCV Processor Constants
//--------------------------------------------------------------------------
//
// Christopher Celio
// 2011 Feb 1

package bian

import chisel3._

trait ScalarOpConstants {

  //************************************
  // Control Signals
  val Y        = true.B
  val N        = false.B

  // PC Select Signal
  val PC_4     = 0.U(2.W)  // PC + 4
  val PC_BRJMP = 1.U(2.W)  // brjmp_target
  val PC_JALR  = 2.U(2.W)  // jump_reg_target
  val PC_EXC   = 3.U(2.W)  // exception

  // Branch Type
  val BR_N     = 0.U(4.W)  // Next
  val BR_NE    = 1.U(4.W)  // Branch on NotEqual
  val BR_EQ    = 2.U(4.W)  // Branch on Equal
  val BR_GE    = 3.U(4.W)  // Branch on Greater/Equal
  val BR_GEU   = 4.U(4.W)  // Branch on Greater/Equal Unsigned
  val BR_LT    = 5.U(4.W)  // Branch on Less Than
  val BR_LTU   = 6.U(4.W)  // Branch on Less Than Unsigned
  val BR_J     = 7.U(4.W)  // Jump
  val BR_JR    = 8.U(4.W)  // Jump Register

  // RS1 Operand Select Signal
  val OP1_RS1   = 0.U(2.W) // Register Source #1
  val OP1_PC    = 1.U(2.W) // PC
  val OP1_IMZ   = 2.U(2.W) // Zero-extended Immediate from RS1 field, for use by CSRI instructions
  val OP1_X     = 0.U(2.W)

  // RS2 Operand Select Signal
  val OP2_RS2    = 0.U(3.W) // Register Source #2
  val OP2_ITYPE  = 1.U(3.W) // immediate, I-type
  val OP2_STYPE  = 2.U(3.W) // immediate, S-type
  val OP2_SBTYPE = 3.U(3.W) // immediate, B
  val OP2_UTYPE  = 4.U(3.W) // immediate, U-type
  val OP2_UJTYPE = 5.U(3.W) // immediate, J-type
  val OP2_X      = 0.U(3.W)

  val OP22_X      = 0.U(2.W)
  val OP22_RS2    = 0.U(2.W) // Register Source #2
  val OP22_ITYPE  = 1.U(2.W) // immediate, I-type
  val OP22_UTYPE  = 2.U(2.W) // immediate, U-type

  // Register Operand Output Enable Signal
  val OEN_0   = false.B
  val OEN_1   = true.B

  // Register File Write Enable Signal
  val REN_0   = false.B
  val REN_1   = true.B

  // ALU Operation Signal
  val ALU_ADD    = 0.U(4.W)
  val ALU_SUB    = 1.U(4.W)
  val ALU_SLL    = 2.U(4.W)
  val ALU_SRL    = 3.U(4.W)
  val ALU_SRA    = 4.U(4.W)
  val ALU_AND    = 5.U(4.W)
  val ALU_OR     = 6.U(4.W)
  val ALU_XOR    = 7.U(4.W)
  val ALU_SLT    = 8.U(4.W)
  val ALU_SLTU   = 9.U(4.W)
  val ALU_COPY_1 = 10.U(4.W)
  val ALU_COPY_2 = 11.U(4.W)
  val ALU_X      = 0.U(4.W)

  // Writeback Select Signal
  val WB_ALU  = 0.U(2.W)
  val WB_MEM  = 1.U(2.W)
  val WB_PC4  = 2.U(2.W)
  val WB_CSR  = 3.U(2.W)
  val WB_X    = 0.U(2.W)

  // Memory Enable Signal
  val MEN_0   = false.B
  val MEN_1   = true.B
  val MEN_X   = false.B

  val CYC_X   = 0.U(3.W) // Privileged inst and illegal inst
  val CYC_1   = 0.U(3.W)
  val CYC_2   = 1.U(3.W)
  val CYC_3   = 2.U(3.W)
  val CYC_4   = 3.U(3.W)
  val CYC_5   = 4.U(3.W)
  val CYC_6   = 5.U(3.W)
  val CYC_32  = 6.U(3.W) //for DIV
  val CYC_VAL = 7.U(3.W) //for LS
}