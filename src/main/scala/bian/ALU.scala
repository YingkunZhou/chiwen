package bian

import chisel3._
import chisel3.util.{Cat, MuxCase}
import common.CPUConfig

class ALU extends Module with BackParam {
  val io = IO(new Bundle {
    val data = Input(Vec(2, UInt(data_width.W)))
    val opcode = Input(UInt(ALU_X.getWidth.W))
    val brtype = Input(UInt(BR_N.getWidth.W))
    val result = Output(UInt(data_width.W))
    val add_result = Output(UInt(data_width.W))
    val actual = Output(Bool())
  })
  // ALU
  val alu_shamt: UInt  = io.data(1)(4,0).asUInt
  io.add_result := io.data(0) + io.data(1)
  //only for debug purposes right now until debug() works
  io.result := MuxCase(0.U, Array( // FIXME: one Hot
    (io.opcode === ALU_ADD)   -> io.add_result,
    (io.opcode === ALU_SUB)   -> (io.data(0) - io.data(1)).asUInt,
    (io.opcode === ALU_AND)   -> (io.data(0) & io.data(1)).asUInt,
    (io.opcode === ALU_OR)    -> (io.data(0) | io.data(1)).asUInt,
    (io.opcode === ALU_XOR)   -> (io.data(0) ^ io.data(1)).asUInt,
    (io.opcode === ALU_SLT)   -> (io.data(0).asSInt < io.data(1).asSInt).asUInt,
    (io.opcode === ALU_SLTU)  -> (io.data(0) < io.data(1)).asUInt,
    (io.opcode === ALU_SLL)   -> (io.data(0) << alu_shamt)(data_width-1, 0).asUInt,
    (io.opcode === ALU_SRA)   -> (io.data(0).asSInt >> alu_shamt).asUInt,
    (io.opcode === ALU_SRL)   -> (io.data(0) >> alu_shamt).asUInt,
    (io.opcode === ALU_COPY_1)->  io.data(0),
    (io.opcode === ALU_COPY_2)->  io.data(1)))
  // Jump register target Calculation
  val br_eq: Bool  = io.data(0) === io.data(1)
  val br_lt: Bool  = io.data(0).asSInt < io.data(1).asSInt
  val br_ltu: Bool = io.data(0).asUInt < io.data(1).asUInt
  // Branch Logic
  io.actual :=
    (io.brtype === BR_N   && !br_eq)  ||
    (io.brtype === BR_EQ  && br_eq)   ||
    (io.brtype === BR_GE  && !br_lt)  ||
    (io.brtype === BR_GEU && !br_ltu) ||
    (io.brtype === BR_LT  && br_lt)   ||
    (io.brtype === BR_LTU && br_ltu)
}
