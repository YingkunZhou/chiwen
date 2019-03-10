package myCore

import chisel3._
import chisel3.util.{Cat, Fill}

class MicroDecoder(inst_width: Int) extends Module{
  val io = IO(new Bundle{
    val inst = Input(UInt(inst_width.W))
    val isbj = Output(Bool())
    val isbrch = Output(Bool())
    val isjal = Output(Bool())
    val imm = Output(UInt(20.W))
    val rasPush = Output(Bool())
  })

  val func  = io.inst(6,2)
  val lui   = "b01101".U
  val auipc = "b00101".U
  val jal   = "b11011".U
  val jalr  = "b11001".U
  val brnch = "b11000".U
  val load  = "b00000".U
  val store = "b01000".U
  val ariti = "b00100".U
  val arith = "b01100".U
  val csrr  = "b11100".U
  io.isbj   := func === jal || func === jalr || func === brnch
  io.isjal  := func === jal
  io.isbrch := func === brnch
  val imm_ujtype = Cat(Fill(11,io.inst(31)), io.inst(19,12), io.inst(20), io.inst(30,21), 0.U)
  val imm_sbtype = Cat(Fill(19,io.inst(31)), io.inst(7), io.inst(30, 25), io.inst(11,8), 0.U)
  def link(addr: UInt): Bool = addr === 1.U || addr === 5.U
  io.rasPush := io.isjal && link(io.inst(RD_MSB , RD_LSB))
  io.imm := Mux(io.isjal, imm_ujtype, imm_sbtype)

}
