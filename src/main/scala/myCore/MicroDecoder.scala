package myCore

import bian._
import chisel3._
import chisel3.util.Valid
import common.CPUConfig

class MicroDecoder(implicit conf: CPUConfig) extends Module{
  val io = IO(new Bundle{
    val inst = Input(UInt(conf.xprlen.W))
    val rs = Output(Vec(2, Valid(UInt(5.W))))
    val rd = Output(Valid(UInt(5.W)))
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
  io.rs(0).bits  := io.inst(RS1_MSB, RS1_LSB)
  io.rs(1).bits  := io.inst(RS2_MSB, RS2_LSB)
  io.rd.bits     := io.inst(RD_MSB , RD_LSB)
  io.rs(0).valid := io.rs(0).bits =/= 0.U && (
    func === jalr  ||
    func === brnch ||
    func === load  ||
    func === store ||
    func === ariti ||
    func === arith ||
    func === csrr
  )
  io.rs(1).valid := io.rs(1).bits =/= 0.U && (
    func === brnch ||
    func === store ||
    func === arith ||
    func === csrr
  )
  io.rd.valid := io.rd.bits =/= 0.U && (
    func === lui   ||
    func === auipc ||
    func === jal   ||
    func === jalr  ||
    func === load  ||
    func === ariti ||
    func === arith ||
    func === csrr
  )
}
