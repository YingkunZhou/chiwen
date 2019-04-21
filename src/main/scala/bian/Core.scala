package bian

import chisel3._
import chisel3.util.{DecoupledIO, Valid}
import common.{AxiIO, CPUConfig, MemPortIo}

class InterfaceIO(implicit val conf: CPUConfig) extends Bundle {
  val xcpt = Input(Valid(UInt(conf.data_width.W)))
  val kill = Input(Valid(UInt(conf.data_width.W)))

  val fb_pc    = Input(UInt(conf.data_width.W))
  val fb_type  = Input(UInt(BTBType.SZ.W))
  val feedback = Input(new PredictVal(conf.data_width))

  val inst = Vec(conf.nInst, DecoupledIO(UInt(conf.inst_width.W)))
  val pred = Output(new PredictInfo(conf.data_width))
  val pc   = Output(Vec(conf.nInst, UInt(conf.data_width.W)))
}

class Core(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val imem = new AxiIO(conf.xprlen)
    val dmem = new MemPortIo(conf.xprlen)
    val cyc  = Output(UInt(conf.xprlen.W))
  })

  val frontEnd = Module(new FrontEnd)
  val backEnd  = Module(new BackEnd)
  frontEnd.io.mem  <> io.imem
  backEnd.io.mem   <> io.dmem
  frontEnd.io.back <> backEnd.io.front
  frontEnd.io.cyc  := backEnd.io.cyc
  io.cyc           := backEnd.io.cyc
}