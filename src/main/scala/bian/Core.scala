package bian

import chisel3._
import chisel3.util.Valid
import common.{AxiIO, CPUConfig, MemPortIo}

class InterfaceIO(val data_width: Int) extends Bundle {
  val xcpt     = Input(new Valid(UInt(data_width.W)))
  val kill     = Input(Bool())
  val forward  = Input(Vec(2, Bool()))
  //dec stage
  val inst     = Output(Vec(2, Valid(UInt(data_width.W))))
  //rename stage
  val pred     = Output(new PredictInfo(data_width))
  val pc_split = Output(Bool())
  val inst_split = Output(Bool())

  val fb_pc    = Input(UInt(data_width.W))
  val fb_type  = Input(UInt(BTBType.SZ.W))
  val feedback = Input(Valid(new Predict(data_width)))
}

class Core(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val imem = new AxiIO(conf.xprlen)
    val dmem = new MemPortIo(conf.xprlen)
    val cyc  = Output(UInt(conf.xprlen.W))
  })

  val frontEnd = Module(new FrontEnd())
  val backEnd  = Module(new BackEnd())
  frontEnd.io.mem  <> io.imem
  backEnd.io.mem   <> io.dmem
  frontEnd.io.back <> backEnd.io.front
  frontEnd.io.cyc  := backEnd.io.cyc
  io.cyc           := backEnd.io.cyc
}