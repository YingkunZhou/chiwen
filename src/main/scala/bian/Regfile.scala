package bian

import chisel3._
import common.CPUConfig

class ReadIO(data_width: Int, addr_width: Int) extends Bundle {
  val addr = Input(UInt(addr_width.W))
  val data = Output(UInt(data_width.W))
}

class RegFlIO(val data_width: Int, val addr_width: Int) extends Bundle {
  val addr = Vec(2, UInt(addr_width.W))
  val data = Vec(2, UInt(data_width.W))
}

class WriteIO(data_width: Int, addr_width: Int) extends Bundle {
  val addr = Input(UInt(addr_width.W))
  val data = Input(UInt(data_width.W))
  val en   = Input(Bool())
}

class Regfile(implicit conf: CPUConfig) extends Module with BackParam {
  val io = IO(new Bundle{
    val r = Vec(4, new ReadIO(conf.xprlen, wPhyAddr))
    val w = Vec(4, new WriteIO(conf.xprlen, wPhyAddr))
  })

  val regfile = Mem(nPhyAddr, UInt(conf.xprlen.W))
  for (i <- 0 until 4) { io.r(i).data := regfile(io.r(i).addr) }
  when (io.w(0).en) {regfile(io.w(0).addr) := io.w(0).data}
  when (io.w(1).en) {regfile(io.w(1).addr) := io.w(1).data}
  when (io.w(2).en) {regfile(io.w(2).addr) := io.w(2).data}
  when (io.w(3).en) {regfile(io.w(3).addr) := io.w(3).data}
}
