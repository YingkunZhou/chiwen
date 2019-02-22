package myCore

import chisel3._
import common.CPUConfig

class ReadIO(data_width: Int, addr_width: Int) extends Bundle {
  val addr = Input(UInt(addr_width.W))
  val data = Output(UInt(data_width.W))
}

class WriteIO(data_width: Int, addr_width: Int) extends Bundle {
  val addr = Input(UInt(addr_width.W))
  val data = Input(UInt(data_width.W))
  val en   = Input(Bool())
}

class Regfile(implicit conf: CPUConfig) extends Module with Pram {
  val io = IO(new Bundle{
    val r = Vec(4, new ReadIO(conf.xprlen, wPhyAddr))
    val w = Vec(4, new WriteIO(conf.xprlen, wPhyAddr))
  })

  val regfile = Mem(nPhyAddr, UInt(conf.xprlen.W))
  for (i <- 0 until 4) { regfile(io.w(i).addr) := io.w(i).data }
  for (i <- 0 until 4) { io.r(i).data := regfile(io.r(i).addr) }

}
