package bian

import chisel3._
import chisel3.util.log2Ceil
import common.CPUConfig

class ReadIO(data_width: Int, addr_width: Int) extends Bundle {
  val addr = Input(UInt(addr_width.W))
  val data = Output(UInt(data_width.W))
}

class WriteIO(data_width: Int, addr_width: Int) extends Bundle {
  val addr  = Input(UInt(addr_width.W))
  val data  = Input(UInt(data_width.W))
  val valid = Input(Bool())
}

class Regfile(val data_width: Int, val nPhyAddr: Int) extends Module {
  val io = IO(new Bundle{
    val r = Vec(4, new ReadIO(data_width, log2Ceil(nPhyAddr)))
    val w = Vec(4, new WriteIO(data_width, log2Ceil(nPhyAddr)))
  })

  val regfile = SyncReadMem(nPhyAddr, UInt(data_width.W))
  for (i <- 0 until 4) {
    io.r(i).data := regfile(io.r(i).addr)
    when (io.w(i).valid) {regfile(io.w(i).addr) := io.w(i).data} //no write after write hazard
  }
}
