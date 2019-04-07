package bian

import chisel3._
import chisel3.util.log2Ceil

class ReadIO(val data_width: Int, val addr_width: Int) extends Bundle {
  val addr = Input(UInt(addr_width.W))
  val data = Output(UInt(data_width.W))
}

class WriteIO(val data_width: Int, val addr_width: Int) extends Bundle {
  val addr  = Input(UInt(addr_width.W))
  val data  = Input(UInt(data_width.W))
  val valid = Input(Bool())
}

class Regfile extends Module with BackParam {
  val io = IO(new Bundle{
    val r = Vec(nCommit, new ReadIO(data_width,  log2Ceil(nPhyAddr)))
    val w = Vec(nCommit, new WriteIO(data_width, log2Ceil(nPhyAddr)))
  })

  val regfile = SyncReadMem(nPhyAddr, UInt(data_width.W))
  for (i <- 0 until nCommit) {
    io.r(i).data := regfile(io.r(i).addr)
    when (io.w(i).valid) {regfile(io.w(i).addr) := io.w(i).data} //no write after write hazard
  }
}
