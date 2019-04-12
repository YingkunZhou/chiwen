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
    val read = Vec(nInst, Vec(2, new ReadIO(data_width,  log2Ceil(nPhyAddr))))
    val write = Vec(nCommit, new WriteIO(data_width, log2Ceil(nPhyAddr)))
    val debug = Vec(nCommit, new ReadIO(data_width, log2Ceil(nPhyAddr)))
  })

  val regfile = SyncReadMem(nPhyAddr, UInt(data_width.W))
  for (i <- 0 until nInst; j <- 0 until 2) {
    io.read(i)(j).data := regfile(io.read(i)(j).addr)
  }

  for (i <- 0 until nCommit) {
    io.debug(i).data := regfile(io.debug(i).addr)
    when (io.write(i).valid) {
      regfile(io.write(i).addr) := io.write(i).data
    } //no write after write hazard
  }
}
