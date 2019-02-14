package fuxi

import chisel3._
import chisel3.util.{Cat, log2Ceil}
import common.CPUConfig
import scala.math.pow

class MyMem(n: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val wen     = Input(Bool())
    val addr    = Input(UInt(log2Ceil(n).W))
    val wdata   = Input(UInt(w.W))
    val rdata   = Output(UInt(w.W))
  })

  val mem = SyncReadMem(n, UInt(w.W))
  io.rdata := mem(io.addr)
  when (io.wen) { mem(io.addr) := io.wdata }
}

class CaheCore(val wLines: Int, val wOffset: Int, val wTag: Int, val num: Int)(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle{
    val wen     = Input(Bool())
    val addr    = Input(UInt(conf.xprlen.W))
    val wdata   = Input(Vec(pow(2, wLines).toInt, UInt(conf.xprlen.W)))
    val wstatus = Input(Bool())
    val rdata   = Output(UInt((num * conf.xprlen).W))
    val rvalid  = Output(Bool())

  })
  require(num < 3)
  val nLine = pow(2, wLines).toInt / num
  val nOffset: Int = pow(2, wOffset).toInt
  val data_RAMs = Array.fill(nLine)(Module(new MyMem(nOffset, num * conf.xprlen)).io)
  val tag_RAM   = SyncReadMem(nOffset, UInt(wTag.W)) // TODO: is it faster than Mem and RegNext
  val statusVec = RegInit(VecInit(Seq.fill(nOffset)(false.B)))
  require(conf.pcLSB+wLines+wOffset+wTag == conf.xprlen)
  val tag_ctx  = io.addr(conf.xprlen-1, conf.xprlen-wTag)
  val off_idx  = io.addr(conf.pcLSB+wLines+wOffset-1, conf.pcLSB+wLines)

  val rdata = Wire(Vec(nLine, UInt((num * conf.xprlen).W)))
  // TODO: it is write after read, read data is the old value
  for(i <- 0 until nLine) {
    data_RAMs(i).addr  := off_idx
    data_RAMs(i).wen   := io.wen
    if (num == 1)   data_RAMs(i).wdata := io.wdata(i)
    else/*num == 2*/data_RAMs(i).wdata := Cat(io.wdata(2*i+1), io.wdata(2*i))
    rdata(i) := data_RAMs(i).rdata
  }

  val rtag = tag_RAM(off_idx)
  val rstatus = RegNext(statusVec(off_idx))

  when(io.wen) { tag_RAM(off_idx)   := io.addr(conf.xprlen-1, conf.xprlen-wTag) }
  when(io.wen) { statusVec(off_idx) := io.wstatus }

  val tag      = RegNext(tag_ctx)
  val line_idx = RegNext(io.addr(conf.pcLSB+wLines-1, conf.pcLSB+log2Ceil(num)))
  io.rdata  := rdata(line_idx)
  io.rvalid := rtag === tag && rstatus
}
