package bian

import chisel3._
import chisel3.util._
import common._

object Pulse {
  def apply(in: Bool, forward: Bool): Bool = {
    val in_latch = RegInit(true.B)
    when (forward) { in_latch := true.B
    }.elsewhen(in) { in_latch := false.B}
    in && in_latch
  }
}

class BackEnd(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val mem  = new MemPortIo(conf.xprlen)
    val cyc   = Output(UInt(conf.xprlen.W))
    val front = Flipped(new InterfaceIO(conf.xprlen))
  })

  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)
  val frontQueue = Module(new FrontQueue).io
  frontQueue.info_i := io.front.info
  for (i <- 0 until conf.nInst) {
    frontQueue.inst_i(i).bits := io.front.inst(i).bits
    io.front.forward(i) := frontQueue.forward
  }
  frontQueue.inst_i(0).valid := io.front.inst(0).valid
  frontQueue.inst_i(1).valid := io.front.inst(1).valid && io.front.split
}