//**************************************************************************
// RISCV Processor Register File
//--------------------------------------------------------------------------
//

package fuxi

import chisel3._
import common.CPUConfig

class Regfile(implicit val conf: CPUConfig) extends Module {
  val io = IO(new Bundle{
    val rs1_addr = Input(Vec(2, UInt(5.W)))
    val rs1_data = Output(Vec(2, UInt(conf.xprlen.W)))
    val rs2_addr = Input(Vec(2, UInt(5.W)))
    val rs2_data = Output(Vec(2, UInt(conf.xprlen.W)))

    val waddr    = Input(Vec(2, UInt(5.W)))
    val wdata    = Input(Vec(2, UInt(conf.xprlen.W)))
    val wen      = Input(Vec(2, Bool()))
  })

  val regfile = Mem(32,UInt(conf.xprlen.W))
  when (io.wen(0)) { regfile(io.waddr(0)) := io.wdata(0) }
  when (io.wen(1)) { regfile(io.waddr(1)) := io.wdata(1) } // FIXME: can it work ???

  for (i <- 0 until 2) {
    io.rs1_data(i) := Mux(io.rs1_addr(i) =/= 0.U, regfile(io.rs1_addr(i)), 0.U)
    io.rs2_data(i) := Mux(io.rs2_addr(i) =/= 0.U, regfile(io.rs2_addr(i)), 0.U)
  }
}
