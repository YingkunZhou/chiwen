//**************************************************************************
// RISCV Processor Register File
//--------------------------------------------------------------------------
//

package rv32_5stage

import chisel3._
import common.CPUConfig

class RFileIo(implicit val conf: CPUConfig) extends Bundle()
{
   val rs1_addr = Input(UInt(5.W))
   val rs1_data = Output(UInt(conf.xprlen.W))
   val rs2_addr = Input(UInt(5.W))
   val rs2_data = Output(UInt(conf.xprlen.W))
    
   val waddr    = Input(UInt(5.W))
   val wdata    = Input(UInt(conf.xprlen.W))
   val wen      = Input(Bool())
}

class RegisterFile(implicit val conf: CPUConfig) extends Module
{
   val io = IO(new RFileIo())

   val regfile = Mem(32,UInt(conf.xprlen.W))

   when (io.wen && (io.waddr =/= 0.U))
   {
      regfile(io.waddr) := io.wdata
   }

   io.rs1_data := Mux((io.rs1_addr =/= 0.U), regfile(io.rs1_addr), 0.U)
   io.rs2_data := Mux((io.rs2_addr =/= 0.U), regfile(io.rs2_addr), 0.U)
       
}
