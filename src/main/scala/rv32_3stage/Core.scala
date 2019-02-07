//**************************************************************************
// RISCV Processor 
//--------------------------------------------------------------------------

package rv32_3stage

import chisel3._
import common.{CPUConfig, MemPortIo}

class CoreIo(implicit val conf: CPUConfig) extends Bundle
{
  val imem = new MemPortIo(conf.xprlen)
  val dmem = new MemPortIo(conf.xprlen)
}

class Core(implicit val conf: CPUConfig) extends Module
{
   val io = IO(new CoreIo())

   val frontend = Module(new FrontEnd())
   val cpath  = Module(new CtlPath())
   val dpath  = Module(new DatPath())

   frontend.io.imem <> io.imem
   frontend.io.cpu <> cpath.io.imem
   frontend.io.cpu <> dpath.io.imem
   frontend.io.cpu.req.valid := cpath.io.imem.req.valid

   cpath.io.ctl  <> dpath.io.ctl
   cpath.io.dat  <> dpath.io.dat
   
   cpath.io.dmem <> io.dmem
   dpath.io.dmem <> io.dmem
}
