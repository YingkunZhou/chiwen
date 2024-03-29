//**************************************************************************
// RISCV Processor 
//--------------------------------------------------------------------------
 
package rv32_5stage

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
   val c  = Module(new CtlPath())
   val d  = Module(new DatPath())
   
   c.io.ctl  <> d.io.ctl
   c.io.dat  <> d.io.dat
   
   io.imem <> c.io.imem
   io.imem <> d.io.imem
   io.imem.req.valid := c.io.imem.req.valid

   io.dmem <> c.io.dmem
   io.dmem <> d.io.dmem
   
}