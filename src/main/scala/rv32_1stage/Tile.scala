//**************************************************************************
// RISCV Processor Tile
//--------------------------------------------------------------------------
//

package rv32_1stage
{

import chisel3._
import common.{AsyncScratchPadMemory, CPUConfig, DMIIO, DebugModule}

class Tile(implicit val conf: CPUConfig) extends Module
{
   val io = IO(new Bundle {
      val dmi = Flipped(new DMIIO())
   })

   // notice that while the core is put into reset, the scratchpad needs to be
   // alive so that the Debug Module can load in the program.
   val debug = Module(new DebugModule())
   val core   = Module(new Core())
   core.io := DontCare
   val memory = Module(new AsyncScratchPadMemory(num_core_ports = 2))
   core.io.dmem <> memory.io.core_ports(0)
   core.io.imem <> memory.io.core_ports(1)
   debug.io.debugmem <> memory.io.debug_port
   core.reset := debug.io.resetcore | reset.toBool
   debug.io.dmi <> io.dmi
}

}