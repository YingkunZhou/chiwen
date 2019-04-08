//**************************************************************************
// RISCV Processor Tile
//--------------------------------------------------------------------------
//

package chiwen

import chisel3._
import common._

class Tile(implicit val conf: CPUConfig) extends Module
{
  val io = IO(new Bundle {
    val dmi = Flipped(new DMIIO())
  })

  val core   = Module(new Core())
  core.io := DontCare
  val memory = Module(new AsyncScratchPadMemory(num_core_ports = 2))
  val debug = Module(new DebugModule())
  val trans = Module(new Transform())
  val simpleTrans = Module(new SimpleTrans())

  memory.io.core_ports(1) <> trans.io.outer
  core.io.imem <> trans.io.inner
  trans.io.cyc := core.io.cyc
  memory.io.core_ports(0) <> simpleTrans.io.outer
  core.io.dmem <> simpleTrans.io.inner
  simpleTrans.io.cyc := core.io.cyc
  debug.io.debugmem <> memory.io.debug_port

  core.reset := debug.io.resetcore | reset.toBool
  debug.io.dmi <> io.dmi
}
