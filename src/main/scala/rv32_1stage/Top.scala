package rv32_1stage

import chisel3._
import common.{CPUConfig, SimDTM}

class Top extends Module
{
   val io = IO(new Bundle{
      val success = Output(Bool())
    })

   implicit val sodor_conf = CPUConfig()
   val tile = Module(new Tile)
   val dtm = Module(new SimDTM).connect(clock, reset.toBool, tile.io.dmi, io.success)
}


// FIXME: extends App or ChiselFlatSpec
object Top {
  def main(args: Array[String]): Unit = {
    chisel3.Driver.execute(args, () => new Top)
  }
}
