package common
import chisel3._
case class CPUConfig()
{
  val xprlen = 32
  val pcInc = 4
  val nInst = pcInc >> 2
  val hasbrJPredictor = true
  val delayFechinst   = true
  val pcLSB = 2
  val incRd = 0.U(4.W)
  val iccRd = 1.U(4.W)
}
