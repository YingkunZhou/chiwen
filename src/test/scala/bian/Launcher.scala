package bian

import chisel3.iotesters.{Driver, TesterOptionsManager}
import common.CPUConfig
import utils.TutorialRunner

object Launcher {
  val args = Array("--display-base", "16")
  implicit val conf = CPUConfig()
  val tests = Map(
    "State" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new StateCtrl(conf.data_width), manager) {
        c => new StateTest(c)
      }
    }
  )

  def main(args: Array[String]): Unit = {
    TutorialRunner("bian", tests, args)
  }
}
