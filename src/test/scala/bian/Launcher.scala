package bian

import chisel3.iotesters.{Driver, TesterOptionsManager}
import common.CPUConfig
import utils.TutorialRunner

object Launcher {
  val args = Array("--display-base", "16")
  implicit val conf = CPUConfig()
  val tests = Map(
    "State" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new StateCtrl, manager) {
        c => new StateTest(c)
      }
    },
    "BTB" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new BTB, manager) {
        c => new BTBTest(c)
      }
    },
    "Frontqueue" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new FrontQueue, manager) {
        c => new FrontqueueTest(c)
      }
    },
    "BranchJump" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new BranchJump, manager) {
        c => new BJTest(c)
      }
    },
    "LoadStore" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new LoadStore, manager) {
        c => new LSTest(c)
      }
    }
  )

  def main(args: Array[String]): Unit = {
    TutorialRunner("bian", tests, args)
  }
}
