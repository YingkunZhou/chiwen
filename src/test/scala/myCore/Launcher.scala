package myCore

import chisel3.iotesters.{Driver, TesterOptionsManager}
import common.CPUConfig
import utils.TutorialRunner

object Launcher {
  val args = Array("--display-base", "16")
  implicit val conf = CPUConfig()
  val tests = Map(
    "Decoder" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new OneHotDecoder, manager) {
        c => new DecoderTest(c)
      }
    },
    "Encoder" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new OneHotEncoder(4), manager) {
        c => new EncoderTest(c)
      }
    }
  )

  def main(args: Array[String]): Unit = {
    TutorialRunner("myCore", tests, args)
  }
}
