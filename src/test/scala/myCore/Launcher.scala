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
      Driver.execute(() => new OneHotEncoder(16), manager) {
        c => new EncoderTest(c)
      }
    },
    "RAS" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new RAS(8), manager) {
        c => new RASTest(c)
      }
    },
    "LRU" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new LRU(4), manager) {
        c => new LRUTest(c)
      }
    },
    "Predictor" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new Predictor, manager) {
        c => new PredTest(c)
      }
    },
    "Ccore" -> {manager: TesterOptionsManager =>
      Driver.execute(() => new CaheCore(4,6,20), manager) {
        c => new CCoreText(c)
      }
    }
  )

  def main(args: Array[String]): Unit = {
    TutorialRunner("myCore", tests, args)
  }
}
