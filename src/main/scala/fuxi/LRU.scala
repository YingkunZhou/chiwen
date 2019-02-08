package fuxi

import chisel3._
import chisel3.util._

class LRU(n: Int) extends Module {
  val io = IO(new Bundle {
    val oldest = Output(UInt(log2Ceil(n).W))
    val newest = Flipped(new ValidIO(UInt(log2Ceil(n).W)))
  })

  val bits = RegInit(VecInit(Seq.fill((n-1)*n/2)(false.B)))
  val cmp = Wire(Vec(n-1, Bool()))
  require(n == 4)
  if (n == 4) {
    cmp(0) := (!bits(0) &&  bits(1+2) &&  bits(1+3))
    cmp(1) := (!bits(1) && !bits(3)   &&  bits(2+3))
    cmp(2) := (!bits(2) && !bits(4)   && !bits(5))
    io.oldest := Cat(cmp(2)||cmp(1), cmp(2)||cmp(0))

    when (io.newest.bits === 0.U && io.newest.valid) { bits(0) := false.B; bits(1) := false.B; bits(2) := false.B; }
    when (io.newest.bits === 1.U && io.newest.valid) { bits(0) := true.B;  bits(3) := false.B; bits(4) := false.B; }
    when (io.newest.bits === 2.U && io.newest.valid) { bits(1) := true.B;  bits(3) := true.B;  bits(5) := false.B; }
    when (io.newest.bits === 3.U && io.newest.valid) { bits(2) := true.B;  bits(4) := true.B;  bits(5) := true.B;  }

//    printf(p"bits = ${bits(0)}, ${bits(1)}, ${bits(2)}, ${bits(3)}, ${bits(4)}, ${bits(5)}\n")
  }
}