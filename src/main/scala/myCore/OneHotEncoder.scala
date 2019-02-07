package myCore

import chisel3._
import chisel3.util.log2Ceil

class OneHotEncoder(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(w, Bool()))
    val out = Output(UInt(log2Ceil(w).W))
  })

  io.out := io.in.indices.map(i => VecInit(Seq.fill(log2Ceil(w)){ io.in(i) }).asUInt() & i.U).reduce(_ | _)
//  printf(p"result = ${io.out}\n")
}
