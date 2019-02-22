package myCore

import chisel3._
import chisel3.util.Valid
import common.CPUConfig


class InterfaceIO(val inst_width: Int, val data_width: Int) extends Bundle {
  val xcpt     = Input(new Valid(UInt(data_width.W)))
  val if_kill  = Input(Bool())
  val dec_kill = Input(Bool())
  val forward  = Input(Bool())

  val inst     = Output(Vec(2, Valid(UInt(inst_width.W))))
  val pc       = Output(Vec(2, UInt(data_width.W)))
  val pred     = Output(Vec(2, new Predict(data_width)))

  val rasIO    = new RasIO(data_width)
  val feedBack = Input(new FeedBack(data_width))
}
//
//class FrontEnd (implicit conf: CPUConfig) extends Module with BTBParams {
//  val io = IO(new Bundle{
//    val cyc      = Input(UInt(conf.xprlen.W))
//    val mem      = new AxiIO(conf.xprlen)
//    val back     = new InterfaceIO(conf.inst_width, conf.data_width)
//  })
//
//  val btb        = Module(new BTB())
//  val ras        = Module(new RAS(nRAS))
//  val fetchi     = Module(new FetchInst())
//  btb.io.cyc    := io.cyc
//  fetchi.io.cyc := io.cyc
//
//  val if_reg_pc  = RegInit(START_ADDR)
//  val if_pc_next = Wire(UInt(conf.xprlen.W))
//  if (conf.hasBTB) {
//    if_pc_next :=
//      Mux(io.back.xcpt.valid, io.back.xcpt.bits,
//      Mux(io.back.dec_kill,   io.back.feedBack.target,
//      Mux(io.back.if_kill,    ras.io.peek,
//      /*predictor*/           btb.io.predict.Tg)))
//  } else {
//    val if_pc_plus: UInt = if_reg_pc + conf.pcInc.asUInt(conf.xprlen.W)
//    if_pc_next :=
//      Mux(io.back.xcpt.valid, io.back.xcpt.bits,
//      Mux(io.back.dec_kill,   io.back.feedBack.target,
//      if_pc_plus))
//  }
//
//  when (fetchi.io.pc_forward) { if_reg_pc := if_pc_next }
//
//  fetchi.io.mem      <> io.mem
//  fetchi.io.pc       := if_reg_pc
//  fetchi.io.if_btb   := btb.io.predict
//  fetchi.io.if_kill  := io.back.if_kill  || io.back.xcpt.valid
//  fetchi.io.dec_kill := io.back.dec_kill || io.back.xcpt.valid
//  fetchi.io.forward  := io.back.forward
//
//  io.back.inst := fetchi.io.inst
//  io.back.pc   := fetchi.io.dec_pc
//  io.back.pred := fetchi.io.dec_btb
//
//  ras.io <> io.back.rasIO
//
//  btb.io.pc       := if_reg_pc
//  btb.io.peekRAS  := ras.io.peek
//  btb.io.feedBack <> io.back.feedBack
//}


