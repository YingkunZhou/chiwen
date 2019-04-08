package chiwen

import chisel3._
import common.{AxiIO, CPUConfig}

class FrontEnd (implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    val cyc      = Input(UInt(conf.xprlen.W))
    val mem      = new AxiIO(conf.xprlen)
    val back     = new InterfaceIO(conf.xprlen)
  })

  val btb      = Module(new BTB()).io
//  val ras      = Module(new RAS(nRAS)).io
  val fetchi   = Module(new FetchInst()).io
  val microDec = Module(new MicroDecoder(conf.inst_width)).io
  btb.cyc    := io.cyc
  fetchi.cyc := io.cyc

//  val mispredict = Wire(Bool())
//  val if_kill = Pulse(mispredict, forward = io.back.forward)

  val if_reg_pc  = RegInit(START_ADDR)
  val if_pc_next = Wire(UInt(conf.xprlen.W))
  if (conf.hasBTB) {
    if_pc_next :=
      Mux(io.back.xcpt.valid, io.back.xcpt.bits,
      Mux(io.back.kill,       io.back.feedBack.tgt,
//      Mux(if_kill,            ras.peek,
      /*predictor*/           btb.predict.tgt))//)
  } else {
    val if_pc_plus: UInt = if_reg_pc + 4.asUInt(conf.xprlen.W)
    if_pc_next :=
      Mux(io.back.xcpt.valid, io.back.xcpt.bits,
      Mux(io.back.kill,   io.back.feedBack.tgt,
      if_pc_plus))
  }

  when (fetchi.pc_forward) { if_reg_pc := if_pc_next }

  fetchi.mem      <> io.mem
  fetchi.if_pc    := if_reg_pc
  fetchi.if_btb   := btb.predict
  fetchi.if_kill  := io.back.kill || io.back.xcpt.valid //|| if_kill
  fetchi.dec_kill := io.back.kill || io.back.xcpt.valid
  fetchi.forward  := io.back.forward
  microDec.inst   := fetchi.inst.bits
//  val ras_pop = fetchi.inst.valid && microDec.jump(Jump.pop)
//  if (conf.hasBTB) mispredict := ras_pop && ras.peek =/= fetchi.dec_btb.tgt
//  else mispredict := false.B

  io.back.inst := fetchi.inst
  io.back.pc   := fetchi.dec_pc
//  io.back.jump := microDec.jump
  io.back.pred.redirect := fetchi.dec_btb.redirect
  io.back.pred.typ := fetchi.dec_btb.typ
//  io.back.pred.tgt := Mux(mispredict, ras.peek, fetchi.dec_btb.tgt)
  io.back.pred.tgt := fetchi.dec_btb.tgt
  io.back.pred.you := fetchi.dec_btb.you
  io.back.pred.idx := fetchi.dec_btb.idx

//  ras.pop := io.back.ras_pop
//  ras.push := io.back.ras_push
  btb.if_pc := if_reg_pc
  btb.fb_pc := io.back.fb_pc
//  btb.raspeek  := ras.peek
  btb.feedBack := io.back.feedBack

  if (conf.verbose) {
    when (io.cyc > 16427.U && io.cyc < 16475.U) {
      printf(p"FrontEnd: if_pc_next: ${Hexadecimal(if_pc_next)}, dec_kill: ${io.back.kill}\n")
    }
  }
}


