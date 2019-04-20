package fuxi

import chisel3._
import common.{AxiIO, CPUConfig}

class FrontEnd(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    val cyc      = Input(UInt(conf.xprlen.W))
    val mem      = new AxiIO(conf.xprlen)
    val back     = new InterfaceIO(conf.xprlen)
  })
  val btb      = Module(new BTB()).io
  val fetchi   = Module(new FetchInst()).io
  val microDec = Array.fill(2)(Module(new MicroDecoder(conf.inst_width)).io)
  val dec_isbj = (0 until 2).map(i => fetchi.inst(i).valid && microDec(i).is_bj)
  val dec_branch = (0 until 2).map(i => fetchi.inst(i).valid && microDec(i).branch).reduce(_||_)
  btb.cyc    := io.cyc
  fetchi.cyc := io.cyc

  val dec_btb_error = Wire(Vec(2, Bool())) //btb predict error occur and dec stage find it
  val if_reg_pc  = RegInit(START_ADDR)
  val if_next_pc =
        Mux(io.back.xcpt.valid, io.back.xcpt.bits,
        Mux(io.back.kill,       io.back.feedBack.tgt,
        Mux(dec_btb_error(0),   fetchi.dec_pc(1),
        Mux(dec_btb_error(1),   fetchi.dec_pc(1) + 4.U,
        Mux(btb.split,          btb.predict(0).tgt,
        /*predictor*/           btb.predict(1).tgt)))))

  when (fetchi.pc_forward) { if_reg_pc := if_next_pc }

  fetchi.mem      <> io.mem
  fetchi.pc       := if_reg_pc
  fetchi.pc_split := btb.split
  fetchi.if_btb   := btb.predict
  fetchi.if_kill  := io.back.kill || io.back.xcpt.valid || dec_btb_error.reduce(_||_)
  fetchi.dec_kill := io.back.kill || io.back.xcpt.valid
  fetchi.forward(0) := io.back.forward(0)
  fetchi.forward(1) := io.back.forward(1) && !dec_isbj.reduce(_&&_)
  fetchi.inst_split := dec_btb_error(0)
  io.back.pc    := fetchi.dec_pc
  io.back.inst  := fetchi.inst
  io.back.split := dec_btb_error(0)

  for (i <- 0 until conf.nInst) {
    microDec(i).inst  := fetchi.inst(i).bits
    dec_btb_error(i)  := Pulse(fetchi.inst(i).valid && !microDec(i).is_bj && fetchi.dec_btb(i).redirect, io.back.forward(i))
  }

  io.back.bj_sel := microDec.map(_.is_bj)
  io.back.branch := Mux(dec_isbj(0), microDec(0).branch, microDec(1).branch)
  io.back.call   := Mux(dec_isbj(0), microDec(0).call, microDec(1).call)
  io.back.retn   := Mux(dec_isbj(0), microDec(0).retn, microDec(1).retn)
  io.back.pred   := Mux(dec_isbj(0), fetchi.dec_btb(0), fetchi.dec_btb(1))

  btb.if_pc.valid := fetchi.pc_forward
  btb.if_pc.bits  := if_reg_pc
  btb.fb_pc.valid := io.back.kill
  btb.fb_pc.bits  := io.back.fb_pc

  btb.branch   := dec_branch

  btb.fb_type  := io.back.fb_type
  btb.feedBack := io.back.feedBack
  btb.ras_push := io.back.ras_push
  btb.ras_pop  := io.back.ras_pop
}
