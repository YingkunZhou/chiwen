package fuxi

import chisel3._
import chisel3.util.Cat
import common.CPUConfig

class FrontEnd(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    val cyc      = Input(UInt(conf.xprlen.W))
    val mem      = new AxiIO(conf.xprlen)
    val back     = new InterfaceIO(conf.xprlen)
  })

  val btb      = Module(new BTB()).io
  val ras      = Module(new RAS(nRAS)).io
  val fetchi   = Module(new FetchInst()).io
  val microDec = Array.fill(2)(Module(new MicroDecoder(conf.inst_width)).io)
  val dec_isbj = (0 until conf.nInst).map(i => fetchi.inst(i).valid && microDec(i).is_bj)
  btb.cyc    := io.cyc
  fetchi.cyc := io.cyc

  val mispredict = Wire(Vec(conf.nInst, Bool()))
  val error_pred = Wire(Vec(conf.nInst, Bool()))
  io.back.split := mispredict(0) || error_pred(0)
  val if_kill = Pulse(Mux(dec_isbj(0) , mispredict(0), mispredict(1)),
                      Mux(dec_isbj(0) , io.back.forward(0), io.back.forward(1)))
  val btb_error  = (0 until conf.nInst).map(i => Pulse(error_pred(i), io.back.forward(i))) //FIXME!! how to deal with btb error elegent???
  val if_reg_pc  = RegInit(START_ADDR)
  val if_next_pc =
        Mux(io.back.xcpt.valid, io.back.xcpt.bits,
        Mux(io.back.kill,       io.back.feedBack.tgt,
        Mux(btb_error(0),       Cat(fetchi.dec_pc(0)(conf.data_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W)),
        Mux(if_kill,            ras.peek,
        Mux(btb_error(1),       fetchi.dec_pc(1) + 4.U,
        Mux(btb.split,          btb.predict(0).tgt,
        /*predictor*/           btb.predict(1).tgt))))))

  when (fetchi.pc_forward) { if_reg_pc := if_next_pc }

  fetchi.mem      <> io.mem
  fetchi.pc       := if_reg_pc
  fetchi.pc_split := btb.split
  fetchi.if_btb   := btb.predict
  fetchi.if_kill  := io.back.kill || io.back.xcpt.valid || if_kill
  fetchi.dec_kill := io.back.kill || io.back.xcpt.valid
  fetchi.forward(0) := io.back.forward(0)
  fetchi.forward(1) := io.back.forward(1) && !dec_isbj.reduce(_&&_)

  io.back.pc   := fetchi.dec_pc
  io.back.inst := fetchi.inst
  val dec_btb_tgt = Wire(Vec(conf.nInst, UInt(conf.data_width.W)))
  for (i <- 0 until conf.nInst) {
    microDec(i).inst  := fetchi.inst(i).bits
    io.back.bj_sel(i) := microDec(i).is_bj
    dec_btb_tgt(i) := Mux(mispredict(i), ras.peek, fetchi.dec_btb(i).tgt)
    mispredict(i) := fetchi.inst(i).valid && microDec(i).jump(Jump.pop) && ras.peek =/= fetchi.dec_btb(i).tgt
    error_pred(i) := fetchi.inst(i).valid && !microDec(i).is_bj && fetchi.dec_btb(i).redirect
  }

  io.back.branch   := Mux(dec_isbj(0), microDec(0).isbrnch, microDec(1).isbrnch)
  io.back.jump     := Mux(dec_isbj(0), microDec(0).jump, microDec(1).jump)
  io.back.pred.tgt := Mux(dec_isbj(0), dec_btb_tgt(0), dec_btb_tgt(1))
  io.back.pred.typ := Mux(dec_isbj(0), fetchi.dec_btb(0).typ, fetchi.dec_btb(1).typ)
  io.back.pred.you := Mux(dec_isbj(0), fetchi.dec_btb(0).you, fetchi.dec_btb(1).you)
  io.back.pred.idx := Mux(dec_isbj(0), fetchi.dec_btb(0).idx, fetchi.dec_btb(1).idx)
  io.back.pred.redirect := Mux(dec_isbj(0), fetchi.dec_btb(0).redirect, fetchi.dec_btb(1).redirect)

  ras.pop := io.back.ras_pop
  ras.push := io.back.ras_push

  btb.if_pc := if_reg_pc
  btb.fb_pc := io.back.fb_pc
  btb.raspeek  := ras.peek
  btb.feedBack := io.back.feedBack
}
