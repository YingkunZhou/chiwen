package bian

import chisel3._
import chisel3.util.{Cat, Fill}
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

  val dec_mispredict = Wire(Vec(conf.nInst, Bool()))
  val error_pred = Wire(Vec(conf.nInst, Bool()))
  io.back.split := dec_mispredict(0) || error_pred(0)

  val if_kill = Pulse(Mux(dec_isbj(0) , dec_mispredict(0), dec_mispredict(1)),
                      Mux(dec_isbj(0) , io.back.forward(0), io.back.forward(1)))
  val dec_kill = Pulse(io.back.pred.cancel, io.back.forward(1)) //FIXME ??? is it right???
  val rectify_tgt = Wire(UInt(conf.data_width.W))
  val btb_error  = (0 until conf.nInst).map(i => Pulse(error_pred(i), io.back.forward(i))) //FIXME!! how to deal with btb error elegent???
  val if_reg_pc  = RegInit(START_ADDR)
  val if_next_pc =
        Mux(io.back.xcpt.valid, io.back.xcpt.bits,
        Mux(io.back.kill,       io.back.feedBack.bits.tgt,
        Mux(dec_kill,           rectify_tgt,
        Mux(btb_error(0),       Cat(fetchi.dec_pc(0)(conf.data_width-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W)),
        Mux(if_kill,            ras.peek,
        Mux(btb_error(1),       fetchi.dec_pc(1) + 4.U,
        Mux(btb.split,          btb.predict(0).tgt,
        /*predictor*/           btb.predict(1).tgt)))))))

  when (fetchi.pc_forward) { if_reg_pc := if_next_pc }

  fetchi.mem      <> io.mem
  fetchi.pc       := if_reg_pc
  fetchi.pc_split := btb.split
  fetchi.if_btb   := btb.predict
  fetchi.if_kill  := io.back.kill || io.back.xcpt.valid || if_kill || dec_kill
  fetchi.dec_kill := io.back.kill || io.back.xcpt.valid || dec_kill
  fetchi.forward(0) := io.back.forward(0)
  fetchi.forward(1) := io.back.forward(1) && !dec_isbj.reduce(_&&_)

  io.back.pc   := fetchi.dec_pc
  io.back.inst := fetchi.inst
  val dec_btb_tgt = Wire(Vec(conf.nInst, UInt(conf.data_width.W)))
  for (i <- 0 until conf.nInst) {
    microDec(i).inst  := fetchi.inst(i).bits
    dec_btb_tgt(i) := Mux(dec_mispredict(i), ras.peek, fetchi.dec_btb(i).tgt)
    dec_mispredict(i) := fetchi.inst(i).valid && microDec(i).jump(Jump.pop) && ras.peek =/= fetchi.dec_btb(i).tgt
    error_pred(i) := fetchi.inst(i).valid && !microDec(i).is_bj && fetchi.dec_btb(i).redirect
  }

  val bj_reg = Reg(new Bundle {
    val redirect = Bool() // = 0 cont || = 1 jump
    val tgt = UInt(conf.data_width.W)
    val branch = Bool()
    val jump   = UInt(Jump.NUM.W)
    val bj_sel = Vec(2, Bool()) //determine pick which btb
    val is_jal = Bool()
    val inst = UInt(conf.inst_width.W)
    val pc   = UInt(conf.data_width.W)
  })
  when (io.back.forward(1)) {
    bj_reg.redirect := Mux(dec_isbj(0), fetchi.dec_btb(0).redirect, fetchi.dec_btb(1).redirect)
    bj_reg.tgt := Mux(dec_isbj(0), dec_btb_tgt(0), dec_btb_tgt(1))
    bj_reg.jump   := Mux(dec_isbj(0), microDec(0).jump, microDec(1).jump)
    bj_reg.branch := Mux(dec_isbj(0), microDec(0).isbrnch, microDec(1).isbrnch)
    bj_reg.bj_sel := dec_isbj
    bj_reg.is_jal := Mux(dec_isbj(0), microDec(0).isjal, microDec(1).isjal)
    bj_reg.pc   := Mux(dec_isbj(0), fetchi.dec_pc(0), fetchi.dec_pc(1))
    bj_reg.inst := Mux(dec_isbj(0), fetchi.inst(0).bits, fetchi.inst(1).bits)
  }
  // sign-extend immediates
  val sext_imm = Mux(bj_reg.branch,
    Cat(Fill(20, bj_reg.inst(31)), bj_reg.inst(7), bj_reg.inst(30, 25), bj_reg.inst(11,8), 0.U(1.W)),
    Cat(Fill(12, bj_reg.inst(31)), bj_reg.inst(19,12), bj_reg.inst(20), bj_reg.inst(30,21), 0.U(1.W)))
  rectify_tgt := bj_reg.pc + sext_imm
  io.back.pred.cancel := (bj_reg.redirect || bj_reg.is_jal) && rectify_tgt =/= bj_reg.tgt //around 24 gates
  io.back.pred.redirect := Mux(io.back.pred.cancel, true.B, bj_reg.redirect)
  io.back.pred.tgt := Mux(bj_reg.branch || bj_reg.is_jal, rectify_tgt, bj_reg.tgt)
  io.back.pred.jump   := bj_reg.jump
  io.back.pred.branch := bj_reg.branch
  io.back.pred.bj_sel := bj_reg.bj_sel
  io.back.pred.is_jal := bj_reg.is_jal

  ras.pop  := io.back.ras_pop
  ras.push := io.back.ras_push

  btb.if_pc := if_reg_pc
  btb.fb_pc := io.back.fb_pc
  btb.fb_type  := io.back.fb_type
  btb.raspeek  := ras.peek
  btb.feedBack := io.back.feedBack
}
