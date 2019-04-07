package bian

import chisel3._
import chisel3.util.{Cat, Fill}
import common.{AxiIO, CPUConfig}

class Predict(val data_width: Int) extends Bundle {
  val redirect = Bool() // = 0 cont || = 1 jump
  val tgt = UInt(data_width.W)
}

class PredictInfo(data_width: Int) extends Predict(data_width) {
  val branch  = Bool()
  val brchjr  = Vec(2, Bool()) //determine pick which btb
  val rectify = Vec(2, Bool())
  val is_jal  = Bool()
  val split   = Bool() //mainly caused by jal
}

class PredictReg(val inst_width: Int, data_width: Int)
  extends PredictInfo(data_width) {
  val pc = UInt(data_width.W)
  val imm = UInt((inst_width-7).W)
  val update = Bool()
}

class BrjrEntryIO(val id_width: Int, data_width: Int)
  extends Predict(data_width) {
  val id = UInt(id_width.W)
  val pc = UInt(data_width.W)
  val cont = UInt(data_width.W)
  val branch = Bool()
  val brtype = UInt(BR_N.getWidth.W)
}

class FrontEnd(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    val cyc      = Input(UInt(conf.xprlen.W))
    val mem      = new AxiIO(conf.xprlen)
    val back     = new InterfaceIO(conf.xprlen)
  })
  /*TODO List
  * add more complex predict strategy
  * */
  val btb      = Module(new BTB()).io
  val fetchi   = Module(new FetchInst()).io
  btb.cyc    := io.cyc
  fetchi.cyc := io.cyc
  val microDec = Array.fill(2)(Module(new MicroDecoder(conf.inst_width)).io)
  for (i <- 0 until 2) microDec(i).inst := fetchi.inst(i).bits
  val dec_isbj = (0 until conf.nInst).map(i => fetchi.inst(i).valid && microDec(i).is_bj)
  val dec_priv = fetchi.inst(0).valid && microDec(0).privil
  val rectify = Wire(new Bundle {
    val valid = Bool()
    val noteq = Bool()
    val imm = UInt(conf.data_width.W)
    val tgt = UInt(conf.data_width.W)
  })
  val dec_kill = Pulse(rectify.valid, io.back.forward(1)) //FIXME ??? is it right???
  val btb_error = Wire(Vec(conf.nInst, Bool()))
  val dec_btb_error  = Wire(Vec(2, Bool()))
  val if_reg_pc   = RegInit(START_ADDR)
  val if_next_pc  =
        Mux(io.back.xcpt.valid, io.back.xcpt.bits,
        Mux(io.back.kill,       io.back.feedback.bits.tgt,
        Mux(dec_kill,           rectify.tgt,
        Mux(dec_btb_error(0),   fetchi.dec_pc(1),
        Mux(dec_btb_error(1),   fetchi.dec_pc(1) + 4.U,
        Mux(btb.split,          btb.predict(0).tgt,
        /*predictor*/           btb.predict(1).tgt))))))

  btb_error := (0 until conf.nInst).map(i => fetchi.inst(i).valid && !microDec(i).is_bj && fetchi.dec_btb(i).redirect)
  dec_btb_error := (0 until conf.nInst).map(i => Pulse(btb_error(i), io.back.forward(i)))

  when (fetchi.pc_forward) { if_reg_pc := if_next_pc }
  fetchi.mem      <> io.mem
  fetchi.pc       := if_reg_pc
  fetchi.pc_split := btb.split
  fetchi.if_btb   := btb.predict
  fetchi.if_kill  := io.back.kill || io.back.xcpt.valid || dec_kill || dec_btb_error.reduce(_||_)
  fetchi.dec_kill := io.back.kill || io.back.xcpt.valid || dec_kill
  fetchi.inst_split := dec_btb_error(0)
  fetchi.forward(0) := io.back.forward(0)
  fetchi.forward(1) := io.back.forward(1) && !dec_isbj.reduce(_&&_) && !dec_priv

  io.back.pc   := fetchi.dec_pc
  io.back.inst := fetchi.inst

  val pred_reg = Reg(new PredictReg(conf.inst_width, conf.data_width))
  val calc_tgt = (0 until 2).map(i => microDec(i).isjal ||
    (microDec(i).isbrnch && fetchi.dec_btb(i).redirect))
  val rectify_reg = RegInit(VecInit(Seq.fill(2)(false.B)))
  when (io.back.forward(1)) {
    pred_reg.rectify := btb_error
    pred_reg.brchjr  := (0 until conf.nInst).map(i => microDec(i).is_bj)

    pred_reg.branch   := Mux(dec_isbj(0), microDec(0).isbrnch, microDec(1).isbrnch)
    pred_reg.is_jal   := Mux(dec_isbj(0), microDec(0).isjal, microDec(1).isjal)
    pred_reg.tgt      := Mux(dec_isbj(0), fetchi.dec_btb(0).tgt, fetchi.dec_btb(1).tgt)
    pred_reg.redirect := Mux(dec_isbj(0), fetchi.dec_btb(0).redirect, dec_isbj(1) && fetchi.dec_btb(1).redirect)

    pred_reg.imm      := Mux(dec_isbj(0), fetchi.inst(0).bits(31,7), fetchi.inst(1).bits(31,7))
    pred_reg.pc       := Mux(dec_isbj(0), fetchi.dec_pc(0), fetchi.dec_pc(1))
    pred_reg.update   := Mux(dec_isbj(0), microDec(0).brchj, fetchi.inst(1).valid && microDec(1).brchj)
    rectify_reg(1)    := Mux(dec_isbj(0), calc_tgt(0), fetchi.inst(1).valid && calc_tgt(1))

    rectify_reg(0) := fetchi.inst(0).valid && calc_tgt(0)
    pred_reg.split := dec_isbj(0) && fetchi.dec_btb(0).redirect && fetchi.inst(1).valid
  }

  rectify.imm := Mux(pred_reg.branch,
    Cat(Fill(20, pred_reg.imm(31-7)), pred_reg.imm(7-7), pred_reg.imm(30-7, 25-7), pred_reg.imm(11-7, 8-7), 0.U(1.W)),
    Cat(Fill(12, pred_reg.imm(31-7)), pred_reg.imm(19-7,12-7), pred_reg.imm(20-7), pred_reg.imm(30-7,21-7), 0.U(1.W)))
  rectify.tgt := pred_reg.pc + rectify.imm
  rectify.noteq := rectify.tgt =/= pred_reg.tgt
  rectify.valid := rectify_reg(1) &&  rectify.noteq //around 24 gates

  io.back.pred.split      := rectify_reg(0) && rectify.noteq
  io.back.pred.tgt        := Mux(pred_reg.update, rectify.tgt, pred_reg.tgt)
  io.back.pred.redirect   := pred_reg.redirect   || rectify.valid
  io.back.pred.rectify(0) := pred_reg.rectify(0) || io.back.pred.split
  io.back.pred.rectify(1) := pred_reg.rectify(1) || rectify.valid
  io.back.pred.brchjr     := pred_reg.brchjr
  io.back.pred.branch     := pred_reg.branch
  io.back.pred.is_jal     := pred_reg.is_jal
  io.back.pc_split        := pred_reg.split

  btb.if_pc    := if_reg_pc
  btb.fb_pc    := io.back.fb_pc
  btb.fb_type  := io.back.fb_type
  btb.feedBack := io.back.feedback
}
