package bian

import chisel3._
import chisel3.util.{Cat, Fill}
import common.{AxiIO, CPUConfig}

class PredictInfo(data_width: Int) extends Predict(data_width) {
  val brchjr  = Vec(2, Bool()) //determine pick which btb
  val rectify = Vec(2, Bool())
  val branch  = Bool()
  val is_jal  = Bool()
  def Brchjr(i: Int): Bool = brchjr(i) && !is_jal
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
//  val ras      = Module(new RAS(nRAS)).io
  val btb      = Module(new BTB).io
  val fetchi   = Module(new FetchInst).io
  val microDec = Array.fill(2)(Module(new MicroDecoder(conf.inst_width)).io)
  btb.cyc    := io.cyc
  fetchi.cyc := io.cyc

  val calc_tgt = Wire(Vec(conf.nInst, Bool()))
  val dec_isbj = Wire(Vec(conf.nInst, Bool()))
  val btb_miss = Wire(Vec(conf.nInst, Bool()))
  val rectify = Wire(new Bundle {
    val valid = Vec(2, Bool())
    val miss = Bool()
    val imm = UInt(conf.data_width.W)
    val tgt = UInt(conf.data_width.W)
  })
  val dec_kill = Pulse(rectify.valid.reduce(_||_), io.back.forward(1)) //FIXME ??? is it right???
  val dec_miss = (0 until conf.nInst).map(i => Pulse(btb_miss(i), io.back.forward(i)))
  val if_reg_pc   = RegInit(START_ADDR)
  val if_next_pc  =
        Mux(io.back.xcpt.valid, io.back.xcpt.bits,
        Mux(io.back.kill,  io.back.feedback.bits.tgt,
        Mux(dec_kill,      rectify.tgt,
        Mux(dec_miss(0),   fetchi.dec_pc(1),
        Mux(dec_miss(1),   fetchi.dec_pc(1) + 4.U,
        Mux(btb.split,     btb.predict(0).bits.tgt,
        /*predictor*/      btb.predict(1).bits.tgt))))))

  when (fetchi.pc_forward) { if_reg_pc := if_next_pc }

  btb.if_pc    := if_reg_pc
  btb.fb_pc    := io.back.fb_pc
  btb.fb_type  := io.back.fb_type
  btb.feedBack := io.back.feedback

  fetchi.mem      <> io.mem
  fetchi.pc       := if_reg_pc
  fetchi.pc_split := btb.split
  fetchi.if_btb   := btb.predict
  fetchi.if_kill  := io.back.kill || io.back.xcpt.valid || dec_kill || dec_miss.reduce(_||_)
  fetchi.dec_kill := io.back.kill || io.back.xcpt.valid || dec_kill
  fetchi.inst_split := dec_miss(0)
  fetchi.forward(0) := io.back.forward(0)
  fetchi.forward(1) := io.back.forward(1) && !dec_isbj.reduce(_&&_)

  for (i <- 0 until conf.nInst) {
    microDec(i).inst := fetchi.inst(i).bits
//    calc_tgt(i) := microDec(i).isjal || (microDec(i).branch &&
//    (fetchi.dec_btb(i).bits.redirect || !fetchi.dec_btb(i).valid))
    calc_tgt(i) := microDec(i).isjal || (microDec(i).branch && fetchi.dec_btb(i).bits.redirect)

    dec_isbj(i) := fetchi.inst(i).valid &&  microDec(i).is_bj
    btb_miss(i) := fetchi.inst(i).valid && !microDec(i).is_bj && fetchi.dec_btb(i).bits.redirect
    io.back.inst(i).bits  := fetchi.inst(i).bits
    io.back.inst(i).valid := fetchi.inst(i).valid && !fetchi.dec_kill
  }

  val pred_valid = RegInit(VecInit(Seq.fill(conf.nInst)(false.B)))
  val pred_reg = Reg(new PredictReg(conf.inst_width, conf.data_width))
  val rectify_reg = Reg(Vec(conf.nInst, Bool()))

  when (io.back.forward(1)) {
    pred_valid  := io.back.inst.map(_.valid)
    rectify_reg := calc_tgt

    pred_reg.brchjr  := microDec.map(_.is_bj)
    pred_reg.rectify := btb_miss

    pred_reg.branch   := Mux(dec_isbj(0), microDec(0).branch, microDec(1).branch)
    pred_reg.is_jal   := Mux(dec_isbj(0), microDec(0).isjal , microDec(1).isjal)
    pred_reg.tgt      := Mux(dec_isbj(0), fetchi.dec_btb(0).bits.tgt, fetchi.dec_btb(1).bits.tgt)
    pred_reg.redirect := Mux(dec_isbj(0), fetchi.dec_btb(0).bits.redirect,
                             dec_isbj(1) && fetchi.dec_btb(1).bits.redirect)

    pred_reg.update := Mux(dec_isbj(0), microDec(0).brchj,
                fetchi.inst(1).valid && microDec(1).brchj)

    pred_reg.imm    := Mux(dec_isbj(0), fetchi.inst(0).bits(31,7), fetchi.inst(1).bits(31,7))
    pred_reg.pc     := Mux(dec_isbj(0), fetchi.dec_pc(0), fetchi.dec_pc(1))

    pred_reg.split  := fetchi.inst_split
  }

  rectify.imm   := Mux(pred_reg.branch,
    Cat(Fill(20, pred_reg.imm(24)), pred_reg.imm(0), pred_reg.imm(23, 18), pred_reg.imm(4, 1), 0.U(1.W)),
    Cat(Fill(12, pred_reg.imm(24)), pred_reg.imm(12,5), pred_reg.imm(13), pred_reg.imm(23,14), 0.U(1.W)))
  rectify.tgt   := pred_reg.pc + rectify.imm
  rectify.miss  := rectify.tgt =/= pred_reg.tgt
  rectify.valid := (0 until conf.nInst).map(i => pred_valid(i) && rectify_reg(i) && rectify.miss) //around 24 gates

  io.back.pred.tgt      := Mux(pred_reg.update, rectify.tgt, pred_reg.tgt)
  io.back.pred.redirect := pred_reg.redirect || rectify.valid.reduce(_||_)
  io.back.pred.rectify  := (0 until 2).map(i => (pred_reg.rectify(i) && pred_valid(i)) || rectify.valid(i))
  io.back.pred.brchjr   := pred_reg.brchjr
  io.back.pred.branch   := pred_reg.branch
  io.back.pred.is_jal   := pred_reg.is_jal
  io.back.pred.split    := rectify.valid(0)
  io.back.inst_split    := (pred_valid(0) && pred_reg.brchjr.reduce(_&&_)) || pred_reg.split
  //if both is brchjr inst or dec stage back inst occur split case then inst split it
  io.back.pc_split      := pred_valid.reduce(_&&_) && pred_reg.brchjr(0) && pred_reg.redirect

}
