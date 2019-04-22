package bian

import chisel3._
import chisel3.util.{Cat, Fill}
import common.{AxiIO, CPUConfig, CycRange, Str}

class PredictInfo(data_width: Int)
  extends Predict(data_width) {
  val brchjr  = Vec(2, Bool()) //determine pick which btb
  val branch  = Bool()
  val is_jal  = Bool()
  def Brchjr(i: Int): Bool = brchjr(i) && !is_jal
  val split   = Bool() //cause by rectify
  val retn    = Bool()
}

class BrjrEntryIO(val id_width: Int, data_width: Int)
  extends Predict(data_width) {
  val id = UInt(id_width.W)
  val pc = UInt(data_width.W)
  val cont = UInt(data_width.W)
  val branch = Bool()
  val retn   = Bool()
  val brtype = UInt(BR_N.getWidth.W)
}

class FrontEnd(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle{
    val cyc      = Input(UInt(conf.xprlen.W))
    val mem      = new AxiIO(conf.xprlen)
    val back     = new InterfaceIO
  })
  /*TODO List
  * add more complex predict strategy
  * */
  val predict  = Module(new BTB).io
  val fetchi   = Module(new FetchInst).io
  val ftQueue  = Module(new FrontQueue).io
  predict.cyc  := io.cyc
  fetchi.cyc   := io.cyc
  ftQueue.cyc  := io.cyc
  ftQueue.xcpt := io.back.xcpt
  ftQueue.kill := io.back.kill
  io.back.inst <> ftQueue.inst
  io.back.pc   := ftQueue.pc
  io.back.pred := ftQueue.pred
  /*four case to rectify
  * 1. jal inst
  * 2. unpredict branch inst or branch target error
  * 3. btb error to cause not branch jump inst redirect
  * 4. retn inst not redirect
  * */
  val rectify = Wire(new Bundle {
    val valid = Vec(conf.nInst, Bool())
    val redir = Vec(conf.nInst, Bool())
    val imm = UInt(conf.data_width.W)
    val tgt = UInt(conf.data_width.W)
    val pc  = UInt(conf.data_width.W)
    val jump_diff = Bool()
    val retn_diff = Bool()
    val kill_valid = Bool()
    val kill_bits  = UInt(conf.data_width.W)
    def kill: Bool = valid.reduce(_||_)
  })
  val if_next_pc  =
    Mux(io.back.xcpt.valid, io.back.xcpt.bits,
    Mux(io.back.kill.valid, io.back.kill.bits,
    Mux(rectify.kill_valid, rectify.kill_bits,
    Mux(predict.split,      predict.predict(0).tgt,
                            predict.predict(1).tgt)
    )))
  val if_reg_pc = RegInit(START_ADDR)
  when (fetchi.pc_forward) { if_reg_pc := if_next_pc }
  def BRJUMP: UInt = "b110".U
  def BRANCH: UInt = "b11000".U
  val brjump = fetchi.inst.map(_.bits(6,4) === BRJUMP)
  val branch = fetchi.inst.map(_.bits(6,2) === BRANCH)
  val select = fetchi.inst(0).valid && brjump(0)
  val stall  = fetchi.inst(1).valid && brjump(1) && select
  fetchi.mem      <> io.mem
  fetchi.pc       := if_reg_pc
  fetchi.pc_split := predict.split
  fetchi.if_btb   := predict.predict
  fetchi.kill     := io.back.kill.valid || io.back.xcpt.valid || rectify.kill_valid
  fetchi.forward(0) := ftQueue.forward
  fetchi.forward(1) := ftQueue.forward && !stall

  ftQueue.in.valid := fetchi.inst.map(_.valid).reduce(_||_) && !fetchi.kill
  ftQueue.in.inst(0).valid := fetchi.inst(0).valid && !fetchi.kill
  ftQueue.in.inst(1).valid := fetchi.inst(1).valid && !fetchi.kill &&
    !(fetchi.inst(0).valid && Mux(brjump(0), brjump(1), fetchi.dec_btb(0).redirect))
  for (i <- 0 until conf.nInst) ftQueue.in.inst(i).bits := fetchi.inst(i).bits

  val front_reg = RegInit({
    val w = Wire(new Bundle{
      val valid   = Vec(conf.nInst, Bool())
      val brchjr  = Vec(conf.nInst, Bool())
      val fault   = Vec(conf.nInst, Bool())
      val predict = new PredictVal(conf.data_width)
      val branch  = Bool()
      val inst    = UInt(conf.inst_width.W)
      val pc      = UInt(conf.data_width.W)
      def btb_error(i: Int): Bool = valid(i) && fault(i)
      def bj_valid(i: Int): Bool = valid(i) && brchjr(i)
      def bj_enable: Bool = (valid.asUInt & brchjr.asUInt).orR
      def pc_split: Bool = valid.reduce(_&&_) && brchjr(0) && predict.redirect
    })
    w.valid   := Seq.fill(conf.nInst)(false.B)
    w.brchjr  := DontCare
    w.fault   := DontCare
    w.predict := DontCare
    w.branch  := DontCare
    w.inst    := DontCare
    w.pc      := DontCare
    w
  })
  when (io.back.xcpt.valid || io.back.kill.valid) {
    for (i <- 0 until conf.nInst) front_reg.valid(i) := false.B
  }.elsewhen (ftQueue.forward) {
    front_reg.valid   := ftQueue.in.inst.map(_.valid)
    front_reg.brchjr  := brjump
    front_reg.fault   := (0 until 2).map(i => !brjump(i) && fetchi.dec_btb(i).redirect)

    front_reg.branch  := Mux(select, branch(0), branch(1))
    front_reg.predict := Mux(select, fetchi.dec_btb(0),     fetchi.dec_btb(1))
    front_reg.inst    := Mux(select, fetchi.inst(0).bits,   fetchi.inst(1).bits)
    front_reg.pc      := Mux(select, fetchi.dec_pc(0),      fetchi.dec_pc(1))
  }

  val decoder = Module(new MicroDecoder(conf.inst_width)).io
  decoder.inst := front_reg.inst
  val front_feedback = Wire(new PredictVal(conf.data_width))
  front_feedback.valid := rectify.kill_valid
  front_feedback.tgt   := Mux(front_reg.branch || decoder.is_jal, rectify.tgt, predict.peek)
  front_feedback.diff  := false.B
  front_feedback.redirect := decoder.is_jal || decoder.retn || front_reg.branch
  front_feedback.history  := front_reg.predict.history
  when (front_feedback.valid) {
    printf("FrontEnd: Cyc= %d pc %x redirect %x type %c history %x\n"
    , io.cyc
    , predict.fb_pc.bits
    , front_feedback.redirect
    , Mux(predict.fb_type === BTBType.retn.U, Str("R"),
      Mux(predict.fb_type === BTBType.jump.U, Str("J"), Str("B")))
    , front_feedback.history
    )
  }

  predict.if_pc.valid := fetchi.pc_forward
  predict.if_pc.bits  := if_reg_pc

  predict.fb_pc.valid := io.back.kill.valid || rectify.kill_valid
  predict.fb_pc.bits  := Mux(io.back.kill.valid || !rectify.kill_valid, io.back.fb_pc,
    Cat(front_reg.pc(conf.data_width-1,conf.pcLSB+1), !rectify.valid(0), 0.U(conf.pcLSB.W)))
  predict.fb_type    := Mux(io.back.kill.valid || !rectify.kill_valid, io.back.fb_type,
    Mux(front_reg.btb_error(0) || decoder.is_jal, BTBType.jump.U,
    Mux(decoder.retn, BTBType.retn.U, BTBType.branch.U)))
  predict.feedBack   := Mux(io.back.kill.valid || !rectify.kill_valid, io.back.feedback, front_feedback)
  predict.branch     := Pulse(Mux(select, branch(0), branch(1) && fetchi.inst(1).valid), ftQueue.forward)
  predict.retn       := ShakeHand(decoder.retn && front_reg.bj_enable, ftQueue.forward)
  predict.call.valid := ShakeHand(decoder.call && front_reg.bj_enable, ftQueue.forward)
  predict.call.bits  := Cat(front_reg.pc(conf.data_width-1,conf.pcLSB+1),front_reg.bj_valid(1),0.U(conf.pcLSB.W))+4.U

  rectify.imm   := Mux(front_reg.branch,
    Cat(Fill(20, front_reg.inst(31)), front_reg.inst(7), front_reg.inst(30, 25), front_reg.inst(11, 8), 0.U(1.W)),
    Cat(Fill(12, front_reg.inst(31)), front_reg.inst(19,12), front_reg.inst(20), front_reg.inst(30,21), 0.U(1.W)))
  rectify.tgt   := front_reg.pc + rectify.imm
  rectify.jump_diff  := rectify.tgt =/= front_reg.predict.tgt //TO AVOID BTB ERROR
  rectify.retn_diff  := predict.peek =/= front_reg.predict.tgt
  for (i <- 0 until conf.nInst) {
    rectify.redir(i) := front_reg.bj_valid(i) && (
      (decoder.retn     && rectify.retn_diff) ||
      (decoder.is_jal   && rectify.jump_diff) ||
      (front_reg.branch && Mux(front_reg.predict.redirect, rectify.jump_diff, !front_reg.predict.valid)))
    rectify.valid(i) := rectify.redir(i) || front_reg.btb_error(i)
  }
  rectify.pc := Cat(front_reg.pc(conf.data_width-1,conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
  rectify.kill_valid := Pulse(rectify.kill, ftQueue.forward)
  rectify.kill_bits  := Mux(front_reg.btb_error(0) , rectify.pc,
    Mux(front_feedback.redirect, front_feedback.tgt,
    Mux(front_reg.pc_split, front_reg.predict.tgt, rectify.pc) + 4.U))
  //within valid
  ftQueue.in.pred.redirect := rectify.redir.reduce(_||_) || (front_reg.predict.redirect && front_reg.bj_enable)
  ftQueue.in.pred.split    := rectify.valid(0)
  //without valid
  ftQueue.in.pred.tgt      := Mux(front_feedback.redirect, front_feedback.tgt, front_reg.predict.tgt)
  ftQueue.in.pred.brchjr   := front_reg.brchjr
  ftQueue.in.pred.branch   := front_reg.branch
  ftQueue.in.pred.is_jal   := decoder.is_jal
  ftQueue.in.pred.history  := front_reg.predict.history
  ftQueue.in.pred.diff     := front_reg.predict.diff
  ftQueue.in.pred.retn     := decoder.retn
//  when (CycRange(io.cyc, 2013, 2015)) {
//    for (i <- 0 until conf.nInst) {
//      printf("valid: %x->inst: DASM(%x) ", fetchi.inst(i).valid, fetchi.inst(i).bits)
//      printf(p"predict valid ${fetchi.dec_btb(i).valid} " +
//        p"redirect ${fetchi.dec_btb(i).redirect} " +
//        p"tgt ${Hexadecimal(fetchi.dec_btb(i).tgt)}\n")
//    }
////    printf("%x\n", ftQueue.in.pred.tgt)
//    printf(p"forward ${ftQueue.forward} in_valid ${ftQueue.in.inst(0).valid} ${ftQueue.in.inst(1).valid}\n")
//    printf(p"${front_reg.valid} ${rectify.valid} ${rectify.kill_valid}->${Hexadecimal(rectify.kill_bits)} ")
//    printf(p"next pc ${Hexadecimal(if_next_pc)} ${Hexadecimal(if_reg_pc)}\n")
//  }
//  when (CycRange(io.cyc, 185764, 185765)) {
//    printf(p"pc ${Hexadecimal(btb.if_pc)} btb predict ${btb.predict(0).valid}->" +
//      p"<${btb.predict(0).bits.redirect}:${Hexadecimal(btb.predict(0).bits.tgt)}> ")
//    printf(p"dec_isbj_0 ${dec_isbj(0)} redirect_0 ${fetchi.dec_btb(0).bits.redirect}\n")
//  }
}
