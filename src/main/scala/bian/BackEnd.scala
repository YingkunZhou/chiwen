package bian

import chisel3._
import chisel3.util._
import common._

object Pulse {
  def apply(in: Bool, forward: Bool): Bool = {
    val in_latch = RegInit(true.B)
    when (forward) { in_latch := true.B
    }.elsewhen(in) { in_latch := false.B}
    in && in_latch
  }
}

class BackEnd(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val mem  = new MemPortIo(conf.xprlen)
    val cyc   = Output(UInt(conf.xprlen.W))
    val front = Flipped(new InterfaceIO(conf.xprlen))
  })

  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)
  val frontQueue  = Module(new FrontQueue).io
  val instDecoder = Array.fill(conf.nInst)(Module(new InstDecoder()).io)
  val stateCtrl   = Module(new StateCtrl(conf.data_width)).io
  val branchJump  = Module(new BranchJump(conf.data_width)).io
  frontQueue.pred_i := io.front.pred
  frontQueue.inst_i(0).valid := io.front.inst(0).valid
  frontQueue.inst_i(1).valid := io.front.inst(1).valid && !io.front.split
  val split = frontQueue.pred_o.cancel && frontQueue.pred_o.bj_sel(0) //around 28 gates
  branchJump.pred_i.bits.id   := Mux(frontQueue.pred_o.bj_sel(0), stateCtrl.physic(0).id, stateCtrl.physic(1).id)
  branchJump.pred_i.bits.pc   := Mux(frontQueue.pred_o.bj_sel(0), frontQueue.pc(0), frontQueue.pc(1))
  branchJump.pred_i.bits.cont := branchJump.pred_i.bits.pc + 4.U
  branchJump.pred_i.bits.jump := frontQueue.pred_o.jump
  branchJump.pred_i.bits.branch := frontQueue.pred_o.branch
  branchJump.pred_i.bits.redirect := frontQueue.pred_o.redirect
  branchJump.pred_i.bits.tgt := frontQueue.pred_o.tgt
  for (i <- 0 until conf.nInst) {
    frontQueue.inst_i(i).bits := io.front.inst(i).bits
    io.front.forward(i) := frontQueue.forward
    instDecoder(i).inst := frontQueue.inst_o(i).bits
    stateCtrl.logic(i).info.pc  := frontQueue.pc(i)
    stateCtrl.logic(i).info.op  := instDecoder(i).op
    stateCtrl.logic(i).info.imm := instDecoder(i).imm
    stateCtrl.logic(i).rs       := instDecoder(i).rs
    stateCtrl.logic(i).rd       := instDecoder(i).rd
    stateCtrl.logic(i).valid    := frontQueue.inst_o(i).valid
    stateCtrl.logic(i).brchjr   := frontQueue.pred_o.bj_sel(i) && !frontQueue.pred_o.is_jal
  }


}