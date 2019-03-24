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
  val instDecoder = Array.fill(conf.nInst)(Module(new InstDecoder).io)
  val stateCtrl   = Module(new StateCtrl).io
  val branchJump  = Module(new BranchJump).io
  val loadStore   = Module(new LoadStore).io
  val instQueue   = Array.fill(conf.nInst)(Module(new InstQueue).io)
  io.front.forward := Seq(frontQueue.forward, frontQueue.forward)
  frontQueue.split_i := io.front.split
  frontQueue.pred_i  := io.front.pred
  frontQueue.inst_i  := io.front.inst

  val valid = Wire(Vec(conf.nInst, Bool()))
  valid(0) := frontQueue.inst_o(0).valid
  valid(1) := frontQueue.inst_o(1).valid && !frontQueue.split_o //around 30 gates
  for (i <- 0 until conf.nInst) {
    instDecoder(i).inst := frontQueue.inst_o(i).bits
    stateCtrl.logic(i).info.pc  := frontQueue.pc(i)
    stateCtrl.logic(i).info.op  := instDecoder(i).op
    stateCtrl.logic(i).info.imm := instDecoder(i).imm
    stateCtrl.logic(i).rs       := instDecoder(i).rs
    stateCtrl.logic(i).rd       := instDecoder(i).rd
    stateCtrl.logic(i).valid    := valid(i)
  }
  // the base line is without inst valid
  val ready_0 = Wire(Vec(conf.nInst, Bool()))
  val ready_1 = Wire(Bool())
  val nls_ready_0 = Wire(Vec(conf.nInst, Bool()))
  val nls_ready_1 = Wire(Bool())
  nls_ready_0(0) := stateCtrl.order_ready(0) && instQueue(0).ready &&
      (!instDecoder(0).rd.valid || stateCtrl.physic_ready(0)) &&
      (!frontQueue.pred_o.brchjr(0) || branchJump.bReady)
  nls_ready_0(1) := stateCtrl.order_ready(0) && instQueue(1).ready &&
      (!instDecoder(1).rd.valid || stateCtrl.physic_ready(1)) &&
      (!frontQueue.pred_o.brchjr(1) || branchJump.bReady)
  nls_ready_1 := stateCtrl.order_ready(1) && instQueue(1).ready &&
      (!instDecoder(1).rd.valid || stateCtrl.physic_ready(1)) &&
      (!frontQueue.pred_o.brchjr(1) || branchJump.bReady)

  ready_0(0) := nls_ready_0(0) && (!instDecoder(0).mem_en || loadStore.lReady(0))
  ready_0(1) := nls_ready_0(1) && (!instDecoder(1).mem_en || loadStore.lReady(0))
  ready_1 := nls_ready_1 && (!instDecoder(1).mem_en || loadStore.lReady(1))
  frontQueue.ready(0) := ready_0(0)
  frontQueue.ready(1) := Mux(valid(0), ready_1, ready_0(1))

  val order_inc = Wire(Vec(conf.nInst, Bool()))
  val brchjr_inc = Wire(Vec(conf.nInst, Bool()))
  val physic_inc = Wire(Vec(conf.nInst, Bool()))
  val loadst_inc = Wire(Vec(conf.nInst, Bool()))
  val brchjr_valid = (0 until conf.nInst).map(i => frontQueue.pred_o.brchjr(i) && frontQueue.inst_o(i).valid)
  val physic_valid = (0 until conf.nInst).map(i => frontQueue.pred_o.brchjr(i) && valid(i))
  branchJump.pred_i.id   := Mux(brchjr_valid(0), stateCtrl.physic(0).id, stateCtrl.physic(1).id)
  branchJump.pred_i.pc   := Mux(brchjr_valid(0), frontQueue.pc(0), frontQueue.pc(1))
  branchJump.pred_i.tgt  := frontQueue.pred_o.tgt
  branchJump.pred_i.cont := branchJump.pred_i.pc + 4.U
  branchJump.pred_i.jump := frontQueue.pred_o.jump
  branchJump.pred_i.branch := frontQueue.pred_o.branch
  branchJump.pred_i.redirect := frontQueue.pred_o.redirect
  brchjr_inc(0) := brchjr_valid(0) && ready_0(0)
  brchjr_inc(1) := brchjr_valid(1) && Mux(frontQueue.inst_o(0).valid, ready_0(0) && ready_1, ready_0(1))
  branchJump.inc_tail := brchjr_inc.reduce(_||_)

  order_inc(0)  :=(0 until conf.nInst).map(i => valid(i) && ready_0(i)).reduce(_||_)
  order_inc(1)  := valid.reduce(_&&_) && ready_1
  physic_inc(0) :=(physic_valid(0) && ready_0(0)) || (physic_valid(1) && Mux(valid(0), ready_0(0) && ready_1, ready_0(1)))
  physic_inc(1) := physic_valid.reduce(_&&_) && ready_1
  stateCtrl.order_inc  := order_inc
  stateCtrl.physic_inc := physic_inc
  stateCtrl.brchjr := brchjr_inc
  for (i <- 0 until conf.nInst) {
    loadStore.entry(i).bits.id  := stateCtrl.physic(i).id
    loadStore.entry(i).bits.rd  := stateCtrl.physic(i).rd
    loadStore.entry(i).bits.typ := instDecoder(i).mem_typ
    loadStore.entry(i).bits.fcn := instDecoder(i).mem_fcn
  }
  val mem_en = (0 until conf.nInst).map(i => instDecoder(i).mem_en && valid(i))
  loadStore.entry(0).valid := mem_en(0) && nls_ready_0(0)
  loadStore.entry(1).valid := mem_en(1) && Mux(valid(0), nls_ready_0(0) && nls_ready_1, nls_ready_0(1))
  loadStore.mem_fcn(0) := mem_en(0) && instDecoder(0).mem_fcn === M_XRD
  loadStore.mem_fcn(1) := mem_en(0) && instDecoder(0).mem_fcn === M_XWR

}