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

class BackEnd(implicit conf: CPUConfig) extends Module with BackParam {
  val io = IO(new Bundle {
    val mem  = new MemPortIo(conf.xprlen)
    val cyc   = Output(UInt(conf.xprlen.W))
    val front = Flipped(new InterfaceIO(conf.xprlen))
  })

  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)
  val frontQueue = Module(new FrontQueue).io
  for (i <- 0 until nInst)
    io.front.forward(i) := frontQueue.forward
  frontQueue.in.inst := io.front.inst
  frontQueue.in.pred := io.front.pred
  frontQueue.in.pc_split := io.front.pc_split
  frontQueue.in.inst_split := io.front.inst_split
  frontQueue.xcpt.valid := false.B
  frontQueue.xcpt.bits  := csr.io.evec
  val instDecoder = Array.fill(nInst)(Module(new InstDecoder).io)
  for (i <- 0 until nInst) instDecoder(i).inst := frontQueue.inst(i).bits
  val in_valid = frontQueue.inst.map(_.valid)
  val in_wb  = (0 until nInst).map(i => in_valid(i) && instDecoder(i).rd.valid)
  val in_bj  = (0 until nInst).map(i => in_valid(i) && frontQueue.pred.brchjr(i))
  val in_mem = (0 until nInst).map(i => in_valid(i) && instDecoder(i).mem_en)
  val in_pvl = (0 until nInst).map(i => in_valid(i) && instDecoder(i).privil)

  val stateCtrl  = Module(new StateCtrl).io
  stateCtrl.xcpt_i.valid := false.B
  stateCtrl.xcpt_i.id := 0.U
  val loadStore  = Module(new LoadStore).io
  loadStore.xcpt := false.B
  loadStore.head := stateCtrl.head
  val branchJump = Module(new BranchJump).io
  branchJump.xcpt := false.B
  branchJump.head := stateCtrl.head
  frontQueue.kill.valid := branchJump.kill.valid
  frontQueue.kill.bits := branchJump.feedback.bits.tgt
  val inner_kill = RegInit({
    val w = Wire(new KillInfo(wOrder, nBrchjr))
    w.valid := false.B
    w.id := DontCare
    w.bidx := DontCare
    w
  })
  val instQueue = Array.fill(nInst)(Module(new InstQueue).io)
  inner_kill.valid := branchJump.kill.valid || (frontQueue.pred.split && instQueue(1).in.valid)
  inner_kill.id    := Mux(branchJump.kill.valid, branchJump.kill.id, stateCtrl.physic(1).id)
  inner_kill.bidx  := Mux(branchJump.kill.valid, branchJump.kill.bidx, OHToUInt(branchJump.bid1H))
  stateCtrl.split  := RegNext(!branchJump.kill.valid && frontQueue.pred.split)

  val feedback = RegInit({
    val w = Wire(new Bundle {
      val valid = Bool()
      val redirect = Bool()
      val tgt = UInt(data_width.W)
      val pc  = UInt(data_width.W)
      val typ = UInt(BTBType.SZ.W)
    })
    w.valid := false.B
    w.redirect := DontCare
    w.tgt := DontCare
    w.pc  := DontCare
    w.typ := DontCare
    w
  })
  val in_feedback = Wire(Vec(nInst, Bool()))
  in_feedback(0) := in_valid(0) && frontQueue.pred.rectify(0)
  in_feedback(1) := in_valid(1) && frontQueue.pred.rectify(1) && in_feedback(0)
  when (in_feedback(1)) {
    feedback.valid := true.B
  }.elsewhen(!branchJump.feedback.valid) {
    feedback.valid := false.B
  }
  when (in_feedback(1) && (!feedback.valid || !branchJump.feedback.valid)) {
    feedback.redirect := Mux(in_feedback(0), frontQueue.pred.brchjr(0), frontQueue.pred.brchjr(1))
    feedback.pc  := Mux(in_feedback(0), frontQueue.pc(0), frontQueue.pc(1))
    feedback.typ := Mux(frontQueue.pred.branch && feedback.redirect, BTBType.branch.U, BTBType.jump.U)
    feedback.tgt := frontQueue.pred.tgt
  }

  io.front.feedback.valid := branchJump.feedback.valid || feedback.valid

  io.front.feedback.bits.redirect := Mux(branchJump.feedback.valid,
    branchJump.feedback.bits.redirect, feedback.valid && feedback.redirect)

  io.front.fb_type := Mux(branchJump.feedback.valid,
    branchJump.fb_type, feedback.typ)

  io.front.fb_pc := Mux(branchJump.feedback.valid,
    branchJump.fb_pc, feedback.pc)

  io.front.feedback.bits.tgt := Mux(branchJump.feedback.valid,
    branchJump.feedback.bits.tgt, feedback.tgt)

  io.front.kill := branchJump.kill.valid

  for (i <- 0 until nInst) {
    stateCtrl.logic(i).rs := instDecoder(i).rs
    stateCtrl.logic(i).rd := instDecoder(i).rd
    stateCtrl.logic(i).op := instDecoder(i).op
    stateCtrl.logic(i).pc := frontQueue.pc(i)
  }
  stateCtrl.first := in_valid(0)

  val regfile = Module(new Regfile).io
  val wb_data = Wire(Vec(nCommit, UInt(data_width.W)))
  val data_wb = Wire(Vec(nCommit, new ByPass(wPhyAddr)))
  val commit = Wire(Vec(nCommit, new Commit(wOrder, wPhyAddr)))
  stateCtrl.commit := commit
  val in_inst = Wire(Vec(nInst, new InstIssue(wPhyAddr, wOrder)))
  val ir_inst = Wire(Vec(nInst, new InstIssue(wPhyAddr, wOrder)))
  val ir_data = Wire(Vec(nInst, Vec(2, UInt(data_width.W))))
  val bypass_sel = Wire(Vec(nInst, Vec(2, UInt(nCommit.W))))
  for (i <- 0 until nInst) {
    bypass_sel(i) := stateCtrl.physic(i).rs.map(rs =>
      VecInit(data_wb.map(b => b.addr === rs.addr && b.valid)).asUInt)
    in_inst(i).id := stateCtrl.physic(i).id
    for (j <- 0 until 2) {
      in_inst(i).rs(j).valid := stateCtrl.physic(i).rs(j).valid || bypass_sel(i)(j).orR
      regfile.r(2*i+j).addr := Mux(instQueue(i).issue.valid,
        instQueue(i).issue.bits.rs(j).addr, stateCtrl.rsaddr(i)(j))
    }
    in_inst(i).rs(0).addr  := Mux(in_inst(i).info.op2_sel === OP22_UTYPE,
      instDecoder(i).imm7_0(wPhyAddr-1,0), stateCtrl.physic(i).rs(0).addr)
    in_inst(i).rs(1).addr  := Mux(in_inst(i).info.op2_sel === OP22_UTYPE,
      instDecoder(i).imm7_0(7,  wPhyAddr), stateCtrl.physic(i).rs(1).addr)
    in_inst(i).mem_en       := instDecoder(i).mem_en
    in_inst(i).info.rd      := instDecoder(i).rd
    in_inst(i).info.f1      := instDecoder(i).op.cycle === CYC_1
    in_inst(i).info.imm     := instDecoder(i).imm
    in_inst(i).info.branch  := frontQueue.pred.brchjr(i) && frontQueue.pred.branch
    in_inst(i).info.op1_sel := Mux(stateCtrl.physic(i).undef(0), OP1_X, instDecoder(i).op1_sel)
    in_inst(i).info.op2_sel := Mux(stateCtrl.physic(i).undef(1), OP22_X, instDecoder(i).op2_sel)

    instQueue(i).xcpt := false.B
    instQueue(i).head := stateCtrl.head
    instQueue(i).kill.valid := inner_kill.valid
    instQueue(i).kill.bits  := inner_kill.id
    instQueue(i).in.valid   := frontQueue.inst(i).ready && in_valid(i)
    instQueue(i).in.bits    := in_inst(i)
    instQueue(i).issueable  := loadStore.issueable
    instQueue(i).bypass     := data_wb
    for (j <- 0 until nInst) {
      instQueue(i).speed(j) := instQueue(j).forward
    }
    instQueue(i).speed(2) := loadStore.forward

    ir_inst(i) := Mux(instQueue(i).issue.valid, instQueue(i).issue.bits, in_inst(i))
    stateCtrl.req_id(i) := ir_inst(i).id

    ir_data(i)(0) := Mux(ir_inst(i).info.op1_sel === OP1_IMZ, instDecoder(i).imm_z,
      Mux(instQueue(i).issue.valid, stateCtrl.resp_pc(i), frontQueue.pc(i)))

    ir_data(i)(1) := Mux(ir_inst(i).info.op2_sel === OP22_UTYPE,
      Cat(ir_inst(i).info.imm, ir_inst(i).rs(1).addr(7-wPhyAddr,0), ir_inst(i).rs(0).addr, Fill(20,0.U)),
      Cat(Fill(20, ir_inst(i).info.imm(11)), ir_inst(i).info.imm))
  }

  for (i <- 0 until nCommit) {
    regfile.w(i).data  := wb_data(i)
    regfile.w(i).valid := data_wb(i).valid
    regfile.w(i).addr  := data_wb(i).addr
  }
  val common = Wire(Vec(4, Bool()))
  val issue_cap  = Wire(Vec(4, Bool()))
  val physic_cap = Wire(Vec(4, Bool()))
  val memory_cap = Wire(Vec(4, Bool()))
  val branch_cap = Wire(Vec(3, Bool()))
  val privil_cap = Wire(Vec(2, Bool()))

  privil_cap(0) := !instDecoder(0).privil  || stateCtrl.empty
  privil_cap(1) := !(instDecoder(0).privil || instDecoder(1).privil) || stateCtrl.empty

  issue_cap(0)  := stateCtrl.id_ready(0) && instQueue(0).in.ready
  issue_cap(1)  := stateCtrl.id_ready(0) && instQueue(1).in.ready
  issue_cap(2)  := stateCtrl.id_ready(1) && instQueue(1).in.ready
  issue_cap(3)  := issue_cap(0) && issue_cap(2)

  physic_cap(0) := !instDecoder(0).rd.valid || stateCtrl.phy_ready(0)
  physic_cap(1) := !instDecoder(1).rd.valid || stateCtrl.phy_ready(0)
  physic_cap(2) := !instDecoder(1).rd.valid || stateCtrl.phy_ready(1)
  physic_cap(3) := physic_cap(0) && physic_cap(2)

  memory_cap(0) := !instDecoder(0).mem_en || loadStore.in(0).ready
  memory_cap(1) := !instDecoder(1).mem_en || loadStore.in(0).ready
  memory_cap(2) := !instDecoder(1).mem_en || loadStore.in(1).ready
  memory_cap(3) := memory_cap(0) && memory_cap(2)

  branch_cap(0) := !frontQueue.pred.brchjr(0) || branchJump.in.ready
  branch_cap(1) := !frontQueue.pred.brchjr(1) || branchJump.in.ready
  branch_cap(2) := !frontQueue.pred.brchjr.reduce(_||_) || branchJump.in.ready

  common(0) := branch_cap(0) && physic_cap(0) && memory_cap(0) && issue_cap(0)
  common(1) := branch_cap(1) && physic_cap(1) && memory_cap(1) && issue_cap(1)
  common(2) := branch_cap(1) && physic_cap(2) && memory_cap(2) && issue_cap(2)
  common(3) := branch_cap(2) && physic_cap(3) && memory_cap(3) && issue_cap(3)

  frontQueue.inst(0).ready := common(0) && privil_cap(0)
  frontQueue.inst(1).ready := Mux(in_valid(0), common(3), common(1)) && privil_cap(1)
  stateCtrl.inc_order(0) := Mux(in_valid(0), common(0), common(1) && in_valid(1)) && privil_cap(0)
  stateCtrl.inc_order(1) := common(2) && in_valid.reduce(_&&_) && privil_cap(1)

  stateCtrl.phy_valid(0) := in_wb(0) && privil_cap(0) && issue_cap(0) && memory_cap(0) && branch_cap(0)
  stateCtrl.phy_valid(1) := in_wb(1) && privil_cap(1) && Mux(in_valid(0),
    issue_cap(3) && memory_cap(3) && branch_cap(2),
    issue_cap(1) && memory_cap(1) && branch_cap(1))

  loadStore.in(0).valid  := in_mem(0) && issue_cap(0) && physic_cap(0)
  loadStore.in(1).valid  := in_mem(1) && Mux(in_valid(0),
    issue_cap(3) && physic_cap(3) && branch_cap(0),
    issue_cap(1) && physic_cap(1))

  branchJump.in.valid := stateCtrl.bkup_alloc && !frontQueue.pred.is_jal

  stateCtrl.bkup_alloc := branchJump.in.ready &&
    Mux(in_bj(0), issue_cap(0) && physic_cap(0), in_bj(1) &&
    Mux(in_valid(0), issue_cap(3) && physic_cap(3) && memory_cap(0), issue_cap(1) && physic_cap(1)))

  stateCtrl.bidx1H := branchJump.bid1H
  stateCtrl.brchjr := frontQueue.pred.brchjr
  stateCtrl.kill   := inner_kill

  stateCtrl.br_commit := branchJump.commit
  branchJump.in.bits.id := Mux(in_bj(0), stateCtrl.physic(0).id, stateCtrl.physic(1).id)
  branchJump.in.bits.pc := Mux(in_bj(0), frontQueue.pc(0), frontQueue.pc(1))
  branchJump.in.bits.brtype := Mux(in_bj(0), instDecoder(0).br_type, instDecoder(1).br_type)
  branchJump.in.bits.cont := branchJump.in.bits.pc + 4.U
  branchJump.in.bits.branch := frontQueue.pred.branch
  branchJump.in.bits.redirect := frontQueue.pred.redirect
  branchJump.in.bits.tgt := frontQueue.pred.tgt

  loadStore.kill.valid := inner_kill.valid
  loadStore.kill.bits  := inner_kill.id
  stateCtrl.st_commit  := loadStore.stcommit
  io.mem <> loadStore.mem
  for (i <- 0 until nInst) {
    loadStore.in(i).bits.id  := stateCtrl.physic(i).id
    loadStore.in(i).bits.rd  := stateCtrl.physic(i).rd.addr
    loadStore.in(i).bits.fcn := instDecoder(i).mem_fcn
    loadStore.in(i).bits.typ := instDecoder(i).mem_typ
  }
  loadStore.mem_en  := instDecoder(0).mem_en
  loadStore.mem_first := Seq(
    in_mem(0) && instDecoder(0).mem_fcn === M_XRD,
    in_mem(0) && instDecoder(0).mem_fcn === M_XWR)
  /*========================================================================================*/
  val exe_reg_csr = Reg(Bool())
  val exe_reg_csr_cmd  = RegInit(CSR.N)
  val exe_reg_csr_addr = Reg(UInt((CSR_ADDR_MSB-CSR_ADDR_LSB+1).W))
  exe_reg_csr_cmd := Mux(in_pvl(0) && stateCtrl.empty, instDecoder(0).csr_cmd,
                     Mux(in_pvl(1) && stateCtrl.empty, instDecoder(1).csr_cmd, CSR.N))

  exe_reg_csr := in_pvl(0)
  exe_reg_csr_addr := Mux(in_pvl(0), frontQueue.inst(0).bits(CSR_ADDR_MSB, CSR_ADDR_LSB),
    frontQueue.inst(1).bits(CSR_ADDR_MSB, CSR_ADDR_LSB))
  csr.io := DontCare
  csr.io.rw.addr  := exe_reg_csr_addr
  csr.io.rw.wdata := Mux(exe_reg_csr, wb_data(0), wb_data(1))
  csr.io.rw.cmd   := exe_reg_csr_cmd
  csr.io.pc       := stateCtrl.xcpt_o.pc
  csr.io.xcpt     := false.B

  io.front.xcpt.valid := csr.io.eret
  io.front.xcpt.bits  := csr.io.evec
  csr.io.retire := stateCtrl.retire
  // Add your own uarch counters here!
  csr.io.counters.foreach(_.inc := false.B)

  val exe_reg_valid  = RegInit(VecInit(Seq.fill(nInst)(false.B)))
  val exe_reg_issue  = Reg(Vec(nInst, new ExeIssueI(wPhyAddr, wOrder, data_width)))
  val exe_reg_d_sel  = Reg(Vec(nInst, Vec(2, Vec(2, Bool()))))
  val exe_reg_b_sel  = Reg(Vec(nInst, Vec(2, UInt(nCommit.W))))
  val exe_reg_wbdata = RegNext(wb_data)
  val issue_valid = Wire(Vec(nInst, Bool()))

  val exe_rs_data = Wire(Vec(nInst, Vec(2, UInt(data_width.W))))
  val exe_op_data = Wire(Vec(nInst, Vec(2, UInt(data_width.W))))
  val exe_aludata = Wire(Vec(nInst, UInt(data_width.W)))
  for (i <- 0 until nInst) {
    for (j <- 0 until 2) {
      exe_rs_data(i)(j) := Mux(exe_reg_b_sel(i)(j).orR, (0 until nCommit).map(k =>
        Fill(data_width, exe_reg_b_sel(i)(j)(k)) & exe_reg_wbdata(k)).reduce(_|_),
        regfile.r(2*i+j).data)
      exe_op_data(i)(j) :=
        (Fill(data_width, exe_reg_d_sel(i)(j)(IMM)) & exe_reg_issue(i).info.data(j)) |
        (Fill(data_width, exe_reg_d_sel(i)(j)(REG)) & exe_rs_data(i)(j))
    }
    exe_aludata(i) := (exe_reg_issue(i).info.data(1) &
       Fill(data_width, exe_reg_d_sel(i)(1)(IMM) || exe_reg_issue(i).mem_en)) |
      (Fill(data_width, exe_reg_d_sel(i)(1)(REG) && !exe_reg_issue(i).mem_en) & exe_rs_data(i)(1))
  }
  val issueQueue = Array.tabulate(nALU)(n => Module(new IssueQueue(nIssue(n))).io)
  val alu = Array.fill(nALU)(Module(new ALU).io)
  for (i <- 0 until nALU) {
    issueQueue(i).xcpt := false.B
    issueQueue(i).head := stateCtrl.head
    issueQueue(i).kill.valid := inner_kill.valid
    issueQueue(i).kill.bits  := inner_kill.id
    issueQueue(i).bypass := data_wb
    issueQueue(i).bydata := exe_reg_wbdata
    if (i < ALU3) {
      commit(i).valid := Mux(issueQueue(i).issue.valid, issueQueue(i).issue.bits.info.f1,
        issue_valid(i) && exe_reg_issue(i).info.f1) && !branchJump.mask(i)

      commit(i).id := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.id, exe_reg_issue(i).id)

      alu(i).data(0) := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.data(0), exe_op_data(i)(0))

      alu(i).data(1) := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.data_1, exe_aludata(i))

      data_wb(i).valid := Mux(issueQueue(i).issue.valid, issueQueue(i).issue.bits.info.wb_val,
        issue_valid(i) && exe_reg_issue(i).info.wb_val)

      data_wb(i).addr := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.rd.addr, exe_reg_issue(i).info.rd.addr)

      branchJump.issue(i).valid := issueQueue(i).issue.valid || issue_valid(i)
      branchJump.issue(i).branch := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.branch, exe_reg_issue(i).info.branch)

      loadStore.issue(i).valid := issueQueue(i).issue.valid ||
        (exe_reg_valid(i) && exe_reg_issue(i).rs(0).valid)

      loadStore.issue(i).data := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.data(1), exe_rs_data(i)(1))

      loadStore.issue(i).data_ok := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.data_ok, exe_reg_issue(i).rs(1).valid)

      wb_data(i) := MuxCase(alu(i).result, Array(
        (stateCtrl.req_io(i).op.wb_sel === WB_PC4) -> branchJump.cdlink(i),
        (stateCtrl.req_io(i).op.wb_sel === WB_CSR) -> csr.io.rw.rdata))
    } else {
      commit(i).valid := issueQueue(i).issue.valid && issueQueue(i).issue.bits.info.f1 && !branchJump.mask(i)
      commit(i).id    := issueQueue(i).issue.bits.id

      alu(i).data(0) := issueQueue(i).issue.bits.info.data(0)
      alu(i).data(1) := issueQueue(i).issue.bits.data_1

      data_wb(i).valid := issueQueue(i).issue.valid && issueQueue(i).issue.bits.info.wb_val
      data_wb(i).addr  := issueQueue(i).issue.bits.info.rd.addr

      branchJump.issue(i).valid  := issueQueue(i).issue.valid
      branchJump.issue(i).branch := issueQueue(i).issue.bits.info.branch

      loadStore.issue(i).valid   := issueQueue(i).issue.valid
      loadStore.issue(i).data    := issueQueue(i).issue.bits.info.data(1)
      loadStore.issue(i).data_ok := issueQueue(i).issue.bits.data_ok

      wb_data(i) := Mux(stateCtrl.req_io(i).op.wb_sel === WB_PC4,
        branchJump.cdlink(i), alu(i).result)
    }
    commit(i).wb := data_wb(i)
    stateCtrl.req_io(i).id := commit(i).id

    alu(i).opcode := stateCtrl.req_io(i).op.alu_fun
    alu(i).brtype := branchJump.brtype(i)

    branchJump.issue(i).id := commit(i).id
    branchJump.issue(i).actual := alu(i).actual
    branchJump.target(i) := Cat(alu(i).add_result(data_width-1,1), 0.U(1.W))

    loadStore.issue(i).id := commit(i).id
    loadStore.issue(i).addr := alu(i).add_result
  }
  commit(LOAD)  := loadStore.ldcommit
  wb_data(LOAD) := loadStore.wb_data
  data_wb(LOAD) := loadStore.ldcommit.wb

  val issue_cmp = CmpId(exe_reg_issue(0).id, exe_reg_issue(1).id, stateCtrl.head)
  val self_alive = Wire(Vec(nALU, Bool()))
  val self_ready = Wire(Vec(nInst, Bool()))
  val self_accept = Wire(Vec(nInst, Bool()))
  val third_party = Wire(Vec(nInst, Bool()))
  third_party(0) := ( issue_cmp || self_accept(1))
  third_party(1) := (!issue_cmp || self_accept(0))
  issueQueue(ALU3).in.valid := !self_accept.reduce(_&&_) && !self_alive(ALU3)
  issueQueue(ALU3).in.bits  := Mux((!self_accept(0) && issue_cmp(0)) || self_accept(1),
    issueQueue(0).in.bits, issueQueue(1).in.bits)
  for (i <- 0 until nALU) {
    self_alive(i) := !inner_kill.valid ||
      CmpId(issueQueue(i).in.bits.id, inner_kill.id, stateCtrl.head)
  }
  for (i <- 0 until nInst) {
    when (instQueue(i).issue.ready) {
      exe_reg_valid(i) := instQueue(i).issue.valid || instQueue(i).in.valid
      exe_reg_b_sel(i) := Mux(instQueue(i).issue.valid, instQueue(i).data_sel, bypass_sel(i))
      exe_reg_d_sel(i)(0)(REG) := ir_inst(i).info.op1_sel === OP1_RS1
      exe_reg_d_sel(i)(0)(IMM) := ir_inst(i).info.op1_sel === OP1_IMZ ||
                                  ir_inst(i).info.op1_sel === OP1_PC
      exe_reg_d_sel(i)(1)(REG) := ir_inst(i).info.op2_sel === OP22_RS2
      exe_reg_d_sel(i)(1)(IMM) := ir_inst(i).info.op2_sel === OP22_ITYPE ||
                                  ir_inst(i).info.op2_sel === OP22_UTYPE

      exe_reg_issue(i).id          := ir_inst(i).id
      exe_reg_issue(i).rs          := ir_inst(i).rs
      exe_reg_issue(i).mem_en      := ir_inst(i).mem_en
      exe_reg_issue(i).info.rd     := ir_inst(i).info.rd
      exe_reg_issue(i).info.f1     := ir_inst(i).info.f1
      exe_reg_issue(i).info.imm    := ir_inst(i).info.imm
      exe_reg_issue(i).info.branch := ir_inst(i).info.branch
      exe_reg_issue(i).info.data   := ir_data(i)
    }
    self_ready(i)  := (0 until 2).map(j => exe_reg_issue(i).rs(j).valid).reduce(_&&_)
    self_accept(i) := !issueQueue(i).in.valid || issueQueue(i).in.ready
    instQueue(i).issue.ready := self_accept(i) ||
      (third_party(i) && issueQueue(2).tail.ready && (issueQueue(2).tail.valid ||
        CmpId(issueQueue(2).tail.id, exe_reg_issue(i).id, stateCtrl.head)))
    issue_valid(i) := exe_reg_valid(i) && self_ready(i) && self_alive(i)
    issueQueue(i).in.valid := exe_reg_valid(i) && (!self_ready(i) || issueQueue(i).issue.valid)
    issueQueue(i).in.bits  := exe_reg_issue(i)
    issueQueue(i).in.bits.info.data := exe_op_data(i) //only change data
  }

}