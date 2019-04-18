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
    val front = Flipped(new InterfaceIO)
  })

  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)

  val instDecoder = Array.fill(nInst)(Module(new InstDecoder).io)
  for (i <- 0 until nInst) {
    instDecoder(i).inst := io.front.inst(i).bits
  }
  val in_wrb = (0 until nInst).map(i => io.front.inst(i).valid && instDecoder(i).rd.valid)
  val in_mem = (0 until nInst).map(i => io.front.inst(i).valid && instDecoder(i).mem_en)
  val in_pvl = (0 until nInst).map(i => io.front.inst(i).valid && instDecoder(i).privil)
  val in_bjr = (0 until nInst).map(i => io.front.inst(i).valid && io.front.pred.brchjr(i))
  val stateCtrl = Module(new StateCtrl).io
  val loadStore = Module(new LoadStore).io
  stateCtrl.cyc := io.cyc
  stateCtrl.xcpt_i.valid := ((0 until nInst).map(i => io.front.inst(i).valid &&
  instDecoder(i).system).reduce(_||_) && stateCtrl.empty) || loadStore.rollback.valid
  stateCtrl.xcpt_i.id := Mux(loadStore.rollback.valid, loadStore.rollback.bits,
    Mux(io.front.inst(0).valid && instDecoder(0).system,
      stateCtrl.physic(0).id, stateCtrl.physic(1).id))

  loadStore.cyc  := io.cyc
  loadStore.xcpt := stateCtrl.xcpt_o.valid
  loadStore.head := stateCtrl.head
  val branchJump = Module(new BranchJump).io
  branchJump.cyc := io.cyc
  branchJump.xcpt := stateCtrl.xcpt_o.valid
  branchJump.head := stateCtrl.head
  /*TODO List
  * 1. optimizing exception recover procedure
  * 2. how to deal with exception
  * */
  io.front.xcpt.valid := stateCtrl.xcpt_o.valid
  io.front.xcpt.bits  := Mux(csr.io.eret, csr.io.evec, stateCtrl.xcpt_o.bits)
  io.front.kill.valid := branchJump.kill.valid
  io.front.kill.bits  := branchJump.feedback.bits.tgt

  val inner_kill = RegInit({
    val w = Wire(new KillInfo(wOrder, nBrchjr))
    w.valid := false.B
    w.id   := DontCare
    w.bidx := DontCare
    w
  })
  val instQueue = Array.tabulate(nInst)(n => Module(new InstQueue(n)).io)
  inner_kill.valid := branchJump.kill.valid || (io.front.pred.split && instQueue(1).in.valid)
  inner_kill.id    := Mux(branchJump.kill.valid, branchJump.kill.id + 1.U, stateCtrl.physic(1).id)
  inner_kill.bidx  := Mux(branchJump.kill.valid, branchJump.kill.bidx, OHToUInt(branchJump.bid1H))
  stateCtrl.kill   := inner_kill
  //along with kill info
  stateCtrl.split  := RegNext(!branchJump.kill.valid && io.front.pred.split)
  stateCtrl.bidx1H := branchJump.bid1H
  stateCtrl.bj_first := io.front.pred.brchjr(0)
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
  io.front.feedback.valid := feedback.valid
  io.front.feedback.bits.redirect := feedback.valid && feedback.redirect
  io.front.feedback.bits.tgt := feedback.tgt
  io.front.fb_type := feedback.typ
  io.front.fb_pc   := feedback.pc

  val feedback_valid = (0 until nInst).map(i => Pulse(io.front.pred.rectify(i), io.front.inst(i).ready)).reduce(_||_)
  feedback.valid := branchJump.feedback.valid || feedback_valid
  when (branchJump.feedback.valid && (branchJump.kill.valid || !feedback_valid)) {
    feedback.pc    := branchJump.fb_pc
    feedback.tgt   := branchJump.feedback.bits.tgt
    feedback.redirect := branchJump.feedback.bits.redirect
    feedback.typ   := branchJump.fb_type
  }.elsewhen(feedback_valid) {
    feedback.pc    := Mux(io.front.pred.rectify(0), io.front.pc(0), io.front.pc(1))
    feedback.tgt   := io.front.pred.tgt
    feedback.redirect := io.front.pred.redirect
    feedback.typ   := (io.front.pred.redirect && io.front.pred.branch).asUInt
  }

  for (i <- 0 until nInst) {
    stateCtrl.logic(i).rs := instDecoder(i).rs
    stateCtrl.logic(i).rd := instDecoder(i).rd
    stateCtrl.logic(i).op := instDecoder(i).op
    stateCtrl.logic(i).pc := io.front.pc(i)
  }
  stateCtrl.first := io.front.inst(0).valid

  val regfile = Module(new Regfile).io
  regfile.debug <> stateCtrl.what
  val wb_data = Wire(Vec(nCommit, UInt(data_width.W)))
  val data_wb = Wire(Vec(nCommit, new ByPass(wPhyAddr)))
  val commit = Wire(Vec(nCommit, new Commit(wOrder, wPhyAddr)))
  val commit_keep = Wire(Vec(nCommit, Bool()))
  stateCtrl.commit := commit
  for (i <- 0 until nCommit) {
    commit_keep(i) := !branchJump.kill.valid ||
      CmpId(commit(i).id, branchJump.kill.id, stateCtrl.head, wOrder-1) //commit id <= branch kill id
    stateCtrl.commit(i).valid := commit(i).valid && commit_keep(i)
  }

  for (i <- 0 until nCommit) {
    regfile.write(i).data  := wb_data(i)
    regfile.write(i).valid := data_wb(i).valid
    regfile.write(i).addr  := data_wb(i).addr
    when (regfile.write(i).valid) {
      printf("RegFile: Cyc= %d addr %x data %x id %d channal %x\n",
        io.cyc,
        regfile.write(i).addr,
        regfile.write(i).data,
        commit(i).id,
        i.U)
    }
  }

//  when (io.cyc === 11769.U) {regfile.write(1).data := "h00002f2b".U }
//  when (io.cyc === 11779.U) {regfile.write(1).data := "h00002da6".U }
//  when (io.cyc === 11798.U) {regfile.write(0).data := "h00002f4a".U }
//  when (io.cyc === 12174.U) {regfile.write(0).data := "h00002214".U }

  val common = Wire(Vec(4, Bool()))
  val issue_cap  = Wire(Vec(4, Bool()))
  val physic_cap = Wire(Vec(4, Bool()))
  val memory_cap = Wire(Vec(4, Bool()))
  val branch_cap = Wire(Vec(3, Bool()))
  val privil_cap = Wire(Vec(2, Bool()))

  privil_cap(0) := !instDecoder(0).privil || stateCtrl.empty
  privil_cap(1) := !instDecoder(1).privil || stateCtrl.empty

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

  branch_cap(0) := !io.front.pred.brchjr(0) || branchJump.in.ready
  branch_cap(1) := !io.front.pred.brchjr(1) || branchJump.in.ready
  branch_cap(2) := !io.front.pred.brchjr.reduce(_||_) || branchJump.in.ready

  common(0) := branch_cap(0) && physic_cap(0) && memory_cap(0) && issue_cap(0)
  common(1) := branch_cap(1) && physic_cap(1) && memory_cap(1) && issue_cap(1)
  common(2) := branch_cap(1) && physic_cap(2) && memory_cap(2) && issue_cap(2)
  common(3) := branch_cap(2) && physic_cap(3) && memory_cap(3) && issue_cap(3)

  io.front.inst(0).ready := common(0) && privil_cap(0)
  io.front.inst(1).ready := Mux(io.front.inst(0).valid,
    common(3) && !instDecoder(0).privil, common(1)) && privil_cap(1)
  stateCtrl.inc_order(0) := Mux(io.front.inst(0).valid,   common(0) && privil_cap(0),
                                io.front.inst(1).valid && common(1) && privil_cap(1))

  stateCtrl.inc_order(1) := io.front.inst.map(_.valid).reduce(_&&_) && common(2) && privil_cap(1) && !instDecoder(0).privil
  stateCtrl.wrb_valid(0) := in_wrb(0) && privil_cap(0) && issue_cap(0) && memory_cap(0) && branch_cap(0)
  stateCtrl.wrb_valid(1) := in_wrb(1) && privil_cap(1) && Mux(io.front.inst(0).valid,
    issue_cap(3) && memory_cap(3) && branch_cap(2) && !instDecoder(0).privil,
    issue_cap(1) && memory_cap(1) && branch_cap(1))
  stateCtrl.bjr_valid := branchJump.in.ready &&
    Mux(in_bjr(0), issue_cap(0) && physic_cap(0), in_bjr(1) && Mux(io.front.inst(0).valid,
      issue_cap(3) && physic_cap(3) && memory_cap(0) && !instDecoder(0).privil,
      issue_cap(1) && physic_cap(1)))

  branchJump.in.valid := stateCtrl.bjr_valid && !io.front.pred.is_jal
  loadStore.in(0).valid  := in_mem(0) && issue_cap(0) && physic_cap(0)
  loadStore.in(1).valid  := in_mem(1) && Mux(io.front.inst(0).valid,
    issue_cap(3) && physic_cap(3) && branch_cap(0) && !instDecoder(0).privil,
    issue_cap(1) && physic_cap(1))

  stateCtrl.br_commit := branchJump.commit
  branchJump.in.bits.id := Mux(in_bjr(0), stateCtrl.physic(0).id, stateCtrl.physic(1).id)
  branchJump.in.bits.pc := Mux(in_bjr(0), io.front.pc(0), io.front.pc(1))
  branchJump.in.bits.brtype := Mux(in_bjr(0), instDecoder(0).br_type, instDecoder(1).br_type)
  branchJump.in.bits.cont := branchJump.in.bits.pc + 4.U
  branchJump.in.bits.branch := io.front.pred.branch
  branchJump.in.bits.redirect := io.front.pred.redirect
  branchJump.in.bits.tgt := io.front.pred.tgt

  loadStore.kill.valid := inner_kill.valid
  loadStore.kill.bits  := inner_kill.id

  stateCtrl.st_commit.bits := loadStore.stcommit.bits
  stateCtrl.st_commit.valid := loadStore.stcommit.valid && (!branchJump.kill.valid ||
    CmpId(loadStore.stcommit.bits, branchJump.kill.id, stateCtrl.head, wOrder-1))

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


  val in_inst = Wire(Vec(nInst, new InstIssueO(wOrder, wPhyAddr, nCommit)))
  val ir_inst = Wire(Vec(nInst, new InstIssueO(wOrder, wPhyAddr, nCommit)))
  val ir_data = Wire(Vec(nInst, Vec(2, UInt(data_width.W))))
  for (i <- 0 until nInst) {
    for (j <- 0 until 2) {
      regfile.read(i)(j).addr := Mux(instQueue(i).issue.valid,
        instQueue(i).issue.bits.rs(j).addr, stateCtrl.rsaddr(i)(j))
      in_inst(i).rs(j).valid := stateCtrl.physic(i).rs(j).valid || in_inst(i).data_sel(j).orR
    }
    in_inst(i).data_sel     := stateCtrl.physic(i).rs.map(rs =>
      VecInit(data_wb.map(b => b.addr === rs.addr && b.valid)).asUInt)
    in_inst(i).id           := stateCtrl.physic(i).id
    in_inst(i).rs(0).addr   := Mux(in_inst(i).info.op2_sel === OP22_UTYPE,
      instDecoder(i).imm7_0(wPhyAddr-1,0), stateCtrl.physic(i).rs(0).addr)
    in_inst(i).rs(1).addr   := Mux(in_inst(i).info.op2_sel === OP22_UTYPE,
      Cat(0.U(2*wPhyAddr-8), instDecoder(i).imm7_0(7,  wPhyAddr)), stateCtrl.physic(i).rs(1).addr)
    in_inst(i).mem_en       := instDecoder(i).mem_en
    in_inst(i).info.rd      := stateCtrl.physic(i).rd
    in_inst(i).info.f1      := instDecoder(i).op.cycle === CYC_1
    in_inst(i).info.imm     := instDecoder(i).imm
    in_inst(i).info.branch  := io.front.pred.brchjr(i) && io.front.pred.branch
    in_inst(i).info.op1_sel := Mux(stateCtrl.physic(i).undef(0) &&
      instDecoder(i).op1_sel === OP1_RS1,  OP1_X , instDecoder(i).op1_sel)
    in_inst(i).info.op2_sel := Mux(stateCtrl.physic(i).undef(1) &&
      instDecoder(i).op2_sel === OP22_RS2, OP22_X, instDecoder(i).op2_sel)

    instQueue(i).cyc  := io.cyc
    instQueue(i).xcpt := stateCtrl.xcpt_o.valid
    instQueue(i).head := stateCtrl.head
    instQueue(i).kill.valid := inner_kill.valid
    instQueue(i).kill.bits  := inner_kill.id
    instQueue(i).in.valid   := io.front.inst(i).fire && !inner_kill.valid
    instQueue(i).in.bits    := in_inst(i)
    instQueue(i).issueable  := loadStore.issueable
    instQueue(i).bypass     := data_wb
    instQueue(i).speed(2) := loadStore.forward
    for (j <- 0 until nInst) instQueue(i).speed(j) := instQueue(j).forward

    ir_inst(i) := Mux(instQueue(i).issue.valid, instQueue(i).issue.bits, in_inst(i))
    stateCtrl.req_id(i) := ir_inst(i).id

    ir_data(i)(0) := Mux(ir_inst(i).info.op1_sel === OP1_IMZ, instDecoder(i).imm_z,
      Mux(instQueue(i).issue.valid, stateCtrl.resp_pc(i), io.front.pc(i)))

    ir_data(i)(1) := Mux(ir_inst(i).info.op2_sel === OP22_UTYPE,
      Cat(ir_inst(i).info.imm, ir_inst(i).rs(1).addr(7-wPhyAddr,0), ir_inst(i).rs(0).addr, 0.U(12.W)),
      Cat(Fill(20, ir_inst(i).info.imm(11)), ir_inst(i).info.imm))
  }
  /*========================================================================================*/
  val issueQueue = Array.tabulate(nALU)(n => Module(new IssueQueue(nIssue(n), n)).io)
  val exe_reg_issue = RegInit(VecInit(Seq.fill(nInst){
    val w = Wire(new ExeIssueI(wOrder, wPhyAddr, nCommit, data_width))
    w.valid    := false.B
    w.id       := DontCare
    w.mem_en   := DontCare
    w.rs       := DontCare
    w.data_sel := DontCare
    w.info     := DontCare
    w
  }))
  val exe_reg_d_sel = Reg(Vec(nInst, Vec(2, Vec(2, Bool()))))
  val exe_use_regfl = RegInit(VecInit(Seq.fill(2)(true.B)))
  val exe_rs_data = Wire(Vec(nInst, Vec(2, UInt(data_width.W))))
  for (i <- 0 until nInst) {
    when (instQueue(i).issue.ready) {
      exe_use_regfl(i) := true.B
      exe_reg_d_sel(i)(0)(REG) := ir_inst(i).info.op1_sel === OP1_RS1
      exe_reg_d_sel(i)(0)(IMM) := ir_inst(i).info.op1_sel === OP1_IMZ ||
        ir_inst(i).info.op1_sel === OP1_PC

      exe_reg_d_sel(i)(1)(REG) := ir_inst(i).info.op2_sel === OP22_RS2
      exe_reg_d_sel(i)(1)(IMM) := ir_inst(i).info.op2_sel === OP22_ITYPE ||
        ir_inst(i).info.op2_sel === OP22_UTYPE

      exe_reg_issue(i).valid       := (instQueue(i).issue.valid || instQueue(i).in.valid)
      // && stateCtrl.xcpt_o.valid, already exe_reg_issue use regnext to comb cancel it
      exe_reg_issue(i).id          := ir_inst(i).id
      exe_reg_issue(i).rs          := ir_inst(i).rs
      exe_reg_issue(i).mem_en      := ir_inst(i).mem_en
      exe_reg_issue(i).info.rd     := ir_inst(i).info.rd
      exe_reg_issue(i).info.f1     := ir_inst(i).info.f1
      exe_reg_issue(i).info.imm    := ir_inst(i).info.imm
      exe_reg_issue(i).info.branch := ir_inst(i).info.branch
      exe_reg_issue(i).data_sel    := ir_inst(i).data_sel
      exe_reg_issue(i).info.data   := ir_data(i)
    }.otherwise {
      for (j <- 0 until 2) {
        exe_reg_issue(i).rs(j).valid := issueQueue(i).in.bits.rs_valid(j)
        exe_reg_issue(i).data_sel(j) := issueQueue(i).in.bits.data_sel(j) //update data_sel
        when (exe_reg_d_sel(i)(j)(REG)) { //only change for regfile resource read
          exe_reg_issue(i).info.data(j) := exe_rs_data(i)(j)
        }
      }
      exe_use_regfl(i) := false.B
    }
  }

  val exe_inst_val = Wire(Vec(nInst, Bool()))
  val exe_inst_acc = Wire(Vec(nInst, Bool()))
  val exe_op_data  = Wire(Vec(nInst, Vec(2, UInt(data_width.W))))
  val exe_alu_data = Wire(Vec(nInst, UInt(data_width.W)))
  val exe_reg_wbdata = RegNext(wb_data)
  for (i <- 0 until nInst) {
    for (j <- 0 until 2) {
      exe_rs_data(i)(j) := Mux(exe_reg_issue(i).data_sel(j).orR, (0 until nCommit).map(k =>
        Fill(data_width, exe_reg_issue(i).data_sel(j)(k)) & exe_reg_wbdata(k)).reduce(_|_),
        Mux(exe_use_regfl(i), regfile.read(i)(j).data, exe_reg_issue(i).info.data(j)))

      exe_op_data(i)(j) :=
        (Fill(data_width, exe_reg_d_sel(i)(j)(IMM)) & exe_reg_issue(i).info.data(j)) |
        (Fill(data_width, exe_reg_d_sel(i)(j)(REG)) & exe_rs_data(i)(j))
    }
    exe_inst_val(i) := exe_reg_issue(i).valid && !RegNext(stateCtrl.xcpt_o.valid) &&
      !(inner_kill.valid && CmpId(inner_kill.id, exe_reg_issue(i).id, stateCtrl.head, wOrder-1))

    exe_alu_data(i) := (exe_reg_issue(i).info.data(1) &
       Fill(data_width, exe_reg_d_sel(i)(1)(IMM) || exe_reg_issue(i).mem_en)) |
      (Fill(data_width, exe_reg_d_sel(i)(1)(REG) && !exe_reg_issue(i).mem_en) & exe_rs_data(i)(1))
  }
  val alu = Array.fill(nALU)(Module(new ALU).io)
  for (i <- 0 until nALU) {
    issueQueue(i).cyc := io.cyc
    issueQueue(i).xcpt := stateCtrl.xcpt_o.valid
    issueQueue(i).head := stateCtrl.head
    issueQueue(i).kill.valid := inner_kill.valid
    issueQueue(i).kill.bits  := inner_kill.id
    issueQueue(i).bypass := data_wb
    issueQueue(i).bydata := exe_reg_wbdata
    issueQueue(i).issueable := loadStore.issueable
    if (i < ALU3) {
      commit(i).valid := Mux(issueQueue(i).issue.valid, issueQueue(i).issue.bits.info.f1,
        exe_inst_acc(i) && exe_reg_issue(i).info.f1) && !branchJump.mask(i)

      commit(i).id := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.id, exe_reg_issue(i).id)

      alu(i).data(0) := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.data(0), exe_op_data(i)(0))

      alu(i).data(1) := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.data_1, exe_alu_data(i))

      data_wb(i).valid := Mux(issueQueue(i).issue.valid, issueQueue(i).issue.bits.info.wb_val,
        exe_inst_acc(i) && exe_reg_issue(i).info.wb_val)

      data_wb(i).addr := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.rd.addr, exe_reg_issue(i).info.rd.addr)

      branchJump.issue(i).valid := issueQueue(i).issue.valid || exe_inst_acc(i)
      branchJump.issue(i).branch := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.branch, exe_reg_issue(i).info.branch)

      loadStore.issue(i).valid := issueQueue(i).issue.valid ||
        (exe_inst_val(i) && exe_reg_issue(i).rs(0).valid)

      loadStore.issue(i).data := Mux(issueQueue(i).issue.valid,
        issueQueue(i).issue.bits.info.data(1), Fill(data_width, exe_reg_d_sel(i)(1)(REG)) & exe_rs_data(i)(1))

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

  val issue_cmp   = CmpId(exe_reg_issue(ALU1).id, exe_reg_issue(ALU2).id, stateCtrl.head, wOrder-1)
  val self_ready  = Wire(Vec(nInst, Bool()))
  val self_limit  = Wire(Vec(nInst, Bool()))
  val self_accept = Wire(Vec(nInst, Bool()))
  val ALU3_ready  = Wire(Vec(nInst, Bool()))
  val ALU3_valid  = Wire(Vec(nInst, Bool()))
  ALU3_ready(ALU1) := ( issue_cmp || self_accept(ALU2))
  ALU3_ready(ALU2) := (!issue_cmp || self_accept(ALU1))
  for (i <- 0 until nInst) {
    ALU3_valid(i) := (issueQueue(ALU3).tail.valid ||
      CmpId(issueQueue(ALU3).tail.id, exe_reg_issue(i).id, stateCtrl.head, wOrder-1))
    instQueue(i).issue.ready := self_accept(i) ||
      (ALU3_ready(i) && ALU3_valid(i) && issueQueue(ALU3).tail.ready)

    issueQueue(i).in.valid := exe_inst_val(i) && (!self_ready(i) || issueQueue(i).issue.valid ||
      (exe_reg_issue(i).mem_en && self_limit(i)))
    issueQueue(i).in.bits  := exe_reg_issue(i)
    issueQueue(i).in.bits.info.data := exe_op_data(i) //change data

    issueQueue(i).in.bits.data_sel := exe_reg_issue(i).rs.map(rs =>
      VecInit(data_wb.map(b => b.addr === rs.addr && b.valid)).asUInt) //change data sel

    issueQueue(i).in.bits.valid := issueQueue(i).tail.valid || //change valid
      CmpId(issueQueue(i).tail.id, exe_reg_issue(i).id, stateCtrl.head, wOrder-1)
    // if already send addr to loadstore, then...
    issueQueue(i).mem_acc := issueQueue(i).issue.valid || !exe_reg_issue(i).rs(0).valid || self_limit(i)

    issueQueue(i).limit := loadStore.issueable.valid && //contain forwarded limit info
      CmpId(loadStore.issueable.bits, exe_reg_issue(i).id, stateCtrl.head, wOrder-1)

    self_limit(i)  := loadStore.limit.valid && //absolutely current state limit info
      CmpId(loadStore.limit.bits, exe_reg_issue(i).id, stateCtrl.head, wOrder-1)

    self_ready(i)  := (0 until 2).map(j => exe_reg_issue(i).rs(j).valid).reduce(_&&_)
    self_accept(i) := !issueQueue(i).in.valid || issueQueue(i).in.ready
    exe_inst_acc(i) := exe_inst_val(i) && self_ready(i)
  }
  val sel_ALU1 = self_accept(ALU2) || (!self_accept(ALU1) && issue_cmp(ALU1))
  issueQueue(ALU3).in.valid := !self_accept.reduce(_&&_)
  issueQueue(ALU3).in.bits  := Mux(sel_ALU1, issueQueue(ALU1).in.bits, issueQueue(ALU2).in.bits)
  issueQueue(ALU3).in.bits.valid := Mux(sel_ALU1, ALU3_valid(ALU1), ALU3_valid(ALU2))
  issueQueue(ALU3).limit := Mux(sel_ALU1, issueQueue(ALU1).limit, issueQueue(ALU2).limit)
  issueQueue(ALU3).mem_acc := Mux(sel_ALU1, issueQueue(ALU1).mem_acc, issueQueue(ALU2).mem_acc)

  val exe_reg_csr = Reg(Bool())
  val exe_reg_csr_cmd  = RegInit(CSR.N)
  val exe_reg_csr_addr = Reg(UInt((CSR_ADDR_MSB-CSR_ADDR_LSB+1).W))
  exe_reg_csr_cmd := Mux(in_pvl(0) && stateCtrl.empty, instDecoder(0).csr_cmd,
    Mux(in_pvl(1) && stateCtrl.empty, instDecoder(1).csr_cmd, CSR.N))

  exe_reg_csr := in_pvl(0)
  exe_reg_csr_addr := Mux(in_pvl(0), io.front.inst(0).bits(CSR_ADDR_MSB, CSR_ADDR_LSB),
    io.front.inst(1).bits(CSR_ADDR_MSB, CSR_ADDR_LSB))

  csr.io := DontCare
  csr.io.retire := stateCtrl.retire
  // Add your own uarch counters here!
  csr.io.counters.foreach(_.inc := false.B)
  csr.io.rw.addr  := exe_reg_csr_addr
  csr.io.rw.wdata := Mux(exe_reg_csr, alu(0).result, alu(1).result)
  csr.io.rw.cmd   := exe_reg_csr_cmd
//  csr.io.pc       :=
  csr.io.xcpt     := false.B //stateCtrl.xcpt_o.valid
  when (csr.io.rw.cmd =/= CSR.N) {
    printf("CSR: Cyc= %d addr %x wdata %x cmd %x pc %x xcpt %x\n",
      io.cyc,
      csr.io.rw.addr,
      csr.io.rw.wdata,
      csr.io.rw.cmd,
      csr.io.pc,
      csr.io.xcpt)
  }

  for (i <- 0 until nInst) {
    printf("Core: Cyc= %d pc %x pair<%x %x> id %d rd %x:%x->%x rs [%x:%x->%x %x:%x->%x] inst: DASM(%x)\n",
      io.cyc,
      io.front.pc(i),
      io.front.inst(i).valid,
      io.front.inst(i).ready,
      stateCtrl.physic(i).id,
      stateCtrl.physic(i).rd.valid, stateCtrl.logic(i).rd.addr, stateCtrl.physic(i).rd.addr,
      stateCtrl.physic(i).rs(0).valid, stateCtrl.logic(i).rs(0).addr, stateCtrl.physic(i).rs(0).addr,
      stateCtrl.physic(i).rs(1).valid, stateCtrl.logic(i).rs(1).addr, stateCtrl.physic(i).rs(1).addr,
      Mux(instQueue(i).in.valid, io.front.inst(i).bits, BUBBLE))
  }

//  when (CycRange(io.cyc, 700, 800)) {
//    printf(p"xcpt ${stateCtrl.xcpt_o.valid} ${Hexadecimal(io.front.xcpt.bits)}\n")
//    for (i <- 0 until nInst) {
//      printf(p"exe_inst_$i: ${exe_reg_issue(i).valid} ${exe_reg_issue(i).id} ${exe_reg_issue(i).info.f1} ")
//    }
//    printf("\n")
//    printf(p"Core: " +
//      p"${self_accept(1)} := !${issueQueue(1).in.valid} || ${issueQueue(1).in.ready} " +
//      p"in.ready :=  ${issueQueue(1).tail.ready} && ${issueQueue(1).in.bits.valid}" +
//      p"in.bits.valid := ${issueQueue(1).tail.valid} || ${issueQueue(1).tail.id} " +
//      p" ${exe_reg_issue(1).id}, ${stateCtrl.head}\n")
//    printf(p"${branch_cap(0)} && " +
//      p"${physic_cap(0)} && " +
//      p"${memory_cap(0)} && " +
//      p"${issue_cap(0)} && " +
//      p"${privil_cap(0)}\n")
//    printf(p"inner_kill $inner_kill head ${stateCtrl.head} ${issueQueue(0).issue.valid} ${issueQueue(1).issue.valid} \n")
//    printf(
//      p"issueQ: Cyc= ${io.cyc} inner kill valid ${inner_kill.valid}\n" +
//      p"Input valid ${exe_inst_val(0)} " +
//      p"Input execute ${exe_inst_acc(0)} " +
//      p"in_val ${issueQueue(0).in.valid} " +
//      p"in_ready ${issueQueue(0).in.ready} " +
//      p"t_valid ${issueQueue(0).tail.valid} " +
//      p"t_ready ${issueQueue(0).tail.ready} " +
//      p"tail_id ${issueQueue(0).tail.id}\n")
//    printf(p"BranchJump " +
//      p"${branchJump.kill.valid} " +
//      p"${branchJump.kill.id} " +
//      p"${branchJump.kill.bidx}\n")
//    printf(p"InnerKill " +
//      p"${inner_kill.valid} " +
//      p"${inner_kill.id}\n")
//    printf(p"rename stage: " +
//      p"ir_data ${Hexadecimal(ir_data(0)(0))} " +
//      p"ir_rs ${ir_inst(0).rs} " +
//      p"imm ${ir_inst(0).info.imm} " +
//      p"mux op2_Sel ${ir_inst(0).info.op2_sel} \n")
//    printf(p"exe stage: " +
//      p"issue valid $issue_valid raw issue valid $exe_reg_valid " +
//      p"self_ready ${exe_reg_issue(1).rs(0).valid} ${exe_reg_issue(1).rs(1).valid} " +
//      p"rs1 ${exe_reg_issue(1).rs(0).addr} " +
//      p"rs2 ${exe_reg_issue(1).rs(1).addr}\n")
//    printf(p"exe stage: " +
////      p"op_data0 ${Hexadecimal(exe_op_data(1)(0))} " +
//      p"info_data0 ${Hexadecimal(exe_op_data(0)(0))} " +
//      p"info_data1 ${Hexadecimal(exe_op_data(0)(1))} " +
//      p"ready ${instQueue(0).issue.ready} " +
//      p"id ${exe_reg_issue(0).id} " +
//      p"sel_0 ${exe_reg_d_sel(0)(0)(REG)} " +
//      p"sel_1 ${exe_reg_d_sel(0)(1)(REG)}" +
//      p"\n")
////    for (i <- 0 until nInst) {
//      printf(
//        p"issueQvalid ${issueQueue(1).issue.valid} " +
//        p"id ${commit(1).id} " +
//        p"alu_op1 ${Hexadecimal(alu(1).data(0))} " +
//        p"alu_op2 ${Hexadecimal(alu(1).data(1))} " +
//        p"data ${Hexadecimal(issueQueue(1).issue.bits.info.data(1))} " +
//        p"fun ${alu(1).opcode} " +
//        p"result ${data_wb(1).valid}->${data_wb(1).addr} " +
//        p"${ Hexadecimal(alu(1).result)} " +
//        p"\n")
//    }
//    printf(p"issueQueue in valid "); for (i <- 0 until nALU) printf(p"${issueQueue(i).in.valid} ")
//    printf(p"self accept $self_accept " +
//      p"${instQueue(1).issue.ready} \n")
//    printf(p"fb_pc ${Hexadecimal(io.front.fb_pc)} " +
//      p"fb_type ${io.front.fb_type} " +
//      p"valid ${io.front.feedback.valid} " +
//      p"redirect ${io.front.feedback.bits.redirect}" +
//      p"tgt ${Hexadecimal(io.front.feedback.bits.tgt)} " +
//      p"\n")
//  }
//  when (io.cyc === 15837.U) {
//    printf(p"exe stage: " +
//      //      p"op_data0 ${Hexadecimal(exe_op_data(1)(0))} " +
//      p"info_data0 ${Hexadecimal(exe_op_data(0)(0))} " +
//      p"info_data1 ${Hexadecimal(exe_op_data(0)(1))} " +
//      p"ready ${instQueue(0).issue.ready} " +
//      p"id ${exe_reg_issue(0).id}" +
//      //      p"sel0 ${exe_reg_d_sel(1)(0)(REG)} " +
//      //      p"sel1 ${exe_reg_d_sel(1)(1)(IMM)}" +
//      p"\n")
//  }
}