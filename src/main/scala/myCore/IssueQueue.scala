package myCore
import chisel3._
import chisel3.util._

class Info(val data_width: Int) extends Bundle {
  val op = new InnerOp
  val pc = UInt(data_width.W)
  val imm = UInt(12.W)
}

class InfoPlus(data_width: Int) extends Info(data_width) {
  val imm_l = UInt(8.W)
  val imm_z = UInt(5.W)
}

class Issue(val addr_width: Int, val id_width: Int) extends Bundle {
  val id = UInt(id_width.W)
  val rs = Vec(2, Valid(UInt(addr_width.W)))
  val rd = Valid(UInt(addr_width.W))
  val fix1 = Bool()
}

class InfoIssue (addr_width: Int, id_width: Int, val data_width: Int) extends Issue(addr_width, id_width) {
  val info = new InfoPlus(data_width)
}

class IssueQueue(val data_width: Int) extends Module with Pram {
  val io = IO(new Bundle{
    val in  = Flipped(Decoupled(new InfoIssue(wPhyAddr, wOrder, data_width)))
    val out = Decoupled(new InfoIssue(wPhyAddr, wOrder, data_width))
    val bypass  = Input(Vec(2*nCommit, Valid(UInt(wPhyAddr.W))))
    val forward = Output(Valid(UInt(wPhyAddr.W)))
    val counter = Output(UInt(wCount.W))
    val issueID = Output(UInt(wOrder.W))
    val issueInfo = Input(new Info(data_width))
  })

  val reg_issue   = Reg(new Issue(wPhyAddr, wOrder))
  val issue_valid = RegInit(false.B)

  val wire_rs_valid = Wire(Vec(2, Bool()))
  val reg_rs_valid  = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    wire_rs_valid(i) := (0 until nCommit).map(j =>
      io.bypass(j).bits === io.in.bits.rs(i).bits && io.bypass(j).valid).reduce(_||_)  || io.in.bits.rs(i).valid
    reg_rs_valid(i) := (0 until nCommit).map(j =>
      io.bypass(j).bits === reg_issue.rs(i).bits && io.bypass(j).valid).reduce(_||_)   || reg_issue.rs(i).valid
  }

  io.forward.bits  := reg_issue.rd.bits
  io.forward.valid := reg_issue.rd.valid && reg_issue.fix1 && reg_rs_valid(0) && reg_rs_valid(1)

  io.out.valid := issue_valid || io.in.valid
  val wire_issue = Wire(new Issue(wPhyAddr, wOrder))
  io.out.bits.id  := Mux(issue_valid, reg_issue.id, io.in.bits.id)
  io.out.bits.rd  := Mux(issue_valid, reg_issue.rd, io.in.bits.rd)
  io.out.bits.fix1:= Mux(issue_valid, reg_issue.fix1, io.in.bits.fix1)
  io.out.bits.rs  := Mux(issue_valid, reg_issue.rs, wire_issue.rs)
  io.out.bits.info.imm_z := io.in.bits.info.imm_z
  io.out.bits.info.imm_l := Mux(issue_valid, Cat(reg_issue.rs(1).bits(7-wPhyAddr, 0), reg_issue.rs(0).bits), io.in.bits.info.imm_l)
  io.out.bits.info.op := Mux(issue_valid, io.issueInfo.op, io.in.bits.info.op)
  io.out.bits.info.pc := Mux(issue_valid, io.issueInfo.pc, io.in.bits.info.pc)
  io.out.bits.info.imm := Mux(issue_valid, io.issueInfo.imm, io.in.bits.info.imm)
  val long_imm: Bool = io.in.bits.info.op.op2_sel === OP2_UJTYPE || io.in.bits.info.op.op2_sel === OP2_UTYPE
  wire_issue.rd := io.in.bits.rd
  wire_issue.id := io.in.bits.id
  wire_issue.fix1 := io.in.bits.fix1
  wire_issue.rs(0).bits := Mux(long_imm, io.in.bits.info.imm_l(wPhyAddr-1,0), io.in.bits.rs(0).bits)
  wire_issue.rs(1).bits := Mux(long_imm, Cat(Fill(2*wPhyAddr-8, 0.U(1.W)), io.in.bits.info.imm_l(7, wPhyAddr)), io.in.bits.rs(1).bits)
  wire_issue.rs(0).valid := wire_rs_valid(0)
  wire_issue.rs(1).valid := wire_rs_valid(1)

  val instQueue   = Reg(Vec(nEntry, new Issue(wPhyAddr, wOrder)))
  val counter     = RegInit(0.U(wCount.W))
  val head_snoop  = Wire(Vec(2, Bool()))
  val snoop       = Wire(Vec(2, Vec(nEntry, Bool())))
  val snoop_ready = Wire(Vec(nEntry, Bool()))
  val ready_ptr   = Wire(UInt(log2Ceil(nEntry).W))
  val forwd_ptr   = Wire(UInt(log2Ceil(nEntry).W))
  io.counter   := counter
  io.in.ready  := counter =/= nEntry.U /*|| io.rissue.fire*/
  when (!issue_valid || (io.out.fire && counter === 0.U)) {
    reg_issue  := wire_issue
    issue_valid := io.in.valid
  }.elsewhen(io.out.fire) {
    reg_issue  := instQueue(forwd_ptr)
    issue_valid := true.B
  }

  for (j <- 0 until 2) {
    snoop(j) := instQueue.map(inst => io.bypass.map(bypass =>
      bypass.bits === inst.rs(j).bits && bypass.valid).reduce(_||_) || inst.rs(j).valid)
  }
  snoop_ready := (0 until nEntry).map(i => (0 until 2).map(j => snoop(j)(i)).reduce(_&&_))

  ready_ptr := PriorityEncoder(snoop_ready)
  forwd_ptr := Mux(snoop_ready.asUInt.orR && (ready_ptr < counter), ready_ptr, 0.U(log2Ceil(nEntry).W))

  for (i <- 1 until nEntry) {
    when (io.out.fire && forwd_ptr < i.U && i.U < counter) { //the very special case is that 0 < i.U < 1
      for (j <- 0 until 2) {
        instQueue(i-1).rs(j).bits  := instQueue(i).rs(j).bits
        instQueue(i-1).rs(j).valid := snoop(j)(i)
      }
      instQueue(i-1).fix1 := instQueue(i).fix1
      instQueue(i-1).id := instQueue(i).id
    }
  }

 val counter_1: UInt = counter - 1.U
  when (issue_valid) {
    when (io.in.fire && io.out.fire) {
      when (counter =/= 0.U) {
        instQueue(counter_1) := wire_issue
      }
    }.elsewhen(io.in.fire) {
      instQueue(counter)   := wire_issue
      counter := counter + 1.U
    }.elsewhen(io.out.fire && counter =/= 0.U) {
      counter := counter_1
    }
  }

}
