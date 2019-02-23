package myCore

import chisel3._
import chisel3.util._

class BranchIO(val data_width: Int) extends Bundle {
  val op1  = UInt(data_width.W) //branch: rs1_data,   jalr: rs1 + imm
  val op2  = UInt(data_width.W) //branch: rs2_data or jalr: pc
  val brtype = UInt(BR_N.getWidth.W)
}

class BJentry(val data_width: Int) extends Bundle with BTBParams { // not include jal
  val Tp   = UInt(CFIType.SZ.W)
  val Sel  = UInt(log2Ceil(nEntries).W)
  val pred = Bool()
  val contTg = UInt(data_width.W) //store pc+4
  val jumpTg = UInt(data_width.W) //store jump target if jump
}

class BranchCalc(val data_width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new BranchIO(data_width))
    val redirect = Output(Bool())
  })
  val br_eq:  Bool = io.in.op1 === io.in.op2
  val br_lt:  Bool = io.in.op1.asSInt < io.in.op2.asSInt
  val br_ltu: Bool = io.in.op1.asUInt < io.in.op2.asUInt
  io.redirect := MuxCase(false.B, Array(
    (io.in.brtype === BR_NE)  -> !br_eq,
    (io.in.brtype === BR_GE)  -> !br_lt,
    (io.in.brtype === BR_GEU) -> !br_ltu,
    (io.in.brtype === BR_EQ)  -> br_eq,
    (io.in.brtype === BR_LT)  -> br_lt,
    (io.in.brtype === BR_LTU) -> br_ltu
  ))
}

class Branch(val data_width: Int) extends Module with Pram {
  val io = IO(new Bundle {
    val push_id = Input(Valid(UInt(wOrder.W)))
    val bJ_idx  = Output(Valid(UInt(log2Ceil(nBrchjr).W)))
    val push    = Input(new BJentry(data_width))
    val pop_id  = Input(Vec(3, Valid(UInt(wOrder.W))))
    val pop     = Input(Vec(3, new BranchIO(data_width)))
    val kill    = Output(new KillInfo(wOrder, nBrchjr))
    val id_head = Input(UInt(wOrder.W))
  })
  val calc = Array.fill(3)(Module(new BranchCalc(data_width)).io)
  for (i <- 0 until 3) calc(i).in := io.pop(i)

  val branchTable = Reg(Vec(nBrchjr, new BJentry(data_width)))
  val branch_id   = Reg(Vec(nBrchjr, UInt(wOrder.W)))
  val branchvalid = RegInit(0.U(nBrchjr.W))

  val branchMatch = Wire(Vec(3, UInt(nBrchjr.W)))
  val branchEntry = Wire(Vec(3, new BJentry(data_width)))
  for (i <- 0 until 3) {
    branchMatch(i) := branchvalid & VecInit(branch_id.map(_ === io.pop_id(i).bits)).asUInt //cam
    branchEntry(i) := Mux1H(branchMatch(i), branchTable)
  }
  val misspredict = Wire(Vec(3, Bool()))
  misspredict := (0 until 3).map(i => branchEntry(i).pred ^ calc(i).redirect && io.pop_id(i).valid)
  val cmp = Wire(Vec(3, Bool()))
  cmp(0) := (IDcmp(io.pop_id(0).bits, io.pop_id(1).bits, io.id_head) && misspredict(0)) || !misspredict(1)
  cmp(1) := (IDcmp(io.pop_id(0).bits, io.pop_id(2).bits, io.id_head) && misspredict(0)) || !misspredict(2)
  cmp(2) := (IDcmp(io.pop_id(1).bits, io.pop_id(2).bits, io.id_head) && misspredict(1)) || !misspredict(2)
  val oldest = Wire(Vec(3, Bool()))
  oldest(0) :=  cmp(0) &&  cmp(1)
  oldest(1) := !cmp(0) &&  cmp(2)
  oldest(2) := !cmp(1) && !cmp(2)
  io.kill.valid   := misspredict.reduce(_||_)
  io.kill.id      := Mux1H(oldest, io.pop_id)
  io.kill.bJidx1H := Mux1H(oldest, branchMatch)

  val empty_vec: UInt = (~branchvalid).asUInt |
    (0 until 3).map(i => VecInit(branch_id.map(id => id === io.pop_id(i).bits && io.pop_id(i).valid)).asUInt).reduce(_|_)

  io.bJ_idx.bits  := PriorityEncoder(empty_vec)
  io.bJ_idx.valid := empty_vec.orR
  when (io.push_id.valid && io.bJ_idx.valid) {
    branchTable(io.bJ_idx.bits) := io.push_id.bits
  }

  val kill_valid: UInt = VecInit(branch_id.map(id => IDcmp(io.kill.id, id, io.id_head) || io.kill.id === id)).asUInt
  val alloc1H: UInt = PriorityEncoderOH(empty_vec)
  when (io.kill.valid) {
    branchvalid := branchvalid & (~kill_valid).asUInt
  }.otherwise {
    branchvalid := branchvalid & ((~empty_vec).asUInt | alloc1H)
  }
}