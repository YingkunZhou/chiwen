package myCore

import chisel3._
import chisel3.util._

class LogicIO(val data_width: Int) extends Bundle {
  val rs = Input(Vec(2, Valid(UInt(5.W))))
  val rd = Input(Valid(UInt(5.W)))
  val info = Input(new Info(data_width))
  val ready = Output(Bool())
  val valid = Input(Bool())
  val brchjr = Input(Bool())
}

class PhysicIO(val addr_width: Int, val id_width: Int) extends Bundle {
  val rs = Output(Vec(2, Valid(UInt(addr_width.W))))
  val rd = Output(Valid(UInt(addr_width.W)))
  val id = Output(UInt(id_width.W))
  val ready = Input(Bool())
  val valid = Output(Bool())
  def fire: Bool = ready && valid
}

class KillInfo(val id_width: Int, val nBrchjr: Int) extends Bundle {
  val valid = Bool()
  val id    = UInt(id_width.W)
  val bJidx = UInt(log2Ceil(nBrchjr).W)
}

class XcptInfo(val id_width: Int) extends Bundle {
  val id = UInt(id_width.W)
}

object Cmp {
  def apply(in1: UInt, in2: UInt, head: UInt): Bool = {
    Mux(head < in1, in1 < in2 || in2 < head, in1 < in2 && in2 < head)
  }
}

class StateCtrl(val data_width: Int) extends Module with Pram {
  val io = IO(new Bundle {
    val logic  = Vec(nInst, new LogicIO(data_width))
    val physic = Vec(nInst, new PhysicIO(wPhyAddr, wOrder))
    val commit = Input(Vec(nCommit, Valid(UInt(wOrder.W))))
    val xcpt   = Input(Valid(new XcptInfo(wOrder)))
    val kill   = Input(new KillInfo(wOrder, nBrchjr))
    val issueID   = Input(Vec(nInst, UInt(wOrder.W)))
    val issueInfo = Output(Vec(nInst, new Info(data_width)))
  })
  val phyRegValid = RegInit(0.U(nPhyAddr.W))

  val latest_map   = Reg(Vec(32, UInt(log2Ceil(nPhyAddr).W)))
  val latest_used  = RegInit(0.U(nPhyAddr.W))
  val latest_valid = RegInit(VecInit(Seq.fill(32)(false.B)))
  val commit_map   = Reg(Vec(32, UInt(log2Ceil(nPhyAddr).W)))
  val commit_used  = RegInit(0.U(nPhyAddr.W))
  val commit_valid = RegInit(VecInit(Seq.fill(32)(false.B)))
  val backup_map   = Reg(Vec(nBrchjr, Vec(32, UInt(log2Ceil(nPhyAddr).W))))
  val backup_used  = RegInit(VecInit(Seq.fill(nBrchjr)(0.U(nPhyAddr.W))))
  val backup_valid = RegInit(VecInit(Seq.fill(nBrchjr)(VecInit(Seq.fill(32)(false.B)))))

  val id_infoimm = Mem(nOrder, UInt(12.W))
  val id_infoop  = Mem(nOrder, new InnerOp)
  val id_infopc  = Mem(nOrder, UInt(data_width.W))

  val id_logic  = Mem(nOrder, UInt(5.W))
  val id_physic = Mem(nOrder, UInt(wPhyAddr.W))
  val id_wtback = RegInit(VecInit(Seq.fill(nOrder)(false.B)))
  val id_oldphy = Mem(nOrder, UInt(wPhyAddr.W))
  val id_oldval = RegInit(VecInit(Seq.fill(nOrder)(false.B)))

  val id_commit = RegInit(VecInit(Seq.fill(nOrder)(false.B))) //only commit logic, and commit logic will recycle it
  val id_head   = RegInit(VecInit(Seq(0.U(wOrder.W), 1.U(wOrder.W), 2.U(wOrder.W), 3.U(wOrder.W))))
  val id_tail   = RegInit(VecInit(Seq(0.U(wOrder.W), 1.U(wOrder.W))))
  val id_full   = RegInit(false.B)
  val headCommit  = Wire(Vec(nCommit, Bool()))
  val orderCommit = Wire(Vec(nCommit, Bool()))

  val xcpt_info = Reg(new XcptInfo(wOrder))
  val xcpt_valid = RegInit(false.B)
  val xcpt_pc = id_infopc(id_head(0))
  val flush: Bool = (io.xcpt.valid && io.xcpt.bits.id === id_head(0)) || (xcpt_valid && xcpt_info.id === id_head(0))

  when (flush) { xcpt_valid := false.B
  }.elsewhen (io.xcpt.valid) { xcpt_valid := true.B }

  when (io.xcpt.valid) {
    when(xcpt_valid) {
      when(Cmp(io.xcpt.bits.id, xcpt_info.id, id_head(0))) {
        xcpt_info := io.xcpt.bits
      }
    }.otherwise { xcpt_info := io.xcpt.bits }
  }

  for (i <- 0 until nCommit) {
    when (orderCommit(i))           { id_commit(io.commit(i).bits) := false.B
    }.elsewhen (io.commit(i).valid) { id_commit(io.commit(i).bits) := true.B }

    if (i == 0) {
      headCommit(0)  := io.commit.map(commit => id_head(0) === commit.bits && commit.valid).reduce(_||_) && !flush
      orderCommit(0) := headCommit(0)
      when (headCommit(0)) { id_head(0) :=
        Mux(!headCommit(1), id_head(1), Mux(!headCommit(2), id_head(2),
        Mux(!headCommit(3), id_head(3), id_head(0) + 4.U)))
      }
    } else {
      headCommit(i)  := (io.commit.map(commit => id_head(i) === commit.bits && commit.valid).reduce(_||_) || id_commit(id_head(i))) &&
        (!io.xcpt.valid || io.xcpt.bits.id =/= id_head(i)) && (!xcpt_valid || xcpt_info.id =/= id_head(i)) //around 14 gates
      orderCommit(i) := (0 until i).map(j => headCommit(j)).reduce(_&&_) // max around 18 gates
      when (headCommit(0)) { id_head(i) :=
        Mux(!headCommit(1), id_head(1)+i.U, Mux(!headCommit(2), id_head(2)+i.U,
        Mux(!headCommit(3), id_head(3)+i.U, id_head(0)+(i+4).U)))
      }
    }
  }

  val allocWaddr = Wire(Vec(nInst, UInt(wPhyAddr.W)))
  val waddrReady = Wire(Vec(nInst, Bool()))
  val orderReady = Wire(Vec(nInst, Bool()))
  allocWaddr(0) := PriorityEncoder(~latest_used)                    // around 12 gates
  allocWaddr(1) := ~PriorityEncoder(Reverse((~latest_used).asUInt)) // around 14 gates
  waddrReady(0) := (~latest_used).asUInt.orR                        // around  6 gates
  waddrReady(1) := allocWaddr(0) =/= allocWaddr(1)                  // around 16 gates
  orderReady(0) := orderCommit(0) || !id_full
  orderReady(1) := orderCommit(1) || ((orderCommit(0) || id_tail(1) =/= id_head(0)) && !id_full) //around 18 gates
  val alloc_ready  = Wire(Vec(nInst, Bool()))
  alloc_ready(0) := orderReady(0) && (waddrReady(0) || !io.logic(0).rd.valid)
  val orderReady1 = Mux(io.logic(0).valid, io.physic(0).ready && orderReady(1), orderReady(0)) //around 20 gates
  val waddrReady1 = Mux(io.logic(0).valid && io.logic(0).rd.valid,
    waddrReady(1) || (waddrReady(0) && !io.logic(1).rd.valid),
    waddrReady(0) || !io.logic(1).rd.valid) // around 20 gates
  alloc_ready(1) := orderReady1 && waddrReady1 // around 22 gates

  io.physic(0).id := id_tail(0)
  io.physic(1).id := Mux(io.logic(0).valid, id_tail(1), id_tail(0))
  io.physic(0).rd.bits := allocWaddr(0)
  io.physic(1).rd.bits := Mux(io.logic(0).valid && io.logic(0).rd.valid, allocWaddr(1), allocWaddr(0))

  for (i <- 0 until nInst) {
    io.issueInfo(i).pc  := id_infopc(io.issueID(i))
    io.issueInfo(i).op  := id_infoop(io.issueID(i))
    io.issueInfo(i).imm := id_infoimm(io.issueID(i))

    for (j <- 0 until 2) {
      io.physic(i).rs(j).bits  := latest_map(io.logic(i).rs(j).bits)
      io.physic(i).rs(j).valid := phyRegValid(latest_map(io.logic(i).rs(j).bits)) || !io.logic(i).rs(j).valid // around 22 gates
    }
    io.logic(i).ready := io.physic(i).ready && alloc_ready(i)
    io.physic(i).valid := io.logic(i).valid && alloc_ready(i) // around 24 gates
    io.physic(i).rd.valid := io.logic(i).rd.valid
    when (io.physic(i).fire) {
      id_logic (io.physic(i).id) := io.logic(i).rd.bits
      id_wtback(io.physic(i).id) := io.logic(i).rd.valid
      id_physic(io.physic(i).id) := io.physic(i).rd.bits
      id_oldphy(io.physic(i).id) := latest_map(io.logic(i).rd.bits)
      id_oldval(io.physic(i).id) := latest_valid(io.logic(i).rd.bits) && io.logic(i).rd.valid

      id_infopc(io.physic(i).id) := io.logic(i).info.pc
      id_infoop(io.physic(i).id) := io.logic(i).info.op
      id_infoimm(io.physic(i).id):= io.logic(i).info.imm
    }
  }

  val next_tail: UInt = id_tail(0) + 2.U
  when (flush) {
    for (i <- 0 until nInst) id_tail(i) := id_head(i)
  }.elsewhen(io.kill.valid) {
    for (i <- 0 until nInst) id_tail(i) := io.kill.id + i.U
  }.elsewhen (io.physic(0).fire && io.physic(1).fire) {
    id_tail(0)  := next_tail
    id_tail(1)  := id_tail(1) + 2.U
  }.elsewhen(io.physic(0).fire || io.physic(1).fire) {
    id_tail(0)  := id_tail(1)
    id_tail(1)  := id_tail(1) + 1.U
  }

  when (flush || io.kill.valid)                               { id_full := false.B
  }.otherwise {
    when (io.physic(0).fire && io.physic(1).fire) { // around 28 gates
      when (orderCommit(2))                                   { id_full := false.B
      }.elsewhen((next_tail === id_head(0) && !headCommit(0)) ||
        (next_tail === id_head(1) && !headCommit(1)))         { id_full := true.B  }
    }.elsewhen(io.physic(0).fire || io.physic(1).fire) {
      when (orderCommit(1))                                   { id_full := false.B
      }.elsewhen(id_tail(1) === id_head(0) && !headCommit(0)) { id_full := true.B  }
    }.elsewhen (headCommit(0))                                { id_full := false.B }
  }

  val bkupPtr  = RegInit(0.U(log2Ceil(nBrchjr).W))
  val bkupFire = RegNext((0 until nInst).map(i => io.physic(i).fire && io.logic(i).brchjr).reduce(_||_))
  when (bkupFire) {
    backup_map(bkupPtr)  := latest_map
    backup_used(bkupPtr) := latest_used
    backup_valid(bkupPtr):= latest_valid
    if (isPow2(nBrchjr)) bkupPtr := bkupPtr + 1.U
    else bkupPtr := Mux(bkupPtr === (nBrchjr-1).U, 0.U, bkupPtr + 1.U)
  }

  val physic_addr = Wire(Vec(nCommit, UInt(wPhyAddr.W)))
  for (i <- 0 until nCommit) physic_addr(i) := id_physic(id_head(i))

  val refresh: UInt = (0 until nCommit).map(i => Mux(id_oldval(id_head(i)) && orderCommit(i),
    UIntToOH(id_oldphy(id_head(i))), 0.U)).reduce(_|_) //around 24 gates
  val retaken: UInt = (0 until nCommit).map(i => Mux(id_wtback(id_head(i)) && orderCommit(i),
    UIntToOH(physic_addr(i)), 0.U)).reduce(_|_) //around 24 gates
  val used_next = Wire(Vec(2, UInt(nPhyAddr.W)))
  used_next(0) := latest_used & (~refresh).asUInt
  used_next(1) := used_next(0) | PriorityEncoderOH(~latest_used)

  def retire(logic: UInt, physics: UInt): Unit = {
    commit_map(logic)  := physics
    commit_valid(logic):= true.B
  }

  when (!flush) {
    commit_used := (commit_used | retaken) & (~refresh).asUInt
    for (i <- 0 until nCommit) {
      when (orderCommit(i) && id_wtback(id_head(i))) {
        retire(id_logic(id_head(i)), physic_addr(i))
      }
    }
  }

  when(flush) {
    latest_map  := commit_map
    latest_used := commit_used
    latest_valid:= commit_valid
  }.elsewhen(io.kill.valid) {
    latest_map   := backup_map(io.kill.bJidx)
    latest_valid := backup_valid(io.kill.bJidx)
    latest_used  := backup_used(io.kill.bJidx) & latest_used// the common used between backup and latest
  }.otherwise {
    for (i <- 0 until 2) {
      when (io.physic(i).fire && io.logic(i).rd.valid) {
        latest_map(io.logic(i).rd.bits)   := io.physic(i).rd.bits
        latest_valid(io.logic(i).rd.bits) := true.B
      }
    }
    when ((0 until 2).map(i => io.physic(i).fire && io.logic(i).rd.valid).reduce(_&&_)) {
      latest_used := used_next(1) | Reverse(PriorityEncoderOH(Reverse((~latest_used).asUInt))) // around 28 gates
    }.elsewhen((0 until 2).map(i => io.physic(i).fire && io.logic(i).rd.valid).reduce(_||_)) {
      latest_used := used_next(1)
    }.otherwise {
      latest_used := used_next(0)
    }
  }

  printf(p"headCommit $headCommit\n")
  printf(p"id(0) = ${io.physic(0).id}, id(1) = ${io.physic(1).id}, rd(0) = ${io.physic(0).rd} , rd(1) = ${io.physic(1).rd}\n")
  printf(p"alloc_ready = $alloc_ready\n")
  printf(p"used = ${Hexadecimal(latest_used)}\n")
  printf(p"refresh ${Hexadecimal(refresh)}\n")
  printf(p"fire = ${io.physic(0).fire}, ${io.physic(1).fire}\n")
  printf(p"rename 1 ${latest_map(1)} || 2 ${latest_map(2)}\n")
  val cnt = RegInit(0.U(32.W))
  cnt := cnt + 1.U
  printf(p"=======================cnt = $cnt=============================\n")
}
