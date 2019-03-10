package myCore

import chisel3._
import chisel3.util._

class LogicIO(val data_width: Int) extends Bundle {
  val rs = Input(Vec(2, new ByPass(5)))
  val rd = Input(new ByPass(5))
  val info = Input(new Info(data_width))
  val ready = Output(Bool())
  val valid = Input(Bool())
  val is_bj = Input(Bool())
}

class PhysicIO(val addr_width: Int, val id_width: Int) extends Bundle {
  val rs = Output(Vec(2, new ByPass(addr_width)))
  val rd = Output(new ByPass(addr_width))
  val id = Output(UInt(id_width.W))
  val ready = Input(Bool())
  val valid = Output(Bool())
  def fire: Bool = ready && valid
}

class KillInfo(val id_width: Int, val nBrchjr: Int) extends Bundle {
  val id   = UInt(id_width.W)
  val bidx = UInt(log2Ceil(nBrchjr).W)
}

class XcptInfo(val id_width: Int) extends Bundle {
  val id = UInt(id_width.W)
}

object CmpId { //if in1 < in2 then true, else false
  def apply(in1: UInt, in2: UInt, head: UInt): Bool = {
    Mux(head < in1, in1 < in2 || in2 < head, in1 < in2 && in2 < head)
  }
}

class Commit(val id_width: Int, val addr_width: Int) extends Bundle { //TODO: result write back and commit are at different moment
  val valid = Bool()
  val id    = UInt(id_width.W)
  val wb    = new ByPass(addr_width)
}

class State(val nEntry: Int) extends Bundle {
  val using = UInt(nEntry.W)
  val usecnt = UInt(log2Ceil(nEntry+1).W)
  val map   = Vec(32, UInt(log2Ceil(nEntry).W))
  val rename = Vec(32, Bool())
}

class StateCtrl(val data_width: Int) extends Module with Pram {
  val io = IO(new Bundle {
    //linear
    val logic  = Vec(nInst, new LogicIO(data_width))
    val physic = Vec(nInst, new PhysicIO(wPhyAddr, wOrder))
    val phyaddr = Output(Vec(nInst, Vec(2, UInt(wPhyAddr.W)))) //for speed time saving
    //feedback
    val commit = Input(Vec(nCommit, new Commit(wOrder, wPhyAddr)))     //successful
    val xcpt   = Input(Valid(new XcptInfo(wOrder)))          // except
    val kill   = Input(Valid(new KillInfo(wOrder, nBrchjr))) // branch mispredict
    //require
    val pump_id  = Input(Vec(nInst, UInt(wOrder.W)))
    val pump_out = Output(Vec(nInst, new Info(data_width)))
    //branch
    val bidx = Input(UInt(log2Ceil(nBrchjr).W))
  })

  val phyRegValid = RegInit(VecInit(Seq.fill(nPhyAddr)(false.B)))

  val latest = RegInit({
    val w = Wire(new State(nPhyAddr))
    w.map := DontCare
    w.using  := 0.U
    w.usecnt := 0.U
    w.rename := VecInit(Seq.fill(32)(false.B))
    w
  })

  val commit = RegInit({
    val w = Wire(new State(nPhyAddr))
    w.map := DontCare
    w.using  := 0.U
    w.usecnt := 0.U
    w.rename := VecInit(Seq.fill(32)(false.B))
    w
  })

  val backup = Reg(Vec(nBrchjr, new State(nPhyAddr)))
  //2 ports write and 2 ports read ram
  val info_imm  = Mem(nOrder, UInt(13.W))
  //2 ports write and 3 ports read ram
  val info_op   = Mem(nOrder, new InnerOp)
  val info_pc   = Mem(nOrder, UInt(data_width.W))
  //2 ports write and 4 ports read ram
  val id_logic  = Mem(nOrder, UInt(5.W))
  val id_physic = Mem(nOrder, UInt(wPhyAddr.W))
  val id_oldmap = Mem(nOrder, UInt(wPhyAddr.W))

  val reorder = RegInit({
    val w = Wire(new Bundle{
      val commit   = Vec(nOrder, Bool())
      val wbvalid  = Vec(nOrder, Bool())
      val recycle  = Vec(nOrder, Bool()) // recycle physical register resource
      val head = Vec(nCommit, UInt(wOrder.W))
      val tail = Vec(nInst, UInt(wOrder.W))
      val full = Bool()
      def empty:  Bool = head(0) === tail(0) && !full
      def capGe1: Bool = !full
      def capGe2: Bool = !full && tail(1) =/= head(0)
    })
    w.commit  := DontCare
    w.wbvalid := DontCare
    w.recycle := DontCare
    w.head := VecInit(Seq(0.U, 1.U, 2.U, 3.U))
    w.tail := VecInit(Seq(0.U, 1.U))
    w.full := false.B
    w
  })

  val xcpt_valid = RegInit(false.B)
  val xcpt_info  = Reg(new XcptInfo(wOrder))
  val xcpt_epc   = info_pc(reorder.head(0))

  val flush = reorder.head.map(xcpt_valid && xcpt_info.id === _)

  when (flush(0)) { xcpt_valid := false.B
  }.elsewhen (io.xcpt.valid) { xcpt_valid := true.B }

  when (io.xcpt.valid) {
    when(xcpt_valid) {
      when(CmpId(io.xcpt.bits.id, xcpt_info.id, reorder.head(0))) { //io.xcpt will lead commit info
        xcpt_info := io.xcpt.bits
      }
    }.otherwise { xcpt_info := io.xcpt.bits }
  }

  val headCommit  = Wire(Vec(nCommit, Bool()))
  val orderCommit = Wire(Vec(nCommit, Bool()))
  for (i <- 0 until nCommit) {
    when (io.commit(i).valid) { reorder.commit(io.commit(i).id) := true.B }
    if (i == 0) {
      headCommit(0)  := reorder.commit(reorder.head(0)) && !flush(0) && !reorder.empty //abount 12 gates
      orderCommit(0) := headCommit(0)
    } else {
      headCommit(i)  := reorder.commit(reorder.head(i)) && !flush(i) && reorder.head(i) =/= reorder.tail(0) //around 12 gates
      orderCommit(i) := (0 until i).map(j => headCommit(j)).reduce(_&&_) // max around 16 gates orderCommit(1) is around 14 gates
    }
  }

  val realloc = Wire(Vec(nInst, new Bundle{
    val wbaddr = UInt(wPhyAddr.W)
    val waready = Bool() //num
    val idready = Bool() //num
    val ready = Bool()   //one hot
  }))

  io.physic(0).id := reorder.tail(0)
  io.physic(1).id := Mux(io.logic(0).valid, reorder.tail(1), reorder.tail(0))
  io.physic(0).rd.addr := realloc(0).wbaddr
  io.physic(1).rd.addr := Mux(io.logic(0).valid && io.logic(0).rd.valid, realloc(1).wbaddr, realloc(0).wbaddr)

  realloc(0).wbaddr  := PriorityEncoder(~latest.using) // around 12 gates
  realloc(1).wbaddr  := ~PriorityEncoder(Reverse((~latest.using).asUInt)) // around 14 gates
  realloc(0).waready := latest.usecnt =/= nPhyAddr.U
  realloc(1).waready := latest.usecnt < (nPhyAddr-1).U   // around 8 gates
  realloc(0).idready := orderCommit(0) || reorder.capGe1
  realloc(1).idready := orderCommit(1) || (orderCommit(0) && reorder.capGe1) || reorder.capGe2 //around 18 gates

  val orderReady1: Bool = Mux(io.logic(0).valid, realloc(1).idready, realloc(0).idready) //around 20 gates
  val waddrValid0: Bool = io.logic(0).rd.valid && io.logic(0).valid
  val waddrReady1: Bool = Mux(waddrValid0, Mux(io.logic(1).rd.valid, realloc(1).waready, realloc(0).waready),
    (io.logic(1).rd.valid && realloc(0).waready) || !io.logic(1).rd.valid)

  realloc(0).ready := realloc(0).idready && (realloc(0).waready || !io.logic(0).rd.valid) // satisfy first one first
  realloc(1).ready := orderReady1 && waddrReady1 && !(io.logic(0).valid && !io.physic(0).ready)  // around 22 gates

  val sequence = io.logic(1).rs.map(rs => waddrValid0 && io.logic(0).rd.addr === rs.addr)
  for (j <- 0 until 2) {
    io.physic(0).rs(j).addr := latest.map(io.logic(0).rs(j).addr)
    io.physic(1).rs(j).addr := Mux(sequence(j), realloc(0).wbaddr, latest.map(io.logic(1).rs(j).addr))
    io.physic(0).rs(j).valid :=  phyRegValid(latest.map(io.logic(0).rs(j).addr)) || !io.logic(0).rs(j).valid // around 22 gates
    io.physic(1).rs(j).valid := (phyRegValid(latest.map(io.logic(0).rs(j).addr)) && !sequence(j)) || !io.logic(0).rs(j).valid
  }
  val overwrite = waddrValid0 && io.logic(0).rd.addr === io.logic(1).rd.addr
  id_oldmap(io.physic(0).id) := latest.map(io.logic(0).rd.addr)
  id_oldmap(io.physic(1).id) := Mux(overwrite, realloc(0).wbaddr, latest.map(io.logic(1).rd.addr))
  reorder.recycle(io.physic(0).id) := latest.rename(io.logic(0).rd.addr) && io.logic(0).rd.valid
  reorder.recycle(io.physic(1).id) := overwrite || latest.rename(io.logic(1).rd.addr) && io.logic(1).rd.valid

  for (i <- 0 until nInst) {
    for (j <- 0 until 2) { io.phyaddr(i)(j) := latest.map(io.logic(i).rs(j).addr) }

    io.pump_out(i).pc  := info_pc(io.pump_id(i))
    io.pump_out(i).op  := info_op(io.pump_id(i))
    io.pump_out(i).imm := info_imm(io.pump_id(i))

    io.logic(i).ready  := io.physic(i).ready && realloc(i).ready
    io.physic(i).valid := io.logic(i).valid  && realloc(i).ready // around 24 gates
    io.physic(i).rd.valid := io.logic(i).rd.valid

    when (io.physic(i).fire) {
      id_logic (io.physic(i).id) := io.logic(i).rd.addr
      id_physic(io.physic(i).id) := io.physic(i).rd.addr
      info_pc(io.physic(i).id)   := io.logic(i).info.pc
      info_op(io.physic(i).id)   := io.logic(i).info.op
      info_imm(io.physic(i).id)  := io.logic(i).info.imm

      reorder.commit(io.physic(i).id)  := false.B
      reorder.wbvalid(io.physic(i).id) := io.logic(i).rd.valid
    }
  }

  val inc_head = reorder.head.map(_ + nCommit.U)
  when (headCommit(0)) { when (headCommit(1)) { when (headCommit(2)) { when (headCommit(3)) {
    reorder.head(0) := inc_head(0); reorder.head(1) := inc_head(1); reorder.head(2) := inc_head(2); reorder.head(3) := inc_head(3)
  }.otherwise { reorder.head(0) := reorder.head(3); reorder.head(1) := inc_head(0); reorder.head(2) := inc_head(1); reorder.head(3) := inc_head(2) }
  }.otherwise { reorder.head(0) := reorder.head(2); reorder.head(1) := reorder.head(3); reorder.head(2) := inc_head(0); reorder.head(3) := inc_head(1) }
  }.otherwise { reorder.head(0) := reorder.head(1); reorder.head(1) := reorder.head(2); reorder.head(2) := reorder.head(3); reorder.head(3) := inc_head(0) }
  }

  val fire_2 = io.physic.map(_.fire).reduce(_&&_)
  val fire_1 = io.physic.map(_.fire).reduce(_||_)

  val inc_tail = reorder.tail.map(_ + nInst.U)
  when (flush(0)) {
    for (i <- 0 until nInst) reorder.tail(i) := reorder.head(i)
  }.elsewhen(io.kill.valid) {
    for (i <- 0 until nInst) reorder.tail(i) := io.kill.bits.id + i.U
  }.elsewhen(fire_2) {
    reorder.tail(0)  := inc_tail(0)
    reorder.tail(1)  := inc_tail(1)
  }.elsewhen(fire_1) {
    reorder.tail(0)  := reorder.tail(1)
    reorder.tail(1)  := inc_tail(0)
  }
  // if next cycle tail === head then the queue is full
  when (flush(0) || io.kill.valid) {
    reorder.full := false.B
  }.otherwise {
    when (fire_2) {
      when (orderCommit(2)) {
        reorder.full := false.B
      }.elsewhen(
        Mux(!headCommit(0), inc_tail(0) === reorder.head(0),
            !headCommit(1)&&inc_tail(0) === reorder.head(1))) {
        reorder.full := true.B }
    }.elsewhen(fire_1) {
      when (orderCommit(1)) {
        reorder.full := false.B
      }.elsewhen(!headCommit(0) && reorder.tail(1) === reorder.head(0)) {
        reorder.full := true.B }
    }.elsewhen (headCommit(0)) {
      reorder.full := false.B  }
  }

  when (io.physic(0).fire && io.logic(0).is_bj) { // TODO: not include jal
    backup(io.bidx).map   := latest.map
    backup(io.bidx).using := latest.using
    backup(io.bidx).rename := latest.rename
    when (io.logic(0).rd.valid) { // TODO: to deal with one is bj inst and other is normal inst
//      backup_map(io.bidx)(io.logic(0).rd.addr) := io.physic(0).rd.addr
//      backup_use(io.bidx) := used_next(1)
//      backup_val(io.bidx)(io.logic(0).rd.addr) := true.B
    }
  }

  val refresh: UInt = (0 until nCommit).map(i => Mux(reorder.recycle(reorder.head(i)) && orderCommit(i),
    UIntToOH(id_oldmap(reorder.head(i))), 0.U)).reduce(_|_) //around 24 gates
  val used_next = Wire(Vec(2, UInt(nPhyAddr.W)))
  used_next(0) := latest.using & (~refresh).asUInt
  used_next(1) := used_next(0) | PriorityEncoderOH(~latest.using)

  def Commit(logic: UInt, physics: UInt): Unit = {
    commit.map(logic) := physics
    commit.rename(logic) := true.B
  }
  val physic_addr = reorder.head.map(h => id_physic(h))
  val logic_addr  = reorder.head.map(h => id_logic(h))
  val wtbk_valid  = (0 until nCommit).map(i => reorder.wbvalid(reorder.head(i)) && orderCommit(i))
  val retaken: UInt = (0 until nCommit).map(i => Mux(wtbk_valid(i), UIntToOH(physic_addr(i)), 0.U)).reduce(_|_) //around 24 gates
  when (!flush(0)) {
    commit.using := (commit.using | retaken) & (~refresh).asUInt
    for (i <- 0 until nCommit) when (wtbk_valid(i)) { Commit(logic_addr(i), physic_addr(i)) }
  }

  when(flush(0)) {
    latest.map    := commit.map
    latest.using  := commit.using
    latest.rename := commit.rename
  }.elsewhen(io.kill.valid) {
    latest.map    := backup(io.kill.bits.bidx).map
    latest.rename := backup(io.kill.bits.bidx).rename
    latest.using  := backup(io.kill.bits.bidx).using & latest.using // the common used between backup and latest
  }.otherwise {
    for (i <- 0 until nInst) {
      when (io.physic(i).fire && io.logic(i).rd.valid) {
        latest.map(io.logic(i).rd.addr) := io.physic(i).rd.addr
        latest.rename(io.logic(i).rd.addr) := true.B
      }
    }
    when ((0 until nInst).map(i => io.physic(i).fire && io.logic(i).rd.valid).reduce(_&&_)) {
      latest.using := used_next(1) | Reverse(PriorityEncoderOH(Reverse((~latest.using).asUInt))) // around 28 gates
    }.elsewhen((0 until nInst).map(i => io.physic(i).fire && io.logic(i).rd.valid).reduce(_||_)) {
      latest.using := used_next(1)
    }.otherwise {
      latest.using := used_next(0)
    }
  }

  for (i <- 0 until nCommit) {
    when(io.commit(i).valid && io.commit(i).wb.valid) {
      phyRegValid(io.commit(i).wb.addr) := true.B
    }
  }
  for (i <- 0 until nInst) {
    when (io.physic(i).fire && io.logic(i).rd.valid) {
      phyRegValid(io.physic(i).rd.addr) := false.B
    }
  }


  printf(p"headCommit $headCommit\n")
  printf(p"id(0) = ${io.physic(0).id}, id(1) = ${io.physic(1).id}, rd(0) = ${io.physic(0).rd} , rd(1) = ${io.physic(1).rd}\n")
  printf(p"alloc_ready = ${realloc.map(_.ready)}\n")
  printf(p"used = ${Hexadecimal(latest.using)}\n")
  printf(p"refresh ${Hexadecimal(refresh)}\n")
  printf(p"fire = ${io.physic(0).fire}, ${io.physic(1).fire}\n")
  printf(p"rename 1 ${latest.map(1)} || 2 ${latest.map(2)}\n")
  val cnt = RegInit(0.U(32.W))
  cnt := cnt + 1.U
  printf(p"=======================cnt = $cnt=============================\n")
}
