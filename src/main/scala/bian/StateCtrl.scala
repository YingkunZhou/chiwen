package bian

import chisel3._
import chisel3.util._

class Logic(val data_width: Int) extends Bundle {
  val valid = Bool()
  val rs = Vec(2, new ByPass(5))
  val rd = new ByPass(5)
  val info = new Info(data_width)
  val brchjr = Bool()
}

class Physic(val addr_width: Int, val id_width: Int) extends Bundle {
  val rs = Vec(2, new ByPass(addr_width))
  val rd = new ByPass(addr_width)
  val id = UInt(id_width.W)
  val rs_need   = Vec(2, Bool())
  val phy_ready = Bool()
  val id_ready  = Bool()
}

class KillInfo(val id_width: Int, val nBrchjr: Int) extends Bundle {
  val valid = Bool()
  val id   = UInt(id_width.W)
  val bidx = UInt(log2Ceil(nBrchjr).W)
}

class XcptInfo(val id_width: Int) extends Bundle {
  val valid = Bool()
  val id = UInt(id_width.W)
}

class Commit(val id_width: Int, val addr_width: Int) extends Bundle { //TODO: result write back and commit are at different moment
  val valid = Bool()
  val id    = UInt(id_width.W)
  val wb    = new ByPass(addr_width)
}

class UrgentIO(id_width: Int) extends Bundle {
  val id = Input(UInt(id_width.W))
  val op = Output(new InnerOp)
}

class State(val nEntry: Int) extends Bundle {
  val using = UInt(nEntry.W)
  val usecnt = UInt(log2Ceil(nEntry+1).W)
  val maptb  = Vec(32, UInt(log2Ceil(nEntry).W))
  val rename = Vec(32, Bool())
}

object CmpId { //if in1 < in2 then true, else false
  def apply(in1: UInt, in2: UInt, head: UInt): Bool = {
    Mux(head < in1, in1 < in2 || in2 < head, in1 < in2 && in2 < head)
  }
}

class StateCtrl(val data_width: Int) extends Module with Pram {
  val io = IO(new Bundle {
    val ready  = Input(Vec(nInst, Bool()))
    //linear
    val logic  = Input(Vec(nInst, new Logic(data_width)))
    val physic = Output(Vec(nInst, new Physic(wPhyAddr, wOrder)))
    val rsaddr = Output(Vec(nInst, Vec(2, UInt(wPhyAddr.W)))) //for speed time saving
    //feedback
    val commit = Input(Vec(nCommit, new Commit(wOrder, wPhyAddr)))
    val xcpt   = Input(new XcptInfo(wOrder))
    val kill   = Input(new KillInfo(wOrder, nBrchjr)) // TODO: use latch because of one cycle mispredict
    //require
    val pump_id  = Input(Vec(nInst, UInt(wOrder.W)))
    val pump_out = Output(Vec(nInst, new Info(data_width)))
    val urgent   = new UrgentIO(wOrder)
    //branch
    val bidx1H = Input(UInt(nBrchjr.W))
  })

  val phyRegValid = RegInit(0.U(nPhyAddr.W))

  val latest = RegInit({
    val w = Wire(new State(nPhyAddr))
    w.maptb := DontCare
    w.using  := 0.U
    w.usecnt := 0.U
    w.rename := VecInit(Seq.fill(32)(false.B))
    w
  })
  val commit = RegInit({
    val w = Wire(new State(nPhyAddr))
    w.maptb := DontCare
    w.using  := 0.U
    w.usecnt := 0.U
    w.rename := VecInit(Seq.fill(32)(false.B))
    w
  })
  val backup = Reg(Vec(nBrchjr, new State(nPhyAddr)))
  //2 ports write and 2 ports read ram
  val info_imm  = Mem(nOrder, UInt(12.W))
  //2 ports write and 3 ports read ram
  val info_op   = Mem(nOrder, new InnerOp)
  val info_pc   = Mem(nOrder, UInt(data_width.W))
  //2 ports write and 4 ports read ram
  val id_logic  = Mem(nOrder, UInt(5.W))
  val id_physic = Mem(nOrder, UInt(wPhyAddr.W))
  val id_oldphy = Mem(nOrder, UInt(wPhyAddr.W))

  val reorder = RegInit({
    val w = Wire(new Bundle{
      val commit  = UInt(nOrder.W)
      val useing  = Vec(nOrder, Bool())
      val useless = Vec(nOrder, Bool()) // recycle physical register resource
      val head = Vec(nCommit, UInt(wOrder.W))
      val tail = Vec(nInst, UInt(wOrder.W))
      val full = Bool()
    })
    w.commit  := DontCare
    w.useing  := DontCare
    w.useless := DontCare
    w.head := VecInit(Seq(0.U, 1.U, 2.U, 3.U))
    w.tail := VecInit(Seq(0.U, 1.U))
    w.full := false.B
    w
  })

  val xcpt = RegInit({
    val w = Wire(new XcptInfo(wOrder))
    w.valid := false.B
    w.id := DontCare
    w
  })
  val xcpt_ctrl = Wire(new Bundle {
    val epc = UInt(data_width.W)
    val flush = Vec(nCommit, Bool())
  })
  xcpt_ctrl.epc := info_pc(reorder.head(0))
  xcpt_ctrl.flush := reorder.head.map(xcpt.valid && xcpt.id === _)
  when (xcpt_ctrl.flush(0)) {xcpt.valid := false.B
  }.elsewhen (io.xcpt.valid) {xcpt.valid := true.B}
  when (io.xcpt.valid) {
    when(xcpt.valid) { when(CmpId(io.xcpt.id, xcpt.id, reorder.head(0))) { //io.xcpt will lead commit info
      xcpt.id := io.xcpt.id}
    }.otherwise {
      xcpt.id := io.xcpt.id
    }
  }

  val order_ctrl = Wire(new Bundle {
    val capty_gt  = Vec(nInst, Bool())
    val inc_tail  = Vec(nInst, Bool())
    val next_tail = Vec(nInst, UInt(wOrder.W))
    val head_val  = Vec(nCommit, Bool())
    val inc_head  = Vec(nCommit, Bool())
    val next_head = Vec(nCommit, UInt(wOrder.W))
    val physic    = Vec(nCommit, UInt(wPhyAddr.W))
    val oldphy    = Vec(nCommit, UInt(wPhyAddr.W))
    val logic     = Vec(nCommit, UInt(5.W))
    val commited  = Vec(nCommit, Bool())
    val register  = UInt(nOrder.W)
    val retiring  = UInt(nOrder.W)
  })

  order_ctrl.logic  := reorder.head.map(h => id_logic(h))
  order_ctrl.physic := reorder.head.map(h => id_physic(h))
  order_ctrl.oldphy := reorder.head.map(h => id_oldphy(h))
  order_ctrl.head_val(0) := reorder.tail(0) =/= reorder.head(0) || reorder.full
  order_ctrl.commited(0) := order_ctrl.inc_head(0)
  for (i <- 1 until nCommit) {
    order_ctrl.head_val(i) := reorder.tail(0) =/= reorder.head(i)
    order_ctrl.commited(i) := (0 until i).map(j => order_ctrl.inc_head(j)).reduce(_&&_)
  }
  for (i <- 0 until nCommit) {
    order_ctrl.inc_head(i) := reorder.commit(reorder.head(i)) && order_ctrl.head_val(i) && !xcpt_ctrl.flush(i) //abount 12 gates
  }
  order_ctrl.next_head := reorder.head.map(_ + nCommit.U)
  when (order_ctrl.inc_head(0)) {
    when (order_ctrl.inc_head(1)) { when (order_ctrl.inc_head(2)) { when (order_ctrl.inc_head(3)) {
      reorder.head := Seq(order_ctrl.next_head(0), order_ctrl.next_head(1), order_ctrl.next_head(2), order_ctrl.next_head(3))
    }.otherwise {
      reorder.head := Seq(reorder.head(3), order_ctrl.next_head(0), order_ctrl.next_head(1), order_ctrl.next_head(2)) }
    }.otherwise {
      reorder.head := Seq(reorder.head(2), reorder.head(3), order_ctrl.next_head(0), order_ctrl.next_head(1)) }
    }.otherwise {
      reorder.head := Seq(reorder.head(1), reorder.head(2), reorder.head(3), order_ctrl.next_head(0)) }
  }

  order_ctrl.capty_gt(0) := !reorder.full
  order_ctrl.capty_gt(1) :=  reorder.tail(1) =/= reorder.head(0)
  order_ctrl.next_tail := reorder.tail.map(_ + nInst.U)
  //inc_tail is very very important signals
  order_ctrl.inc_tail(0) := (io.logic(0).valid || io.logic(1).valid) && io.ready(0)
  order_ctrl.inc_tail(1) :=  io.logic(0).valid && io.logic(1).valid  && io.ready(1)

  order_ctrl.retiring := (0 until nCommit).map(i => Mux(io.commit(i).valid,
    UIntToOH(io.commit(i).id), 0.U)).reduce(_|_) //around 24 gates
  order_ctrl.register := Mux(order_ctrl.inc_tail(0), UIntToOH(reorder.tail(0)) |
    Mux(order_ctrl.inc_head(1), UIntToOH(reorder.tail(1)), 0.U), 0.U)
  reorder.commit := (reorder.commit & (~order_ctrl.register).asUInt) | order_ctrl.retiring

  val rename = Wire(new Bundle {
    val wt = Bool()
    val wtAwt = Bool()
    val rdAwt = Vec(nInst, Bool())
    val rs = Vec(nInst, Vec(2, UInt(wPhyAddr.W)))
    val rd = Vec(nInst, UInt(wPhyAddr.W))
  })
  rename.wt    := io.logic(0).rd.valid && io.logic(0).valid
  rename.wtAwt := rename.wt && io.logic(0).rd.addr === io.logic(1).rd.addr
  rename.rdAwt := io.logic(1).rs.map(rs => rename.wt && io.logic(0).rd.addr === rs.addr)
  io.rsaddr    := rename.rs
  io.urgent.op := info_op(io.urgent.id)
  for (i <- 0 until nInst) {
    rename.rs(i) := io.logic(i).rs.map(j => latest.maptb(j.addr))
    rename.rd(i) := latest.maptb(io.logic(i).rd.addr)
    io.physic(i).rd.valid := io.logic(i).rd.valid
    io.pump_out(i).pc  := info_pc(io.pump_id(i))
    io.pump_out(i).op  := info_op(io.pump_id(i))
    io.pump_out(i).imm := info_imm(io.pump_id(i))
  }
  for (j <- 0 until 2) {
    io.physic(0).rs(j).addr  := rename.rs(0)(j)
    io.physic(1).rs(j).addr  := Mux(rename.rdAwt(j), io.physic(0).rd.addr, rename.rs(1)(j))
    io.physic(0).rs_need(j)  := latest.rename(io.physic(0).rs(j).addr)
    io.physic(1).rs_need(j)  := latest.rename(io.physic(1).rs(j).addr) || rename.rdAwt(j)
    io.physic(0).rs(j).valid := io.logic(0).rs(j).valid ||  phyRegValid(rename.rs(0)(j)) // around 22 gates
    io.physic(1).rs(j).valid := io.logic(0).rs(j).valid || (phyRegValid(rename.rs(1)(j)) && !rename.rdAwt(j))
  }

  when (order_ctrl.inc_tail(0)) {
    id_logic(reorder.tail(0))  := Mux(io.logic(0).valid, io.logic(0).rd.addr, io.logic(1).rd.addr)
    id_physic(reorder.tail(0)) := Mux(io.logic(0).valid, io.physic(0).rd.addr, io.logic(1).rd.addr)
    id_oldphy(reorder.tail(0)) := Mux(io.logic(0).valid, rename.rd(0), rename.rd(1))
    info_pc(reorder.tail(0))   := Mux(io.logic(0).valid, io.logic(0).info.pc, io.logic(1).info.pc)
    info_op(reorder.tail(0))   := Mux(io.logic(0).valid, io.logic(0).info.op, io.logic(1).info.op)
    info_imm(reorder.tail(0))  := Mux(io.logic(0).valid, io.logic(0).info.imm, io.logic(1).info.imm)
    reorder.useing(reorder.tail(0))  := Mux(io.logic(0).valid, io.logic(0).rd.valid, io.logic(1).rd.valid)
    reorder.useless(reorder.tail(0)) := Mux(io.logic(0).valid,
      latest.rename(io.logic(0).rd.addr) && io.logic(0).rd.valid,
      latest.rename(io.logic(1).rd.addr) && io.logic(1).rd.valid)
    when (order_ctrl.inc_tail(1)) {
      id_logic(reorder.tail(1))  := io.logic(1).rd.addr
      id_physic(reorder.tail(1)) := io.physic(1).rd.addr
      id_oldphy(reorder.tail(1)) := Mux(rename.wtAwt, io.physic(0).rd.addr, rename.rd(1))
      info_pc(reorder.tail(1))   := io.logic(1).info.pc
      info_op(reorder.tail(1))   := io.logic(1).info.op
      info_imm(reorder.tail(1))  := io.logic(1).info.imm
      reorder.useing(reorder.tail(1))  := io.logic(1).rd.valid
      reorder.useless(reorder.tail(1)) := (latest.rename(io.logic(1).rd.addr) && io.logic(1).rd.valid) || rename.wtAwt
    }
  }

  when (xcpt_ctrl.flush(0)) {
    for (i <- 0 until nInst) reorder.tail(i) := reorder.head(i)
  }.elsewhen(io.kill.valid) {
    for (i <- 0 until nInst) reorder.tail(i) := io.kill.id + i.U
  }.elsewhen(order_ctrl.inc_tail(0)) {
    when (order_ctrl.inc_tail(1)) {
      reorder.tail := Seq(order_ctrl.next_tail(0), order_ctrl.next_tail(1))
    }.otherwise {
      reorder.tail := Seq(reorder.tail(1), order_ctrl.next_tail(0))
    }
  }

  when (xcpt_ctrl.flush(0) || io.kill.valid) {
    reorder.full := false.B
  }.elsewhen (order_ctrl.inc_tail(0)) {
    when (order_ctrl.inc_tail(1)) {
      when (order_ctrl.inc_head(0)) {
        when (!order_ctrl.inc_head(0)) {reorder.full := reorder.tail(1) === reorder.head(0)
        }.elsewhen(order_ctrl.inc_head(1) && order_ctrl.inc_head(2)) {
          reorder.full := false.B
        }
      }.otherwise {reorder.full := order_ctrl.next_tail(0) === reorder.head(0)}
    }
  }.elsewhen(order_ctrl.inc_head(0)) {reorder.full := false.B}

  val phy_ctrl = Wire(new Bundle {
    val useless = UInt(nPhyAddr.W)
    val useless_cnt = UInt(wOrder.W)
    val useless_dec = Vec(nCommit, Bool())

    val head = UInt(nPhyAddr.W)
    val inc_head = Vec(nCommit, Bool())

    val tail_push = Vec(nInst, Bool())
    val alloc_1H  = Vec(nInst, UInt(nPhyAddr.W))
    val next_tail = Vec(nInst+1, UInt(nPhyAddr.W))
    val inc_tail  = Vec(nInst, Bool())

    val retiring = UInt(nPhyAddr.W)
    val register = UInt(nPhyAddr.W)
  })
  def retire(logic: UInt, physics: UInt): Unit = {
    commit.maptb(logic)  := physics
    commit.rename(logic) := true.B
  }
  def regist(logic: UInt, physics: UInt): Unit = {
    latest.maptb(logic) := physics
    latest.rename(logic) := true.B
  }

  phy_ctrl.useless_dec := (0 until nCommit).map(i => reorder.useless(reorder.head(i)) && order_ctrl.commited(i))
  phy_ctrl.useless_cnt := PopCount(phy_ctrl.useless_dec)
  phy_ctrl.useless := (0 until nCommit).map(i => Mux(phy_ctrl.useless_dec(i),
    UIntToOH(order_ctrl.oldphy(i)), 0.U)).reduce(_|_) //around 24 gates

  phy_ctrl.inc_head := (0 until nCommit).map(i => reorder.useing(reorder.head(i)) && order_ctrl.commited(i))
  phy_ctrl.head := (0 until nCommit).map(i => Mux(phy_ctrl.inc_head(i),
    UIntToOH(order_ctrl.physic(i)), 0.U)).reduce(_|_) //around 24 gates

  phy_ctrl.next_tail(0) := latest.using & (~phy_ctrl.useless).asUInt
  phy_ctrl.alloc_1H(0)  := PriorityEncoderOH(~latest.using)
  phy_ctrl.next_tail(1) := phy_ctrl.next_tail(0) | phy_ctrl.alloc_1H(0)
  phy_ctrl.alloc_1H(1)  := Reverse(PriorityEncoderOH(Reverse((~latest.using).asUInt)))
  phy_ctrl.next_tail(2) := phy_ctrl.next_tail(1) | phy_ctrl.alloc_1H(1)
  phy_ctrl.tail_push := (0 until nInst).map(i => io.logic(i).valid && io.logic(i).rd.valid)
  phy_ctrl.inc_tail(0)  := io.ready(0) && phy_ctrl.tail_push(0) ||
    phy_ctrl.tail_push(1) && ((io.ready(0) && !io.logic(0).valid) || io.ready(1))
  phy_ctrl.inc_tail(1)  := phy_ctrl.tail_push(0) && phy_ctrl.tail_push(1) && io.ready(1)

  when (!xcpt_ctrl.flush(0)) {
    commit.using  := (~phy_ctrl.useless).asUInt & (commit.using | phy_ctrl.head)
    commit.usecnt := commit.usecnt - phy_ctrl.useless_cnt + PopCount(phy_ctrl.inc_head)
    for (i <- 0 until nCommit) when(phy_ctrl.inc_head(i)) {
      retire(order_ctrl.logic(i), order_ctrl.physic(i))
    }
  }

  phy_ctrl.retiring := (0 until nCommit).map(i => Mux(io.commit(i).valid && io.commit(i).wb.valid,
    UIntToOH(io.commit(i).wb.addr), 0.U)).reduce(_|_)
  phy_ctrl.register := Mux(phy_ctrl.inc_tail(0), phy_ctrl.alloc_1H(0) |
    Mux(phy_ctrl.inc_tail(1), phy_ctrl.alloc_1H(1), 0.U), 0.U)
  phyRegValid := (phyRegValid & (~phy_ctrl.register).asUInt) | phy_ctrl.retiring

  when(xcpt_ctrl.flush(0)) {
    latest.maptb  := commit.maptb
    latest.using  := commit.using
    latest.rename := commit.rename
    latest.usecnt := commit.usecnt
  }.elsewhen(io.kill.valid) {
    latest.maptb  := backup(io.kill.bidx).maptb
    latest.rename := backup(io.kill.bidx).rename
    latest.using  := backup(io.kill.bidx).using & (~phy_ctrl.useless).asUInt
    latest.usecnt := backup(io.kill.bidx).usecnt - phy_ctrl.useless_cnt
  }.otherwise {
    when (order_ctrl.inc_tail(0)) {
      when(io.logic(0).valid) {
        //there are some wtAwt conflict, choose the latter one
        when (io.logic(0).rd.valid) {
          regist(io.logic(0).rd.addr, io.physic(0).rd.addr)
        }
        when (order_ctrl.inc_tail(1) && io.logic(1).rd.valid) {
          regist(io.logic(1).rd.addr, io.physic(1).rd.addr)
        }
      }.elsewhen(io.logic(1).rd.valid) {
        regist(io.logic(1).rd.addr, io.physic(0).rd.addr)
      }
    }
    when (phy_ctrl.inc_tail(0)) {
      when (phy_ctrl.inc_tail(1)) {
        latest.using := phy_ctrl.next_tail(2)
      }.otherwise {
        latest.using := phy_ctrl.next_tail(1)}
    }.otherwise {
      latest.using := phy_ctrl.next_tail(0)
    }
    latest.usecnt := latest.usecnt - phy_ctrl.useless_cnt +
      Mux(phy_ctrl.inc_tail(0), Mux(phy_ctrl.inc_tail(1), 2.U, 1.U), 0.U)
  }

  val backup_reg = Reg(new Bundle {
    val valid  = Vec(nInst, Bool())
    val idx1H  = UInt(nBrchjr.W)
    val logic  = Vec(nInst, UInt(5.W))
    val physic = Vec(nInst, UInt(wPhyAddr.W))
    val remap  = Vec(nInst, Bool())
  })
  val backup_wire = Wire(new Bundle {
    val valid  = Vec(nInst, Bool())
    val using  = UInt(nPhyAddr.W)
    val usecnt = UInt(log2Ceil(nPhyAddr+1).W)
  })
  backup_wire.valid(0) := io.logic(0).valid && io.logic(0).brchjr && io.ready(0)
  backup_wire.valid(1) := io.logic(1).valid && io.logic(1).brchjr && io.ready(0) && (!io.logic(1).valid || io.ready(1))

  backup_reg.valid := backup_wire.valid
  when (backup_wire.valid.reduce(_||_)) {
    backup_reg.idx1H := io.bidx1H
    for (i <- 0 until nInst) {
      backup_reg.logic(i)  := io.logic(i).rd.addr
      backup_reg.physic(i) := io.physic(i).rd.addr
    }
    backup_reg.remap(0) := io.logic(0).valid && io.logic(0).rd.valid
    backup_reg.remap(1) := io.logic(1).valid && io.logic(1).rd.valid && !backup_wire.valid(0)
  }
  backup_wire.usecnt := Mux(backup_reg.remap.reduce(_&&_), 2.U,  Mux(backup_reg.remap.reduce(_||_), 1.U, 0.U))
  backup_wire.using  := Mux(backup_reg.remap.reduce(_&&_), UIntToOH(backup_reg.physic(0)) | UIntToOH(backup_reg.physic(1)),
    Mux(backup_reg.remap.reduce(_||_), UIntToOH(backup_reg.physic(0)), 0.U))

  for (i <- 0 until nBrchjr) {
    when (io.bidx1H(i) && backup_wire.valid.reduce(_||_)) {
      backup(i).maptb  := latest.maptb
      backup(i).rename := latest.rename
      backup(i).using  := phy_ctrl.next_tail(0)
      backup(i).usecnt := latest.usecnt - phy_ctrl.useless_cnt
    }.elsewhen(backup_reg.idx1H(i) && backup_reg.valid.reduce(_||_)) {
      for (j <- 0 until nInst) {
        when (backup_reg.remap(j)) {
          backup(i).maptb(backup_reg.logic(j))  := backup_reg.physic(j)
          backup(i).rename(backup_reg.logic(j)) := true.B
        }
      }
      backup(i).using  := (backup(i).using | backup_wire.using) & (~phy_ctrl.useless).asUInt
      backup(i).usecnt := backup(i).usecnt + backup_wire.usecnt - phy_ctrl.useless_cnt
    }.otherwise {
      backup(i).using  := backup(i).using & (~phy_ctrl.useless).asUInt
      backup(i).usecnt := backup(i).usecnt - phy_ctrl.useless_cnt
    }
  }

  io.physic(0).rd.addr := PriorityEncoder(~latest.using)
  io.physic(1).rd.addr := Mux(rename.wt, OHToUInt(phy_ctrl.alloc_1H(1)), io.physic(0).rd.addr)
  io.physic(0).id := reorder.tail(0)
  io.physic(1).id := Mux(io.logic(0).valid, reorder.tail(1), reorder.tail(0))

  io.physic(0).phy_ready := latest.usecnt =/= nPhyAddr.U
  io.physic(1).phy_ready := Mux(io.logic(0).rd.valid, latest.usecnt =/= (nPhyAddr-1).U, io.physic(0).phy_ready) //based on io.physic(0).phy_ready
  io.physic(0).id_ready  := order_ctrl.capty_gt(0) ||  order_ctrl.inc_head(0)
  io.physic(1).id_ready  := order_ctrl.capty_gt(1) || (order_ctrl.inc_head(0) && (order_ctrl.capty_gt(0) || order_ctrl.inc_head(1)))

}
