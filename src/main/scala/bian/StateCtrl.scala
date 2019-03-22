package bian

import chisel3._
import chisel3.util._

class Logic(val data_width: Int) extends Bundle {
  val valid = Bool()
  val rs = Vec(2, new ByPass(5))
  val rd = new ByPass(5)
  val info = new Info(data_width)
}

class Physic(val addr_width: Int, val id_width: Int) extends Bundle {
  val rs = Vec(2, new ByPass(addr_width))
  val rd = new ByPass(addr_width)
  val id = UInt(id_width.W)
  val rs_need   = Vec(2, Bool()) //TODO
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

class ReqIO(id_width: Int) extends Bundle {
  val id = Input(UInt(id_width.W))
  val op = Output(new InnerOp)
}

class State(val nEntry: Int) extends Bundle {
  val useing = UInt(nEntry.W)
  val usecnt = UInt(log2Ceil(nEntry+1).W)
  val maptb  = Vec(32, UInt(log2Ceil(nEntry).W))
  val rename = Vec(32, Bool())
}

object CmpId { //if in1 < in2 then true, else false
  def apply(in1: UInt, in2: UInt, head: UInt): Bool = {
    Mux(head < in1, in1 < in2 || in2 < head, in1 < in2 && in2 < head)
  }
}

class StateCtrl extends Module with Pram {
  val io = IO(new Bundle {
    //linear
    val logic  = Input(Vec(nInst, new Logic(data_width)))
    val physic = Output(Vec(nInst, new Physic(wPhyAddr, wOrder)))
    val rsaddr = Output(Vec(nInst, Vec(2, UInt(wPhyAddr.W)))) //for speed time saving
    val order_ready  = Output(Vec(nInst, Bool()))
    val physic_ready = Output(Vec(nInst, Bool()))
    val order_inc  = Input(Vec(nInst, Bool()))
    val physic_inc = Input(Vec(nInst, Bool()))
    //branch
    val bidx1H = Input(UInt(nBrchjr.W))
    val brchjr = Input(Vec(nInst, Bool()))
    //feedback
    val commit = Input(Vec(nCommit, new Commit(wOrder, wPhyAddr)))
    val xcpt   = Input(new XcptInfo(wOrder))
    val kill   = Input(new KillInfo(wOrder, nBrchjr)) // TODO: use latch because of one cycle mispredict
    //require
    val head_id  = Output(UInt(wOrder.W))
    val req_id = Input(Vec(nInst, UInt(wOrder.W)))
    val req_io = new ReqIO(wOrder)
    val resp_out = Output(Vec(nInst, new Info(data_width)))
    val br_commit = Input(Valid(UInt(wOrder.W)))
    val st_commit = Input(Valid(UInt(wOrder.W)))
  })

  val phyRegValid = RegInit(0.U(nPhyAddr.W))

  val latest = RegInit({
    val w = Wire(new State(nPhyAddr))
    w.maptb  := DontCare
    w.useing := 0.U
    w.usecnt := 0.U
    w.rename := VecInit(Seq.fill(32)(false.B))
    w
  })
  val commit = RegInit({
    val w = Wire(new State(nPhyAddr))
    w.maptb  := DontCare
    w.useing := 0.U
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
      val head = Vec(nCommit, UInt((wOrder+1).W))
      val tail = Vec(nInst, UInt((wOrder+1).W))

      val commit  = UInt(nOrder.W)
      val useing  = Vec(nOrder, Bool())
      val useless = Vec(nOrder, Bool()) // recycle physical register resource
      def head_val(i: Int): Bool = tail(0) =/= head(i)
      def capty_gt0: Bool = head(0)(wOrder-1,0) =/= tail(0)(wOrder-1,0) || head(0)(wOrder) === head(1)(wOrder)
      def capty_gt1: Bool = tail(1) =/= head(0)
    })
    w.head := VecInit(Seq(0.U, 1.U, 2.U, 3.U))
    w.tail := VecInit(Seq(0.U, 1.U))
    w.commit  := DontCare
    w.useing  := DontCare
    w.useless := DontCare
    w
  })
  io.head_id := reorder.head(0)(wOrder-1,0)
  def push(i: Int, logic: UInt, physic: UInt, oldphy: UInt, pc: UInt,
           op: InnerOp, imm: UInt, useing: Bool, useless: Bool): Unit = {
    id_logic(reorder.tail(i)(wOrder-1,0))  := logic
    id_physic(reorder.tail(i)(wOrder-1,0)) := physic
    id_oldphy(reorder.tail(i)(wOrder-1,0)) := oldphy
    info_pc(reorder.tail(i)(wOrder-1,0))   := pc
    info_op(reorder.tail(i)(wOrder-1,0))   := op
    info_imm(reorder.tail(i)(wOrder-1,0))  := imm
    reorder.useing(reorder.tail(i)(wOrder-1,0))  := useing
    reorder.useless(reorder.tail(i)(wOrder-1,0)) := useless
  }

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
  xcpt_ctrl.epc := info_pc(reorder.head(0)(wOrder-1,0))
  xcpt_ctrl.flush := reorder.head.map(xcpt.valid && xcpt.id === _(wOrder-1,0))
  when (xcpt_ctrl.flush(0)) {xcpt.valid := false.B
  }.elsewhen (io.xcpt.valid) {xcpt.valid := true.B}
  when (io.xcpt.valid) {
    when(xcpt.valid) { when(CmpId(io.xcpt.id, xcpt.id, reorder.head(0)(wOrder-1,0))) { //io.xcpt will lead commit info
      xcpt.id := io.xcpt.id}
    }.otherwise {
      xcpt.id := io.xcpt.id
    }
  }

  val order_ctrl = Wire(new Bundle {
    val next_tail = Vec(nInst, UInt(wOrder.W))
    val inc_head  = Vec(nCommit, Bool())
    val next_head = Vec(nCommit, UInt(wOrder.W))
    val physic    = Vec(nCommit, UInt(wPhyAddr.W))
    val oldphy    = Vec(nCommit, UInt(wPhyAddr.W))
    val logic     = Vec(nCommit, UInt(5.W))
    val commited  = Vec(nCommit, Bool())
    val register  = UInt(nOrder.W)
    val retiring  = UInt(nOrder.W)
  })

  order_ctrl.logic  := reorder.head.map(h => id_logic(h(wOrder-1,0)))
  order_ctrl.physic := reorder.head.map(h => id_physic(h(wOrder-1,0)))
  order_ctrl.oldphy := reorder.head.map(h => id_oldphy(h(wOrder-1,0)))
  order_ctrl.commited(0) := order_ctrl.inc_head(0)
  for (i <- 1 until nCommit)
    order_ctrl.commited(i) := (0 until i).map(j => order_ctrl.inc_head(j)).reduce(_&&_)
  for (i <- 0 until nCommit) {
    //abount 12 gates
    order_ctrl.inc_head(i) := reorder.commit(reorder.head(i)(wOrder-1,0)) && reorder.head_val(i) && !xcpt_ctrl.flush(i)
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

  order_ctrl.next_tail := reorder.tail.map(_ + nInst.U)
  val kill_id = Cat(reorder.head(0)(wOrder), io.kill.id)
  when (xcpt_ctrl.flush(0)) {
    reorder.tail := Seq(reorder.head(0), reorder.head(1))
  }.elsewhen(io.kill.valid) {
    reorder.tail := Seq(kill_id, kill_id + 1.U)
  }.elsewhen(io.order_inc(0)) {
    when (io.order_inc(1)) {
      reorder.tail := Seq(order_ctrl.next_tail(0), order_ctrl.next_tail(1))
    }.otherwise {
      reorder.tail := Seq(reorder.tail(1), order_ctrl.next_tail(0))
    }
  }
  //around 24 gates
  order_ctrl.retiring := (0 until nCommit).map(i =>
     Fill(nOrder, io.commit(i).valid) & UIntToOH(io.commit(i).id)).reduce(_|_) |
    (Fill(nOrder, io.br_commit.valid) & UIntToOH(io.br_commit.bits)) |
    (Fill(nOrder, io.st_commit.valid) & UIntToOH(io.st_commit.bits))

  order_ctrl.register := Mux(io.order_inc(0), UIntToOH(reorder.tail(0)(wOrder-1,0)) |
    Mux(order_ctrl.inc_head(1), UIntToOH(reorder.tail(1)(wOrder-1,0)), 0.U), 0.U)
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
  io.req_io.op := info_op(io.req_io.id)
  for (i <- 0 until nInst) {
    rename.rs(i) := io.logic(i).rs.map(j => latest.maptb(j.addr))
    rename.rd(i) := latest.maptb(io.logic(i).rd.addr)
    io.physic(i).rd.valid := io.logic(i).rd.valid
    io.resp_out(i).pc  := info_pc(io.req_id(i))
    io.resp_out(i).op  := info_op(io.req_id(i))
    io.resp_out(i).imm := info_imm(io.req_id(i))
  }
  for (j <- 0 until 2) {
    io.physic(0).rs(j).addr  := rename.rs(0)(j)
    io.physic(1).rs(j).addr  := Mux(rename.rdAwt(j), io.physic(0).rd.addr, rename.rs(1)(j))
    io.physic(0).rs_need(j)  := latest.rename(io.physic(0).rs(j).addr)
    io.physic(1).rs_need(j)  := latest.rename(io.physic(1).rs(j).addr) || rename.rdAwt(j)
    io.physic(0).rs(j).valid := io.logic(0).rs(j).valid ||  phyRegValid(rename.rs(0)(j)) // around 22 gates
    io.physic(1).rs(j).valid := io.logic(0).rs(j).valid || (phyRegValid(rename.rs(1)(j)) && !rename.rdAwt(j))
  }

  when (io.order_inc(0)) {
    push(i = 0, logic = Mux(io.logic(0).valid, io.logic(0).rd.addr, io.logic(1).rd.addr),
      physic = Mux(io.logic(0).valid, io.physic(0).rd.addr, io.logic(1).rd.addr),
      oldphy = Mux(io.logic(0).valid, rename.rd(0), rename.rd(1)),
      pc = Mux(io.logic(0).valid, io.logic(0).info.pc, io.logic(1).info.pc),
      op = Mux(io.logic(0).valid, io.logic(0).info.op, io.logic(1).info.op),
      imm = Mux(io.logic(0).valid, io.logic(0).info.imm, io.logic(1).info.imm),
      useing = Mux(io.logic(0).valid, io.logic(0).rd.valid, io.logic(1).rd.valid),
      useless = Mux(io.logic(0).valid, latest.rename(io.logic(0).rd.addr) && io.logic(0).rd.valid,
        latest.rename(io.logic(1).rd.addr) && io.logic(1).rd.valid))
    when (io.order_inc(1)) {
      push(i = 1, logic = io.logic(1).rd.addr, physic = io.physic(1).rd.addr,
        oldphy = Mux(rename.wtAwt, io.physic(0).rd.addr, rename.rd(1)), pc = io.logic(1).info.pc,
        op = io.logic(1).info.op, imm = io.logic(1).info.imm, useing = io.logic(1).rd.valid,
        useless = (latest.rename(io.logic(1).rd.addr) && io.logic(1).rd.valid) || rename.wtAwt)
    }
  }

  val phy_ctrl = Wire(new Bundle {
    val useless = UInt(nPhyAddr.W)
    val useless_cnt = UInt(log2Ceil(nPhyAddr+1).W)
    val useless_dec = Vec(nCommit, Bool())

    val useing = UInt(nPhyAddr.W)
    val useing_cnt = UInt(log2Ceil(nPhyAddr+1).W)
    val useing_inc = Vec(nCommit, Bool())

    val inc_tail = Vec(nInst, UInt(nPhyAddr.W))

    val retiring = UInt(nPhyAddr.W)
    val register = UInt(nPhyAddr.W)
  })
  def retire(logic: UInt, physics: UInt): Unit = {
    commit.maptb(logic)  := physics
    commit.rename(logic) := true.B
  }
  def regist(logic: UInt, physics: UInt): Unit = {
    latest.maptb(logic)  := physics
    latest.rename(logic) := true.B
  }

  phy_ctrl.useless_dec := (0 until nCommit).map(i => reorder.useless(reorder.head(i)(wOrder-1,0)) && order_ctrl.commited(i))
  phy_ctrl.useless_cnt := PopCount(phy_ctrl.useless_dec)
  phy_ctrl.useless := (0 until nCommit).map(i =>
    UIntToOH(order_ctrl.oldphy(i))(nPhyAddr-1,0) &
    Fill(nPhyAddr, phy_ctrl.useless_dec(i))).reduce(_|_) //around 24 gates

  phy_ctrl.useing_inc := (0 until nCommit).map(i => reorder.useing(reorder.head(i)(wOrder-1,0)) && order_ctrl.commited(i))
  phy_ctrl.useing_cnt := PopCount(phy_ctrl.useing_inc)
  phy_ctrl.useing := (0 until nCommit).map(i =>
    UIntToOH(order_ctrl.physic(i))(nPhyAddr-1,0) &
    Fill(nPhyAddr, phy_ctrl.useing_inc(i))).reduce(_|_) //around 24 gates

  phy_ctrl.inc_tail(0)  := PriorityEncoderOH(~latest.useing)
  phy_ctrl.inc_tail(1)  := Reverse(PriorityEncoderOH(Reverse((~latest.useing).asUInt)))

  when (!xcpt_ctrl.flush(0)) {
    commit.useing := (commit.useing | phy_ctrl.useing) & (~phy_ctrl.useless).asUInt
    commit.usecnt := (commit.usecnt + phy_ctrl.useing_cnt) - phy_ctrl.useless_cnt
    for (i <- 0 until nCommit) when(phy_ctrl.useing_inc(i)) {
      retire(order_ctrl.logic(i), order_ctrl.physic(i))
    }
  }

  phy_ctrl.retiring := (0 until nCommit).map(i =>
    Fill(nPhyAddr, io.commit(i).valid && io.commit(i).wb.valid) &
    UIntToOH(io.commit(i).wb.addr)).reduce(_|_)
  phy_ctrl.register := Mux(io.physic_inc(0), phy_ctrl.inc_tail(0) |
    Mux(io.physic_inc(1), phy_ctrl.inc_tail(1), 0.U), 0.U)
  phyRegValid := (phyRegValid | phy_ctrl.retiring) & (~phy_ctrl.register).asUInt

  when(xcpt_ctrl.flush(0)) {
    latest.maptb  := commit.maptb
    latest.rename := commit.rename
    latest.useing := commit.useing
    latest.usecnt := commit.usecnt
  }.elsewhen(io.kill.valid) {
    latest.maptb  := backup(io.kill.bidx).maptb
    latest.rename := backup(io.kill.bidx).rename
    latest.useing := backup(io.kill.bidx).useing & (~phy_ctrl.useless).asUInt
    latest.usecnt := backup(io.kill.bidx).usecnt - phy_ctrl.useless_cnt
  }.otherwise {
    when (io.order_inc(0)) {
      when(io.logic(0).valid) {
        // FIXME: there are some wtAwt conflict, choose the latter one
        when (io.logic(0).rd.valid) {
          regist(io.logic(0).rd.addr, io.physic(0).rd.addr)
        }
        when (io.order_inc(1) && io.logic(1).rd.valid) {
          regist(io.logic(1).rd.addr, io.physic(1).rd.addr)
        }
      }.elsewhen(io.logic(1).rd.valid) {
        regist(io.logic(1).rd.addr, io.physic(0).rd.addr)
      }
    }
    latest.useing := (latest.useing | phy_ctrl.register) & (~phy_ctrl.useless).asUInt
    latest.usecnt := (latest.usecnt - phy_ctrl.useless_cnt) +
      Mux(io.physic_inc(0), Mux(io.physic_inc(1), 2.U, 1.U), 0.U)
  }

  val backup_wire = Wire(new Bundle {
    val valid  = Bool()
    val useing = UInt(nPhyAddr.W)
    val usecnt = UInt(log2Ceil(nPhyAddr+1).W)
  })
  val backup_reg = Reg(new Bundle {
    val valid  = Bool()
    val idx1H  = UInt(nBrchjr.W)
    val logic  = Vec(nInst, UInt(5.W))
    val physic = Vec(nInst, UInt(wPhyAddr.W))
    val remap  = Vec(nInst, Bool())
  })

  backup_wire.valid := io.brchjr.reduce(_||_)
  backup_reg.valid  := backup_wire.valid
  when (backup_wire.valid) {
    backup_reg.idx1H := io.bidx1H
    for (i <- 0 until nInst) {
      backup_reg.logic(i)  := io.logic(i).rd.addr
      backup_reg.physic(i) := io.physic(i).rd.addr
    }
    backup_reg.remap(0) := io.logic(0).valid && io.logic(0).rd.valid
    backup_reg.remap(1) := io.logic(1).valid && io.logic(1).rd.valid && !io.brchjr(0)
  }
  backup_wire.usecnt := Mux(backup_reg.remap.reduce(_&&_), 2.U,  Mux(backup_reg.remap.reduce(_||_), 1.U, 0.U))
  backup_wire.useing := Mux(backup_reg.remap.reduce(_&&_), UIntToOH(backup_reg.physic(0)) | UIntToOH(backup_reg.physic(1)),
    Mux(backup_reg.remap.reduce(_||_), UIntToOH(backup_reg.physic(0)), 0.U))

  for (i <- 0 until nBrchjr) {
    when (io.bidx1H(i) && backup_wire.valid) {
      backup(i).maptb  := latest.maptb
      backup(i).rename := latest.rename
      backup(i).useing := latest.useing & (~phy_ctrl.useless).asUInt
      backup(i).usecnt := latest.usecnt - phy_ctrl.useless_cnt
    }.elsewhen(backup_reg.idx1H(i) && backup_reg.valid) {
      for (j <- 0 until nInst) {
        when (backup_reg.remap(j)) {
          backup(i).maptb(backup_reg.logic(j))  := backup_reg.physic(j)
          backup(i).rename(backup_reg.logic(j)) := true.B
        }
      }
      backup(i).useing := (backup(i).useing | backup_wire.useing) & (~phy_ctrl.useless).asUInt
      backup(i).usecnt := (backup(i).usecnt + backup_wire.usecnt) - phy_ctrl.useless_cnt
    }.otherwise {
      backup(i).useing := backup(i).useing & (~phy_ctrl.useless).asUInt
      backup(i).usecnt := backup(i).usecnt - phy_ctrl.useless_cnt
    }
  }

  io.physic(0).rd.addr := PriorityEncoder(~latest.useing)
  io.physic(1).rd.addr := Mux(rename.wt, OHToUInt(phy_ctrl.inc_tail(1)), io.physic(0).rd.addr)
  io.physic(0).id := reorder.tail(0)(wOrder-1,0)
  io.physic(1).id := Mux(io.logic(0).valid, reorder.tail(1)(wOrder-1,0), reorder.tail(0)(wOrder-1,0))

  io.physic_ready(0) := latest.usecnt =/= nPhyAddr.U
  //based on io.physic(0).phy_ready
  io.physic_ready(1) := Mux(io.logic(0).rd.valid, latest.usecnt =/= (nPhyAddr-1).U, io.physic_ready(0))
  io.order_ready(0)  := reorder.capty_gt0 ||  order_ctrl.inc_head(0)
  io.order_ready(1)  := reorder.capty_gt1 || (order_ctrl.inc_head(0) && (reorder.capty_gt0 || order_ctrl.inc_head(1)))
}
