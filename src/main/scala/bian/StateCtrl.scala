package bian

import chisel3._
import chisel3.util._

trait BackParam {
  val nPhyAddr = 60
  val nOrder   = 32
  require(isPow2(nOrder))
  val data_width = 32
  val wPhyAddr = log2Ceil(nPhyAddr)
  val wOrder   = log2Ceil(nOrder)
  val nCommit  = 4
  val nBrchjr  = 4
  val nInst    = 2
  val REG = 0
  val IMM = 1
  val nALU = 3
  val ALU1 = 0
  val ALU2 = 1
  val ALU3 = 2
  val LOAD = 3
  val nIssue = Array(2, 2, 4)
}

class Logic(val data_width: Int) extends Bundle {
  val rs = Vec(2, new ByPass(5))
  val rd = new ByPass(5)
  val op = new OpCode
  val pc = UInt(data_width.W)
}

class Physic(val addr_width: Int, val id_width: Int) extends Bundle {
  val rs = Vec(2, new ByPass(addr_width))
  val rd = new ByPass(addr_width)
  val id = UInt(id_width.W)
  val undef = Vec(2, Bool()) //TODO
}

class KillInfo(val id_width: Int, val nBrchjr: Int) extends Bundle {
  val valid = Bool()
  val id   = UInt(id_width.W)
  val bidx = UInt(log2Ceil(nBrchjr).W)
}

class XcptInfoI(val id_width: Int) extends Bundle {
  val valid = Bool()
  val id = UInt(id_width.W)
}

class XcptInfoO(id_width: Int, val data_width: Int)
  extends XcptInfoI(id_width) {
  val pc = UInt(data_width.W)
}

class Commit(val id_width: Int, val addr_width: Int) extends Bundle { //TODO: result write back and commit are at different moment
  val valid = Bool()
  val id    = UInt(id_width.W)
  val wb    = new ByPass(addr_width)
}

class OpReqIO(val id_width: Int) extends Bundle {
  val id = Input(UInt(id_width.W))
  val op = Output(new OpCode)
}

class State(val nEntry: Int) extends Bundle {
  val useing = UInt(nEntry.W)
  val usecnt = UInt(log2Ceil(nEntry+1).W)
  val maptb  = Vec(32, UInt(log2Ceil(nEntry).W))
  val rename = Vec(32, Bool())
}

object CmpId { //if in1 <= in2 then true, else false FIXME tongyi
  def apply(in1: UInt, in2: UInt, head: UInt): Bool = {
    (head < in1 && (in1 <= in2 || in2 < head)) ||
    (head > in1 &&  in1 <= in2 && in2 < head)  ||
    head === in1
  }
}

class StateCtrl extends Module with BackParam {
  val io = IO(new Bundle {
    //linear
    val logic     = Input(Vec(nInst, new Logic(data_width)))
    val first     = Input(Bool()) // use some trick here
    val physic    = Output(Vec(nInst, new Physic(wPhyAddr, wOrder)))
    val rsaddr    = Output(Vec(nInst, Vec(2, UInt(wPhyAddr.W)))) //for speed time saving
    val id_ready  = Output(Vec(nInst, Bool()))
    val phy_ready = Output(Vec(nInst, Bool()))
    val inc_order = Input(Vec(nInst, Bool()))
    val phy_valid = Input(Vec(nInst, Bool()))
    //branch
    val bidx1H = Input(UInt(nBrchjr.W))
    val brchjr = Input(Vec(nInst, Bool())) //without valid
    val bkup_alloc = Input(Bool())
    //feedback
    val br_commit = Input(Valid(UInt(wOrder.W)))
    val st_commit = Input(Valid(UInt(wOrder.W)))
    val commit = Input(Vec(nCommit, new Commit(wOrder, wPhyAddr)))
    val kill   = Input(new KillInfo(wOrder, nBrchjr)) // TODO: use latch because of one cycle mispredict
    val split  = Input(Bool())
    val head   = Output(UInt(wOrder.W))
    val empty  = Output(Bool())
    val xcpt_i = Input(new XcptInfoI(wOrder))
    val xcpt_o = Output(new XcptInfoO(wOrder, nBrchjr))
    //require
    val req_io  = Vec(nALU, new OpReqIO(wOrder))
    val req_id  = Input(Vec(nInst, UInt(wOrder.W)))
    val resp_pc = Output(Vec(nInst, UInt(data_width.W)))
    val retire = Output(UInt(3.W))
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
  //2 ports write and 3 ports read ram
  val id_pc = Mem(nOrder, UInt(data_width.W))
  val id_op = Mem(nOrder, new OpCode)
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
      def emtpy: Bool = head(0) === tail(0)
      def tail_val0: Bool = head(0)(wOrder-1,0) =/= tail(0)(wOrder-1,0) || head(0)(wOrder) === head(1)(wOrder)
      def tail_val1: Bool = head(0)(wOrder-1,0) =/= tail(1)(wOrder-1,0) //FIXME
      def head_val(i: Int): Bool = tail(0) =/= head(i) && commit(head(i)(wOrder-1,0))
      def newphy(i: Int, in: Bool): Unit = { useing(tail(i)(wOrder-1,0)) := in}
      def oldphy(i: Int, in: Bool): Unit = {useless(tail(i)(wOrder-1,0)) := in}
      def kill_id(id: UInt): UInt = Cat(Mux(id < head(0)(wOrder-1,0),!head(0)(wOrder), head(0)(wOrder)), id)
    })
    w.head := VecInit(Seq(0.U, 1.U, 2.U, 3.U))
    w.tail := VecInit(Seq(0.U, 1.U))
    w.commit  := DontCare
    w.useing  := DontCare
    w.useless := DontCare
    w
  })
  io.head  := reorder.head(0)(wOrder-1,0)
  io.empty := reorder.emtpy
  val xcpt = RegInit({
    val w = Wire(new XcptInfoI(wOrder))
    w.valid := false.B
    w.id := DontCare
    w
  })
  val xcpt_ctrl = Wire(new Bundle {
    val epc = UInt(data_width.W)
    val flush = Vec(nCommit, Bool())
  })
  xcpt_ctrl.epc   := id_pc(reorder.head(0)(wOrder-1,0))
  xcpt_ctrl.flush := reorder.head.map(xcpt.valid && xcpt.id === _(wOrder-1,0))
  when (io.xcpt_i.valid && (!xcpt.valid ||
    CmpId(io.xcpt_i.id, xcpt.id, reorder.head(0)(wOrder-1,0)))) {
    xcpt.id := io.xcpt_i.id
  }
  when (xcpt_ctrl.flush(0))  {xcpt.valid := false.B
  }.elsewhen (io.xcpt_i.valid) {xcpt.valid := true.B
  }

  io.xcpt_o.valid := xcpt_ctrl.flush(0)
  io.xcpt_o.id := xcpt.id
  io.xcpt_o.pc := xcpt_ctrl.epc
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
    val kill_id   = UInt((wOrder+1).W)
  })

  order_ctrl.logic  := reorder.head.map(h => id_logic(h(wOrder-1,0)))
  order_ctrl.physic := reorder.head.map(h => id_physic(h(wOrder-1,0)))
  order_ctrl.oldphy := reorder.head.map(h => id_oldphy(h(wOrder-1,0)))
  for (i <- 0 until nCommit) {
    order_ctrl.inc_head(i) := reorder.head_val(i) && !xcpt_ctrl.flush(i)
    order_ctrl.commited(i) := (0 until i+1).map(j => order_ctrl.inc_head(j)).reduce(_&&_) // about 12~16 gates
  }
  io.retire := PopCount(order_ctrl.commited)
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
  order_ctrl.kill_id := reorder.kill_id(io.kill.id)

  when (xcpt_ctrl.flush(0)) {
    reorder.tail := Seq(reorder.head(0), reorder.head(1))
  }.elsewhen(io.kill.valid) {
    reorder.tail := Seq(order_ctrl.kill_id, order_ctrl.kill_id + 1.U)
  }.elsewhen(io.inc_order(0)) {
    when (io.inc_order(1)) {
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

  order_ctrl.register := Mux(io.inc_order(0), UIntToOH(reorder.tail(0)(wOrder-1,0)) |
    Mux(order_ctrl.inc_head(1), UIntToOH(reorder.tail(1)(wOrder-1,0)), 0.U), 0.U)
  reorder.commit := (reorder.commit & (~order_ctrl.register).asUInt) | order_ctrl.retiring

  val rename = Wire(new Bundle {
    val wt = Bool()
    val wtAwt = Bool()
    val rdAwt = Vec(nInst, Bool())
    val rs = Vec(nInst, Vec(2, UInt(wPhyAddr.W)))
    val rd = Vec(nInst, UInt(wPhyAddr.W))
  })
  rename.wt    := io.logic(0).rd.valid && io.first
  rename.wtAwt := rename.wt && io.logic(0).rd.addr === io.logic(1).rd.addr
  rename.rdAwt := io.logic(1).rs.map(rs => rename.wt && io.logic(0).rd.addr === rs.addr)
  io.rsaddr    := rename.rs
  for (i <- 0 until nALU) {
    io.req_io(i).op := id_op(io.req_io(i).id)
  }
  for (i <- 0 until nInst) {
    rename.rs(i)  := io.logic(i).rs.map(j => latest.maptb(j.addr))
    rename.rd(i)  := latest.maptb(io.logic(i).rd.addr)
    io.physic(i).rd.valid := io.logic(i).rd.valid
    io.resp_pc(i) := id_pc(io.req_id(i))
  }
  for (j <- 0 until 2) {
    io.physic(0).rs(j).addr  := rename.rs(0)(j)
    io.physic(1).rs(j).addr  := Mux(rename.rdAwt(j), io.physic(0).rd.addr, rename.rs(1)(j))
    io.physic(0).undef(j)  := !latest.rename(io.logic(0).rs(j).addr)
    io.physic(1).undef(j)  := !latest.rename(io.logic(1).rs(j).addr) && !rename.rdAwt(j)
    io.physic(0).rs(j).valid := io.logic(0).rs(j).valid || io.physic(0).undef(j) ||  phyRegValid(rename.rs(0)(j)) // around 22 gates
    io.physic(1).rs(j).valid := io.logic(1).rs(j).valid || io.physic(1).undef(j) || (phyRegValid(rename.rs(1)(j)) && !rename.rdAwt(j))
  }

  def push(tail: UInt, logic: UInt, physic: UInt, oldphy: UInt, pc: UInt, op: OpCode): Unit = {
    id_logic(tail)  := logic
    id_physic(tail) := physic
    id_oldphy(tail) := oldphy
    id_pc(tail) := pc
    id_op(tail) := op
  }
  when (io.inc_order(0)) {
    push(tail = reorder.tail(0)(wOrder-1,0), logic = Mux(io.first, io.logic(0).rd.addr, io.logic(1).rd.addr),
      physic = Mux(io.first, io.physic(0).rd.addr, io.logic(1).rd.addr),
      oldphy = Mux(io.first, rename.rd(0), rename.rd(1)),
      pc = Mux(io.first, io.logic(0).pc, io.logic(1).pc),
      op = Mux(io.first, io.logic(0).op, io.logic(1).op))
    reorder.newphy(0, Mux(io.first, io.logic(0).rd.valid, io.logic(1).rd.valid))
    reorder.oldphy(0, Mux(io.first, latest.rename(io.logic(0).rd.addr) && io.logic(0).rd.valid,
      latest.rename(io.logic(1).rd.addr) && io.logic(1).rd.valid))
    when (io.inc_order(1)) {
      push(tail = reorder.tail(1)(wOrder-1,0), logic = io.logic(1).rd.addr, physic = io.physic(1).rd.addr,
        oldphy = Mux(rename.wtAwt, io.physic(0).rd.addr, rename.rd(1)), pc = io.logic(1).pc, op = io.logic(1).op)
      reorder.newphy(1, io.logic(1).rd.valid)
      reorder.oldphy(1, (latest.rename(io.logic(1).rd.addr) && io.logic(1).rd.valid) || rename.wtAwt)
    }
  }

  val phy_ctrl = Wire(new Bundle {
    val useless = UInt(nPhyAddr.W)
    val useless_cnt = UInt(log2Ceil(nPhyAddr+1).W)
    val useless_dec = Vec(nCommit, Bool())

    val useing = UInt(nPhyAddr.W)
    val useing_cnt = UInt(log2Ceil(nPhyAddr+1).W)
    val useing_inc = Vec(nCommit, Bool())

    val tail = Vec(nInst, UInt(nPhyAddr.W))
    val tail_cnt = UInt(log2Ceil(nPhyAddr+1).W)
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

  phy_ctrl.tail(0)  := PriorityEncoderOH(~latest.useing)
  phy_ctrl.tail(1)  := Reverse(PriorityEncoderOH(Reverse((~latest.useing).asUInt)))

  when (!xcpt_ctrl.flush(0)) {
    commit.useing := (commit.useing | phy_ctrl.useing) & (~phy_ctrl.useless).asUInt
    commit.usecnt := (commit.usecnt + phy_ctrl.useing_cnt) - phy_ctrl.useless_cnt
    for (i <- 0 until nCommit) when(phy_ctrl.useing_inc(i)) {
      retire(order_ctrl.logic(i), order_ctrl.physic(i))
    }
  }

  phy_ctrl.retiring := (0 until nCommit).map(i =>
    Fill(nPhyAddr, io.commit(i).wb.valid) & UIntToOH(io.commit(i).wb.addr)).reduce(_|_) // TODO: just wb valid

  val phy_ready = Wire(Vec(nInst, Bool()))
  phy_ctrl.register :=
    (Fill(nPhyAddr, io.phy_valid.reduce(_||_) && phy_ready(0)) & phy_ctrl.tail(0)) |
    (Fill(nPhyAddr, io.phy_valid.reduce(_&&_) && phy_ready(1)) & phy_ctrl.tail(1))
  phy_ctrl.tail_cnt := Mux(io.phy_valid.reduce(_&&_) && phy_ready(1), 2.U,
    Mux(io.phy_valid.reduce(_||_) && phy_ready(0), 1.U, 0.U))

  phyRegValid := (phyRegValid | phy_ctrl.retiring) & (~phy_ctrl.register).asUInt
  val backup_reg = Reg(new Bundle {
    val valid  = Bool()
    val idx1H  = UInt(nBrchjr.W)
    val logic  = Vec(nInst, UInt(5.W))
    val physic = Vec(nInst, UInt(wPhyAddr.W))
    val remap  = Vec(nInst, Bool())
    def physic0: UInt = UIntToOH(physic(0))
    def useing: UInt = Mux(remap.reduce(_&&_), physic0 | UIntToOH(physic(1)),
      Mux(remap.reduce(_||_), physic0, 0.U))
    def usecnt: UInt = Mux(remap.reduce(_&&_), 2.U,  Mux(remap.reduce(_||_), 1.U, 0.U))
    def split: Bool = remap(0) && io.split
    def split_useing: UInt = Mux(split, physic0, 0.U)
    def split_usecnt: UInt = Mux(split, 1.U, 0.U)
  })

  when(xcpt_ctrl.flush(0)) {
    latest.maptb  := commit.maptb
    latest.rename := commit.rename
    latest.useing := commit.useing
    latest.usecnt := commit.usecnt
  }.elsewhen(io.kill.valid) {
    latest.maptb  := backup(io.kill.bidx).maptb
    latest.rename := backup(io.kill.bidx).rename
    when (backup_reg.split) {
      latest.maptb(backup_reg.logic(0))  := backup_reg.physic(0)
      latest.rename(backup_reg.logic(0)) := true.B
    }
    latest.useing := (backup(io.kill.bidx).useing | backup_reg.split_useing) & (~phy_ctrl.useless).asUInt
    latest.usecnt := backup(io.kill.bidx).usecnt + backup_reg.split_usecnt - phy_ctrl.useless_cnt
  }.otherwise {
    when (io.inc_order(0)) {
      when(io.first) {
        // FIXME: there are some wtAwt conflict, choose the latter one
        when (io.logic(0).rd.valid) {
          regist(io.logic(0).rd.addr, io.physic(0).rd.addr)
          when (io.inc_order(1) && io.logic(1).rd.valid) {
            regist(io.logic(1).rd.addr, io.physic(1).rd.addr)
          }
        }.elsewhen (io.inc_order(1) && io.logic(1).rd.valid) {
          regist(io.logic(1).rd.addr, io.physic(0).rd.addr)
        }
      }.elsewhen(io.logic(1).rd.valid) {
        regist(io.logic(1).rd.addr, io.physic(0).rd.addr)
      }
    }
    latest.useing := (latest.useing | phy_ctrl.register) & (~phy_ctrl.useless).asUInt
    latest.usecnt := (latest.usecnt - phy_ctrl.useless_cnt) + phy_ctrl.tail_cnt
  }

  backup_reg.valid  := io.bkup_alloc
  when (io.bkup_alloc) {
    backup_reg.idx1H := io.bidx1H
    for (i <- 0 until nInst) {
      backup_reg.logic(i)  := io.logic(i).rd.addr
      backup_reg.physic(i) := io.physic(i).rd.addr
    }
    backup_reg.remap(0) := io.first && io.logic(0).rd.valid
    backup_reg.remap(1) := (!io.first || io.inc_order(1)) && io.logic(1).rd.valid && io.brchjr(1)
  }
  for (i <- 0 until nBrchjr) {
    when (io.bidx1H(i) && io.bkup_alloc) {
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
      backup(i).useing := (backup(i).useing | backup_reg.useing) & (~phy_ctrl.useless).asUInt
      backup(i).usecnt := (backup(i).usecnt + backup_reg.usecnt) - phy_ctrl.useless_cnt
    }.otherwise {
      backup(i).useing := backup(i).useing & (~phy_ctrl.useless).asUInt
      backup(i).usecnt := backup(i).usecnt - phy_ctrl.useless_cnt
    }
  }

  io.physic(0).rd.addr := PriorityEncoder(~latest.useing)
  io.physic(1).rd.addr := Mux(rename.wt, OHToUInt(phy_ctrl.tail(1)), io.physic(0).rd.addr)
  io.physic(0).id := reorder.tail(0)(wOrder-1,0)
  io.physic(1).id := Mux(io.first, reorder.tail(1)(wOrder-1,0), reorder.tail(0)(wOrder-1,0))
  phy_ready(0) := latest.usecnt =/= nPhyAddr.U
  phy_ready(1) := latest.usecnt  <  nPhyAddr.U
  io.phy_ready(0) := phy_ready(0)
  io.phy_ready(1) := Mux(io.logic(0).rd.valid, phy_ready(1), phy_ready(0))
  io.id_ready(0)  := reorder.tail_val0 ||  order_ctrl.inc_head(0)
  io.id_ready(1)  := reorder.tail_val1 || (order_ctrl.inc_head(0) &&
    (reorder.tail_val0 || order_ctrl.inc_head(1)))

//  printf(p"phy_ready ${io.phy_ready} id_ready ${io.id_ready} phy_inc ${io.phy_alloc}, id_inc ${io.inc_order}\n")
//  for (i <- 0 until nInst) {
//    printf("physic ")
//    for (j <- 0 until 2) {
//      printf(p"${io.physic(i).rs_need(j)}:${io.physic(i).rs(j).valid} ${io.logic(i).rs(j).addr} -> ${io.physic(i).rs(j).addr} ")
//    }
//    printf(p"${io.logic(i).rd.addr} -> ${io.physic(i).rd.addr} id ${io.physic(i).id}\n")
//  }
//
//  printf(p"latest rename ${latest.rename} useing ${Hexadecimal(latest.useing)} usecnt ${latest.usecnt}\n")
//  printf(p"head ${reorder.head(0)} tail ${reorder.tail(0)}\n")
////  printf(p"phy_ctrl $phy_ctrl\n")
//  val cnt = RegInit(0.U(32.W))
//  cnt := cnt + 1.U
//  printf(p"=======================cnt = $cnt=============================\n")
}
