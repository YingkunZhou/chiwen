package fuxi

import chisel3._
import chisel3.util._
import common._

object Stage {
  val DEC = 0
  val EXE = 1
  val MEM = 2
  val Num: Int = MEM + 1
}

class WbCrtl extends Bundle {
  val rf_wen = Bool()
}

class MemCrtl extends WbCrtl {
  val mem_en   = Bool()
  val csr_cmd  = UInt(CSR.SZ)
  val illegal  = Bool()
}

class ExeCrtl extends MemCrtl {
  val br_type  = UInt(BR_N.getWidth.W)
  val branch   = Bool()
  val jump     = UInt(Jump.NUM.W)
  val btbTp    = UInt(CFIType.SZ.W)
}

class DecCrtl extends WbCrtl {
  val jump    = UInt(Jump.NUM.W)
  val branch  = Bool()
  val btbTp   = UInt(CFIType.SZ.W)
  val rs1_oen = Bool()
  val rs2_oen = Bool()
}

class Wb extends Bundle {
  val rf_wen = Bool()
  val wbaddr = UInt(5.W)
}

class Mem(implicit val conf: CPUConfig) extends Wb {
  val mem_en   = Bool()
  val csr_cmd  = UInt(CSR.SZ)
  val illegal  = Bool()
  val pc       = UInt(conf.xprlen.W)
  val inst     = UInt(conf.xprlen.W)
  val rs2_data = UInt(conf.xprlen.W)
  val wb_sel   = UInt(WB_X.getWidth.W)
  val mem_fcn  = UInt(M_X.getWidth.W)
  val mem_typ  = UInt(MT_X.getWidth.W)
}

class Exe(val nEntries : Int)(implicit conf: CPUConfig) extends Mem {
  val br_type  = UInt(BR_N.getWidth.W)
  val branch   = Bool()
  val jump     = UInt(Jump.NUM.W)
  val btb      = new Predict(conf.xprlen)
  val op1_data = UInt(conf.xprlen.W)
  val op2_data = UInt(conf.xprlen.W)
  val alu_fun  = UInt(ALU_X.getWidth.W)
}

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
  // Decode Stage ===========================================================================================================================================
  // ========================================================================================================================================================
  val xcpt      = Wire(Valid(UInt(conf.xprlen.W)))
  val stall     = Wire(Vec(2, Vec(Stage.Num, Bool())))
  val dec       = Array.fill(2)(Module(new InstDecoder()).io)
  val dec_wire  = Wire(Vec(2, new DecCrtl))
  val dec_valid = Wire(Vec(2, Bool()))
  val exe       = Reg(Vec(2, new Exe(nEntries)))
  val exe_valid = RegInit(VecInit(Seq.fill(2)(false.B)))
  val exe_wire  = Wire(Vec(2, new ExeCrtl))
  val mem       = Reg(Vec(2, new Mem()))
  val mem_valid = RegInit(VecInit(Seq.fill(2)(false.B)))
  val mem_wire  = Wire(Vec(2, new MemCrtl))
  val wb        = Reg(Vec(2, new Wb()))
  val wb_valid  = RegInit(VecInit(Seq.fill(2)(false.B)))
  val wb_wire   = Wire(Vec(2, new WbCrtl))

  val exe_wbdata = Wire(Vec(2, UInt(conf.xprlen.W)))
  val mem_wbdata = Wire(Vec(2, UInt(conf.xprlen.W)))
  val wb_wbdata  = Reg(Vec(2, UInt(conf.xprlen.W)))

  for (i <- 0 until 2) {
    dec(i).inst  := io.front.inst(i).bits
    dec_valid(i) := io.front.inst(i).valid
    dec_wire(i).btbTp   := Mux(dec_valid(i), io.front.pred(i).Tp, CFIType.invalid.U)
    dec_wire(i).jump    := Mux(dec_valid(i), dec(i).cinfo.cfi_jump, 0.U)
    dec_wire(i).branch  := dec_valid(i) && dec(i).cinfo.cfi_branch
    dec_wire(i).rs1_oen := dec_valid(i) && dec(i).cinfo.rs1_oen
    dec_wire(i).rs2_oen := dec_valid(i) && dec(i).cinfo.rs2_oen
    dec_wire(i).rf_wen  := dec_valid(i) && dec(i).cinfo.rf_wen && dec(i).wbaddr =/= 0.U
  }
  // Bypass Muxes
  val dec_op1_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_op2_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_rs1_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_rs2_data  = Wire(Vec(2, UInt(conf.xprlen.W)))

  // Register File
  val regfile = Module(new Regfile())
  val rf_rs1_data = Wire(Vec(2, UInt()))
  val rf_rs2_data = Wire(Vec(2, UInt()))
  for (i <- 0 until 2) {
    regfile.io.rs1_addr(i) := dec(i).rs1_addr
    regfile.io.rs2_addr(i) := dec(i).rs2_addr
    rf_rs1_data(i) := regfile.io.rs1_data(i)
    rf_rs2_data(i) := regfile.io.rs2_data(i)
    // roll the OP1 mux into the bypass mux logic
    dec_rs1_data(i) := MuxCase(rf_rs1_data(i), Array(
      ((exe(1).wbaddr === dec(i).rs1_addr) && exe_wire(1).rf_wen) -> exe_wbdata(1),
      ((exe(0).wbaddr === dec(i).rs1_addr) && exe_wire(0).rf_wen) -> exe_wbdata(0),
      ((mem(1).wbaddr === dec(i).rs1_addr) && mem_wire(1).rf_wen) -> mem_wbdata(1),
      ((mem(0).wbaddr === dec(i).rs1_addr) && mem_wire(0).rf_wen) -> mem_wbdata(0),
      (( wb(1).wbaddr === dec(i).rs1_addr) &&  wb_wire(1).rf_wen) -> wb_wbdata(1),
      (( wb(0).wbaddr === dec(i).rs1_addr) &&  wb_wire(0).rf_wen) -> wb_wbdata(0)
    ))

    dec_rs2_data(i) := MuxCase(rf_rs2_data(i), Array(
      ((exe(1).wbaddr === dec(i).rs2_addr) && exe_wire(1).rf_wen) -> exe_wbdata(1),
      ((exe(0).wbaddr === dec(i).rs2_addr) && exe_wire(0).rf_wen) -> exe_wbdata(0),
      ((mem(1).wbaddr === dec(i).rs2_addr) && mem_wire(1).rf_wen) -> mem_wbdata(1),
      ((mem(0).wbaddr === dec(i).rs2_addr) && mem_wire(0).rf_wen) -> mem_wbdata(0),
      (( wb(1).wbaddr === dec(i).rs2_addr) &&  wb_wire(1).rf_wen) -> wb_wbdata(1),
      (( wb(0).wbaddr === dec(i).rs2_addr) &&  wb_wire(0).rf_wen) -> wb_wbdata(0)
    ))

    dec_op1_data(i) := MuxCase(dec_rs1_data(i), Array(
      (dec(i).cinfo.op1_sel === OP1_IMZ)-> dec(i).dinfo.imm_z,
      (dec(i).cinfo.op1_sel === OP1_PC) -> io.front.pc(i)))

    dec_op2_data(i) := MuxCase(dec_rs2_data(i), Array(
      (dec(i).cinfo.op2_sel === OP2_ITYPE)  -> dec(i).dinfo.imm_i,
      (dec(i).cinfo.op2_sel === OP2_STYPE)  -> dec(i).dinfo.imm_s,
      (dec(i).cinfo.op2_sel === OP2_SBTYPE) -> dec(i).dinfo.imm_sb,
      (dec(i).cinfo.op2_sel === OP2_UTYPE)  -> dec(i).dinfo.imm_u,
      (dec(i).cinfo.op2_sel === OP2_UJTYPE) -> dec(i).dinfo.imm_uj))
  }

  val dec_sel = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    dec_sel(i) := dec_wire(i).jump(Jump.pop).toBool
  }
  val dec_jump  = Mux(dec_sel(0), dec_wire(0).jump, dec_wire(1).jump)
  val dec_btbTp = Mux(dec_sel(0), dec_wire(0).btbTp, dec_wire(1).btbTp)
  val dec_btbTg = Mux(dec_sel(0), io.front.pred(0).Tg, io.front.pred(1).Tg)

  val if_mispredict: Bool = dec_jump(Jump.pop) && (io.front.rasIO.peek =/= dec_btbTg || dec_btbTp =/= CFIType.retn.U)

  for (i <- 0 until 2) {
    when ((stall(i)(Stage.DEC) && !stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid) {
      exe_valid(i) := false.B
    }.elsewhen(!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
      exe_valid(i) := dec_valid(i)

      exe(i).rf_wen   := dec(i).cinfo.rf_wen
      exe(i).mem_en   := dec(i).cinfo.mem_en
      // convert CSR instructions with raddr1 == 0 to read-only CSR commands
      exe(i).csr_cmd  := Mux((dec(i).cinfo.csr_cmd === CSR.S || dec(i).cinfo.csr_cmd === CSR.C) &&
                              dec(i).rs1_addr === 0.U, CSR.R, dec(i).cinfo.csr_cmd)
      exe(i).illegal  := dec(i).cinfo.illegal
      exe(i).br_type  := dec(i).cinfo.br_type
      exe(i).pc       := io.front.pc(i)
      exe(i).op1_data := dec_op1_data(i)
      exe(i).op2_data := dec_op2_data(i)
      exe(i).rs2_data := dec_rs2_data(i)
      exe(i).inst     := dec(i).inst
      exe(i).alu_fun  := dec(i).cinfo.alu_fun
      exe(i).wb_sel   := dec(i).cinfo.wb_sel
      exe(i).wbaddr   := dec(i).wbaddr
      exe(i).mem_fcn  := dec(i).cinfo.mem_fcn
      exe(i).mem_typ  := dec(i).cinfo.mem_typ
      exe(i).branch   := dec(i).cinfo.cfi_branch
      exe(i).jump     := dec(i).cinfo.cfi_jump
      exe(i).btb.Tp    := io.front.pred(i).Tp
      exe(i).btb.Sel   := io.front.pred(i).Sel
      exe(i).btb.Tg    := Mux(if_mispredict, io.front.rasIO.peek, io.front.pred(i).Tg)
    }
  }
  // Execute Stage ==========================================================================================================================================
  //=========================================================================================================================================================
  val alus = Array.fill(2)(Module(new ALU()).io)
  val fb_tg = Wire(Vec(2, UInt(conf.xprlen.W)))
  val pc_right = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    exe_wire(i).rf_wen  := exe(i).rf_wen && exe_valid(i) && exe(i).wbaddr =/= 0.U
    exe_wire(i).mem_en  := exe(i).mem_en && exe_valid(i)
    exe_wire(i).csr_cmd := Mux(exe_valid(i), exe(i).csr_cmd, CSR.N)
    exe_wire(i).br_type := Mux(exe_valid(i), exe(i).br_type, BR_N)
    exe_wire(i).branch  := exe_valid(i) && exe(i).branch
    exe_wire(i).jump    := Mux(exe_valid(i), exe(i).jump, 0.U)
    exe_wire(i).btbTp   := Mux(exe_valid(i), exe(i).btb.Tp, CFIType.invalid.U)
    exe_wire(i).illegal := exe(i).illegal && exe_valid(i)

    alus(i).op1      := exe(i).op1_data
    alus(i).op2      := exe(i).op2_data
    alus(i).pc       := exe(i).pc
    alus(i).rs2_data     := exe(i).rs2_data
    alus(i).ctrl.fun     := exe(i).alu_fun
    alus(i).ctrl.br_type := exe_wire(i).br_type
    alus(i).ctrl.wb_sel  := exe(i).wb_sel

    exe_wbdata(i) := alus(i).result
    fb_tg(i) := Mux(alus(i).ctrl.pc_sel === PC_BRJMP, alus(i).target.brjmp,
                Mux(alus(i).ctrl.pc_sel === PC_JALR,  alus(i).target.jpreg,
                                                      alus(i).target.conti))
    pc_right(i) := fb_tg(i) === exe(i).btb.Tg || !exe_valid(i)
  }
  // FIXME: in order to save time, may loose some precision
  val exe_sel = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    exe_sel(i) := exe_wire(i).br_type =/= BR_N || exe_wire(i).btbTp =/= CFIType.invalid.U
  }
  val exe_tg = Wire(new Target(conf.xprlen))
  val exe_btb = Wire(new Predict(conf.xprlen))
  val exe_jump   = Mux(exe_sel(0), exe_wire(0).jump, exe_wire(1).jump)
  val exe_branch = Mux(exe_sel(0), exe_wire(0).branch, exe_wire(1).branch)
  exe_btb.Tp    := Mux(exe_sel(0), exe_wire(0).btbTp, exe_wire(1).btbTp)
  exe_btb.Tg    := Mux(exe_sel(0), exe(0).btb.Tg, exe(1).btb.Tg)
  exe_btb.Sel   := Mux(exe_sel(0), exe(0).btb.Sel, exe(1).btb.Sel)
  exe_tg        := Mux(exe_sel(0), alus(0).target, alus(1).target)
  val exe_pc_sel = Mux(exe_sel(0), alus(0).ctrl.pc_sel, alus(1).ctrl.pc_sel)

  io.front.rasIO.pop := Pulse(exe_jump(Jump.pop).toBool, !stall(1)(Stage.MEM))
  io.front.rasIO.push.valid := Pulse(exe_jump(Jump.push).toBool, !stall(1)(Stage.MEM))
  io.front.rasIO.push.bits  := exe_tg.conti

  io.front.feedBack.sel.valid := Pulse(exe_btb.Tp =/= CFIType.invalid.U, !stall(1)(Stage.MEM))
  io.front.feedBack.sel.bits  := exe_btb.Sel
  io.front.feedBack.redirect  := Pulse(exe_pc_sel === PC_BRJMP || exe_pc_sel === PC_JALR, !stall(1)(Stage.MEM))
  io.front.feedBack.pc        := Mux(exe_sel(0), exe(0).pc, exe(1).pc)
  io.front.feedBack.cfiType :=
    Mux(exe_branch,                                 CFIType.branch.U,
    Mux(exe_jump(Jump.pop),                         CFIType.retn.U,
    Mux(exe_jump(Jump.none) || exe_jump(Jump.push), CFIType.jump.U,
                                                    CFIType.invalid.U
    )))

  io.front.feedBack.target := Mux(exe_sel(0), fb_tg(0), fb_tg(1))

  val dec_mispredict: Bool = !pc_right.asUInt.andR
  val mem_reg_exe_out = Reg(Vec(2, UInt(conf.xprlen.W)))
  val mem_reg_jpnpc   = RegInit(VecInit(Seq.fill(2)(0.U(conf.xprlen.W))))
  for (i <- 0 until 2) {
    when ((stall(i)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid) {
      mem_valid(i)    := false.B
      mem_reg_jpnpc(i):= 0.U
    } .elsewhen (!stall(1)(Stage.MEM)) {
      if (i == 1) mem_valid(1) := exe_valid(1) && pc_right(0)
      else mem_valid(0) := exe_valid(0)

      mem(i).rf_wen   := exe(i).rf_wen
      mem(i).mem_en   := exe(i).mem_en
      mem(i).csr_cmd  := exe(i).csr_cmd
      mem(i).illegal  := exe(i).illegal
      mem(i).pc       := exe(i).pc
      mem(i).inst     := exe(i).inst
      mem(i).wb_sel   := exe(i).wb_sel
      mem(i).wbaddr   := exe(i).wbaddr
      mem(i).rs2_data := exe(i).rs2_data
      mem(i).mem_fcn  := exe(i).mem_fcn
      mem(i).mem_typ  := exe(i).mem_typ

      mem_reg_exe_out(i) := exe_wbdata(i)
      mem_reg_jpnpc(i):= (Fill(conf.xprlen, alus(i).ctrl.pc_sel === PC_BRJMP) & alus(i).target.brjmp) |
        (Fill(conf.xprlen, alus(i).ctrl.pc_sel === PC_JALR) & alus(i).target.jpreg)
    }
  }
  // Memory Stage ============================================================================================================================================
  //==========================================================================================================================================================
  for (i <- 0 until 2) {
    mem_wire(i).rf_wen  := mem(i).rf_wen && mem_valid(i) && mem(i).wbaddr =/= 0.U
    mem_wire(i).mem_en  := mem(i).mem_en && mem_valid(i)
    mem_wire(i).csr_cmd := Mux(mem_valid(i), mem(i).csr_cmd, CSR.N)
    mem_wire(i).illegal := mem(i).illegal && mem_valid(i)
  }

  val mem_sel = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    mem_sel(i) := mem_wire(i).csr_cmd =/= CSR.N || mem_wire(i).mem_en
  }
  val xcpt_conflict: Bool = mem_valid(0) && (mem_wire(1).illegal || mem_reg_jpnpc(1)(1,0).orR)
  val mem_0: Bool =  mem_sel(0) || xcpt_conflict

  val mem_inst     = Mux(mem_0, mem(0).inst, mem(1).inst)
  val mem_exe_out  = Mux(mem_0, mem_reg_exe_out(0), mem_reg_exe_out(1))
  val mem_csr_cmd  = Mux(mem_0, mem_wire(0).csr_cmd, mem_wire(1).csr_cmd)
  val mem_pc       = Mux(mem_0, mem(0).pc, mem(1).pc)
  val mem_illegal  = Mux(mem_0, mem_wire(0).illegal, mem_wire(1).illegal)
  val mem_jpnpc    = Mux(mem_0, mem_reg_jpnpc(0), mem_reg_jpnpc(1))
  val mem_en       = Mux(mem_0, mem_wire(0).mem_en, mem_wire(1).mem_en)
  val mem_fcn      = Mux(mem_0, mem(0).mem_fcn, mem(1).mem_fcn)
  val mem_typ      = Mux(mem_0, mem(0).mem_typ, mem(1).mem_typ)
  val mem_rs2_data = Mux(mem_0, mem(0).rs2_data, mem(1).rs2_data)
  // Control Status Registers
  csr.io := DontCare
  csr.io.rw.addr  := mem_inst(CSR_ADDR_MSB,CSR_ADDR_LSB)
  csr.io.rw.wdata := mem_exe_out
  csr.io.rw.cmd   := mem_csr_cmd
  csr.io.pc       := mem_pc

  val ls_addr_ma_valid = MuxLookup(mem_typ(1,0) ,false.B, Array(
    2.U -> mem_exe_out(0),
    3.U -> mem_exe_out(1,0).orR
  ))

  val ma_jump: Bool    = mem_jpnpc(1,0).orR
  val ma_load: Bool    = mem_en && mem_fcn === M_XRD && ls_addr_ma_valid
  val ma_store: Bool   = mem_en && mem_fcn === M_XWR && ls_addr_ma_valid
  val ma_illegal: Bool = Mux(mem_0, mem_wire(0).illegal, mem_wire(1).illegal)
  csr.io.xcpt  := ma_load || ma_store || ma_jump || mem_illegal
  csr.io.cause := MuxCase(0.U, Array(
    ma_jump    -> Causes.misaligned_fetch.U,
    ma_illegal -> Causes.illegal_instruction.U,
    ma_load    -> Causes.misaligned_load.U,
    ma_store   -> Causes.misaligned_store.U
  ))
  csr.io.tval  := MuxCase(0.U, Array(
    ma_jump    -> mem_jpnpc,
    ma_illegal -> mem_inst,
    ma_load    -> mem_exe_out,
    ma_store   -> mem_exe_out
  ))
  xcpt.valid := ma_jump || ma_load || ma_store || ma_illegal || csr.io.eret
  xcpt.bits  := csr.io.evec

  // datapath to data memory outputs =============================
  io.mem.req.valid     := mem_en && !ma_store && !ma_load
  io.mem.req.bits.addr := mem_exe_out
  io.mem.req.bits.fcn  := mem_fcn
  io.mem.req.bits.typ  := mem_typ
  io.mem.req.bits.data := mem_rs2_data
  //===============================================================
  for (i <- 0 until 2) {
    // WB Mux
    mem_wbdata(i) := MuxCase(mem_reg_exe_out(i), Array( // default is wb_alu and wb_pc4
      // (mem_reg_wb_sel === WB_ALU) -> mem_reg_alu_out,
      // (mem_reg_wb_sel === WB_PC4) -> mem_reg_alu_out,
      (mem(i).wb_sel === WB_MEM) -> io.mem.resp.bits.data,
      (mem(i).wb_sel === WB_CSR) -> csr.io.rw.rdata))

    when (stall(i)(Stage.MEM) || xcpt.valid) {
      wb_valid(i) := false.B
    } .otherwise {
      wb_valid(i) := mem_valid(i)
      wb(i).rf_wen := mem(i).rf_wen
      wb(i).wbaddr := mem(i).wbaddr
      wb_wbdata(i) := mem_wbdata(i)
    }
  }
  // Writeback Stage ===========================================================================================================================================
  //============================================================================================================================================================
  for (i <- 0 until 2) {
    wb_wire(i).rf_wen   := wb(i).rf_wen && wb_valid(i) && wb(i).wbaddr =/= 0.U
    regfile.io.waddr(i) := wb(i).wbaddr
    regfile.io.wdata(i) := wb_wbdata(i)
    regfile.io.wen(i)   := wb_wire(i).rf_wen
  }

  val retire = Wire(Vec(2, Bool()))
  retire(0) := wb_valid.asUInt.xorR
  retire(1) := wb_valid.asUInt.andR
  csr.io.retire   := retire.asUInt //FIXME
  // Add your own uarch counters here!
  csr.io.counters.foreach(_.inc := false.B)
  //control pipeline signals====================================================================================================================================
  //============================================================================================================================================================
  val exe_load_inst = Wire(Vec(2, Bool()))
  val rs1_addr_N0   = Wire(Vec(2, Bool()))
  val rs2_addr_N0   = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    exe_load_inst(i) := exe_wire(i).mem_en && exe(i).mem_fcn === M_XRD
    rs1_addr_N0(i) := dec(i).rs1_addr =/= 0.U
    rs2_addr_N0(i) := dec(0).rs2_addr =/= 0.U
  }

  stall(0)(Stage.DEC) :=
    (exe(0).wbaddr === dec(0).rs1_addr && rs1_addr_N0(0) && dec_wire(0).rs1_oen && exe_load_inst(0)) ||
    (exe(1).wbaddr === dec(0).rs1_addr && rs1_addr_N0(0) && dec_wire(0).rs1_oen && exe_load_inst(1)) ||
    (exe(0).wbaddr === dec(0).rs2_addr && rs2_addr_N0(0) && dec_wire(0).rs2_oen && exe_load_inst(0)) ||
    (exe(1).wbaddr === dec(0).rs2_addr && rs2_addr_N0(0) && dec_wire(0).rs2_oen && exe_load_inst(1)) ||
    exe_wire(0).csr_cmd =/= CSR.N || exe_wire(1).csr_cmd =/= CSR.N

  stall(1)(Stage.DEC) :=
    (exe(0).wbaddr === dec(1).rs1_addr && rs1_addr_N0(1) && dec_wire(1).rs1_oen && exe_load_inst(0)) ||
    (exe(1).wbaddr === dec(1).rs1_addr && rs1_addr_N0(1) && dec_wire(1).rs1_oen && exe_load_inst(1)) ||
    (exe(0).wbaddr === dec(1).rs2_addr && rs2_addr_N0(1) && dec_wire(1).rs2_oen && exe_load_inst(0)) ||
    (exe(1).wbaddr === dec(1).rs2_addr && rs2_addr_N0(1) && dec_wire(1).rs2_oen && exe_load_inst(1)) ||
    (dec(0).wbaddr === dec(1).rs1_addr && rs1_addr_N0(1) && dec_wire(1).rs1_oen && dec_wire(0).rf_wen) ||
    (dec(0).wbaddr === dec(1).rs2_addr && rs2_addr_N0(1) && dec_wire(1).rs2_oen && dec_wire(0).rf_wen) ||
    dec_sel.asUInt.andR || stall(0)(Stage.DEC)

  stall(0)(Stage.EXE) := false.B
  stall(1)(Stage.EXE) := exe_sel.asUInt.andR || stall(0)(Stage.EXE)
  stall(0)(Stage.MEM) := mem_en && !io.mem.resp.valid
  stall(1)(Stage.MEM) := xcpt_conflict || mem_sel.asUInt.andR || stall(0)(Stage.MEM)

  io.front.dec_kill := Pulse(dec_mispredict, forward = !stall(1)(Stage.MEM))
  io.front.if_kill  := Pulse(if_mispredict || dec_mispredict, forward = !stall(1)(Stage.MEM))
  io.front.xcpt     := xcpt
  io.front.forward  := !stall(1).asUInt.orR

  // Printout
  //  printf("Core: Cyc= %d WB[ %x %x: 0x%x] (0x%x, 0x%x, 0x%x, 0x%x, 0x%x) %c %c %c ExeInst: DASM(%x)\n"
  //    , io.cyc
  //    , wb_reg_rf_wen
  //    , wb_reg_wbaddr
  //    , wb_reg_wbdata
  //    , if_reg_pc
  //    , dec_pc
  //    , exe_reg_pc
  //    , mem_reg_pc
  //    , RegNext(mem_reg_pc)
  //    , Mux(mem_stall, Str("F"),             //FREEZE-> F
  //      Mux(dec_stall, Str("S"), Str(" ")))  //STALL->S
  //    , Mux(alu.io.ctrl.pc_sel === 1.U, Str("B"),    //BJ -> B
  //      Mux(alu.io.ctrl.pc_sel === 2.U, Str("J"),    //JR -> J
  //      Mux(alu.io.ctrl.pc_sel === 3.U, Str("E"),    //EX -> E
  //      Mux(alu.io.ctrl.pc_sel === 0.U, Str(" "), Str("?")))))
  //    , Mux(csr.io.illegal, Str("X"), Str(" "))
  //    , Mux(xcpt, BUBBLE, exe_reg_inst)
  //    )
}