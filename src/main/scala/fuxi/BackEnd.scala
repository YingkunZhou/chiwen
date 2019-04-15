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
  val bj_sel  = Bool()
  val br_type = UInt(BR_N.getWidth.W)
}

class DecCrtl extends WbCrtl {
  val rs1_oen = Bool()
  val rs2_oen = Bool()
  val csr_cmd = UInt(CSR.SZ)
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

class Exe(implicit conf: CPUConfig) extends Mem {
  val br_type  = UInt(BR_N.getWidth.W)
  val bj_sel   = Bool()
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
  val regfile = Module(new Regfile())
  val alu = Array.fill(2)(Module(new ALU()).io)
  val dec = Array.fill(2)(Module(new InstDecoder()).io)
  io.cyc := csr.io.time(conf.xprlen-1,0)
  // Decode Stage ===========================================================================================================================================
  // ========================================================================================================================================================
  val xcpt      = Wire(Valid(UInt(conf.xprlen.W)))
  val stall     = Wire(Vec(2, Vec(Stage.Num, Bool())))
  val dec_wire  = Wire(Vec(2, new DecCrtl))
  val exe       = Reg(Vec(2, new Exe))
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
  // Bypass Muxes
  val dec_op1_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_op2_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_rs1_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_rs2_data  = Wire(Vec(2, UInt(conf.xprlen.W)))

  // Register File
  val rf_rs1_data = Wire(Vec(2, UInt()))
  val rf_rs2_data = Wire(Vec(2, UInt()))

  for (i <- 0 until 2) {
    dec(i).inst  := io.front.inst(i).bits
    dec_wire(i).rs1_oen := io.front.inst(i).valid && dec(i).cinfo.rs1_oen
    dec_wire(i).rs2_oen := io.front.inst(i).valid && dec(i).cinfo.rs2_oen
    dec_wire(i).rf_wen  := io.front.inst(i).valid && dec(i).cinfo.rf_wen && dec(i).wbaddr =/= 0.U
    dec_wire(i).csr_cmd := Mux(io.front.inst(i).valid, dec(i).cinfo.csr_cmd, CSR.N)

    regfile.io.rs1_addr(i) := dec(i).rs1_addr
    regfile.io.rs2_addr(i) := dec(i).rs2_addr
    rf_rs1_data(i) := regfile.io.rs1_data(i)
    rf_rs2_data(i) := regfile.io.rs2_data(i)

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

    when (!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
      exe(i).rf_wen   := dec(i).cinfo.rf_wen
      exe(i).mem_en   := dec(i).cinfo.mem_en
      // convert CSR instructions with raddr1 == 0 to read-only CSR commands
      exe(i).csr_cmd  := Mux((dec(i).cinfo.csr_cmd === CSR.S || dec(i).cinfo.csr_cmd === CSR.C) &&
                              dec(i).rs1_addr === 0.U, CSR.R, dec(i).cinfo.csr_cmd)
      exe(i).illegal  := dec(i).cinfo.illegal
      exe(i).br_type  := dec(i).cinfo.br_type
      exe(i).bj_sel   := io.front.bj_sel(i)
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
    }
  }
  val exe_btb    = Reg(new Predict(conf.data_width))
  val exe_branch = Reg(Bool())
  val exe_jump   = Reg(UInt(Jump.NUM.W))
  when (!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
    exe_btb    := io.front.pred
    exe_branch := io.front.branch
  }

  val mispredict = Wire(Bool()) //based on is branch jump inst
  val exe_cancel = Wire(Bool())
  when (((stall(0)(Stage.DEC) || stall(1)(Stage.EXE)) && !stall(0)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid) {
    exe_valid(0) := false.B
  }.elsewhen(!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
    exe_valid(0) := io.front.inst(0).valid && !mispredict
  }

  when ((stall(1)(Stage.DEC) && !stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid || exe_cancel) {
    exe_valid(1) := false.B
  }.elsewhen(!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
    exe_valid(1) := io.front.inst(1).valid && !mispredict && !io.front.split
  }

  // Execute Stage ==========================================================================================================================================
  //=========================================================================================================================================================
  val exe_bj_valid    = exe_wire.map(_.bj_sel).reduce(_||_) //only have one
  val exe_wire_branch = exe_bj_valid && exe_branch
  val exe_wire_jump   = Mux(exe_bj_valid, exe_jump, 0.U)

  val mem_reg_wbdata = Reg(Vec(2, UInt(conf.xprlen.W)))
  for (i <- 0 until 2) {
    exe_wire(i).rf_wen  := exe_valid(i) && exe(i).rf_wen && exe(i).wbaddr =/= 0.U
    exe_wire(i).mem_en  := exe_valid(i) && exe(i).mem_en
    exe_wire(i).csr_cmd := Mux(exe_valid(i), exe(i).csr_cmd, CSR.N)
    exe_wire(i).br_type := Mux(exe_valid(i), exe(i).br_type, BR_N)
    exe_wire(i).bj_sel  := exe_valid(i) && exe(i).bj_sel
    exe_wire(i).illegal := exe_valid(i) && exe(i).illegal

    alu(i).op1          := exe(i).op1_data
    alu(i).op2          := exe(i).op2_data
    alu(i).pc           := exe(i).pc
    alu(i).rs2_data     := exe(i).rs2_data
    alu(i).ctrl.fun     := exe(i).alu_fun
    alu(i).ctrl.br_type := exe_wire(i).br_type
    alu(i).ctrl.wb_sel  := exe(i).wb_sel

    exe_wbdata(i) := alu(i).result

    when (!stall(1)(Stage.MEM)) {
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
      mem_reg_wbdata(i) := exe_wbdata(i)
    }
  }

  val target = Mux(exe_wire(0).bj_sel, alu(0).target, alu(1).target)
  val pc_sel = Mux(exe_wire(0).bj_sel, alu(0).ctrl.pc_sel, alu(1).ctrl.pc_sel)

//  io.front.ras_pop := Pulse(exe_wire_jump(Jump.pop).toBool, !stall(1)(Stage.MEM))
//  io.front.ras_push.valid := Pulse(exe_wire_jump(Jump.push).toBool, !stall(1)(Stage.MEM))
//  io.front.ras_push.bits  := target.conti
//  val bj_type =
//    Mux(exe_wire_branch,            BTBType.branch.U,
//    Mux(exe_wire_jump(Jump.pop),    BTBType.retn.U,
//    Mux(exe_wire_jump(Jump.none) ||
//        exe_wire_jump(Jump.push),   BTBType.jump.U,
//                                    BTBType.invalid.U
//    )))

  io.front.feedBack.valid := Pulse(exe_bj_valid, !stall(1)(Stage.MEM))
  io.front.feedBack.bits.redirect := Pulse(pc_sel === PC_BRJMP || pc_sel === PC_JALR, !stall(1)(Stage.MEM))
  io.front.feedBack.bits.tgt :=
    Mux(pc_sel === PC_BRJMP, target.brjmp,
    Mux(pc_sel === PC_JALR,  target.jpreg,
      target.conti
    ))
  io.front.fb_pc := Mux(exe_wire(0).bj_sel, exe(0).pc, exe(1).pc)
  io.front.fb_type := Mux(exe_wire_branch, BTBType.branch.U, BTBType.jump.U)


  val not_expect: Bool = io.front.feedBack.bits.tgt =/= exe_btb.tgt
  mispredict := not_expect && exe_bj_valid
  exe_cancel := not_expect && exe_wire(0).bj_sel

  val mem_reg_jpnpc  = RegInit(0.U(conf.xprlen.W))
  when (!stall(1)(Stage.MEM)) {
    mem_reg_jpnpc :=
      (Fill(conf.xprlen, pc_sel === PC_BRJMP) & target.brjmp) |
      (Fill(conf.xprlen, pc_sel === PC_JALR)  & target.jpreg)
  }

  when ((stall(0)(Stage.EXE) || stall(1)(Stage.MEM)) && !stall(0)(Stage.MEM) || xcpt.valid) {
    mem_valid(0) := false.B
  }.elsewhen (!stall(1)(Stage.MEM)) {
    mem_valid(0) := exe_valid(0)
  }

  when ((stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid) {
    mem_valid(1) := false.B
  }.elsewhen (!stall(1)(Stage.MEM)) {
    mem_valid(1) := exe_valid(1) && !exe_cancel
  }

  io.mem.req.valid     := exe_wire(0).mem_en || (exe_wire(1).mem_en && !exe_cancel)//&& !ma_store && !ma_load
  io.mem.req.bits.addr := Mux(exe_wire(0).mem_en, exe_wbdata(0), exe_wbdata(1))
  io.mem.req.bits.fcn  := Mux(exe_wire(0).mem_en, exe(0).mem_fcn , exe(1).mem_fcn)
  io.mem.req.bits.typ  := Mux(exe_wire(0).mem_en, exe(0).mem_typ , exe(1).mem_typ)
  io.mem.req.bits.data := Mux(exe_wire(0).mem_en, exe(0).rs2_data, exe(1).rs2_data)

  // Memory Stage ============================================================================================================================================
  //==========================================================================================================================================================
  val mem_sel = Wire(Vec(2, Bool()))
  val mem_wire_jump = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    mem_wire(i).rf_wen  := mem_valid(i) && mem(i).rf_wen && mem(i).wbaddr =/= 0.U
    mem_wire(i).mem_en  := mem_valid(i) && mem(i).mem_en
    mem_wire(i).csr_cmd := Mux(mem_valid(i), mem(i).csr_cmd, CSR.N)
    mem_wire(i).illegal := mem_valid(i) && mem(i).illegal
    mem_wire_jump(i) := mem_valid(i) && mem_reg_jpnpc(1,0).orR

    mem_sel(i) := mem_wire(i).csr_cmd =/= CSR.N ||
                  mem_wire(i).mem_en  ||
                  mem_wire(i).illegal ||
                  mem_wire_jump(i)
    // WB Mux
    mem_wbdata(i) := MuxCase(mem_reg_wbdata(i), Array( // default is wb_alu and wb_pc4
      (mem(i).wb_sel === WB_MEM) -> io.mem.resp.bits.data,
      (mem(i).wb_sel === WB_CSR) -> csr.io.rw.rdata))

    wb_valid(i) := mem_valid(i)
    wb(i).rf_wen := mem(i).rf_wen
    wb(i).wbaddr := mem(i).wbaddr
    wb_wbdata(i) := mem_wbdata(i)
  }

  val mem_inst     = Mux(mem_sel(0), mem(0).inst, mem(1).inst)
  val mem_exe_out  = Mux(mem_sel(0), mem_reg_wbdata(0), mem_reg_wbdata(1))
  val mem_csr_cmd  = Mux(mem_sel(0), mem_wire(0).csr_cmd, mem_wire(1).csr_cmd)
  val mem_pc       = Mux(mem_sel(0), mem(0).pc, mem(1).pc)
  val mem_illegal  = Mux(mem_sel(0), mem_wire(0).illegal, mem_wire(1).illegal)
  val mem_en       = Mux(mem_sel(0), mem_wire(0).mem_en, mem_wire(1).mem_en)
  val mem_fcn      = Mux(mem_sel(0), mem(0).mem_fcn, mem(1).mem_fcn)
  val mem_typ      = Mux(mem_sel(0), mem(0).mem_typ, mem(1).mem_typ)
  val mem_rs2_data = Mux(mem_sel(0), mem(0).rs2_data, mem(1).rs2_data)
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

  val ma_load: Bool    = mem_en && mem_fcn === M_XRD && ls_addr_ma_valid
  val ma_store: Bool   = mem_en && mem_fcn === M_XWR && ls_addr_ma_valid
  val ma_jump: Bool    = Mux(mem_sel(0), mem_wire_jump(0) , mem_wire_jump(1))
  val ma_illegal: Bool = Mux(mem_sel(0), mem_wire(0).illegal, mem_wire(1).illegal)
  csr.io.xcpt  := ma_load || ma_store || ma_jump || mem_illegal
  csr.io.cause := MuxCase(0.U, Array(
    ma_jump    -> Causes.misaligned_fetch.U,
    ma_illegal -> Causes.illegal_instruction.U,
    ma_load    -> Causes.misaligned_load.U,
    ma_store   -> Causes.misaligned_store.U
  ))
  csr.io.tval  := MuxCase(0.U, Array(
    ma_jump    -> mem_reg_jpnpc,
    ma_illegal -> mem_inst,
    ma_load    -> mem_exe_out,
    ma_store   -> mem_exe_out
  ))
  xcpt.valid := ma_jump || ma_load || ma_store || ma_illegal || csr.io.eret
  xcpt.bits  := csr.io.evec

  when (stall(0)(Stage.MEM) || (xcpt.valid && mem_sel(0))) {
    wb_valid(0) := false.B
  } .otherwise { wb_valid(0) := mem_valid(0) }

  when (stall(1)(Stage.MEM) || xcpt.valid) {
    wb_valid(1) := false.B
  } .otherwise { wb_valid(1) := mem_valid(1) }
  //===============================================================
  // Writeback Stage ===========================================================================================================================================
  //============================================================================================================================================================
  for (i <- 0 until 2) {
    wb_wire(i).rf_wen   := wb_valid(i) && wb(i).rf_wen && wb(i).wbaddr =/= 0.U
    regfile.io.waddr(i) := wb(i).wbaddr
    regfile.io.wdata(i) := wb_wbdata(i)
    regfile.io.wen(i)   := wb_wire(i).rf_wen
  }

//  when (io.cyc === 5070.U) { regfile.io.wdata(0) := "h00001631".U }
//  when (io.cyc === 10037.U) { regfile.io.wdata(0) := "h00002ad8".U }
//  when (io.cyc === 5102.U) { regfile.io.wdata(0) := "h000010cc".U }
//  when (io.cyc === 10048.U) { regfile.io.wdata(0) := "h0000210a".U }

  val retire = Wire(Vec(2, Bool()))
  retire(0) := wb_valid.asUInt.xorR
  retire(1) := wb_valid.asUInt.andR
  csr.io.retire := retire.asUInt //FIXME
  // Add your own uarch counters here!
  csr.io.counters.foreach(_.inc := false.B)
  //control pipeline signals====================================================================================================================================
  //============================================================================================================================================================
  val exe_load_inst = Wire(Vec(2, Bool()))
  val rs1_addr_N0   = Wire(Vec(2, Bool()))
  val rs2_addr_N0   = Wire(Vec(2, Bool()))
  val rAW           = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    exe_load_inst(i) := exe_wire(i).mem_en && exe(i).mem_fcn === M_XRD
    rs1_addr_N0(i)   := dec(i).rs1_addr =/= 0.U
    rs2_addr_N0(i)   := dec(i).rs2_addr =/= 0.U
    rAW(i) :=
      (exe(0).wbaddr === dec(i).rs1_addr && rs1_addr_N0(i) && dec_wire(i).rs1_oen && exe_load_inst(0)) ||
      (exe(0).wbaddr === dec(i).rs2_addr && rs2_addr_N0(i) && dec_wire(i).rs2_oen && exe_load_inst(0)) ||
      (exe(1).wbaddr === dec(i).rs1_addr && rs1_addr_N0(i) && dec_wire(i).rs1_oen && exe_load_inst(1)) ||
      (exe(1).wbaddr === dec(i).rs2_addr && rs2_addr_N0(i) && dec_wire(i).rs2_oen && exe_load_inst(1))
  }

  stall(0)(Stage.DEC) := rAW(0) || exe_wire(0).csr_cmd =/= CSR.N || exe_wire(1).csr_cmd =/= CSR.N

  stall(1)(Stage.DEC) :=
    (dec(0).wbaddr === dec(1).rs1_addr && rs1_addr_N0(1) && dec_wire(1).rs1_oen && dec_wire(0).rf_wen) ||
    (dec(0).wbaddr === dec(1).rs2_addr && rs2_addr_N0(1) && dec_wire(1).rs2_oen && dec_wire(0).rf_wen) ||
    dec_wire(0).csr_cmd =/= CSR.N || rAW(1) || stall(0)(Stage.DEC)

  stall(0)(Stage.EXE) := false.B
  stall(1)(Stage.EXE) := stall(0)(Stage.EXE) || exe_wire.map(_.mem_en).reduce(_&&_)
  stall(0)(Stage.MEM) := mem_wire(0).mem_en && !io.mem.resp.valid
  stall(1)(Stage.MEM) := mem_sel.asUInt.andR || (mem_wire(1).mem_en && !io.mem.resp.valid) || stall(0)(Stage.MEM)

  io.front.kill := Pulse(mispredict, forward = !stall(1)(Stage.MEM))
  io.front.xcpt := xcpt
  io.front.forward(0) := !stall(0)(Stage.DEC) && !stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)
  io.front.forward(1) := !stall(1)(Stage.DEC) && !stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)

  // Printout
  for (i <- 0 until conf.nInst) {
    printf("Core: Cyc= %d PC(%x, %x, %x) [%c%c %c%c %c%c] %c%c %c%c Exe: DASM(%x)\n"
      , io.cyc
      , io.front.pc(i)
      , exe(i).pc
      , mem(i).pc
      , Mux(stall(i)(Stage.MEM), Str("M"), Str(" ")), Str(""+i)
      , Mux(stall(i)(Stage.EXE), Str("E"), Str(" ")), Str(""+i)
      , Mux(stall(i)(Stage.DEC), Str("D"), Str(" ")), Str(""+i)
      , Mux(alu(i).ctrl.pc_sel === 1.U, Str("B"),    //BJ -> B
        Mux(alu(i).ctrl.pc_sel === 2.U, Str("J"),    //JR -> J
        Mux(alu(i).ctrl.pc_sel === 3.U, Str("E"),    //EX -> E
        Mux(alu(i).ctrl.pc_sel === 0.U, Str(" "), Str("?"))))), Str(""+i)
      , Mux(csr.io.illegal, Str("X"), Str(" ")), Str(""+i)
      , Mux(!exe_valid(i) || xcpt.valid || (exe_cancel && i.U === 1.U) ||
        stall(1)(Stage.MEM) || (stall(1)(Stage.EXE) && i.U === 1.U), BUBBLE, exe(i).inst)
    )
  }
  for (i <- 0 until conf.nInst) {
    when (wb_valid(i)) {
      printf("Core: Cyc= %d WB[ %x %x %x]\n"
        , io.cyc
        , wb_wire(i).rf_wen
        , wb(i).wbaddr
        , regfile.io.wdata(i)
      )
    }
  }
  //  when (io.cyc === 14228.U || io.cyc === 14229.U) {
  //    printf(p"Debug: ${io.cyc} exe_valid $exe_valid\n")
  //  }
}