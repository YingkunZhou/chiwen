package myCore

import chisel3._
import common._

class BackEnd(implicit conf: CPUConfig) extends Module with BTBParams with Pram {
  val io = IO(new Bundle {
    val mem  = new MemPortIo(conf.xprlen)
    val cyc   = Output(UInt(conf.xprlen.W))
    val front = Flipped(new InterfaceIO(conf.inst_width, conf.data_width))
  })

  val csr = Module(new CSRFile())
  io.cyc := csr.io.time(conf.xprlen-1,0)
  val instQueue   = Module(new InstQueue).io
  val stateCtrl   = Module(new StateCtrl(conf.data_width)).io
  val instDecoder = Array.fill(nInst)(Module(new InstDecoder).io)
  val issueQueue  = Array.fill(nInst)(Module(new IssueQueue(conf.data_width)).io)
  for (i <- 0 until 2) {
    io.front.forward          := instQueue.forward
    instQueue.in(i).valid     := io.front.inst(i).valid
    instQueue.in(i).bits.inst := io.front.inst(i).bits
    instQueue.in(i).bits.pc   := io.front.pc(i)
    instQueue.out(i).ready    := stateCtrl.logic(i).ready
    instDecoder(i).inst       := instQueue.out(i).bits.inst
    stateCtrl.logic(i).valid  := instQueue.out(i).valid
    stateCtrl.logic(i).rd     := instDecoder(i).rd
    stateCtrl.logic(i).rs     := instDecoder(i).rs
    stateCtrl.logic(i).info.pc       := instQueue.out(i).bits.pc
    stateCtrl.logic(i).info.op       := instDecoder(i).op
    stateCtrl.logic(i).info.imm      := instDecoder(i).imm
    issueQueue(i).in.bits.info.pc    := instQueue.out(i).bits.pc
    issueQueue(i).in.bits.info.op    := instDecoder(i).op
    issueQueue(i).in.bits.info.imm   := instDecoder(i).imm
    issueQueue(i).in.bits.info.imm_l := instDecoder(i).imm_l
    issueQueue(i).in.bits.info.imm_z := instDecoder(i).imm_z
    stateCtrl.physic(i).ready := issueQueue(i).in.ready
    stateCtrl.issueID(i)      := issueQueue(i).issueID
    issueQueue(i).issueInfo   := stateCtrl.issueInfo(i)
    issueQueue(i).in.valid    := stateCtrl.physic(i).valid
    issueQueue(i).in.bits.rs  := stateCtrl.physic(i).rs
    issueQueue(i).in.bits.rd  := stateCtrl.physic(i).rd
    issueQueue(i).in.bits.id  := stateCtrl.physic(i).id
    issueQueue(i).in.bits.fix1:= instDecoder(i).fix1
  }
//  // Decode Stage ===========================================================================================================================================
//  // ========================================================================================================================================================
//  val xcpt      = Wire(Valid(UInt(conf.xprlen.W)))
//  val stall     = Wire(Vec(Stage.Num, Bool()))
//  val dec       = Module(new InstDecoder())
//  val dec_wire  = Wire(new DecCrtl)
//  val dec_valid = Wire(Bool())
//  val exe       = Reg(new Exe(nEntries))
//  val exe_valid = RegInit(false.B)
//  val exe_wire  = Wire(new ExeCrtl)
//  val mem       = Reg(new Mem())
//  val mem_valid = RegInit(false.B)
//  val mem_wire  = Wire(new MemCrtl)
//  val wb        = Reg(new Wb())
//  val wb_valid  = RegInit(false.B)
//  val wb_wire   = Wire(new WbCrtl)
//
//  val exe_wbdata = Wire(UInt(conf.xprlen.W))
//  val mem_wbdata = Wire(UInt(conf.xprlen.W))
//  val wb_wbdata  = Reg(UInt(conf.xprlen.W))
//
//  dec.io.inst  := io.front.inst.bits
//  dec_valid    := io.front.inst.valid
//  dec_wire.btbTp      := Mux(dec_valid, io.front.pred.Tp, CFIType.invalid.U)
//  dec_wire.cfi_jump   := Mux(dec_valid, dec.io.cinfo.cfi_jump, 0.U)
//  dec_wire.cfi_branch := dec_valid && dec.io.cinfo.cfi_branch
//  dec_wire.rs1_oen    := dec_valid && dec.io.cinfo.rs1_oen
//  dec_wire.rs2_oen    := dec_valid && dec.io.cinfo.rs2_oen
//  dec_wire.rf_wen     := dec_valid && dec.io.cinfo.rf_wen && dec.io.cinfo.wbaddr =/= 0.U
//
//  // Bypass Muxes
//  val dec_op1_data  = Wire(UInt(conf.xprlen.W))
//  val dec_op2_data  = Wire(UInt(conf.xprlen.W))
//  val dec_rs1_data  = Wire(UInt(conf.xprlen.W))
//  val dec_rs2_data  = Wire(UInt(conf.xprlen.W))
//
//  // Register File
//  val regfile = Module(new Regfile())
//  val rf_rs1_data = Wire(Vec(2, UInt()))
//  val rf_rs2_data = Wire(Vec(2, UInt()))
//  regfile.io.rs1_addr := dec.io.cinfo.rs1_addr
//  regfile.io.rs2_addr := dec.io.cinfo.rs2_addr
//  rf_rs1_data := regfile.io.rs1_data
//  rf_rs2_data := regfile.io.rs2_data
//  // roll the OP1 mux into the bypass mux logic
//  dec_rs1_data := MuxCase(rf_rs1_data, Array(
//    ((exe.wbaddr === dec.io.cinfo.rs1_addr) && exe_wire.rf_wen) -> exe_wbdata,
//    ((mem.wbaddr === dec.io.cinfo.rs1_addr) && mem_wire.rf_wen) -> mem_wbdata,
//    (( wb.wbaddr === dec.io.cinfo.rs1_addr) &&  wb_wire.rf_wen) -> wb_wbdata
//  ))
//
//  dec_rs2_data := MuxCase(rf_rs2_data, Array(
//    ((exe.wbaddr === dec.io.cinfo.rs2_addr) && exe_wire.rf_wen) -> exe_wbdata,
//    ((mem.wbaddr === dec.io.cinfo.rs2_addr) && mem_wire.rf_wen) -> mem_wbdata,
//    (( wb.wbaddr === dec.io.cinfo.rs2_addr) &&  wb_wire.rf_wen) -> wb_wbdata
//  ))
//
//  dec_op1_data := MuxCase(dec_rs1_data, Array(
//    (dec.io.cinfo.op1_sel === OP1_IMZ)-> dec.io.dinfo.imm_z,
//    (dec.io.cinfo.op1_sel === OP1_PC) -> io.front.pc))
//
//  dec_op2_data := MuxCase(dec_rs2_data, Array(
//    (dec.io.cinfo.op2_sel === OP2_ITYPE)  -> dec.io.dinfo.imm_i,
//    (dec.io.cinfo.op2_sel === OP2_STYPE)  -> dec.io.dinfo.imm_s,
//    (dec.io.cinfo.op2_sel === OP2_SBTYPE) -> dec.io.dinfo.imm_sb,
//    (dec.io.cinfo.op2_sel === OP2_UTYPE)  -> dec.io.dinfo.imm_u,
//    (dec.io.cinfo.op2_sel === OP2_UJTYPE) -> dec.io.dinfo.imm_uj))
//
//  val if_mispredict = Wire(Bool())
//  if (!conf.hasBTB) if_mispredict := false.B
//  else if_mispredict := dec.io.cinfo.cfi_jump(Jump.pop) && (io.front.rasIO.peek =/= io.front.pred.Tg || io.front.pred.Tp =/= CFIType.retn.U)
//
//  when ((stall(Stage.DEC) && !stall(Stage.MEM)) || xcpt.valid) {
//    // (kill exe stage) insert NOP (bubble) into Execute stage on front-end stall (e.g., hazard clearing)
//    exe_valid := false.B
//  }.elsewhen(!stall(Stage.MEM)) {
//    exe_valid    := dec_valid
//    exe.rf_wen   := dec.io.cinfo.rf_wen
//    exe.mem_en   := dec.io.cinfo.mem_en
//    // convert CSR instructions with raddr1 == 0 to read-only CSR commands
//    exe.csr_cmd  := Mux((dec.io.cinfo.csr_cmd === CSR.S || dec.io.cinfo.csr_cmd === CSR.C) &&
//      dec.io.cinfo.rs1_addr === 0.U, CSR.R, dec.io.cinfo.csr_cmd)
//    exe.illegal  := dec.io.cinfo.illegal
//    exe.br_type  := dec.io.cinfo.br_type
//    exe.pc       := io.front.pc
//    exe.op1_data := dec_op1_data
//    exe.op2_data := dec_op2_data
//    exe.rs2_data := dec_rs2_data
//    exe.inst     := dec.io.inst
//    exe.alu_fun  := dec.io.cinfo.alu_fun
//    exe.wb_sel   := dec.io.cinfo.wb_sel
//    exe.wbaddr   := dec.io.cinfo.wbaddr
//    exe.mem_fcn  := dec.io.cinfo.mem_fcn
//    exe.mem_typ  := dec.io.cinfo.mem_typ
//    exe.branch   := dec.io.cinfo.cfi_branch
//    exe.jump     := dec.io.cinfo.cfi_jump
//    exe.btb.Tp    := io.front.pred.Tp
//    exe.btb.Tg    := Mux(if_mispredict, io.front.rasIO.peek, io.front.pred.Tg)
//    exe.btb.Sel   := io.front.pred.Sel
//  }
//  // Execute Stage ==========================================================================================================================================
//  //=========================================================================================================================================================
//  exe_wire.rf_wen  := exe.rf_wen && exe_valid
//  exe_wire.mem_en  := exe.mem_en && exe_valid
//  exe_wire.csr_cmd := Mux(exe_valid, exe.csr_cmd, CSR.N)
//  exe_wire.br_type := Mux(exe_valid, exe.br_type, BR_N)
//  exe_wire.branch  := exe_valid && exe.branch
//  exe_wire.jump    := Mux(exe_valid, exe.jump, 0.U)
//  exe_wire.btbTp   := Mux(exe_valid, exe.btb.Tp, CFIType.invalid.U)
//  exe_wire.illegal := exe.illegal && exe_valid
//
//  val alus = Module(new ALU())
//  alus.io.alu_op1      := exe.op1_data
//  alus.io.alu_op2      := exe.op2_data
//  alus.io.rs2_data     := exe.rs2_data
//  alus.io.pc           := exe.pc
//  alus.io.ctrl.fun     := exe.alu_fun
//  alus.io.ctrl.br_type := exe_wire.br_type
//  alus.io.ctrl.wb_sel  := exe.wb_sel
//
//  exe_wbdata := alus.io.alu_result
//
//  io.front.rasIO.pop := exe_wire.jump(Jump.pop).toBool
//  io.front.rasIO.push.valid := exe_wire.jump(Jump.push).toBool
//  io.front.rasIO.push.bits  := alus.io.target.conti
//
//  io.front.feedBack.sel.valid := exe_wire.btbTp =/= CFIType.invalid.U
//  io.front.feedBack.sel.bits  := exe.btb.Sel
//  io.front.feedBack.redirect  := alus.io.ctrl.pc_sel === PC_BRJMP || alus.io.ctrl.pc_sel === PC_JALR
//  io.front.feedBack.pc        := exe.pc
//  val fb_cfiType =
//    Mux(exe_wire.branch,                                 CFIType.branch.U,
//      Mux(exe_wire.jump(Jump.pop),                         CFIType.retn.U,
//        Mux(exe_wire.jump(Jump.none) || exe_wire.jump(Jump.push), CFIType.jump.U,
//          CFIType.invalid.U
//        )))
//  io.front.feedBack.cfiType := fb_cfiType
//  val fb_tg =
//    Mux(alus.io.ctrl.pc_sel === PC_BRJMP, alus.io.target.brjmp,
//      Mux(alus.io.ctrl.pc_sel === PC_JALR,  alus.io.target.jpreg,
//        alus.io.target.conti
//      ))
//  io.front.feedBack.target := fb_tg
//
//  val dec_mispredict = Wire(Bool())
//  if (conf.hasBTB) dec_mispredict := fb_tg =/= exe.btb.Tg && fb_cfiType =/= CFIType.invalid.U
//  else dec_mispredict := alus.io.ctrl.pc_sel =/= PC_4
//
//  val mem_reg_exe_out = Reg(UInt(conf.xprlen.W))
//  val mem_reg_jpnpc   = RegInit(0.U(conf.xprlen.W))
//  when (xcpt.valid) {
//    mem_valid    := false.B
//    mem_reg_jpnpc:= 0.U
//  } .elsewhen (!stall(Stage.MEM)) {
//    mem_valid    := exe_valid
//    mem.rf_wen   := exe.rf_wen
//    mem.mem_en   := exe.mem_en
//    mem.csr_cmd  := exe.csr_cmd
//    mem.illegal  := exe.illegal
//    mem.pc       := exe.pc
//    mem.inst     := exe.inst
//    mem.wb_sel   := exe.wb_sel
//    mem.wbaddr   := exe.wb_sel
//    mem.rs2_data := exe.rs2_data
//    mem.mem_fcn  := exe.mem_fcn
//    mem.mem_typ  := exe.mem_typ
//
//    mem_reg_exe_out := exe_wbdata
//    mem_reg_jpnpc:= (Fill(conf.xprlen, alus.io.ctrl.pc_sel === PC_BRJMP) & alus.io.target.brjmp) |
//                    (Fill(conf.xprlen, alus.io.ctrl.pc_sel === PC_JALR) & alus.io.target.jpreg)
//  }
//  // Memory Stage ============================================================================================================================================
//  //==========================================================================================================================================================
//  mem_wire.rf_wen  := mem.rf_wen && mem_valid
//  mem_wire.mem_en  := mem.mem_en && mem_valid
//  mem_wire.csr_cmd := Mux(mem_valid, mem.csr_cmd, CSR.N)
//  mem_wire.illegal := mem.illegal && mem_valid
//  // Control Status Registers
//  csr.io := DontCare
//  csr.io.rw.addr  := mem.inst(CSR_ADDR_MSB,CSR_ADDR_LSB)
//  csr.io.rw.wdata := mem_reg_exe_out
//  csr.io.rw.cmd   := mem_wire.csr_cmd
//  csr.io.pc       := mem.pc
//
//  val ls_addr_ma_valid = MuxLookup(mem.mem_typ(1,0) ,false.B, Array(
//    2.U -> mem_reg_exe_out(0),
//    3.U -> mem_reg_exe_out(1,0).orR
//  ))
//  val ma_jump: Bool    = mem_reg_jpnpc(1,0).orR
//  val ma_load: Bool    = mem_wire.mem_en && mem.mem_fcn === M_XRD && ls_addr_ma_valid
//  val ma_store: Bool   = mem_wire.mem_en && mem.mem_fcn === M_XWR && ls_addr_ma_valid
//  csr.io.xcpt  := ma_load || ma_store || ma_jump || mem_wire.illegal
//  csr.io.cause := MuxCase(0.U, Array(
//    ma_jump    -> Causes.misaligned_fetch.U,
//    mem_wire.illegal -> Causes.illegal_instruction.U,
//    ma_load    -> Causes.misaligned_load.U,
//    ma_store   -> Causes.misaligned_store.U
//  ))
//  csr.io.tval  := MuxCase(0.U, Array(
//    ma_jump    -> mem_reg_jpnpc,
//    mem_wire.illegal -> mem.inst,
//    ma_load    -> mem_reg_exe_out,
//    ma_store   -> mem_reg_exe_out
//  ))
//  xcpt.valid := ma_jump || ma_load || ma_store || mem_wire.illegal || csr.io.eret
//  xcpt.bits  := csr.io.evec
//
//  // datapath to data memory outputs =============================
//  io.mem.req.valid     := mem_wire.mem_en && !ma_store && !ma_load
//  io.mem.req.bits.addr := mem_reg_exe_out
//  io.mem.req.bits.fcn  := mem.mem_fcn
//  io.mem.req.bits.typ  := mem.mem_typ
//  io.mem.req.bits.data := mem.rs2_data
//  //===============================================================
//
//  // WB Mux
//  mem_wbdata := MuxCase(mem_reg_exe_out, Array( // default is wb_alu and wb_pc4
//    // (mem_reg_wb_sel === WB_ALU) -> mem_reg_alu_out,
//    // (mem_reg_wb_sel === WB_PC4) -> mem_reg_alu_out,
//    (mem.wb_sel === WB_MEM) -> io.mem.resp.bits.data, // dumuzhou
//    (mem.wb_sel === WB_CSR) -> csr.io.rw.rdata)) //dumuzhou
//
//  when (stall(Stage.MEM) || xcpt.valid) {
//    wb_valid := false.B
//  } .otherwise {
//    wb_valid := mem_valid
//    wb.rf_wen := mem.rf_wen
//    wb.wbaddr := mem.wbaddr
//    wb_wbdata := mem_wbdata
//  }
//  // Writeback Stage ===========================================================================================================================================
//  //============================================================================================================================================================
//  wb_wire.rf_wen   := wb.rf_wen && wb_valid
//  regfile.io.waddr := wb.wbaddr
//  regfile.io.wdata := wb_wbdata
//  regfile.io.wen   := wb_wire.rf_wen
//
//  csr.io.retire    := wb_valid //FIXME
//  // Add your own uarch counters here!
//  csr.io.counters.foreach(_.inc := false.B)
//  //control pipeline signals====================================================================================================================================
//  //============================================================================================================================================================
//  val exe_load_inst: Bool = exe_wire.mem_en && exe.mem_fcn === M_XRD
//
//  stall(Stage.DEC) :=
//    (exe_load_inst && exe.wbaddr === dec.io.cinfo.rs1_addr && dec.io.cinfo.rs1_addr =/= 0.U && dec_wire.rs1_oen) ||
//    (exe_load_inst && exe.wbaddr === dec.io.cinfo.rs2_addr && dec.io.cinfo.rs1_addr =/= 0.U && dec_wire.rs2_oen) ||
//    exe.csr_cmd =/= CSR.N
//
//  stall(Stage.EXE) := false.B
//  // stall(mem_stall) full pipeline on no response from memory
//  stall(Stage.MEM) := mem_wire.mem_en && !io.mem.resp.valid
//
//  io.front.if_kill  := if_mispredict || io.front.dec_kill
//  io.front.dec_kill := dec_mispredict
//  io.front.xcpt     := xcpt
//  io.front.forward  := !stall.asUInt.orR
//
//  // Printout
//  //  printf("Core: Cyc= %d WB[ %x %x: 0x%x] (0x%x, 0x%x, 0x%x, 0x%x, 0x%x) %c %c %c ExeInst: DASM(%x)\n"
//  //    , io.cyc
//  //    , wb_reg_rf_wen
//  //    , wb_reg_wbaddr
//  //    , wb_reg_wbdata
//  //    , if_reg_pc
//  //    , dec_pc
//  //    , exe_reg_pc
//  //    , mem_reg_pc
//  //    , RegNext(mem_reg_pc)
//  //    , Mux(mem_stall, Str("F"),             //FREEZE-> F
//  //      Mux(dec_stall, Str("S"), Str(" ")))  //STALL->S
//  //    , Mux(alu.io.ctrl.pc_sel === 1.U, Str("B"),    //BJ -> B
//  //      Mux(alu.io.ctrl.pc_sel === 2.U, Str("J"),    //JR -> J
//  //      Mux(alu.io.ctrl.pc_sel === 3.U, Str("E"),    //EX -> E
//  //      Mux(alu.io.ctrl.pc_sel === 0.U, Str(" "), Str("?")))))
//  //    , Mux(csr.io.illegal, Str("X"), Str(" "))
//  //    , Mux(xcpt, BUBBLE, exe_reg_inst)
//  //    )
}
