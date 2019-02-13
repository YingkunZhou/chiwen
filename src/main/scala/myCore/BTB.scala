package myCore

import chisel3._
import chisel3.util._
import common.CPUConfig

trait BTBParams {
  val nEntries: Int = 32
  val nPages  : Int = 4
  val nRAS    : Int = 8
  val OFF_MSB : Int = 13
  val OFF_LSB : Int = 0
  val cntLen  : Int = 2
}

object CFIType {
  val NUM = 4
  val SZ = log2Ceil(NUM)
  val invalid = 0
  val retn    = 1
  val branch  = 2
  val jump    = 3
}

class feedBackIO(wEntries: Int)(implicit conf: CPUConfig) extends Bundle {
  val redirect = Input(Bool())
  val pc       = Input(UInt(conf.xprlen.W))
  val target   = Input(UInt(conf.xprlen.W))
  val cfiType  = Input(UInt(CFIType.SZ.W))
  val sel      = Flipped(new ValidIO(UInt(wEntries.W)))
}

class targetIO(wEntries: Int)(implicit conf: CPUConfig) extends Bundle {
  val bits    = Output(UInt(conf.xprlen.W))
  val cifType = Output(UInt(CFIType.SZ.W))
  val sel     = Output(UInt(wEntries.W))
}

class Predictor(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    // pc stage inquire
    val pc       = Input(UInt(conf.xprlen.W))
    val peekRAS  = Input(UInt(conf.xprlen.W)) // used for return type
    val target   = new targetIO(log2Ceil(nEntries))   // output
    // exe stage forward back
    val feedBack = new feedBackIO(log2Ceil(nEntries)) // input
    val cyc = Input(UInt(conf.xprlen.W))
  })

  // DATA structure
  val pc_cands = Wire(Vec(CFIType.NUM, UInt(conf.xprlen.W)))
  val pc_plus: UInt = io.pc + conf.pcInc.U(conf.xprlen.W)

  val off_valids   = RegInit(VecInit(Seq.fill(nEntries)(false.B)))
  val cfiTypes     = Reg(Vec(nEntries, UInt(CFIType.SZ.W)))
  val page_idxs    = Reg(Vec(nEntries, UInt(log2Ceil(nPages).W)))
  val hcnts        = Reg(Vec(nEntries, UInt(cntLen.W)))
  // notice that the PC to Target is 1 to 1 map
  val pc_offsets   = Reg(Vec(nEntries, UInt((OFF_MSB + 1 - OFF_LSB).W)))
  val tg_offsets   = Reg(Vec(nEntries, UInt((OFF_MSB + 1 - OFF_LSB).W)))

  val pg_valids    = RegInit(VecInit(Seq.fill(nPages)(false.B)))
  val pc_pages     = Reg(Vec(nPages, UInt((conf.xprlen-OFF_MSB-1).W)))
  val tg_pages     = Reg(Vec(nPages, UInt((conf.xprlen-OFF_MSB-1).W)))

  // CAM consult **pc stage**
  val offset_matches = Wire(Vec(nEntries, Bool()))
  val idx_matches    = Wire(Vec(nEntries, Bool()))
  val page_matches   = Wire(Vec(nPages  , Bool()))
  val pc_offset      = io.pc(OFF_MSB, OFF_LSB)
  val pc_page        = io.pc(conf.xprlen-1, OFF_MSB+1)

  page_matches     := pc_pages.map(_ === pc_page)
  val pg_sel:  UInt = page_matches.asUInt & pg_valids.asUInt
  offset_matches   := pc_offsets.map(_ === pc_offset)
  idx_matches      := page_idxs.map(pg_sel(_))
  val off_sel: UInt = offset_matches.asUInt & idx_matches.asUInt & off_valids.asUInt
//  printf("offset match: %x,  idx match: %x, page match: %x, the off valid: %x "
//    , offset_matches.asUInt
//    , idx_matches.asUInt
//    , page_matches.asUInt
//    , off_valids.asUInt)

  val tg_offset  = Mux1H(off_sel, tg_offsets)
  val cfiType    = Mux1H(off_sel, cfiTypes)
  val hcnt       = Mux1H(off_sel, hcnts)
  //  val pg_idx0    = Mux1H(off_sel0, page_idxs)
  //  val tg_page    = tg_pages(pg_idx0)
  val tg_page    = Mux1H(pg_sel, tg_pages) // FIXME: for time saveing, it is a bug actually. The correct way is above
  val brjmp      = Cat(tg_page, tg_offset)

  /*
  * 0. if it is invalid, then use pc_plus
  * 1. if it is return, then peek the RAS, then use the top of the RAS -- using RAS
  * 2. if it is branch, then use brjump, but need to check take or not
  * 3. if it is jump, then use brjump
  * */
  pc_cands(CFIType.invalid) := pc_plus
  pc_cands(CFIType.retn) := io.peekRAS
  pc_cands(CFIType.jump) := brjmp
  pc_cands(CFIType.branch) := Mux(hcnt(1), brjmp, pc_plus)
  val cfiType1H = UIntToOH(cfiType)
  // Target ports
  io.target.bits := Mux1H(cfiType1H, pc_cands)
  io.target.sel  := OHToUInt(off_sel)
  io.target.cifType := cfiType

  // Feedback ports **exe stage**
  /*
  * 1. page already exists === need pc page equal and target page equal
  * 2. page can be inserted
  * 3. page must replace the least recently used page
  * */
  val pc_pg_matches = Wire(Vec(nPages, Bool()))
  val tg_pg_matches = Wire(Vec(nPages, Bool()))
  val pc_pg = io.feedBack.pc(conf.xprlen-1, OFF_MSB+1)
  val tg_pg = io.feedBack.target(conf.xprlen-1, OFF_MSB+1)
  pc_pg_matches := pc_pages.map(_ === pc_pg)
  tg_pg_matches := tg_pages.map(_ === tg_pg)
  val page_sel: UInt = pc_pg_matches.asUInt & tg_pg_matches.asUInt & pg_valids.asUInt
  val page_exist: Bool = page_sel.orR
  val page_idx = OHToUInt(page_sel)
  val pg_valids_N: UInt  = (~pg_valids.asUInt).asUInt
  val new_pg_idx  = PriorityEncoder(pg_valids_N)
  val pg_insert:  Bool = pg_valids_N.orR
  val pg_LRU = Module(new LRU(nPages))
  val pg_idx = Mux(pg_insert, new_pg_idx, pg_LRU.io.oldest)
  pg_LRU.io.newest.bits := Mux(page_exist, page_idx, pg_idx)
  pg_LRU.io.newest.valid := io.feedBack.redirect

  val pg_replace: Bool = !pg_insert && !page_exist
  /*
  * 1. index can be inserted naturally
  * 2. index can be inserted because of page replacement invalidate some offsets
  * 3. index must be replaced randomly
  * */
  val pg_idx_invalid = Wire(Vec(nEntries, Bool()))
  pg_idx_invalid := page_idxs.map(_ === pg_LRU.io.oldest && pg_replace)
  val off_valids_N: UInt = (~off_valids.asUInt).asUInt /*| pg_idx_matches.asUInt*/ // for time conside otherwise it is linear
  val off_insert: Bool = off_valids_N.orR
  val new_off_idx = PriorityEncoder(off_valids_N)
  require(nEntries == 32)
  val lfsr5 = RegInit(1.U(log2Ceil(nEntries).W))
  lfsr5 := Cat(lfsr5(log2Ceil(nEntries)-2,0), lfsr5(4)^lfsr5(2))
  val off_idx = Mux(off_insert, new_off_idx, lfsr5)

  /*
  * 1. offset insert, page insert
  * 2. offset insert, page replace
  * 3. offset replace, page insert
  * 4. offset replace, page replace
  * */

  /*1. io.feedBack.sel.valid(true), io.feedBack.valid(true)   --exist branch taken
  * 2. io.feedBack.sel.valid(true), io.feedBack.valid(false)  --exist branch not taken
  * 3. io.feedBack.sel.valid(false), io.feedBack.valid(true)  --need insert and replace
  * 4. io.feedBack.sel.valid(false), io.feedBack.valid(false) --no side effect
  * */
  def update_map(idx: UInt) = {
    when (page_exist) {
      page_idxs(idx)  := page_idx
//      when(io.cyc === 45280.U) { printf(p"page_idx = $page_idx ") }
    }.otherwise {
//      printf("replaced!!! the pg_idx_invalid is: %x\n", pg_idx_invalid.asUInt)
      pg_valids(pg_idx)  := true.B
      tg_pages(pg_idx)   := io.feedBack.target(conf.xprlen - 1, OFF_MSB + 1)
      pc_pages(pg_idx)   := io.feedBack.pc(conf.xprlen - 1, OFF_MSB + 1)
      page_idxs(idx)     := pg_idx
      for (i <- 0 until nEntries) {
        when (pg_idx_invalid(i)) { off_valids(i)  := false.B }
      }
//      when(io.cyc === 45280.U) { printf(p"pg_idx = $pg_idx ") }
    }

    off_valids(idx)  := true.B
    tg_offsets(idx)  := io.feedBack.target(OFF_MSB, OFF_LSB)
    pc_offsets(idx)  := io.feedBack.pc(OFF_MSB, OFF_LSB)
    cfiTypes(idx)    := io.feedBack.cfiType
//    printf(p"off_insert = $off_insert, idx = $idx \n")
  }

  when (io.feedBack.redirect) {
    update_map(Mux(io.feedBack.sel.valid, io.feedBack.sel.bits, off_idx))
  }

  when (io.feedBack.sel.valid) {    // BTB already exist, need update the map
//    printf(" sel.valid ")
    when (io.feedBack.cfiType === CFIType.branch.U) {
//      printf(" branch type ")
      val fb_hcnt = hcnts(io.feedBack.sel.bits)
      when (io.feedBack.redirect) {
        when (fb_hcnt =/= Fill(cntLen, 1.U(1.W))) {
          hcnts(io.feedBack.sel.bits) := fb_hcnt + 1.U
        }
      }.elsewhen(fb_hcnt =/= 0.U) {
        hcnts(io.feedBack.sel.bits) := fb_hcnt - 1.U
      }
    }.elsewhen(!io.feedBack.redirect) {
      off_valids(io.feedBack.sel.bits) := false.B
    }
//    printf("\n")
  }
  .elsewhen(io.feedBack.redirect) { // BTB not exist, need insert or replace
//    printf(" insert or replace: %x, off_idx: %x ", off_insert, off_idx)
    hcnts(off_idx) := 2.U
  }

}
