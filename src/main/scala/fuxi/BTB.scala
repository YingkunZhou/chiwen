package fuxi

import chisel3._
import chisel3.util._
import common.CPUConfig

trait BTBParams {
  val nEntries: Int = 64
  val nPages  : Int = 4
  val nRAS    : Int = 8
  val OFF_MSB : Int = 13
  val OFF_LSB : Int = 2
  val cntLen  : Int = 2
}

object BTBType {
  val invalid = 0
  val retn    = 1
  val branch  = 2
  val jump    = 3
  val NUM = jump + 1
  val SZ = log2Ceil(NUM)
}

class Predict(val data_width: Int) extends Bundle with BTBParams {
  val redirect = Bool() // = 0 cont || = 1 jump
  val typ = UInt(BTBType.SZ.W)
  val tgt = UInt(data_width.W)
  val you = Bool()
  val idx = UInt(log2Ceil(nEntries).W)
}

class BTB(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle{
    val if_pc = Input(UInt(conf.xprlen.W)) // 8 Bytes aligned
    val fb_pc = Input(UInt(conf.xprlen.W))
    val predict  = Output(Vec(2, new Predict(conf.xprlen)))
    val feedBack = Input(new Predict(conf.xprlen))
    val split    = Output(Bool())

    val raspeek  = Input(UInt(conf.xprlen.W)) // used for return type
    val cyc = Input(UInt(conf.xprlen.W))
  })

  // DATA structure
  val off_valids   = RegInit(VecInit(Seq.fill(nEntries)(false.B)))
  val cfiTypes     = Reg(Vec(nEntries, UInt(BTBType.SZ.W)))
  val page_idxs    = Reg(Vec(nEntries, UInt(log2Ceil(nPages).W)))
  val hcnts        = Reg(Vec(nEntries, UInt(cntLen.W)))
  // notice that the PC to Target is 1 to 1 map
  val pc_offsets   = Reg(Vec(nEntries, UInt((OFF_MSB + 1 - OFF_LSB).W)))
  val tg_offsets   = Reg(Vec(nEntries, UInt((OFF_MSB + 1 - OFF_LSB).W)))

  val pg_valids    = RegInit(VecInit(Seq.fill(nPages)(false.B)))
  val pc_pages     = Reg(Vec(nPages, UInt((conf.xprlen-OFF_MSB-1).W)))
  val tg_pages     = Reg(Vec(nPages, UInt((conf.xprlen-OFF_MSB-1).W)))

  // CAM consult **pc stage**
  val pc_offset   = Wire(Vec(2, UInt((OFF_MSB + 1 - OFF_LSB).W)))
  val tg_offset   = Wire(Vec(2, UInt((OFF_MSB + 1 - OFF_LSB).W)))
  val off_sel     = Wire(Vec(2, UInt(nEntries.W)))
  val btbType     = Wire(Vec(2, UInt(BTBType.SZ.W)))
  val hcnt        = Wire(Vec(2, UInt(cntLen.W)))
  val brjmp       = Wire(Vec(2, UInt(conf.xprlen.W)))
  val pc_page     = io.if_pc(conf.xprlen-1, OFF_MSB+1)
  val page_sel    = VecInit(pc_pages.map(_ === pc_page)).asUInt & pg_valids.asUInt
  val tg_page     = Mux1H(page_sel, tg_pages)
  val idx_matches = Wire(Vec(nEntries, Bool()))
  idx_matches     := page_idxs.map(page_sel(_))
  /*
  * 0. if it is invalid, then use pc_plus
  * 1. if it is return, then peek the RAS, then use the top of the RAS -- using RAS
  * 2. if it is branch, then use brjump, but need to check take or not
  * 3. if it is jump, then use brjump
  * */
  val pc_plus = Wire(Vec(2, UInt(conf.xprlen.W)))
  pc_plus(0) := Cat(io.if_pc(conf.xprlen-1, conf.pcLSB+1), 1.U(1.W), 0.U(conf.pcLSB.W))
  pc_plus(1) := pc_plus(0) + 4.U(conf.xprlen.W)
  val pc_cands = Wire(Vec(2, Vec(BTBType.NUM, UInt(conf.xprlen.W))))
  io.split := io.predict(0).redirect && !io.if_pc(OFF_LSB).toBool
  for (i <- 0 until 2) {
    pc_offset(i)  := Cat(io.if_pc(OFF_MSB, OFF_LSB+1), i.U(1.W))
    off_sel(i)    := VecInit(pc_offsets.map(_ === pc_offset(i))).asUInt & idx_matches.asUInt & off_valids.asUInt
    tg_offset(i)  := Mux1H(off_sel(i), tg_offsets)
    btbType(i)    := Mux1H(off_sel(i), cfiTypes)
    hcnt(i)       := Mux1H(off_sel(i), hcnts)
    brjmp(i)      := Cat(tg_page, tg_offset(i), 0.U(OFF_LSB.W))

    pc_cands(i)(BTBType.invalid) := pc_plus(i)
    pc_cands(i)(BTBType.retn)    := io.raspeek
    pc_cands(i)(BTBType.jump)    := brjmp(i)
    pc_cands(i)(BTBType.branch)  := brjmp(i)
    // pc_cands(i)(CFIType.branch)  := Mux(hcnt(i)(1).toBool, brjmp(i), pc_plus)
    // FIXME: can optimized???
    io.predict(i).redirect := io.predict(i).you && (btbType(i) =/= BTBType.branch.U || hcnt(i)(1).toBool)
    io.predict(i).you := off_sel(i).orR
    io.predict(i).tgt := pc_cands(i)(Mux(io.predict(i).redirect, btbType(i), BTBType.invalid.U))
    io.predict(i).idx := OHToUInt(off_sel(i))
    io.predict(i).typ := btbType(i)
  }
  // Feedback ports **exe stage**
  /*
  * 1. page already exists === need pc page equal and target page equal
  * 2. page can be inserted
  * 3. page must replace the least recently used page
  * */
  val pc_pg = io.fb_pc(conf.xprlen-1, OFF_MSB+1)
  val new_tg_pg = io.feedBack.tgt(conf.xprlen-1, OFF_MSB+1)
  val pg_sel: UInt = VecInit(pc_pages.map(_ === pc_pg)).asUInt & pg_valids.asUInt
  val tg_pg = Mux1H(pg_sel, tg_pages)

  val page_exist: Bool = pg_sel.orR
  val page_idx = OHToUInt(pg_sel)
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
  for (i <- 0 until nEntries) {
    pg_idx_invalid(i) := (pg_replace && page_idxs(i) === pg_LRU.io.oldest) ||
                         (page_exist && page_idxs(i) === page_idx && tg_pg =/= new_tg_pg)
  }
  val off_valids_N: UInt = (~off_valids.asUInt).asUInt /*| pg_idx_matches.asUInt*/ // for time conside otherwise it is linear
  val off_insert: Bool = off_valids_N.orR
  val new_off_idx = PriorityEncoder(off_valids_N)
//  require(nEntries == 32)
//  val lfsr5 = RegInit(16.U(log2Ceil(nEntries).W))
//  val off_idx = Mux(off_insert, new_off_idx, lfsr5)
//  lfsr5 := Cat(lfsr5(log2Ceil(nEntries)-2,0), lfsr5(4)^lfsr5(2)) FIXME

  require(nEntries == 64)
  val lfsr6 = RegInit(32.U(log2Ceil(nEntries).W))
  val off_idx = Mux(off_insert, new_off_idx, lfsr6)
  lfsr6 := Cat(lfsr6(1)^lfsr6(0), lfsr6(log2Ceil(nEntries)-1,1))
  /*
  * 1. offset insert, page insert
  * 2. offset insert, page replace
  * 3. offset replace, page insert
  * 4. offset replace, page replace
  * */

  /*
  * 1. io.feedBack.sel.valid(true), io.feedBack.valid(true)   --exist branch taken
  * 2. io.feedBack.sel.valid(true), io.feedBack.valid(false)  --exist branch not taken
  * 3. io.feedBack.sel.valid(false), io.feedBack.valid(true)  --need insert and replace
  * 4. io.feedBack.sel.valid(false), io.feedBack.valid(false) --no side effect
  * */
  def update_map(idx: UInt): Unit = {
    when (page_exist) {
      page_idxs(idx)     := page_idx
      tg_pages(page_idx) := new_tg_pg
    }.otherwise {
      pg_valids(pg_idx)  := true.B
      tg_pages(pg_idx)   := new_tg_pg
      pc_pages(pg_idx)   := pc_pg
      page_idxs(idx)     := pg_idx
    }
    for (i <- 0 until nEntries) {
      when (pg_idx_invalid(i)) { off_valids(i)  := false.B }
    }
    off_valids(idx)  := true.B
    tg_offsets(idx)  := io.feedBack.tgt(OFF_MSB, OFF_LSB)
    pc_offsets(idx)  := io.fb_pc(OFF_MSB, OFF_LSB)
    cfiTypes(idx)    := io.feedBack.typ
  }

  when (io.feedBack.redirect) { update_map(Mux(io.feedBack.you, io.feedBack.idx, off_idx)) }

  when (io.feedBack.you) {    // BTB already exist, need update the map
    when (io.feedBack.typ === BTBType.branch.U) {
      val fb_hcnt = hcnts(io.feedBack.idx)
      when (io.feedBack.redirect) {
        when (fb_hcnt =/= Fill(cntLen, 1.U(1.W))) {
          hcnts(io.feedBack.idx) := fb_hcnt + 1.U
        }
      }.elsewhen(fb_hcnt =/= 0.U) {
        hcnts(io.feedBack.idx) := fb_hcnt - 1.U
      }
    }.elsewhen(!io.feedBack.redirect) {
      off_valids(io.feedBack.idx) := false.B
    }
  }
  .elsewhen(io.feedBack.redirect) { // BTB not exist, need insert or replace
    hcnts(off_idx) := 2.U
  }

}
