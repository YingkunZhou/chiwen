package common

import chisel3._
import chisel3.util.Cat

class AxiRIO(val data_width: Int) extends Bundle {
  val data     = Input(UInt(data_width.W))
  val valid    = Input(Bool())
  val last     = Input(Bool())
  val id       = Input(UInt(4.W))
}

class AxiARIO(val data_width: Int) extends Bundle {
  val ready   = Input(Bool())
  val valid   = Output(Bool())
  val id      = Output(UInt(4.W))
  val addr    = Output(UInt(data_width.W))
  val burst   = Output(UInt(2.W))
  val size    = Output(UInt(3.W))
  val len     = Output(UInt(8.W))
}

class AxiIO(val data_width: Int) extends Bundle {
  val r  = new AxiRIO(data_width)
  val ar = new AxiARIO(data_width)
}

object Latch {
  def apply(in: Bool, wait: Bool, addition: Bool = true.B): Bool = {
    val in_latch = RegInit(false.B)
    when (wait) { in_latch := false.B
    }.elsewhen(in && addition) {in_latch := true.B}
    in || in_latch
  }
}

class Transform(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val outer = new MemPortIo(conf.xprlen)
    val inner = Flipped(new AxiIO(conf.xprlen))
    val cyc = Input(UInt(conf.xprlen.W))
  })

  if (conf.delayFechinst) {
    val cnt = RegInit(0.U(4.W))
    val lfsr5 = RegInit(1.U(5.W))
    cnt   := cnt + 1.U
    lfsr5 := Cat(lfsr5(3,0), lfsr5(4)^lfsr5(2))

    val actual_cnt = RegInit(0.U(4.W))
    val expect_cnt = RegInit(0.U(4.W))
    val addr_sent  = RegInit(false.B)
    val fire: Bool = io.inner.ar.ready && io.inner.ar.valid

    val valid_cnt = RegInit(0.U(4.W))
    val first_valid = actual_cnt === expect_cnt && addr_sent
    val burst_addr = Reg(UInt(conf.xprlen.W))
    val double_valid  = RegInit(false.B)
    val first_addr = Reg(UInt(conf.xprlen.W))
    when (valid_cnt =/= 0.U) {
      addr_sent  := false.B
    }.elsewhen (fire) { // actual_cnt === expect_cnt
      addr_sent  := true.B //while actual_cnt =/= expect_cnt maintain valid
      actual_cnt := actual_cnt + 1.U
      expect_cnt := cnt
      first_addr := io.inner.ar.addr
    }.elsewhen(actual_cnt =/= expect_cnt) {
      actual_cnt := actual_cnt + 1.U
    }.otherwise { // actual_cnt === expect_cnt
      addr_sent  := false.B
    }

    when(valid_cnt =/= 0.U) {
      valid_cnt := valid_cnt + 1.U
      burst_addr := burst_addr + 4.U
    }.elsewhen (first_valid || double_valid) { //valid_cnt === 0.U
      valid_cnt := valid_cnt + 1.U
      burst_addr:= io.inner.ar.addr + 4.U
    }

    val double_addr = Reg(UInt(conf.xprlen.W))
    when (valid_cnt === 0.U) {
      double_valid := false.B
    }.elsewhen(fire) { //valid_cnt =/= 0.U
      double_valid := true.B //while valid_cnt =/= 0.U maintain valid
      double_addr := io.inner.ar.addr
    }
    val valid: Bool = valid_cnt =/= 0.U || first_valid || double_valid
    io.outer.req.valid     := valid
    io.inner.r.valid       := valid
    io.outer.req.bits.addr := Mux(valid_cnt =/= 0.U, burst_addr,
      Mux(first_valid, first_addr, double_addr))

    io.inner.ar.ready      := Latch(lfsr5 > 12.U && lfsr5 < 18.U, cnt(2).toBool)
    io.inner.r.last        := valid_cnt === 15.U
    io.inner.r.data        := io.outer.resp.bits.data
    if (conf.use_cc) io.inner.r.id := conf.iccRd
    else io.inner.r.id := conf.incRd
  }
  else {
    io.outer.req.valid     := io.inner.ar.valid
    io.outer.req.bits.addr := io.inner.ar.addr
    io.inner.ar.ready      := io.outer.req.ready
    io.inner.r.valid       := RegNext(io.outer.resp.valid && io.inner.ar.ready)
    io.inner.r.data        := RegNext(io.outer.resp.bits.data)
    io.inner.r.last        := DontCare
    io.inner.r.id          := conf.incRd
  }

  io.outer.req.bits.fcn  := M_XRD
  io.outer.req.bits.typ  := MT_WU
  io.outer.req.bits.data := DontCare
}

class SimpleTrans(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val outer = new MemPortIo(conf.xprlen)
    val inner = Flipped(new MemPortIo(conf.xprlen))
    val cyc = Input(UInt(conf.xprlen.W))
  })
  io.inner.req.ready := true.B
  val valid = RegInit(false.B)
  val bits  = Reg(new MemReq(conf.data_width))
  val stall = valid && !io.outer.resp.valid
  when (!stall) {
    valid := io.inner.req.valid
    bits  := io.inner.req.bits
  }
  //TODO: print memory trace
  io.outer.req.valid := valid
  io.outer.req.bits  := bits
  io.inner.resp := io.outer.resp
  when (io.inner.req.valid && !stall) {
    printf("Memory: Cyc= %d ", io.cyc)
    when (io.inner.req.bits.fcn === M_XWR) {
      printf("STORE[ %x %x %x]\n",
        io.inner.req.bits.typ,
        io.inner.req.bits.addr,
        io.inner.req.bits.data)
    }.otherwise {
      printf("LOAD[ %x %x]\n",
        io.inner.req.bits.typ,
        io.inner.req.bits.addr)
    }
  }

}