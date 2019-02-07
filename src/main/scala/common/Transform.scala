package common

import chisel3._
import chisel3.util.Cat
import myCore.{AxiIO, Latch}

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

    //    when(io.cyc === 171.U) {
    //      printf("Transform: valid = %x addr = %x inst = %x\n", io.outer.req.valid, io.outer.req.bits.addr, io.outer.resp.bits.data)
    //    }
  }
  else {
    io.outer.req.valid     := io.inner.ar.valid
    io.outer.req.bits.addr := io.inner.ar.addr
    io.inner.ar.ready := io.outer.req.ready
    io.inner.r.valid  := RegNext(io.outer.resp.valid && io.inner.ar.ready)
    io.inner.r.data   := RegNext(io.outer.resp.bits.data)
    io.inner.r.last  := DontCare
  }


  io.outer.req.bits.fcn  := M_XRD
  io.outer.req.bits.typ  := MT_WU
  io.outer.req.bits.data := DontCare

  io.inner.r.id    := conf.iccRd
}

class SimpleTrans(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val outer = new MemPortIo(conf.xprlen)
    val inner = Flipped(new MemPortIo(conf.xprlen))
  })

  val lfsr5 = RegInit(1.U(5.W))
  lfsr5 := Cat(lfsr5(3,0), lfsr5(4)^lfsr5(2))

  val cnt = RegInit(0.U(3.W))
  val expect_cnt = RegInit(0.U(3.W))
  val valid = RegInit(false.B)
  when (io.inner.req.valid) {
    valid := true.B
    cnt := cnt + 1.U
    expect_cnt := lfsr5(2,0)
  }.elsewhen(cnt =/= expect_cnt) {
    cnt := cnt + 1.U
  }.otherwise {
    valid := false.B
  }

  io.inner <> io.outer
//  io.inner.resp <> io.outer.resp
//  io.inner.req.bits <> io.outer.req.bits
//  io.outer.req.valid := valid && cnt === expect_cnt
//  io.inner.req.ready := io.outer.req.ready

}