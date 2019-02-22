package myCore

import chisel3._
import chisel3.util._
import common.CPUConfig

trait Pram {
  val nEntry   = 8
  val nPhyAddr = 64
  val nOrder   = 32
  val nBuffer  = 8
  require(isPow2(nOrder)) //FIXME
  val wCount   = log2Ceil(nEntry+1)
  val wPhyAddr = log2Ceil(nPhyAddr)
  val wOrder   = log2Ceil(nOrder)
  val nByPass  = 7
  val nCommit  = 4
  val nBrchjr  = 4
  val nInst    = 2
}

class Front(implicit val conf: CPUConfig) extends Bundle {
  val inst = UInt(conf.inst_width.W)
  val pc   = UInt(conf.data_width.W)
}

class InstBuffer(val nEntry: Int)(implicit val conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val flush  = Input(Valid(UInt(log2Ceil(nEntry).W)))
    val raddr  = Input(UInt(log2Ceil(nEntry).W))
    val out    = Decoupled(new Front)
    val wen    = Input(Bool())
    val waddr  = Input(UInt(log2Ceil(nEntry).W))
    val in     = Input(Valid(new Front))
  })
  val buffer = Mem(nEntry, new Front)
  val valids = RegInit(VecInit(Seq.fill(nEntry)(false.B)))
  io.out.bits := buffer(io.raddr)
  when (io.wen) { buffer(io.waddr) := io.in.bits }

  io.out.valid := valids(io.raddr)
  // Attention: please don't change the order
  when (io.out.fire) { valids(io.raddr) := false.B }
  when (io.wen) { valids(io.waddr) := io.in.valid }
  when (io.flush.valid) { valids(io.flush.bits) := false.B }
}

object QueuePtr {
  val head = 0
  val tail = 1
}

import QueuePtr._
class InstQueue(implicit val conf: CPUConfig) extends Module with Pram {
  val io = IO(new Bundle {
    val forward = Output(Bool())
    val flush   = Input(Bool())
    val in      = Input(Vec(nInst, Valid(new Front)))
    val out     = Vec(nInst, Decoupled(new Front))
  })

  val instBuffer = Array.fill(nInst)(Module(new InstBuffer(nBuffer)).io)
  val ptr = RegInit(VecInit(Seq.fill(2)(0.U(log2Ceil(nBuffer).W))))
  val ptrInc  = Wire(Vec(2, Bool()))
  val ptrNext = Wire(Vec(2, UInt(log2Ceil(nBuffer).W)))
  val nFull   = RegInit(true.B)
  io.forward := nFull
  for (i <- 0 until nInst) {
    instBuffer(i).flush.valid := io.flush
    instBuffer(i).flush.bits  := 0.U
    instBuffer(i).raddr := ptr(head)
    instBuffer(i).waddr := ptr(tail)
    instBuffer(i).wen   := ptrInc(tail)

    instBuffer(i).in    := io.in
    instBuffer(i).out   <> io.out
  }
  ptrInc(head) := io.out(0).fire  && io.out(1).fire  ||
                  io.out(0).fire  && !io.out(1).valid ||
                  io.out(1).fire  && !io.out(0).valid

  ptrInc(tail) := nFull && (io.in(0).valid || io.in(1).valid)

  for (i <- 0 until 2) {
    if (isPow2(nBuffer)) ptrNext(i) := ptr(i) + 1.U
    else ptrNext(i) := Mux(ptr(i) === (nBuffer-1).U, 0.U, ptr(i) + 1.U)
    when(io.flush)         { ptr(i) := 0.U
    }.elsewhen (ptrInc(i)) { ptr(i) := ptrNext(i) }
  }

  when (io.flush) { nFull := false.B
  }.otherwise {
    when (ptrInc(head) && !ptrInc(tail)) { nFull := true.B }
    when (ptrInc(tail) && !ptrInc(head) && ptrNext(tail) === ptr(head)) { nFull := false.B }
  }

}