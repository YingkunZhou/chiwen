package fuxi

import chisel3._

object Jump {
  val none = 0
  val push = 1
  val pop  = 2
  val NUM: Int  = pop + 1
}

class MicroDecoder(inst_width: Int) extends Module{
  val io = IO(new Bundle{
    val inst = Input(UInt(inst_width.W))
//    val jump = Output(UInt(Jump.NUM.W))
    val isjal = Output(Bool())
    val isjalr = Output(Bool())
    val is_bj = Output(Bool())
    val branch = Output(Bool())
    val call = Output(Bool())
    val retn = Output(Bool())
  })

  val func  = io.inst(6,2)
  val lui   = func === "b01101".U
  val auipc = func === "b00101".U
  val jal   = func === "b11011".U
  val jalr  = func === "b11001".U
  val brnch = func === "b11000".U
  val load  = func === "b00000".U
  val store = func === "b01000".U
  val ariti = func === "b00100".U
  val arith = func === "b01100".U
  val csrr  = func === "b11100".U
  io.isjal := jal
  io.isjalr := jalr
  io.branch := brnch
  io.is_bj := jal || jalr || brnch
  def link(addr: UInt): Bool = addr === 1.U || addr === 5.U
  /*
  * A JAL instruction should push the return address onto
  * a return-address stack (RAS) only when rd=x1/x5
  *
  * JALR instructions should push/pop a RAS as shown in the Table:
  *   rd    |   rs1    |    rs1 = rd    |   RAS action
  *1 !link  |  !link   |        -       |   none
  *2 !link  |   link   |        -       |   pop
  *3  link  |  !link   |        -       |   push
  *4  link  |   link   |        0       |   push and pop
  *5  link  |   link   |        1       |   push
  */

  val rs1_addr = io.inst(RS1_MSB, RS1_LSB)
  val wbaddr   = io.inst(RD_MSB , RD_LSB)
  val pop_push = jalr && link(wbaddr) && link(rs1_addr) && wbaddr =/= rs1_addr //#4
//
//  val jump = Wire(Vec(Jump.NUM, Bool()))
//  io.jump := jump.asUInt
//
//  jump(Jump.none) := (jal && !link(wbaddr)) ||
//    (jalr && !link(wbaddr) && !link(rs1_addr))// #1
//
//  jump(Jump.push) := (jal  &&  link(wbaddr))   ||
//    (jalr &&  link(wbaddr) && !link(rs1_addr)) || // #3
//    (jalr &&  link(wbaddr) &&  link(rs1_addr)  && wbaddr === rs1_addr) || // #5
//    pop_push // #4
//
//  jump(Jump.pop) := (jalr && !link(wbaddr) && link(rs1_addr))  || // #2
//    pop_push // #2

  io.call := (jal  &&  link(wbaddr))  ||
    (jalr &&  link(wbaddr) && !link(rs1_addr)) || // #3
    (jalr &&  link(wbaddr) &&  link(rs1_addr)  && wbaddr === rs1_addr) || // #5
    pop_push // #4
  io.retn := (jalr && !link(wbaddr) && link(rs1_addr))  || // #2
    pop_push // #2
}
