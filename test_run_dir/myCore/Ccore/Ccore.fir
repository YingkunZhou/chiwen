;buildInfoPackage: chisel3, version: 3.1.6, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2018-12-21 23:42:51.273, builtAtMillis: 1545435771273
circuit CaheCore : 
  module MyMem : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_5 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_6 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_7 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_8 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_9 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_10 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_11 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_12 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_13 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_14 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module MyMem_15 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<6>, flip wdata : UInt<32>, rdata : UInt<32>}
    
    smem mem : UInt<32>[64] @[CaheCore.scala 17:24]
    infer mport _T_15 = mem[io.addr], clock @[CaheCore.scala 18:18]
    io.rdata <= _T_15 @[CaheCore.scala 18:12]
    when io.wen : @[CaheCore.scala 19:17]
      infer mport _T_16 = mem[io.addr], clock @[CaheCore.scala 19:22]
      _T_16 <= io.wdata @[CaheCore.scala 19:32]
      skip @[CaheCore.scala 19:17]
    
  module CaheCore : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip wen : UInt<1>, flip addr : UInt<32>, flip wdata : UInt<32>[16], flip wstatus : UInt<1>, rdata : UInt<32>, rvalid : UInt<1>, flip cyc : UInt<32>}
    
    inst MyMem of MyMem @[CaheCore.scala 35:43]
    MyMem.clock <= clock
    MyMem.reset <= reset
    inst MyMem_1 of MyMem_1 @[CaheCore.scala 35:43]
    MyMem_1.clock <= clock
    MyMem_1.reset <= reset
    inst MyMem_2 of MyMem_2 @[CaheCore.scala 35:43]
    MyMem_2.clock <= clock
    MyMem_2.reset <= reset
    inst MyMem_3 of MyMem_3 @[CaheCore.scala 35:43]
    MyMem_3.clock <= clock
    MyMem_3.reset <= reset
    inst MyMem_4 of MyMem_4 @[CaheCore.scala 35:43]
    MyMem_4.clock <= clock
    MyMem_4.reset <= reset
    inst MyMem_5 of MyMem_5 @[CaheCore.scala 35:43]
    MyMem_5.clock <= clock
    MyMem_5.reset <= reset
    inst MyMem_6 of MyMem_6 @[CaheCore.scala 35:43]
    MyMem_6.clock <= clock
    MyMem_6.reset <= reset
    inst MyMem_7 of MyMem_7 @[CaheCore.scala 35:43]
    MyMem_7.clock <= clock
    MyMem_7.reset <= reset
    inst MyMem_8 of MyMem_8 @[CaheCore.scala 35:43]
    MyMem_8.clock <= clock
    MyMem_8.reset <= reset
    inst MyMem_9 of MyMem_9 @[CaheCore.scala 35:43]
    MyMem_9.clock <= clock
    MyMem_9.reset <= reset
    inst MyMem_10 of MyMem_10 @[CaheCore.scala 35:43]
    MyMem_10.clock <= clock
    MyMem_10.reset <= reset
    inst MyMem_11 of MyMem_11 @[CaheCore.scala 35:43]
    MyMem_11.clock <= clock
    MyMem_11.reset <= reset
    inst MyMem_12 of MyMem_12 @[CaheCore.scala 35:43]
    MyMem_12.clock <= clock
    MyMem_12.reset <= reset
    inst MyMem_13 of MyMem_13 @[CaheCore.scala 35:43]
    MyMem_13.clock <= clock
    MyMem_13.reset <= reset
    inst MyMem_14 of MyMem_14 @[CaheCore.scala 35:43]
    MyMem_14.clock <= clock
    MyMem_14.reset <= reset
    inst MyMem_15 of MyMem_15 @[CaheCore.scala 35:43]
    MyMem_15.clock <= clock
    MyMem_15.reset <= reset
    smem tag_RAM : UInt<20>[64] @[CaheCore.scala 36:30]
    wire _T_128 : UInt<1>[64] @[CaheCore.scala 37:34]
    _T_128[0] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[1] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[2] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[3] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[4] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[5] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[6] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[7] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[8] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[9] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[10] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[11] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[12] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[13] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[14] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[15] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[16] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[17] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[18] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[19] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[20] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[21] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[22] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[23] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[24] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[25] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[26] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[27] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[28] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[29] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[30] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[31] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[32] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[33] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[34] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[35] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[36] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[37] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[38] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[39] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[40] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[41] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[42] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[43] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[44] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[45] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[46] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[47] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[48] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[49] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[50] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[51] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[52] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[53] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[54] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[55] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[56] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[57] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[58] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[59] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[60] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[61] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[62] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    _T_128[63] <= UInt<1>("h00") @[CaheCore.scala 37:34]
    reg statusVec : UInt<1>[64], clock with : (reset => (reset, _T_128)) @[CaheCore.scala 37:26]
    node _T_523 = bits(io.addr, 5, 2) @[CaheCore.scala 38:34]
    reg line_idx : UInt, clock @[CaheCore.scala 38:26]
    line_idx <= _T_523 @[CaheCore.scala 38:26]
    node tag_ctx = bits(io.addr, 31, 12) @[CaheCore.scala 40:25]
    node off_idx = bits(io.addr, 11, 6) @[CaheCore.scala 41:25]
    wire rdata : UInt<32>[16] @[CaheCore.scala 43:19]
    MyMem.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem.io.wdata <= io.wdata[0] @[CaheCore.scala 48:24]
    rdata[0] <= MyMem.io.rdata @[CaheCore.scala 49:14]
    MyMem_1.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_1.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_1.io.wdata <= io.wdata[1] @[CaheCore.scala 48:24]
    rdata[1] <= MyMem_1.io.rdata @[CaheCore.scala 49:14]
    MyMem_2.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_2.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_2.io.wdata <= io.wdata[2] @[CaheCore.scala 48:24]
    rdata[2] <= MyMem_2.io.rdata @[CaheCore.scala 49:14]
    MyMem_3.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_3.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_3.io.wdata <= io.wdata[3] @[CaheCore.scala 48:24]
    rdata[3] <= MyMem_3.io.rdata @[CaheCore.scala 49:14]
    MyMem_4.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_4.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_4.io.wdata <= io.wdata[4] @[CaheCore.scala 48:24]
    rdata[4] <= MyMem_4.io.rdata @[CaheCore.scala 49:14]
    MyMem_5.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_5.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_5.io.wdata <= io.wdata[5] @[CaheCore.scala 48:24]
    rdata[5] <= MyMem_5.io.rdata @[CaheCore.scala 49:14]
    MyMem_6.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_6.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_6.io.wdata <= io.wdata[6] @[CaheCore.scala 48:24]
    rdata[6] <= MyMem_6.io.rdata @[CaheCore.scala 49:14]
    MyMem_7.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_7.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_7.io.wdata <= io.wdata[7] @[CaheCore.scala 48:24]
    rdata[7] <= MyMem_7.io.rdata @[CaheCore.scala 49:14]
    MyMem_8.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_8.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_8.io.wdata <= io.wdata[8] @[CaheCore.scala 48:24]
    rdata[8] <= MyMem_8.io.rdata @[CaheCore.scala 49:14]
    MyMem_9.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_9.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_9.io.wdata <= io.wdata[9] @[CaheCore.scala 48:24]
    rdata[9] <= MyMem_9.io.rdata @[CaheCore.scala 49:14]
    MyMem_10.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_10.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_10.io.wdata <= io.wdata[10] @[CaheCore.scala 48:24]
    rdata[10] <= MyMem_10.io.rdata @[CaheCore.scala 49:14]
    MyMem_11.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_11.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_11.io.wdata <= io.wdata[11] @[CaheCore.scala 48:24]
    rdata[11] <= MyMem_11.io.rdata @[CaheCore.scala 49:14]
    MyMem_12.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_12.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_12.io.wdata <= io.wdata[12] @[CaheCore.scala 48:24]
    rdata[12] <= MyMem_12.io.rdata @[CaheCore.scala 49:14]
    MyMem_13.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_13.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_13.io.wdata <= io.wdata[13] @[CaheCore.scala 48:24]
    rdata[13] <= MyMem_13.io.rdata @[CaheCore.scala 49:14]
    MyMem_14.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_14.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_14.io.wdata <= io.wdata[14] @[CaheCore.scala 48:24]
    rdata[14] <= MyMem_14.io.rdata @[CaheCore.scala 49:14]
    MyMem_15.io.addr <= off_idx @[CaheCore.scala 46:24]
    MyMem_15.io.wen <= io.wen @[CaheCore.scala 47:24]
    MyMem_15.io.wdata <= io.wdata[15] @[CaheCore.scala 48:24]
    rdata[15] <= MyMem_15.io.rdata @[CaheCore.scala 49:14]
    infer mport rtag = tag_RAM[off_idx], clock @[CaheCore.scala 52:21]
    reg rstatus : UInt<1>, clock @[CaheCore.scala 53:24]
    rstatus <= statusVec[off_idx] @[CaheCore.scala 53:24]
    when io.wen : @[CaheCore.scala 54:17]
      node _T_550 = bits(reset, 0, 0) @[CaheCore.scala 55:11]
      node _T_552 = eq(_T_550, UInt<1>("h00")) @[CaheCore.scala 55:11]
      when _T_552 : @[CaheCore.scala 55:11]
        printf(clock, UInt<1>(1), "tag = %x off_idx = %x\n", tag_ctx, off_idx) @[CaheCore.scala 55:11]
        skip @[CaheCore.scala 55:11]
      node _T_554 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_556 = eq(_T_554, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_556 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<1>("h00"), io.wdata[0]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_558 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_560 = eq(_T_558, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_560 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<1>("h01"), io.wdata[1]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_562 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_564 = eq(_T_562, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_564 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<2>("h02"), io.wdata[2]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_566 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_568 = eq(_T_566, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_568 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<2>("h03"), io.wdata[3]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_570 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_572 = eq(_T_570, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_572 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h04"), io.wdata[4]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_574 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_576 = eq(_T_574, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_576 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h05"), io.wdata[5]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_578 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_580 = eq(_T_578, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_580 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h06"), io.wdata[6]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_582 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_584 = eq(_T_582, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_584 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h07"), io.wdata[7]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_586 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_588 = eq(_T_586, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_588 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h08"), io.wdata[8]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_590 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_592 = eq(_T_590, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_592 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h09"), io.wdata[9]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_594 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_596 = eq(_T_594, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_596 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0a"), io.wdata[10]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_598 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_600 = eq(_T_598, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_600 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0b"), io.wdata[11]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_602 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_604 = eq(_T_602, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_604 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0c"), io.wdata[12]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_606 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_608 = eq(_T_606, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_608 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0d"), io.wdata[13]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_610 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_612 = eq(_T_610, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_612 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0e"), io.wdata[14]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_614 = bits(reset, 0, 0) @[CaheCore.scala 60:13]
      node _T_616 = eq(_T_614, UInt<1>("h00")) @[CaheCore.scala 60:13]
      when _T_616 : @[CaheCore.scala 60:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0f"), io.wdata[15]) @[CaheCore.scala 60:13]
        skip @[CaheCore.scala 60:13]
      node _T_617 = bits(reset, 0, 0) @[CaheCore.scala 62:11]
      node _T_619 = eq(_T_617, UInt<1>("h00")) @[CaheCore.scala 62:11]
      when _T_619 : @[CaheCore.scala 62:11]
        printf(clock, UInt<1>(1), "\n") @[CaheCore.scala 62:11]
        skip @[CaheCore.scala 62:11]
      skip @[CaheCore.scala 54:17]
    when io.wen : @[CaheCore.scala 65:16]
      infer mport _T_620 = tag_RAM[off_idx], clock @[CaheCore.scala 65:25]
      node _T_621 = bits(io.addr, 31, 12) @[CaheCore.scala 65:45]
      _T_620 <= _T_621 @[CaheCore.scala 65:35]
      skip @[CaheCore.scala 65:16]
    when io.wen : @[CaheCore.scala 66:16]
      statusVec[off_idx] <= io.wstatus @[CaheCore.scala 66:37]
      skip @[CaheCore.scala 66:16]
    reg tag : UInt, clock @[CaheCore.scala 68:23]
    tag <= tag_ctx @[CaheCore.scala 68:23]
    node _T_628 = or(line_idx, UInt<4>("h00"))
    node _T_629 = bits(_T_628, 3, 0)
    io.rdata <= rdata[_T_629] @[CaheCore.scala 69:13]
    node _T_630 = eq(rtag, tag) @[CaheCore.scala 70:21]
    node _T_631 = and(_T_630, rstatus) @[CaheCore.scala 70:29]
    io.rvalid <= _T_631 @[CaheCore.scala 70:13]
    node _T_633 = eq(io.cyc, UInt<8>("h0e9")) @[CaheCore.scala 71:16]
    when _T_633 : @[CaheCore.scala 71:27]
      reg _T_635 : UInt, clock @[CaheCore.scala 75:16]
      _T_635 <= off_idx @[CaheCore.scala 75:16]
      node _T_636 = bits(reset, 0, 0) @[CaheCore.scala 72:11]
      node _T_638 = eq(_T_636, UInt<1>("h00")) @[CaheCore.scala 72:11]
      when _T_638 : @[CaheCore.scala 72:11]
        printf(clock, UInt<1>(1), "tag = %x rtag = %x off_idx = %x\n", tag, rtag, _T_635) @[CaheCore.scala 72:11]
        skip @[CaheCore.scala 72:11]
      node _T_640 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_642 = eq(_T_640, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_642 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<1>("h00"), rdata[0]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_644 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_646 = eq(_T_644, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_646 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<1>("h01"), rdata[1]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_648 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_650 = eq(_T_648, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_650 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<2>("h02"), rdata[2]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_652 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_654 = eq(_T_652, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_654 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<2>("h03"), rdata[3]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_656 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_658 = eq(_T_656, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_658 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h04"), rdata[4]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_660 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_662 = eq(_T_660, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_662 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h05"), rdata[5]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_664 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_666 = eq(_T_664, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_666 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h06"), rdata[6]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_668 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_670 = eq(_T_668, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_670 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<3>("h07"), rdata[7]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_672 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_674 = eq(_T_672, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_674 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h08"), rdata[8]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_676 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_678 = eq(_T_676, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_678 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h09"), rdata[9]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_680 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_682 = eq(_T_680, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_682 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0a"), rdata[10]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_684 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_686 = eq(_T_684, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_686 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0b"), rdata[11]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_688 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_690 = eq(_T_688, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_690 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0c"), rdata[12]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_692 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_694 = eq(_T_692, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_694 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0d"), rdata[13]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_696 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_698 = eq(_T_696, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_698 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0e"), rdata[14]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_700 = bits(reset, 0, 0) @[CaheCore.scala 79:13]
      node _T_702 = eq(_T_700, UInt<1>("h00")) @[CaheCore.scala 79:13]
      when _T_702 : @[CaheCore.scala 79:13]
        printf(clock, UInt<1>(1), "%x: %x | ", UInt<4>("h0f"), rdata[15]) @[CaheCore.scala 79:13]
        skip @[CaheCore.scala 79:13]
      node _T_703 = bits(reset, 0, 0) @[CaheCore.scala 81:11]
      node _T_705 = eq(_T_703, UInt<1>("h00")) @[CaheCore.scala 81:11]
      when _T_705 : @[CaheCore.scala 81:11]
        printf(clock, UInt<1>(1), "\n") @[CaheCore.scala 81:11]
        skip @[CaheCore.scala 81:11]
      skip @[CaheCore.scala 71:27]
    
