;buildInfoPackage: chisel3, version: 3.1.6, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2018-12-21 23:42:51.273, builtAtMillis: 1545435771273
circuit OneHotDecoder : 
  module OneHotDecoder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<2>, flip en : UInt<1>, out : UInt<4>}
    
    reg myReg : UInt<4>[4], clock @[OneHotDecoder.scala 13:18]
    when io.en : @[OneHotDecoder.scala 14:16]
      myReg[0] <= UInt<1>("h00") @[OneHotDecoder.scala 16:16]
      myReg[1] <= UInt<1>("h00") @[OneHotDecoder.scala 16:16]
      myReg[2] <= UInt<1>("h00") @[OneHotDecoder.scala 16:16]
      myReg[3] <= UInt<1>("h00") @[OneHotDecoder.scala 16:16]
      myReg[io.in] <= UInt<1>("h01") @[OneHotDecoder.scala 18:18]
      skip @[OneHotDecoder.scala 14:16]
    io.out <= myReg[io.in] @[OneHotDecoder.scala 20:10]
    
