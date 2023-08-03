package csr 

import chisel3._
import chisel3.util._

import tools._

import difftest._

class CSR extends Module with CoreParameters{
  val io = IO(new Bundle {
    val rdEn       = Input (Bool())
    val rdAddr     = Input (UInt(12.W))
    val rdIllegal  = Output(Bool())
    val rdData     = Output(UInt(64.W))

    val wrEn       = Input (Bool())
    val wrCmd      = Input (UInt(2.W)) //11-wr, 01-set, 10-clean
    val wrAddr     = Input (UInt(12.W))
    val wrData     = Input (UInt(64.W))
    val wrIllegal = Output(Bool())

    val excp       = Input (new ExcpBundle)
    val csrinfo    = Output(new CSRInfoBundle)
  })  

//function
//---------------------------------------------------------------------------------
  def csrExist(addr: UInt) : Bool = { 
    addr === CSRs.mstatus.U ||
    addr === CSRs.misa.U ||
    addr === CSRs.mie.U ||
    addr === CSRs.mtvec.U ||
    addr === CSRs.mscratch.U ||
    addr === CSRs.mepc.U ||
    addr === CSRs.mcause.U ||
    addr === CSRs.mtval.U ||
    addr === CSRs.mip.U ||
    addr === CSRs.mvendorid.U ||
    addr === CSRs.marchid.U ||
    addr === CSRs.mimpid.U ||
    addr === CSRs.mhartid.U  
  }
  def csrRO(addr: UInt) : Bool = { 
    addr === CSRs.mvendorid.U ||
    addr === CSRs.marchid.U ||
    addr === CSRs.mimpid.U ||
    addr === CSRs.mhartid.U  
  }
//---------------------------------------------------------------------------------
  class MSTATUS extends Bundle{
    val mbe = Bool()    //0 
    val mpp = UInt(2.W) //11
    val mie = Bool()    //0 
    def value = Cat(0.U(26.W), mbe, 0.U(24.W), mpp, 0.U(11.W))
  }

  val mstatus = RegInit("b0110".U.asTypeOf(new MSTATUS))
  when(io.wrEn && io.wrCmd === "b11".U && io.wrAddr === CSRs.mstatus.U){
    mstatus.mie := io.wrData(3)
  }.elsewhen(io.wrEn && io.wrCmd === "b10".U && io.wrAddr === CSRs.mstatus.U){
    mstatus.mie := Mux(io.wrData(3), true.B, mstatus.mie)
  }.elsewhen(io.wrEn && io.wrCmd === "b01".U && io.wrAddr === CSRs.mstatus.U){
    mstatus.mie := Mux(io.wrData(3), false.B, mstatus.mie)
  }.elsewhen(io.excp.valid){
    mstatus.mie := false.B
  }


  class MISA extends Bundle{
    val mxl = UInt(2.W) //10
    val m   = Bool() //1 
    val i   = Bool() //1 
    def value = Cat(mxl, 0.U(49.W), m, 0.U(3.W), i, 0.U(8.W))
  }
  val misa = RegInit("b1011".U.asTypeOf(new MISA))
  dontTouch(misa) 

  class MTVEC extends Bundle{
    val base = UInt(62.W) //0.U
    val mode = UInt(2.W)  //00
    def value = Cat(base, mode)
  }
  val mtvec = RegInit(0.U.asTypeOf(new MTVEC))
  when(io.wrEn && io.wrCmd === "b11".U && io.wrAddr === CSRs.mtvec.U){
    mtvec.base := io.wrData(63, 2)
    mtvec.mode := io.wrData(1, 0)
  }.elsewhen(io.wrEn && io.wrCmd === "b10".U && io.wrAddr === CSRs.mtvec.U){
    mtvec.base := mtvec.base & ~io.wrData(63, 2)
    mtvec.mode := mtvec.mode & ~io.wrData(1, 0)
  }.elsewhen(io.wrEn && io.wrCmd === "b01".U && io.wrAddr === CSRs.mtvec.U){
    mtvec.base := io.wrData(63, 2) | mtvec.base
    mtvec.mode := io.wrData(1, 0) | mtvec.mode
  }
  //dontTouch(mtvec) 

  class MEPC extends Bundle {
    val mepc = UInt(64.W)
    def value = mepc
  }
  val mepc = RegInit(0.U.asTypeOf(new MEPC))
  when(io.wrEn && io.wrCmd === "b11".U && io.wrAddr === CSRs.mepc.U){
    mepc.mepc := io.wrData
  }.elsewhen(io.wrEn && io.wrCmd === "b10".U && io.wrAddr === CSRs.mepc.U){
    mepc.mepc := mepc.mepc & ~io.wrData
  }.elsewhen(io.wrEn && io.wrCmd === "b01".U && io.wrAddr === CSRs.mepc.U){
    mepc.mepc := io.wrData | mepc.mepc
  }.elsewhen(io.excp.valid){
    mepc.mepc := io.excp.epc
  }
  dontTouch(mepc)

  class MSCRATCH extends Bundle {
    val mscratch = UInt(64.W)
    def value = mscratch
  }
  val mscratch = RegInit(0.U.asTypeOf(new MSCRATCH))
  when(io.wrEn && io.wrCmd === "b11".U && io.wrAddr === CSRs.mscratch.U){
    mscratch.mscratch := io.wrData
  }.elsewhen(io.wrEn && io.wrCmd === "b10".U && io.wrAddr === CSRs.mscratch.U){
    mscratch.mscratch := mscratch.mscratch & ~io.wrData
  }.elsewhen(io.wrEn && io.wrCmd === "b01".U && io.wrAddr === CSRs.mscratch.U){
    mscratch.mscratch := io.wrData | mscratch.mscratch
  }
  dontTouch(mscratch)


  //??? can mcause write by csrrw?
  class MCAUSE extends Bundle{
    val interrupt = Bool()    //0
    val excpCode  = UInt(4.W) //0000
    def value = Cat(interrupt, 0.U(59.W), excpCode)
  }
  val mcause = RegInit(0.U.asTypeOf(new MCAUSE))
  when(io.excp.valid){
    mcause.excpCode := io.excp.ecode
  }
  dontTouch(mcause)


  class MTVAL extends Bundle {
    val mtval = UInt(64.W)
    def value = mtval
  }
  val mtval = RegInit(0.U.asTypeOf(new MTVAL))
  when(io.wrEn && io.wrCmd === "b11".U && io.wrAddr === CSRs.mtval.U){
    mtval.mtval := io.wrData
  }.elsewhen(io.wrEn && io.wrCmd === "b10".U && io.wrAddr === CSRs.mtval.U){
    mtval.mtval := mtval.mtval & ~io.wrData
  }.elsewhen(io.wrEn && io.wrCmd === "b01".U && io.wrAddr === CSRs.mtval.U){
    mtval.mtval := io.wrData | mtval.mtval
  }.elsewhen(io.excp.valid){
    mtval.mtval := io.excp.tval
  }
  dontTouch(mtval)
 io.rdData := MuxLookup(io.rdAddr, 0.U, Array(
   CSRs.mstatus.U  -> mstatus.value,
   CSRs.misa.U     -> misa.value,
   CSRs.mtvec.U    -> mtvec.value,
   CSRs.mscratch.U -> mscratch.value,
   CSRs.mepc.U     -> mepc.value,
   CSRs.mcause.U   -> mcause.value,
   CSRs.mtval.U    -> mtval.value))


  io.rdIllegal := io.rdEn && !csrExist(io.rdAddr)
  io.wrIllegal := io.wrEn && (!csrExist(io.wrAddr) || csrRO(io.wrAddr))

  io.csrinfo.mtvec := mtvec.value
  io.csrinfo.mepc  := mepc.mepc

  //if(EnableDifftest){
  //  val difftest = Module(new DifftestArchEvent)
  //  difftest.io.clock := clock
  //  difftest.io.exceptionPC := mepc.mepc
  //}
  //if(EnableDifftest){
  //  val difftest = Module(new DifftestCSRState)
  //  difftest.io.clock := clock
  //  difftest.io.mstatus := mstatus.value
  //  difftest.io.mepc    := mepc.value
  //  difftest.io.mtvec   := mtvec.value
  //  difftest.io.mcause  := mcause.value
  //  difftest.io.priviledgeMode := 3.U
  //}
}

