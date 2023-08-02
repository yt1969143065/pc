package exe 

import chisel3._
import chisel3.util._

import tools._
import difftest._

class MUL extends Module {
  val io = IO(new Bundle{
    val signA = Input (Bool())
    val signB = Input (Bool())
    val isHi  = Input (Bool())
    val isW   = Input (Bool())
    val a     = Input (UInt(64.W))
    val b     = Input (UInt(64.W))
    val c     = Output(UInt(64.W))
  })  
  //---------------------------------------------------
  // 2-cycle, just behavior modle, not real circuit
  //---------------------------------------------------

  //input
  val signA = io.signA
  val signB = io.signB
  val isHi = io.isHi
  val isW = io.isW
  val a = io.a
  val b = io.b
  //output
  val c = Reg(UInt(64.W))
  io.c := c

  val srcA = Mux(signA, Cat(a(63), a), Cat("b0".U, a)) 
  val srcB = Mux(signB, Cat(b(63), b), Cat("b0".U, b)) 
  val result = (srcA.asSInt * srcB.asSInt).asUInt 

  c :=  
       Mux(isW,  Cat(Fill(32, result(31)), result(31, 0)), 
                 Mux(isHi, result(127, 64), result(63, 0)))
}


class DIV extends Module {
  val io = IO(new Bundle{
    val valid = Input (Bool())
    val sign  = Input (Bool())
    val isRem = Input (Bool())
    val isW   = Input (Bool())
    val a     = Input (UInt(64.W))
    val b     = Input (UInt(64.W))
    //val done  = Output(Bool())     //T7 iwb
    val c     = Output(UInt(64.W)) //T10 iwb
  })  
  //---------------------------------------------------
  // 10-cycle, just behavior modle, not real circuit
  //---------------------------------------------------

  //input
  val valid = io.valid
  val sign = io.sign
  val isRem = io.isRem
  val isW = io.isW
  val a = io.a
  val b = io.b
  //output
  //val done = Wire(Bool())
  val c = Wire(UInt(64.W))
  //io.done := done
  io.c := c

  val srcA = Mux(sign, Cat(a(63), a), Cat("b0".U, a)) 
  val srcB = Mux(sign, Cat(b(63), b), Cat("b0".U, b)) 
  val resultQ = Mux(srcB === 0.U, ~(0.U(65.W)), ((srcA.asSInt / srcB.asSInt)).asUInt(63, 0)) 
  val resultR = Mux(srcB === 0.U, srcA, ((srcA.asSInt % srcB.asSInt)).asUInt(63, 0)) 
  val srcAw = Mux(sign, Cat(a(31), a(31, 0)), Cat("b0".U, a(31, 0)))
  val srcBw = Mux(sign, Cat(b(31), b(31, 0)), Cat("b0".U, b(31, 0)))
  val resultQwOrg = Mux(srcBw === 0.U, ~(0.U(33.W)), (srcAw.asSInt / srcBw.asSInt).asUInt)
  val resultRwOrg = Mux(srcBw === 0.U, srcAw, (srcAw.asSInt % srcBw.asSInt).asUInt)
  val resultQw = Cat(Fill(32, resultQwOrg(31)), resultQwOrg(31, 0)) 
  val resultRw = Cat(Fill(32, resultRwOrg(31)), resultRwOrg(31, 0)) 

  val cRes = Mux(isW, 
                  Mux(isRem, resultRw, resultQw),
                  Mux(isRem, resultR , resultQ )
             )

  /*
  val validReg = RegInit(VecInit(Seq.fill(7)(false.B)))
  for(i <- 0 until 7){
    if(i == 0){
      validReg(i) := valid
    }else{
      validReg(i) := validReg(i-1)
    }
  }
  done := validReg(6)
  */
  val dataReg = Reg(Vec(10, UInt(64.W)))
  for(i <- 0 until 10){
    if(i == 0){
      dataReg(i) := cRes
    }else{
      dataReg(i) := dataReg(i-1)
    }
  }
  c := dataReg(9)
}



class MDU extends Module with CoreParameters{
  val io = IO(new Bundle {
    val iexe   = Input(new MicroOp)
    val done = Output(Bool())
    val iwb  = Output(new MicroOp)
    val redirect = Input(new Redirect)
  })
  //input
  val iexe = io.iexe
  val redirect = io.redirect
  //output
  val iwb = RegInit(0.U.asTypeOf(new MicroOp))
  val done = Wire(Bool())
  io.iwb := iwb
  io.done := done

  val mulUnit = Module(new MUL)
  mulUnit.io.signA :=
    Mux(iexe.fuOp === MduOpType.mul   , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.mulh  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.mulhsu, true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.mulw  , true.B, false.B)
  mulUnit.io.signB :=
    Mux(iexe.fuOp === MduOpType.mul   , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.mulh  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.mulw  , true.B, false.B)
  mulUnit.io.isHi :=
    Mux(iexe.fuOp === MduOpType.mulh  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.mulhu , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.mulhsu, true.B, false.B)
  mulUnit.io.isW :=
    Mux(iexe.fuOp === MduOpType.mulw  , true.B, false.B)
  mulUnit.io.a := iexe.src1Data
  mulUnit.io.b := iexe.src2Data

  val divUnit = Module(new DIV)
  divUnit.io.valid := iexe.valid && iexe.exeCyc === ExeCyc.NA
  divUnit.io.sign :=
    Mux(iexe.fuOp === MduOpType.div   , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.rem   , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.divw  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.remw  , true.B, false.B)
  divUnit.io.isRem :=
    Mux(iexe.fuOp === MduOpType.rem   , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.remu  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.remw  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.remuw , true.B, false.B)
  divUnit.io.isW :=
    Mux(iexe.fuOp === MduOpType.divw  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.divuw  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.remw  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.remw  , true.B, false.B) |
    Mux(iexe.fuOp === MduOpType.remuw , true.B, false.B)
  divUnit.io.a := iexe.src1Data
  divUnit.io.b := iexe.src2Data

  val resultC1 = iexe.src2Data
  val resultC2 = mulUnit.io.c
  val resultCX = divUnit.io.c

  val instC2Save = RegInit(0.U.asTypeOf(new MicroOp))
  val instCXSave = RegInit(0.U.asTypeOf(new MicroOp))
  val instCXPipe = RegInit(VecInit(Seq.fill(3)(0.U.asTypeOf(new MicroOp))))

  //done := divUnit.io.done
  val validReg = RegInit(VecInit(Seq.fill(7)(false.B)))
  for(i <- 0 until 7){
    if(i == 0){
      validReg(i) := iexe.valid && iexe.exeCyc === ExeCyc.NA && !(redirect.flush || redirect.ldRBK && redirect.flushedLid(iexe.lid) || redirect.brRBK && redirect.flushedBid(iexe.bid))
    }else{
      validReg(i) := Mux(redirect.flush || redirect.ldRBK && redirect.flushedLid(instCXSave.lid) || redirect.brRBK && redirect.flushedBid(instCXSave.bid), false.B, validReg(i-1))
    }
  }
  done := validReg(6)

  when(iexe.valid & iexe.exeCyc === ExeCyc.c2) {
    instC2Save := iexe
  }.otherwise {
    instC2Save := 0.U.asTypeOf(new MicroOp)
  }

  when(iexe.valid && iexe.exeCyc === ExeCyc.NA && (redirect.flush || redirect.ldRBK && redirect.flushedLid(iexe.lid) || redirect.brRBK && redirect.flushedBid(iexe.bid))) {
    instCXSave := 0.U.asTypeOf(new MicroOp)
  }.elsewhen(iexe.valid & iexe.exeCyc === ExeCyc.NA) {
    instCXSave := iexe
  }.elsewhen(redirect.flush || redirect.ldRBK && redirect.flushedLid(instCXSave.lid) || redirect.brRBK && redirect.flushedBid(instCXSave.bid)) {
    instCXSave := 0.U.asTypeOf(new MicroOp)
  }.elsewhen(done) {
    instCXSave := 0.U.asTypeOf(new MicroOp)
  }

  for(i <- 0 until 3) {
    if(i == 0) {
      when(done && !redirect.flush && !(redirect.ldRBK && redirect.flushedLid(instCXSave.lid)) && !(redirect.brRBK && redirect.flushedBid(instCXSave.bid))){
        instCXPipe(i) := instCXSave
      }.otherwise{
        instCXPipe(i) := 0.U.asTypeOf(new MicroOp)
      }
    } else {
      when(!redirect.flush && !(redirect.ldRBK && redirect.flushedLid(instCXPipe(i-1).lid)) && !(redirect.brRBK && redirect.flushedBid(instCXPipe(i-1).bid))){
        instCXPipe(i) := instCXPipe(i-1)
      }.otherwise{
        instCXPipe(i) := 0.U.asTypeOf(new MicroOp)
      }
    }
  }

  val c1Out = Wire(new MicroOp)
  val c2Out = Wire(new MicroOp)
  val cxOut = Wire(new MicroOp)

  c1Out := Mux(iexe.valid & iexe.exeCyc === ExeCyc.c1, iexe, 0.U.asTypeOf(new MicroOp))
  c1Out.dstData := resultC1

  c2Out := instC2Save
  c2Out.dstData := resultC2

  cxOut := instCXPipe(2)
  cxOut.dstData := resultCX

  iwb := iexe
  iwb.valid := c1Out.valid || c2Out.valid || cxOut.valid
  iwb.dstVld :=
    Mux(c1Out.valid, c1Out.dstVld, false.B) ||
    Mux(c2Out.valid, c2Out.dstVld, false.B) ||
    Mux(cxOut.valid, cxOut.dstVld, false.B)
  iwb.pReg :=
    Mux(c1Out.valid, c1Out.pReg, 0.U) |
    Mux(c2Out.valid, c2Out.pReg, 0.U) |
    Mux(cxOut.valid, cxOut.pReg, 0.U)
  iwb.iid :=
    Mux(c1Out.valid, c1Out.iid, 0.U) |
    Mux(c2Out.valid, c2Out.iid, 0.U) |
    Mux(cxOut.valid, cxOut.iid, 0.U)
  iwb.dstData :=
    Mux(c1Out.valid, c1Out.dstData, 0.U) |
    Mux(c2Out.valid, c2Out.dstData, 0.U) |
    Mux(cxOut.valid, cxOut.dstData, 0.U)

  if(EnableDifftest){
    val difftest = Module(new DifftestIntWriteback)
    difftest.io.clock := clock
    difftest.io.valid := iwb.valid
    difftest.io.dest  := iwb.pReg
    difftest.io.data  := iwb.dstData
  }
}

