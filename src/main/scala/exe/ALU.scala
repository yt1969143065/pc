package exe 

import chisel3._
import chisel3.util._

import tools._
import difftest._

class ALU extends Module with CoreParameters{
  val io = IO(new Bundle {
    val iexe  = Input(new MicroOp)
    val iwb = Output(new MicroOp)
    val redirect = Input(new Redirect)
  })  
  //input
  val iexe = io.iexe
  val redirect = io.redirect
  //output
  val iwb = RegInit(0.U.asTypeOf(new MicroOp))
  io.iwb := iwb 
  io.iwb.valid := Mux(redirect.flush || redirect.ldRBK && redirect.flushedLid(iwb.lid) || redirect.brRBK && redirect.flushedBid(iwb.bid), false.B, iwb.valid)

  val resultMOVE = iexe.src2Data
  val resultXOR  = iexe.src1Data ^ iexe.src2Data
  val resultOR   = iexe.src1Data | iexe.src2Data
  val resultAND  = iexe.src1Data & iexe.src2Data
  val resultADD  = iexe.src1Data + iexe.src2Data
  val resultADDW = Cat(Fill(32, resultADD(31)), resultADD(31, 0)) 
  val resultSUB  = iexe.src1Data - iexe.src2Data
  val resultSUBW = Cat(Fill(32, resultSUB(31)), resultSUB(31, 0)) 
  val resultSLT  = Mux(resultSUB(63), 1.U(64.W), 0.U(64.W)) 
  val sltuTmp    = Cat("b0".U, iexe.src1Data) - Cat("b0".U, iexe.src2Data)
  val resultSLTU = Mux(sltuTmp(64), 1.U(64.W), 0.U(64.W))
  val resultSLL  = (iexe.src1Data << iexe.src2Data(5,0))(63, 0)
  val sllwTmp    = (iexe.src1Data << iexe.src2Data(4,0))(63, 0)  
  val resultSLLW = Cat(Fill(32, sllwTmp(31)), sllwTmp(31, 0)) 
  val resultSRL  = iexe.src1Data >> iexe.src2Data(5, 0)  
  val srlwTmp    = iexe.src1Data(31, 0) >> iexe.src2Data(4, 0)
  val resultSRLW = Cat(Fill(32, srlwTmp(31)), srlwTmp(31, 0)) 
  val resultSRA  = (iexe.src1Data.asSInt >> iexe.src2Data(5,0)).asUInt
  val resultSRAW = Cat(Fill(32, iexe.src1Data(31)), iexe.src1Data(31,0).asSInt >>  iexe.src2Data(4,0))

  val result = Wire(UInt(64.W))
  result :=
    Mux(iexe.fuOp === AluOpType.move, resultMOVE, 0.U) | 
    Mux(iexe.fuOp === AluOpType.xor , resultXOR , 0.U) | 
    Mux(iexe.fuOp === AluOpType.or  , resultOR  , 0.U) | 
    Mux(iexe.fuOp === AluOpType.and , resultAND , 0.U) |
    Mux(iexe.fuOp === AluOpType.add , resultADD , 0.U) |
    Mux(iexe.fuOp === AluOpType.addw, resultADDW, 0.U) |
    Mux(iexe.fuOp === AluOpType.sub , resultSUB , 0.U) |
    Mux(iexe.fuOp === AluOpType.subw, resultSUBW, 0.U) |
    Mux(iexe.fuOp === AluOpType.slt , resultSLT , 0.U) |
    Mux(iexe.fuOp === AluOpType.sltu, resultSLTU, 0.U) |
    Mux(iexe.fuOp === AluOpType.sll , resultSLL , 0.U) |
    Mux(iexe.fuOp === AluOpType.sllw, resultSLLW, 0.U) |
    Mux(iexe.fuOp === AluOpType.srl , resultSRL , 0.U) |
    Mux(iexe.fuOp === AluOpType.srlw, resultSRLW, 0.U) |
    Mux(iexe.fuOp === AluOpType.sra , resultSRA , 0.U) |
    Mux(iexe.fuOp === AluOpType.sraw, resultSRAW, 0.U)  
    
  
  iwb := DontCare
  iwb.valid   := iexe.valid
  iwb.dstVld  := iexe.dstVld
  iwb.pReg    := iexe.pReg
  iwb.iid     := iexe.iid
  iwb.lid     := iexe.lid
  iwb.bid     := iexe.bid
  iwb.dstData := result

  if(EnableDifftest){
    val difftest = Module(new DifftestIntWriteback)
    difftest.io.clock := clock
    difftest.io.valid := iwb.valid
    difftest.io.dest  := iwb.pReg
    difftest.io.data  := iwb.dstData
  }
}

