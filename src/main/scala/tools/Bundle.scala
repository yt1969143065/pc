package tools

import chisel3._
import chisel3.util._
import tools.BitType._

trait CoreParameters {
  val EnableDifftest = true
  //configurable
  val DecQNum = 32
  val PRegNum = 128 
  val IidNum = 128 
  val BidNum = 32
  val LidNum = 32
  val SidNum = 32
  val numEntriesEachInst = 8 

  //not configurable
  val VAddrWidth = 39
  val PoolInNum  = 8   //pool input number, 8 dispatch in, not configurable
  val PoolOutNum = 4   //pool output number, 4 issue out,  not configurable
  val RFReadNum  = 4   //regfile read number, not configurable
  val WbNum      = 8   //wb number, not configurable
  val LdNum      = 4 
  val StNum      = 4 
  val ExcpWidth  = 4 

  val NumEntries = numEntriesEachInst * PoolInNum 
  val NumEntriesIn = numEntriesEachInst * 2 
  val NumEntriesOut = numEntriesEachInst * 4 
  val DecQWidth = log2Up(DecQNum)
  val PidWidth = log2Up(NumEntries)
  val PRegWidth = log2Up(PRegNum)
  val IidWidth = log2Up(IidNum)
  val BidWidth = log2Up(BidNum) 
  val LidWidth = log2Up(LidNum) 
  val SidWidth = log2Up(SidNum) 

  //PMA
  val FlashBase = 0x10000000L
  val FlashMask =  0xfffffffL
  val FlashIdxWidth = log2Up(FlashMask) 
  val RAMBase = 0x80000000L
  val RAMMask = 0xfffffffL 
  val RAMIdxWidth = log2Up(RAMMask)

  def inFlasRange(addr:UInt) : Bool = {
    addr >= FlashBase.U &&
    addr <= FlashBase.U + FlashMask.U
  }
  def inRamRange(addr:UInt) : Bool = {
    addr >= RAMBase.U &&
    addr <= (RAMBase.U + RAMMask.U) &&
    !inFlasRange(addr)
  }

}

class MicroOp extends Bundle with CoreParameters{
  val valid     = Bool()
  val pc        = UInt(VAddrWidth.W)
  val inst      = UInt(32.W)

  val excpVld   = Bool()
  val excpCode  = UInt(ExcpWidth.W)

  val src1Type  = UInt((SrcType.rs.getWidth ).W)
  val src1Reg   = UInt(5.W)
  val src2Type  = UInt((SrcType.rs.getWidth ).W)
  val src2Reg   = UInt(5.W)
  val src3Type  = UInt((SrcType.rs.getWidth ).W)
  val dstType   = UInt((DstType.rd.getWidth  ).W)
  val dstReg    = UInt(5.W)
  val imm       = UInt(64.W)

  val src1x0   = Bool()
  val src2x0   = Bool()
  val dstx0    = Bool()

  val dstVld    = Bool()

  val toAl      = Bool() //AluPool
  val toMd      = Bool() //MduPool
  val fuOp      = FuOpType()
  val isBr      = Bool()
  val isJ       = Bool()
  val isJr      = Bool()
  val isLd      = Bool()
  val isSt      = Bool()
  val isCsr     = Bool()
  val isMret    = Bool()
  val isFencei  = Bool()
  val isFence   = Bool()
  val isEcall   = Bool()
  val exeCyc    = UInt((ExeCyc.c1.getWidth).W)   //3 uncertain
  val wbVld     = Bool()

  val iid       = UInt(IidWidth.W)
  val bid       = UInt(BidWidth.W)
  val lid       = UInt(LidWidth.W)
  val sid       = UInt(SidWidth.W)
  val pid       = UInt(PidWidth.W)
  val pReg      = UInt(PRegWidth.W)

  val oldPReg   = UInt(PRegWidth.W)
  val dstRdy    = Bool() //oldDstRdy

  val preTarget = UInt(VAddrWidth.W)
  val bakTarget = UInt(VAddrWidth.W)
  val preDir    = Bool()
  val tarRight  = Bool()
  val dirRight  = Bool()
  //val wbVld     = Bool()

  val src1PReg  = UInt(PRegWidth.W)
  val src2PReg  = UInt(PRegWidth.W)
  val src1Rdy   = Bool()
  val src2Rdy   = Bool()

  val src1Data  = UInt(64.W)
  val src2Data  = UInt(64.W)
  val src3Data  = UInt(64.W)
  val dstData   = UInt(64.W)

  val brTaken   = Bool()

  val bidVec    = UInt(BidNum.W)
  val lidVec    = UInt(LidNum.W)

  val flush     = Bool()

}

class AxiABundle extends Bundle with CoreParameters{
  val id    = UInt(8.W)
  val addr  = UInt(VAddrWidth.W)
  val len   = UInt(8.W) //0, 1-time
  val size  = UInt(3.W) //101, 32B
  val burst = UInt(2.W) //01, INCR
  val lock  = Bool()
  val cache = UInt(4.W) 
  val prot  = UInt(3.W)
  val qos   = UInt(4.W)
}
class AxiArBundle extends AxiABundle
class AxiRBundle  extends Bundle with CoreParameters{
  val id     = UInt(8.W)
  val data   = UInt(256.W)
  val resp   = UInt(2.W)
  val last   = Bool()
}
class AxiAwBundle extends AxiABundle
class AxiWBundle extends Bundle with CoreParameters{
  val data   = UInt(256.W)
  val strb   = UInt(32.W)
  val last   = Bool()
}
class AxiBBundle extends Bundle with CoreParameters{
  val id     = UInt(8.W)
  val resp   = UInt(2.W)
}

class AXI4 extends Bundle{
  val ar =         DecoupledIO(new(AxiArBundle)) 
  val  r = Flipped(DecoupledIO(new(AxiRBundle))) 
  val aw =         DecoupledIO(new(AxiAwBundle))
  val  w =         DecoupledIO(new(AxiWBundle))
  val  b = Flipped(DecoupledIO(new(AxiBBundle))) 
}

class Redirect extends Bundle with CoreParameters {
  val flush = Bool()
  val brRBK = Bool()
  val flushedBid = UInt(BidNum.W)
  val ldRBK = Bool()
  val flushedLid = UInt(LidNum.W)
  val rbkLid = UInt(LidWidth.W)
}

class LdReqBundle extends Bundle with CoreParameters{
  val valid    = Bool()
  val lid      = UInt(LidWidth.W)
  val pid      = UInt(PidWidth.W)
  val op       = FuOpType()
  val addr     = UInt(64.W)
  val mask     = UInt(16.W)
}
class LdRspBundle extends Bundle with CoreParameters{
  val valid    = Bool()
  val pid      = UInt(PidWidth.W)
  val data     = UInt(64.W)
}
class StReqBundle extends Bundle with CoreParameters{
  val valid    = Bool()
  val sid      = UInt(SidWidth.W)
  val lid      = UInt(LidWidth.W)
  val iid      = UInt(IidWidth.W)
  val op       = FuOpType()
  val addr     = UInt(64.W)
  val mask     = UInt(16.W)
  val data     = UInt(64.W)
}
class StRspBundle extends Bundle with CoreParameters{
  val valid    = Bool()
  val iid      = UInt(IidWidth.W)
}

class CtrlSigs extends Bundle {
  val legal    = Bool()
  val toAl     = Bool()
  val toMd     = Bool()
  val src1Type = UInt((SrcType.rs.getWidth ).W)
  val src2Type = UInt((SrcType.rs.getWidth ).W)
  val src3Type = UInt((SrcType.rs.getWidth ).W)
  val dstType  = UInt((DstType.rd.getWidth  ).W)
  val immType  = UInt((ImmType.immI.getWidth).W)
  val fuOp     = FuOpType()
  val isBr     = Bool()
  val isJ      = Bool()
  val isJr     = Bool()
  val isLd     = Bool()
  val isSt     = Bool()
  val isCsr    = Bool()
  val isMret   = Bool()
  val isFencei = Bool()
  val exeCyc   = UInt((ExeCyc.c1.getWidth).W)

  def default : List[BitPat] =
  //   leagal
  //   |  toAl
  //   |  |  toMd
  //   |  |  |    src1Type
  //   |  |  |    |           src2Type
  //   |  |  |    |           |                           DstType
  //   |  |  |    |           |                           |             ImmType
  //   |  |  |    |           |                           |             |           FuOpType
  //   |  |  |    |           |               src3Type    |             |              |           isBr         
  //   |  |  |    |           |               |           |             |              |           |  isJ       
  //   |  |  |    |           |               |           |             |              |           |  |  isJr    
  //   |  |  |    |           |               |           |             |              |           |  |  |  isLd  
  //   |  |  |    |           |               |           |             |              |           |  |  |  |  isSt
  //   |  |  |    |           |               |           |             |              |           |  |  |  |  |  isCsr  
  //   |  |  |    |           |               |           |             |              |           |  |  |  |  |  |  isMret 
  //   |  |  |    |           |               |           |             |              |           |  |  |  |  |  |  |  isFencei 
  //   |  |  |    |           |               |           |             |              |           |  |  |  |  |  |  |  |     
  //   |  |  |    |           |               |           |             |              |           |  |  |  |  |  |  |  |     
  List(N, N, Y, SrcType.None, SrcType.None, SrcType.None, DstType.None, ImmType.DC, AluOpType.sll, X, X, X, X, X, X, X, X, ExeCyc.c1)
  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) : CtrlSigs = {
    //val decoder = chisel3.util.experimental.decode.decoder(inst, TruthTable(table, default))
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(
                   legal    ,
                   toAl     ,
                   toMd     ,
                   src1Type ,
                   src2Type ,
                   src3Type ,
                   dstType  ,
                   immType  ,
                   fuOp     ,
                   isBr     ,
                   isJ      ,
                   isJr     ,
                   isLd     ,
                   isSt     ,
                   isCsr    ,
                   isMret   ,
                   isFencei ,
                   exeCyc
    )
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class ExcpBundle extends Bundle{
  val valid = Bool()
  val ecode = UInt(6.W)
  val epc   = UInt(64.W)
  val tval  = UInt(64.W)
}

class CSRInfoBundle extends Bundle{
  val mtvec = UInt(64.W)
  val mepc  = UInt(64.W)
}

