package scheduler

import chisel3._
import chisel3.util._

import tools._
import pools._
import rmu._


class Scheduler extends Module with CoreParameters {
  val io = IO(new Bundle {
    val stall       = Output(Bool())
    val iallocate   = Input (Vec(PoolInNum, new MicroOp))

    val aluPoolPoor = Input (Bool())
    val aluPidIn    = Input (Vec(PoolInNum, UInt(PidWidth.W)))
    val mduPoolPoor = Input (Bool())
    val mduPidIn    = Input (Vec(PoolInNum, UInt(PidWidth.W)))
    val ienpool     = Output(Vec(PoolInNum, new MicroOp))

    val rmuStall    = Input (Bool())
    val freePReg    = Input (Vec(PoolInNum, UInt(PRegWidth.W)))
    val allocate    = Output(Vec(PoolInNum, new MicroOp))
    val srcRdReg0   = Output(Vec(2, Vec(PoolInNum, UInt(5.W))))    
    val srcRdPReg0  = Input (Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
    val srcRdRdy0   = Input (Vec(2, Vec(PoolInNum, Bool())))
    val dstRdReg    = Output(Vec(PoolInNum, UInt(5.W)))
    val dstRdPReg   = Input (Vec(PoolInNum, UInt(PRegWidth.W)))
    val dstRdRdy    = Input (Vec(PoolInNum, Bool())) 
    val srcRdReg    = Output(Vec(2, Vec(PoolInNum, UInt(5.W))))    
    val srcRdPReg   = Input (Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
    val srcRdRdy    = Input (Vec(2, Vec(PoolInNum, Bool())))

    val exWb        = Input (Vec(WbNum, new MicroOp))
    val brEx        = Input (Vec(4,     new MicroOp))
    val jrEx        = Input (Vec(4,     new MicroOp))
    val stWb        = Input (new MicroOp) 
    //val ldExcpWb    = Input (Vec(4,     new MicroOp))
    //val stExcpWb    = Input (Vec(4,     new MicroOp))
    val mduExcpWb   = Input (Vec(4,     new MicroOp)) //instruction illeagal

    val ldRbk       = Input (new MicroOp)
    val redirect    = Output(new Redirect)
    val pcChange    = Output(new MicroOp)

    val lidRls      = Input (UInt(PoolInNum.W))
    val sidRls      = Input (UInt(PoolInNum.W))
    val iidRls      = Output(Vec(PoolInNum, new MicroOp))

    val bidSafe     = Output(UInt(BidNum.W))
    
    val csrRdEn     = Output(Bool())
    val csrRdAddr   = Output(UInt(12.W))
    val csrRdIllegal = Input(Bool())
    val csrRdData   = Input(UInt(64.W))

    val csrinfo    = Input(new CSRInfoBundle)

    val envRegRdAddr = if(EnableDifftest) Some(Output(Vec(32, UInt(PRegWidth.W)))) else None
    val envRegRdData = if(EnableDifftest) Some(Input (Vec(32, UInt(64.W)))) else None

    val ldQtailPtr = Output(UInt(LidWidth.W))
    val stQtailPtr = Output(UInt(SidWidth.W))

    val lidRlsVec = Output(UInt(LidNum.W))
    val bidRlsVec = Output(UInt(BidNum.W))

    val excp        = Output(new ExcpBundle)

    val bpUpVld     = Output(UInt(8.W)) 
    val bpUpDir     = Output(UInt(8.W))
  })  

 //input
  val iallocate = io.iallocate
  val aluPoolPoor = io.aluPoolPoor
  val aluPidIn = io.aluPidIn
  val mduPoolPoor = io.mduPoolPoor
  val mduPidIn = io.mduPidIn
  val rmuStall = io.rmuStall
  val freePReg = io.freePReg
  val dstRdPReg = io.dstRdPReg
  val dstRdRdy = io.dstRdRdy
  val srcRdPReg0 = io.srcRdPReg0
  val srcRdPReg = io.srcRdPReg
  val srcRdRdy0 = io.srcRdRdy0
  val srcRdRdy = io.srcRdRdy
  val exWb = io.exWb
  val brEx = io.brEx
  val jrEx = io.jrEx
  val stWb = io.stWb
  //val ldExcpWb = io.ldExcpWb
  //val stExcpWb = io.stExcpWb
  val mduExcpWb = io.mduExcpWb
  val ldRbk = io.ldRbk
  val lidRls = io.lidRls
  val sidRls = io.sidRls
  val csrRdIllegal = io.csrRdIllegal
  val csrRdData = io.csrRdData
  val csrinfo = io.csrinfo
  //output
  val stall = Wire(Bool())
  val ienpool = Wire(Vec(PoolInNum, new MicroOp))
  val allocate = Wire(Vec(PoolInNum, new MicroOp))
  val dstRdReg = Wire(Vec(PoolInNum, UInt(5.W)))
  val srcRdReg0 = Wire(Vec(2, Vec(PoolInNum, UInt(5.W))))
  val srcRdReg = Wire(Vec(2, Vec(PoolInNum, UInt(5.W))))
  val redirect = Wire(new Redirect)
  val pcChange = Wire(new MicroOp)
  val iidRls   = Wire(Vec(PoolInNum, new MicroOp))
  val bidSafe = Wire(UInt(BidNum.W))
  val csrRdEn = RegInit(false.B)
  val csrRdAddr = Reg(UInt(12.W))
  io.stall := stall
  io.ienpool := ienpool
  io.allocate := allocate
  io.dstRdReg := dstRdReg
  io.srcRdReg0 := srcRdReg0
  io.srcRdReg := srcRdReg
  io.redirect := redirect
  io.pcChange := pcChange
  io.iidRls := iidRls
  io.bidSafe := bidSafe
  io.csrRdEn := csrRdEn
  io.csrRdAddr := csrRdAddr

  val mIid = Module (new Iid)
  val mBid = Module (new Bid)
  val mLid = Module (new Lid)
  val mSid = Module (new Sid)

  val enPool = RegInit(VecInit.fill(PoolInNum)(0.U.asTypeOf(new MicroOp)))
  val stall1 = Wire(Bool())

  //Iid
  mIid.io.allocate := allocate
  mIid.io.redirect := redirect
  mIid.io.iidWb := exWb ++ mBid.io.brWb :+ stWb
  //mIid.io.iidExcp := ldExcpWb ++ stExcpWb ++ mduExcpWb 
  mIid.io.iidExcp := mduExcpWb
  mIid.io.envRegRdData.get := io.envRegRdData.get
  io.envRegRdAddr.get := mIid.io.envRegRdAddr.get
  //Bid
  mBid.io.allocate := allocate
  mBid.io.brEx := brEx
  mBid.io.jrEx := jrEx
  mBid.io.flush := mIid.io.flush
  mBid.io.ldRbk := ldRbk
  mBid.io.lidSafe := mLid.io.lidSafe
  //Lid
  mLid.io.stall := stall
  mLid.io.allocate := allocate
  mLid.io.release := lidRls
  //Sid
  mSid.io.stall := stall
  mSid.io.allocate := allocate
  mSid.io.release := sidRls

  val partialGoR = RegInit(false.B)
 val validPartial = RegInit(0.U(PoolInNum.W))

  val partialGo = Wire(Bool())
  val validFinal = Wire(UInt(PoolInNum.W))
  val validMretFinal = Wire(UInt(PoolInNum.W))
  val validEcallFinal = Wire(UInt(PoolInNum.W))
  val validFenceiFinal = Wire(UInt(PoolInNum.W))
  val validFenceFinal = Wire(UInt(PoolInNum.W))
  val validCsrFinal = Wire(UInt(PoolInNum.W))
  val brInRange  = Wire(UInt(PoolInNum.W))
  val ldInRange  = Wire(UInt(PoolInNum.W))
  val stInRange  = Wire(UInt(PoolInNum.W))
  val dstDiff    = Wire(UInt(PoolInNum.W))
  val dstDiffVec = Wire(Vec(PoolInNum, Bool())) //must use Vec
  val validCur = Mux(pcChange.valid, 0.U, Mux(partialGoR, validPartial, VecInit(iallocate.map(_.valid)).asUInt))
  val isBr       = Wire(UInt(PoolInNum.W))
  val isLd       = Wire(UInt(PoolInNum.W))
  val isSt       = Wire(UInt(PoolInNum.W))

  isBr := VecInit((0 until PoolInNum).map(i => validCur(i) && (iallocate(i).isBr || iallocate(i).isJr || iallocate(i).isJ))).asUInt
  isLd := VecInit((0 until PoolInNum).map(i => validCur(i) && iallocate(i).isLd)).asUInt
  isSt := VecInit((0 until PoolInNum).map(i => validCur(i) && iallocate(i).isSt)).asUInt
  val isCsr = VecInit((0 until PoolInNum).map(i => validCur(i) && iallocate(i).isCsr)).asUInt
  val isMret = VecInit((0 until PoolInNum).map(i => validCur(i) && iallocate(i).isMret)).asUInt
  val isEcall = VecInit((0 until PoolInNum).map(i => validCur(i) && iallocate(i).isEcall)).asUInt
  val isFencei = VecInit((0 until PoolInNum).map(i => validCur(i) && iallocate(i).isFencei)).asUInt
  val isFence  = VecInit((0 until PoolInNum).map(i => validCur(i) && iallocate(i).isFence )).asUInt

  brInRange := VecInit((0 until PoolInNum).map(i => PopCount(isBr(i, 0)) < 5.U)).asUInt
  ldInRange := VecInit((0 until PoolInNum).map(i => PopCount(isLd(i, 0)) < 5.U)).asUInt
  stInRange := VecInit((0 until PoolInNum).map(i => PopCount(isSt(i, 0)) < 5.U)).asUInt


  dstDiff   := dstDiffVec.asUInt
  for(i <- 0 until PoolInNum) {
    if(i == 0) {
      dstDiffVec(i) := true.B
    } else {
      dstDiffVec(i) := dstDiffVec(i-1) && !(0 until i).map(j =>
        iallocate(i).dstReg === iallocate(j).dstReg && iallocate(i).dstType === iallocate(j).dstType && iallocate(j).dstType === DstType.rd && validCur(j) && !iallocate(i).dstx0).reduce(_ || _)
    }
  }

  validMretFinal   := VecInit((0 until PoolInNum).map(i => if(i==0) true.B else PopCount(validCur(i-1, 0)) < 1.U)).asUInt & Mux(mIid.io.free,                 isMret,   0.U)
  validEcallFinal   := VecInit((0 until PoolInNum).map(i => if(i==0) true.B else PopCount(validCur(i-1, 0)) < 1.U)).asUInt & Mux(mIid.io.free,                 isEcall,   0.U)
  validFenceiFinal := VecInit((0 until PoolInNum).map(i => if(i==0) true.B else PopCount(validCur(i-1, 0)) < 1.U)).asUInt & Mux(mIid.io.free && mSid.io.free, isFencei, 0.U)
  validFenceFinal  := VecInit((0 until PoolInNum).map(i => if(i==0) true.B else PopCount(validCur(i-1, 0)) < 1.U)).asUInt & Mux(mIid.io.free && mSid.io.free, isFence,  0.U)
  validCsrFinal    := VecInit((0 until PoolInNum).map(i => if(i==0) true.B else PopCount(validCur(i-1, 0)) < 1.U)).asUInt & Mux(mIid.io.free,                 isCsr,    0.U)
  val fenceMask = VecInit((0 until 8).map(i => Mux(isFence(i), "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  val csrMask   = VecInit((0 until 8).map(i => Mux(isCsr(i),   "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  validFinal := Mux(stall1, 0.U, validCur & brInRange & ldInRange & stInRange & dstDiff & csrMask & fenceMask & (~isCsr) & (~isMret) & (~isEcall) & (~isFencei) & (~isFence) | validCsrFinal | validMretFinal | validFenceiFinal | validFenceFinal | validEcallFinal)
  validPartial := validCur & ~validFinal
  partialGo  := validFinal =/= validCur
  partialGoR := partialGo


  val pcChangeByMret = RegInit(0.U.asTypeOf(new MicroOp))
  pcChangeByMret := DontCare
  pcChangeByMret.valid := validMretFinal.orR
  pcChangeByMret.pc := csrinfo.mepc
  val pcChangeByEcall = RegInit(0.U.asTypeOf(new MicroOp))
  pcChangeByEcall := DontCare
  pcChangeByEcall.valid := validEcallFinal.orR
  pcChangeByEcall.pc := Cat(csrinfo.mtvec(63, 2), 0.U(2.W))
  val pcChangeByFencei = RegInit(0.U.asTypeOf(new MicroOp))
  pcChangeByFencei := DontCare
  pcChangeByFencei.valid := validFenceiFinal.orR
  pcChangeByFencei.pc := 0.U   //fencei pc + 4

  stall := partialGo |
           rmuStall |   //pRegPoor + freelistStall
           aluPoolPoor |
           mduPoolPoor |
           mIid.io.poor |
           mBid.io.poor |
           mLid.io.poor |
           mSid.io.poor
  stall1 :=
           rmuStall |   //pRegPoor + freelistStall
           aluPoolPoor |
           mduPoolPoor |
           mIid.io.poor |
           mBid.io.poor |
           mLid.io.poor |
           mSid.io.poor
 for(i <- 0 until PoolInNum) {
    allocate(i)           := iallocate(i)
    allocate(i).valid     := Mux(redirect.flush || redirect.ldRBK || redirect.brRBK, false.B, validFinal(i))
    allocate(i).iid       := mIid.io.iidOut(i)
    allocate(i).bid       := mBid.io.bidOut(i)
    allocate(i).lid       := mLid.io.lidOut(i)
    allocate(i).sid       := mSid.io.sidOut(i)
    allocate(i).pid       := Mux(iallocate(i).toAl, aluPidIn(i), mduPidIn(i))
    allocate(i).pReg      := Mux(iallocate(i).dstx0, 0.U, freePReg(i))
    allocate(i).oldPReg   := dstRdPReg(i)
    allocate(i).dstRdy    := dstRdRdy(i)
    allocate(i).src1PReg  := srcRdPReg0(0)(i)
    allocate(i).src2PReg  := srcRdPReg0(1)(i)
    allocate(i).src1Rdy   := srcRdRdy0(0)(i)
    allocate(i).src2Rdy   := srcRdRdy0(1)(i)
  }

  //csr read
  val csrRdPtrOH = RegInit(0.U(PoolInNum.W))
  csrRdPtrOH := PriorityEncoderOH(isCsr)
  csrRdEn   := validCsrFinal.orR && Mux1H(validCsrFinal, iallocate.map(i => !(i.fuOp === CsrOpType.wr && i.dstx0 || i.fuOp === CsrOpType.wri && i.dstx0 )))
  csrRdAddr := PriorityMux(isCsr, iallocate.map(i => i.imm(11, 0)))

  enPool := allocate
  val src1PRegOld = RegInit(VecInit.fill(8)(false.B))
  val src2PRegOld = RegInit(VecInit.fill(8)(false.B))
  val src1EqDst = Wire(Vec(8, UInt(8.W)))
  val src2EqDst = Wire(Vec(8, UInt(8.W)))
  for(i <- 0 until 8){
    src1EqDst(i) := VecInit((0 until 8).map(j =>
      allocate(i).src1Type === SrcType.rs &&
      allocate(i).src1Reg === allocate(j).dstReg &&
      allocate(j).valid &&
      allocate(j).dstType === DstType.rd &&
     !allocate(j).dstx0
    )).asUInt
    src2EqDst(i) := VecInit((0 until 8).map(j =>
      allocate(i).src2Type === SrcType.rs &&
      allocate(i).src2Reg === allocate(j).dstReg &&
      allocate(j).valid &&
      allocate(j).dstType === DstType.rd &&
     !allocate(j).dstx0
    )).asUInt
    src1PRegOld(i) := src1EqDst(i)(7, i).orR
    src2PRegOld(i) := src2EqDst(i)(7, i).orR
  }

  for(i <- 0 until PoolInNum) {
    ienpool(i) := enPool(i)
    ienpool(i).src1PReg := Mux(src1PRegOld(i), enPool(i).src1PReg, srcRdPReg(0)(i))
    ienpool(i).src2PReg := Mux(src2PRegOld(i), enPool(i).src2PReg, srcRdPReg(1)(i))
    ienpool(i).src1Rdy  := Mux(src1PRegOld(i), enPool(i).src1Rdy, srcRdRdy(0)(i))
    ienpool(i).src2Rdy  := Mux(src2PRegOld(i), enPool(i).src2Rdy, srcRdRdy(1)(i))
    ienpool(i).src2Data := Mux(csrRdEn && csrRdPtrOH(i), csrRdData, enPool(i).src2Data)
    ienpool(i).excpVld  := Mux(csrRdEn && csrRdPtrOH(i), enPool(i).excpVld || csrRdIllegal, enPool(i).excpVld)
  }

  srcRdReg0(0) := iallocate.map(_.src1Reg)
  srcRdReg0(1) := iallocate.map(_.src2Reg)
  dstRdReg     := iallocate.map(_.dstReg)
  srcRdReg(0)  := enPool.map(_.src1Reg)
  srcRdReg(1)  := enPool.map(_.src2Reg)

  redirect := mBid.io.redirect
  pcChange := mBid.io.pcChange
  pcChange.valid := mBid.io.pcChange.valid || pcChangeByMret.valid || pcChangeByFencei.valid || pcChangeByEcall.valid
  pcChange.fuOp    :=
    Mux(pcChangeByFencei.valid, 1.U, 0.U)
  pcChange.pc    :=
    Mux(mBid.io.pcChange.valid, mBid.io.pcChange.pc, 0.U) |
    Mux(pcChangeByMret.valid,   pcChangeByMret.pc,   0.U) |
    Mux(pcChangeByEcall.valid,  pcChangeByEcall.pc,  0.U) |
    Mux(pcChangeByFencei.valid, pcChangeByFencei.pc, 0.U)


  iidRls  := mIid.io.release
  bidSafe := mBid.io.bidSafe

  io.ldQtailPtr := mLid.io.ldQtailPtr
  io.stQtailPtr := mSid.io.stQtailPtr
  io.lidRlsVec := mLid.io.lidRlsVec
  io.bidRlsVec := mBid.io.bidRlsVec

  io.excp := mIid.io.excp

  io.bpUpVld := mBid.io.bpUpVld
  io.bpUpDir := mBid.io.bpUpDir
}
