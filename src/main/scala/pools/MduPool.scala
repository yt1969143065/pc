package pools

import chisel3._
import chisel3.util._

import tools._


//mdu + store + csr
class MduPool extends Module with CoreParameters{
  val io = IO(new Bundle {
    val redirect     = Input (new Redirect) 
    val poor         = Output(Bool()) 
    val pidOut       = Output(Vec(PoolInNum,  UInt(PidWidth.W) ))
    val allocate     = Input (Vec(PoolInNum,  new MicroOp         ))
    val ienpool      = Input (Vec(PoolInNum,  new MicroOp         ))
    val regRdReq     = Output(Vec(RFReadNum,  new MicroOp))
    val regRdRsp     = Input (Vec(RFReadNum,  UInt(64.W)       ))
    val exeDone      = Input (Vec(PoolOutNum, Bool()           ))
    val iexe         = Output(Vec(PoolOutNum, new MicroOp      ))  
    val wakeOut      = Output(Vec(PoolOutNum, new MicroOp      ))  
    val wakeIn       = Input (Vec(WbNum,      new MicroOp      ))  
    val dataIn       = Input (Vec(WbNum,      UInt(64.W)       ))
    val wakeReq      = Input (Vec(WbNum,      new MicroOp      ))  
    val wakeData     = Input (Vec(WbNum,      UInt(64.W)       ))
    val rdySt        = Input (Vec(PoolOutNum, Bool()           ))
    val outSt        = Output(Vec(PoolOutNum, new StReqBundle  ))  
    val iexeJr       = Output(Vec(PoolOutNum, new MicroOp      ))  

    val csrWrEn      = Output(Bool())
    val csrWrCmd     = Output(UInt(2.W))
    val csrWrAddr    = Output(UInt(12.W))
    val csrWrData    = Output(UInt(64.W))
    val csrWrIllegal = Input(Bool())

  })  

  //input port
  val allocate = io.allocate
  val ienpool = io.ienpool
  val regRdRsp = io.regRdRsp
  val redirect = io.redirect
  val rdySt = io.rdySt
  val exeDone = io.exeDone
  val csrWrIllegal = io.csrWrIllegal
  val wakeIn = Wire(Vec(WbNum, new MicroOp))
  val dataIn = Wire(Vec(WbNum, UInt(64.W)))
  //output
  val poor = RegInit(false.B)
  val pidOut = RegInit(VecInit((0 until PoolInNum).map(i => if(i%2 == 1) ((i/2+1)*NumEntriesIn-1).U else (i/2*NumEntriesIn).U)))
  val regRdReq = Wire(Vec(RFReadNum,  new MicroOp))
  val iexe = Wire(Vec(PoolOutNum, new MicroOp))
  val wakeOut = Wire(Vec(PoolOutNum, new MicroOp))
  val outSt = Wire(Vec(PoolOutNum, new StReqBundle))
  val iexeJr = Wire(Vec(PoolOutNum, new MicroOp))
  val csrWrEn   = Wire(Bool())
  val csrWrCmd  = Wire(UInt(2.W))
  val csrWrAddr = Wire(UInt(12.W))
  val csrWrData = Wire(UInt(64.W))
  io.poor := poor
  io.pidOut := pidOut
  io.regRdReq := regRdReq
  io.iexe := iexe
  io.wakeOut := wakeOut
  io.outSt := outSt
  io.iexeJr := iexeJr
  io.csrWrEn := csrWrEn
  io.csrWrCmd := csrWrCmd
  io.csrWrAddr := csrWrAddr
  io.csrWrData := csrWrData

  for(i <- 0 until 8){ 
    val wakeInVldD1 = RegInit(false.B)
    val wakeInVldD2 = RegInit(false.B)
    wakeInVldD1 := io.wakeIn(i).valid
    wakeInVldD2 := wakeInVldD1
    wakeIn(i) := Mux(io.wakeIn(i).valid, io.wakeIn(i), io.wakeReq(i))
    dataIn(i) := Mux(wakeInVldD2, io.dataIn(i), io.wakeData(i))
  }

  val validNxt = Wire(Vec(NumEntries, Bool()))

  val valid        = RegInit(VecInit.fill(NumEntries)(false.B))
  val validD1      = RegInit(VecInit.fill(NumEntries)(false.B))
  val isSt         = RegInit(VecInit.fill(NumEntries)(false.B))
  val isCsr        = RegInit(VecInit.fill(NumEntries)(false.B))
  val isJr         = RegInit(VecInit.fill(NumEntries)(false.B))
  val isJ          = RegInit(VecInit.fill(NumEntries)(false.B))
  val op           = Reg    (Vec(NumEntries, FuOpType()))
  val exeCyc       = Reg    (Vec(NumEntries, UInt(2.W)))
  val iid          = Reg    (Vec(NumEntries, UInt(IidWidth.W)))
  val lid          = Reg    (Vec(NumEntries, UInt(LidWidth.W)))
  val sid          = Reg    (Vec(NumEntries, UInt(SidWidth.W)))
  val bid          = Reg    (Vec(NumEntries, UInt(BidWidth.W)))
  val src1PReg     = Reg    (Vec(NumEntries, UInt(PRegWidth.W)))
  val src2PReg     = Reg    (Vec(NumEntries, UInt(PRegWidth.W)))
  val dstVld       = RegInit(VecInit.fill(NumEntries)(false.B))
  val pReg         = Reg    (Vec(NumEntries, UInt(PRegWidth.W)))

  val src1Rdy      = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1DRdy     = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1FwdVld_0 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1FwdVld_1 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1FwdSrc_0 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src1FwdSrc_1 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src1Data     = Reg    (Vec(NumEntries, UInt(64.W)))

  val src2Rdy      = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2DRdy     = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2FwdVld_0 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2FwdVld_1 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2FwdSrc_0 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src2FwdSrc_1 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src2Data     = Reg    (Vec(NumEntries, UInt(64.W)))

  val src3Data     = Reg    (Vec(NumEntries, UInt(64.W)))

  val excpVld      = RegInit(VecInit.fill(NumEntries)(false.B))
  val excpCode     = Reg    (Vec(NumEntries, UInt(ExcpWidth.W)))


  val select       = Wire   (Vec(PoolOutNum, new MicroOp))
  val issueVld     = RegInit(VecInit.fill(PoolOutNum)(false.B))
  val issuePid     = Reg    (Vec(PoolOutNum, UInt(PidWidth.W)))
  val issueDstAddr = Reg    (Vec(PoolOutNum, UInt(PRegWidth.W)))
  val issueExeCyc  = Reg    (Vec(PoolOutNum, UInt(2.W)))
  val issue        = Wire   (Vec(PoolOutNum, new MicroOp))
  val exe1         = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))
  val exe          = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))
  val busy         = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))

  val selectSt     = Wire   (Vec(PoolOutNum, new MicroOp))
  val issueVldSt   = RegInit(VecInit.fill(PoolOutNum)(false.B))
  val issuePidSt   = Reg    (Vec    (PoolOutNum, UInt(PidWidth.W)))
  val issueSt      = Wire   (Vec(PoolOutNum, new MicroOp))
  val exeSt        = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))

  val issueJr      = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))
  val exeJr        = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))

//function
//---------------------------------------------------------------------------------
  def outSelect[T <: Data](datas:Vec[T], ptr:UInt, i:Int) : T = {
    if (i/2 == 1) {
      VecInit(datas.slice(NumEntriesOut, NumEntriesOut*2))(ptr)
    } else {
      VecInit(datas.slice(0, NumEntriesOut))(ptr)
    }
  }
//---------------------------------------------------------------------------------

  poor := (0 until PoolInNum/2).map( i =>
              PopCount(validNxt.asUInt(( i +1)*NumEntriesIn-1, i * NumEntriesIn)) > (NumEntriesIn-2).asUInt
              ).reduce(_ | _)

  for(i <- 0 until PoolInNum/2) {
    pidOut(  i) := PriorityEncoder(~validNxt.asUInt((i+1)*NumEntriesIn-1, i*NumEntriesIn)) + (i*NumEntriesIn).asUInt
    pidOut(7-i) := (NumEntriesIn - 1).asUInt - PriorityEncoder(Reverse(~validNxt.asUInt((i+1)*NumEntriesIn-1, i*NumEntriesIn))) + (i*NumEntriesIn).asUInt
  }

  //valid update
  for (i <- 0 until NumEntries) {
    val inId0 = i / NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    val outId0 = i / NumEntriesOut * 2
    val outId1 = i / NumEntriesOut * 2 + 1
    validNxt(i) :=
      (valid(i) ||
       allocate(inId0).valid && allocate(inId0).toMd && UIntToOH(allocate(inId0).pid)(i) ||
       allocate(inId1).valid && allocate(inId1).toMd && UIntToOH(allocate(inId1).pid)(i)) &&
     !(issue(outId0).valid && UIntToOH(issue(outId0).pid)(i) ||
       issue(outId1).valid && UIntToOH(issue(outId1).pid)(i) ||
       ienpool(inId0).valid && ienpool( inId0).toMd && UIntToOH(ienpool(inId0).pid)(i) && (redirect.brRBK || redirect.ldRBK) ||
       ienpool(inId1).valid && ienpool( inId1).toMd && UIntToOH(ienpool(inId1).pid)(i) && (redirect.brRBK || redirect.ldRBK) ||
       issueSt(outId0).valid && UIntToOH(issueSt(outId0).pid)(i) && rdySt(outId0) ||
       issueSt(outId1).valid && UIntToOH(issueSt(outId1).pid)(i) && rdySt(outId1) ||
       redirect.brRBK && redirect.flushedBid(bid(i))  ||
       redirect.ldRBK && redirect.flushedLid(lid(i))  ||
       redirect.flush)
    valid(i) := validNxt(i)
  }

  validD1 := valid

  val csrWrPid = Wire(UInt(PidWidth.W))
  for (i <- 0 until NumEntries) {
    val inId0 = i/NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.asUInt) {
      iid(i)       := ienpool(inId0).iid
      bid(i)       := ienpool(inId0).bid
      lid(i)       := ienpool(inId0).lid
      sid(i)       := ienpool(inId0).sid
      src1PReg(i)  := ienpool(inId0).src1PReg
      src2PReg(i)  := ienpool(inId0).src2PReg
      src3Data(i)  := ienpool(inId0).imm
      pReg(i)      := ienpool(inId0).pReg
      dstVld(i)    := ienpool(inId0).dstVld
      isSt(i)      := ienpool(inId0).isSt
      isCsr(i)     := ienpool(inId0).isCsr
      isJr(i)      := ienpool(inId0).isJr
      isJ (i)      := ienpool(inId0).isJ
      exeCyc(i)    := ienpool(inId0).exeCyc
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.asUInt) {
      iid(i)       := ienpool(inId1).iid
      bid(i)       := ienpool(inId1).bid
      lid(i)       := ienpool(inId1).lid
      sid(i)       := ienpool(inId1).sid
      src1PReg(i)  := ienpool(inId1).src1PReg
      src2PReg(i)  := ienpool(inId1).src2PReg
      src3Data(i)  := ienpool(inId1).imm
      pReg(i)      := ienpool(inId1).pReg
      dstVld(i)    := ienpool(inId1).dstVld
      isSt(i)      := ienpool(inId1).isSt
      isCsr(i)     := ienpool(inId1).isCsr
      isJr(i)      := ienpool(inId1).isJr
      isJ (i)      := ienpool(inId1).isJ
      exeCyc(i)    := ienpool(inId1).exeCyc
    }

    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.asUInt) {
      excpVld (i) := ienpool(inId0).excpVld
      excpCode(i) := ienpool(inId0).excpCode
      op(i)        := ienpool(inId0).fuOp
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.asUInt) {
      excpVld (i) := ienpool(inId1).excpVld
      excpCode(i) := ienpool(inId1).excpCode
      op(i)        := ienpool(inId1).fuOp
    }.elsewhen(csrWrEn && csrWrPid === i.asUInt){
      excpVld (i) := excpVld(i) || csrWrIllegal
      excpCode(i) := Mux(csrWrIllegal, 2.U, excpCode(i))
      op(i)       := 9.U //MduOpType.move 
    }
  }

  //state update
  val inSrc1WakeupVec = Wire(Vec(PoolInNum, UInt(WbNum.W)))
  val inSrc2WakeupVec = Wire(Vec(PoolInNum, UInt(WbNum.W)))
  val inSrc1Wakeup = Wire(Vec(PoolInNum, Bool()))
  val inSrc2Wakeup = Wire(Vec(PoolInNum, Bool()))
  for (i <- 0 until PoolInNum) {
    inSrc1WakeupVec(i) := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === ienpool(i).src1PReg && ienpool(i).src1Type===SrcType.rs && !ienpool(i).src1Rdy && !ienpool(i).src1x0 && ienpool(i).valid && ienpool(i).toMd)).asUInt
    inSrc2WakeupVec(i) := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === ienpool(i).src2PReg && ienpool(i).src2Type===SrcType.rs && !ienpool(i).src2Rdy && !ienpool(i).src2x0 && ienpool(i).valid && ienpool(i).toMd)).asUInt
    inSrc1Wakeup(i) := inSrc1WakeupVec(i).orR
    inSrc2Wakeup(i) := inSrc2WakeupVec(i).orR
  }

  val regRdReqD1 = RegInit(VecInit.fill(4)(0.U.asTypeOf(new MicroOp)))
  for (i <- 0 until NumEntries) {
    val inId0 = i / NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    val dId1 = i / NumEntriesOut * 2
    val dId2 = i / NumEntriesOut * 2 + 1
    val entrySrc1WakeupVec = Wire(UInt(WbNum.W))
    val entrySrc1Wakeup    = Wire(Bool())
    val entrySrc2WakeupVec = Wire(UInt(WbNum.W))
    val entrySrc2Wakeup = Wire(Bool())
    entrySrc1WakeupVec := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === src1PReg(i) && !src1Rdy(i) && valid(i))).asUInt
    entrySrc1Wakeup := entrySrc1WakeupVec.orR
    entrySrc2WakeupVec := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === src2PReg(i) && !src2Rdy(i) && valid(i))).asUInt
    entrySrc2Wakeup := entrySrc2WakeupVec.orR

    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src1Rdy     (i) := ienpool(inId0).src1Type=/=SrcType.rs || ienpool(inId0).src1Rdy || inSrc1Wakeup(inId0) || ienpool(inId0).src1x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src1Rdy     (i) := ienpool(inId1).src1Type=/=SrcType.rs || ienpool(inId1).src1Rdy || inSrc1Wakeup(inId1) || ienpool(inId1).src1x0
    }.elsewhen(entrySrc1Wakeup){
      src1Rdy     (i) := true.B
    }

    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src1DRdy    (i) := ienpool(inId0).src1Type=/=SrcType.rs || ienpool(inId0).src1x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src1DRdy    (i) := ienpool(inId1).src1Type=/=SrcType.rs || ienpool(inId1).src1x0
    }.elsewhen(entrySrc1Wakeup){
      src1DRdy    (i) := true.B
    }.elsewhen(regRdReqD1(dId1).valid && regRdReqD1(dId1).pid === i.U){
      src1DRdy    (i) := true.B
    }


    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src1FwdVld_0(i) := inSrc1Wakeup(inId0)
      src1FwdSrc_0(i) := inSrc1WakeupVec(inId0)
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src1FwdVld_0(i) := inSrc1Wakeup(inId1)
      src1FwdSrc_0(i) := inSrc1WakeupVec(inId1)
    }.elsewhen(entrySrc1Wakeup){
      src1FwdVld_0(i) := true.B
      src1FwdSrc_0(i) := entrySrc1WakeupVec
    }.otherwise{
      src1FwdVld_0(i) := false.B
      src1FwdSrc_0(i) := 0.U
    }

    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src2Rdy     (i) := ienpool(inId0).src2Type=/=SrcType.rs || ienpool(inId0).src2Rdy || inSrc2Wakeup(inId0) || ienpool(inId0).src2x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src2Rdy     (i) := ienpool(inId1).src2Type=/=SrcType.rs || ienpool(inId1).src2Rdy || inSrc2Wakeup(inId1) || ienpool(inId1).src2x0
    }.elsewhen(entrySrc2Wakeup){
      src2Rdy     (i) := true.B
    }

    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src2DRdy    (i) := ienpool(inId0).src2Type=/=SrcType.rs || ienpool(inId0).src2x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src2DRdy    (i) := ienpool(inId1).src2Type=/=SrcType.rs || ienpool(inId1).src2x0
    }.elsewhen(entrySrc2Wakeup){
      src2DRdy    (i) := true.B
    }.elsewhen(regRdReqD1(dId2).valid && regRdReqD1(dId2).pid === i.U){
      src2DRdy    (i) := true.B
    }
    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src2FwdVld_0(i) := inSrc2Wakeup(inId0)
      src2FwdSrc_0(i) := inSrc2WakeupVec(inId0)
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src2FwdVld_0(i) := inSrc2Wakeup(inId1)
      src2FwdSrc_0(i) := inSrc2WakeupVec(inId1)
    }.elsewhen(entrySrc2Wakeup){
      src2FwdVld_0(i) := true.B
      src2FwdSrc_0(i) := entrySrc2WakeupVec
    }.otherwise{
      src2FwdVld_0(i) := false.B
      src2FwdSrc_0(i) := 0.U
    }

    src1FwdVld_1(i) := src1FwdVld_0(i)
    src1FwdSrc_1(i) := src1FwdSrc_0(i)
    src2FwdVld_1(i) := src2FwdVld_0(i)
    src2FwdSrc_1(i) := src2FwdSrc_0(i)

  }

  val r0ReqVld =
     src1Rdy.asUInt(NumEntries/2-1, 0) &
    ~src1DRdy.asUInt(NumEntries/2-1, 0) &
    ~Mux(regRdReqD1(0).valid, UIntToOH(regRdReqD1(0).pid)(NumEntries/2-1, 0), 0.U)
  regRdReq(0) := DontCare
  regRdReq(0).valid :=  r0ReqVld.orR
  regRdReq(0).pid  := PriorityEncoder(r0ReqVld)
  regRdReq(0).pReg := PriorityMux    (r0ReqVld, src1PReg.slice(0, NumEntries/2))
  regRdReqD1(0) := regRdReq(0)

  val r1ReqVld =
     src2Rdy.asUInt(NumEntries/2-1, 0) &
    ~src2DRdy.asUInt(NumEntries/2-1, 0) &
    ~Mux(regRdReqD1(1).valid, UIntToOH(regRdReqD1(1).pid)(NumEntries/2-1, 0), 0.U)
  regRdReq(1) := DontCare
  regRdReq(1).valid := r1ReqVld.orR
  regRdReq(1).pid  := PriorityEncoder(r1ReqVld)
  regRdReq(1).pReg := PriorityMux    (r1ReqVld, src2PReg.slice(0, NumEntries/2))
  regRdReqD1(1) := regRdReq(1)

  val r2ReqVld =
     src1Rdy.asUInt(NumEntries-1, NumEntries/2)  &
    ~src1DRdy.asUInt(NumEntries-1, NumEntries/2) &
    ~Mux(regRdReqD1(2).valid, UIntToOH(regRdReqD1(2).pid)(NumEntries-1, NumEntries/2), 0.U)
  regRdReq(2) := DontCare
  regRdReq(2).valid := r2ReqVld.orR
  regRdReq(2).pid  := PriorityEncoder(r2ReqVld) + (NumEntries/2).asUInt
  regRdReq(2).pReg := PriorityMux    (r2ReqVld, src1PReg.slice(NumEntries/2, NumEntries))
  regRdReqD1(2) := regRdReq(2)

  val r3ReqVld =
     src2Rdy.asUInt(NumEntries-1, NumEntries/2)  &
    ~src2DRdy.asUInt(NumEntries-1, NumEntries/2) &
    ~Mux(regRdReqD1(3).valid, UIntToOH(regRdReqD1(3).pid)(NumEntries-1, NumEntries/2), 0.U)
  regRdReq(3) := DontCare
  regRdReq(3).valid := r3ReqVld.orR
  regRdReq(3).pid  := PriorityEncoder(r3ReqVld) + (NumEntries/2).asUInt
  regRdReq(3).pReg := PriorityMux    (r3ReqVld, src2PReg.slice(NumEntries/2, NumEntries))
  regRdReqD1(3) := regRdReq(3)

  for (i <- 0 until NumEntries) {
    val inId0 = i / NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    val ldId0 = i / NumEntriesOut * 2
    val ldId1 = i / NumEntriesOut * 2 + 1
    val dId1 = i / NumEntriesOut * 2
    val dId2 = i / NumEntriesOut * 2 + 1

    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src1Data(i) := ienpool(inId0).src1Data
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src1Data(i) := ienpool(inId1).src1Data
    }.elsewhen(src1FwdVld_1(i)){
      src1Data(i) := Mux1H(src1FwdSrc_1(i), dataIn)
    }.elsewhen(regRdReqD1(dId1).valid && regRdReqD1(dId1).pid === i.asUInt){
      src1Data(i) := regRdRsp(dId1)
    }

    when(ienpool(inId0).valid && ienpool(inId0).toMd && ienpool(inId0).pid === i.U){
      src2Data(i) := ienpool(inId0).src2Data
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toMd && ienpool(inId1).pid === i.U){
      src2Data(i) := ienpool(inId1).src2Data
    }.elsewhen(src2FwdVld_1(i)) {
      src2Data(i) := Mux1H(src2FwdSrc_1(i), dataIn)
    }.elsewhen(regRdReqD1(dId2).valid && regRdReqD1(dId2).pid === i.asUInt){
      src2Data(i) := regRdRsp(dId2)
    }
  }

  val exeDoneD1      = RegNext(exeDone)
  for(i <- 0 until PoolOutNum) {
    val MsbNum = NumEntriesOut*(i/2+1)-1
    val LsbNum = NumEntriesOut*(i/2)
    val selectRdy = Wire(UInt(NumEntriesOut.W))
    val selectMaskCyc0 = Wire(Bool())
    val selectMaskCyc1 = Wire(Bool())
    val selectMaskCyc3 = Wire(Bool())
    selectMaskCyc0 := issueVld(i) &&  issueExeCyc(i) === 2.U || exeDoneD1(i)
    selectMaskCyc1 := exeDone(i)
    selectMaskCyc3 := issueVld(i) &&  issueExeCyc(i) === 3.U || busy(i).valid

    selectRdy :=
      valid.asUInt(MsbNum, LsbNum) &
      validD1.asUInt(MsbNum, LsbNum) &
      dstVld.asUInt(MsbNum, LsbNum) &
      src1DRdy.asUInt(MsbNum, LsbNum) &
      src2DRdy.asUInt(MsbNum, LsbNum) &
      VecInit((LsbNum until MsbNum+1).map(pReg(_)(0)=== (i%2).asUInt)).asUInt  &
     ~VecInit((LsbNum to MsbNum).map(i => redirect.flush || redirect.ldRBK && redirect.flushedLid(lid(i)) || redirect.brRBK && redirect.flushedBid(bid(i)))).asUInt  &
     ~Mux(selectMaskCyc0, VecInit((LsbNum to MsbNum).map(exeCyc(_) === 0.U)).asUInt, 0.U) &
     ~Mux(selectMaskCyc1, VecInit((LsbNum to MsbNum).map(exeCyc(_) === 1.U)).asUInt, 0.U) &
     ~Mux(selectMaskCyc3, VecInit((LsbNum to MsbNum).map(exeCyc(_) === 3.U)).asUInt, 0.U) &
     ~Mux(issueVld(i), UIntToOH(issuePid(i))(MsbNum, LsbNum), 0.U)

    select(i)         := DontCare
    select(i).valid   := selectRdy.orR
    select(i).pid     := PriorityEncoder(selectRdy) + (i/2*NumEntriesOut).asUInt

    issueVld(i)       := select(i).valid
    issuePid(i)       := select(i).pid
    issueDstAddr(i)   := outSelect(pReg, select(i).pid, i)
    issueExeCyc(i)    := outSelect(exeCyc, select(i).pid, i)

    issue(i)          := DontCare
    issue(i).valid    := issueVld(i) && !(redirect.flush || redirect.ldRBK && redirect.flushedLid(issue(i).lid) || redirect.brRBK && redirect.flushedBid(issue(i).bid))
    issue(i).pid      := issuePid(i)
    issue(i).lid      := outSelect(lid, issue(i).pid, i)
    issue(i).bid      := outSelect(bid, issue(i).pid, i)
    issue(i).pReg  := issueDstAddr(i)
    issue(i).exeCyc   := issueExeCyc(i)
    issue(i).fuOp     := outSelect(op, issue(i).pid, i)
    issue(i).src1Data := Mux(outSelect(src1FwdVld_1, issue(i).pid, i), Mux1H(outSelect(src1FwdSrc_1, issue(i).pid, i), dataIn),
                                                                       outSelect(src1Data, issue(i).pid, i) )
    issue(i).src2Data := Mux(outSelect(src2FwdVld_1, issue(i).pid, i), Mux1H(outSelect(src2FwdSrc_1, issue(i).pid, i), dataIn),
                                                                       outSelect(src2Data, issue(i).pid, i) )
    issue(i).src3Data := outSelect(src3Data, issue(i).pid, i)
    issue(i).iid      := outSelect(iid, issue(i).pid, i)
    issue(i).excpVld  := outSelect(excpVld, issue(i).pid, i)
    issue(i).excpCode := outSelect(excpCode, issue(i).pid, i)

    when(issue(i).valid && issue(i).exeCyc === 3.U) {
      busy(i) := issue(i)
    }.elsewhen(RegNext(exeDone(i))){
      busy(i) := 0.U.asTypeOf(new MicroOp)
    }.elsewhen(redirect.flush || redirect.ldRBK && redirect.flushedLid(busy(i).lid) || redirect.brRBK && redirect.flushedBid(busy(i).bid)){
      busy(i) := 0.U.asTypeOf(new MicroOp)
    }

    exe1(i) := Mux(issue(i).valid && issue(i).exeCyc === 1.U, issue(i),
               Mux(RegNext(exeDone(i)), busy(i), 0.U.asTypeOf(new MicroOp)))
    //exe (i) := Mux(issue(i).valid && issue(i).exeCyc === 0.U, issue(i), exe1(i))
    exe (i) := issue(i)

    wakeOut(i) := DontCare
    wakeOut(i).valid  := Mux(issue(i).valid && issue(i).exeCyc === 0.U, issue(i).valid, exe1(i).valid)
    wakeOut(i).pReg  := Mux(issue(i).valid && issue(i).exeCyc === 0.U, issue(i).pReg, exe1(i).pReg)

    issueJr(i)          := DontCare
    issueJr(i).valid    :=  issueVld(i) && (outSelect(isJr, issuePid(i), i) || outSelect(isJ, issuePid(i), i))
    issueJr(i).isJr     := outSelect(isJr, issuePid(i), i)
    issueJr(i).bid      := issue(i).bid
    issueJr(i).src1Data := issue(i).src1Data
    issueJr(i).src3Data := issue(i).src3Data

    exeJr(i) := issueJr(i)

    iexe(i)   := exe(i)
    iexe(i).valid := exe(i).valid && !(redirect.flush || redirect.ldRBK && redirect.flushedLid(exe(i).lid) || redirect.brRBK && redirect.flushedBid(exe(i).bid))
  }


  //st
  for(i <- 0 until PoolOutNum) {
    val MsbNum = NumEntriesOut*(i/2+1)-1
    val LsbNum = NumEntriesOut*(i/2)
    val selectRdy = Wire(UInt(NumEntriesOut.W))
    selectRdy :=
      valid.asUInt(MsbNum, LsbNum) &
      validD1.asUInt(MsbNum, LsbNum) &
      isSt.asUInt(MsbNum, LsbNum) &
      src1DRdy.asUInt(MsbNum, LsbNum) &
      src2DRdy.asUInt(MsbNum, LsbNum) &
      VecInit((LsbNum to MsbNum).map(pReg(_)(0)=== (i%2).asUInt)).asUInt &
     ~(Mux(issueVldSt(i), UIntToOH(issuePidSt(i))(MsbNum, LsbNum), 0.U))


    selectSt(i)         := DontCare
    selectSt(i).valid   := selectRdy.orR
    selectSt(i).pid     := PriorityEncoder(selectRdy) + (i/2*NumEntriesOut).asUInt

    issueVldSt(i)       := Mux(io.rdySt(i), selectSt(i).valid, issueVldSt(i))
    issuePidSt(i)       := Mux(io.rdySt(i), selectSt(i).pid, issuePidSt(i))

    issueSt(i)          := DontCare
    issueSt(i).valid    := issueVldSt(i)
    issueSt(i).pid      := issuePidSt(i)
    issueSt(i).sid      := outSelect(sid    , issueSt(i).pid, i)
    issueSt(i).lid      := outSelect(lid    , issueSt(i).pid, i)
    issueSt(i).fuOp     := outSelect(op     , issueSt(i).pid, i)
    issueSt(i).iid      := outSelect(iid    , issueSt(i).pid, i)
    issueSt(i).src1Data := Mux(outSelect(src1FwdVld_1, issueSt(i).pid, i),
                               Mux1H(outSelect(src1FwdSrc_1, issueSt(i).pid, i), dataIn),
                               outSelect(src1Data, issueSt(i).pid, i) )
    issueSt(i).src2Data := Mux(outSelect(src2FwdVld_1, issueSt(i).pid, i),
                               Mux1H(outSelect(src2FwdSrc_1, issueSt(i).pid, i), dataIn),
                               outSelect(src2Data, issueSt(i).pid, i) )
    issueSt(i).src3Data := outSelect(src3Data, issueSt(i).pid, i)

    exeSt(i) := Mux(io.rdySt(i), issueSt(i), exeSt(i))

    outSt(i).valid := exeSt(i).valid
    outSt(i).mask  := DontCare
    outSt(i).sid   := exeSt(i).sid
    outSt(i).lid   := exeSt(i).lid
    outSt(i).iid   := exeSt(i).iid
    outSt(i).op    := exeSt(i).fuOp
    outSt(i).addr  := exeSt(i).src1Data + exeSt(i).src3Data
    outSt(i).data  := exeSt(i).src2Data
  }

  //Jr
  iexeJr    := exeJr

  //csr
  val selectCsr     = Wire(new MicroOp)
  val issueVldCsr   = RegInit(false.B)
  val issuePidCsr   = Reg    (UInt(PidWidth.W))
  val issueCsr      = Wire   (new MicroOp)
  val exeCsr        = RegInit(0.U.asTypeOf(new MicroOp))

  val selectRdy = Wire(UInt(NumEntries.W))
  selectRdy :=
    valid.asUInt &
    validD1.asUInt &
    isCsr.asUInt &
    src1DRdy.asUInt &
   ~(Mux(issueVldCsr, UIntToOH(issuePidCsr), 0.U))
  selectCsr       := DontCare
  selectCsr.valid := selectRdy.orR
  selectCsr.pid   := OHToUInt(selectRdy)

  issueVldCsr := selectCsr.valid
  issuePidCsr := selectCsr.pid

  issueCsr          := DontCare
  issueCsr.valid    := issueVldCsr
  issueCsr.pid      := issuePidCsr
  issueCsr.src1Data := Mux(src1FwdVld_1(issuePidCsr),  Mux1H(src1FwdSrc_1(issuePidCsr), dataIn), src1Data(issuePidCsr))
  issueCsr.src3Data := src3Data(issuePidCsr)
  issueCsr.fuOp     := op(issuePidCsr)

  exeCsr := issueCsr

  csrWrEn   := exeCsr.valid
  csrWrPid  := exeCsr.pid
  csrWrCmd  := Mux(exeCsr.fuOp === CsrOpType.wr  , "b11".U, 0.U) |
               Mux(exeCsr.fuOp === CsrOpType.wri , "b11".U, 0.U) |
               Mux(exeCsr.fuOp === CsrOpType.set , "b01".U, 0.U) |
               Mux(exeCsr.fuOp === CsrOpType.seti, "b01".U, 0.U) |
               Mux(exeCsr.fuOp === CsrOpType.clr , "b10".U, 0.U) |
               Mux(exeCsr.fuOp === CsrOpType.clri, "b10".U, 0.U)
  csrWrAddr := exeCsr.src3Data(11, 0)
  csrWrData := exeCsr.src1Data

}

