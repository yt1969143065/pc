package pools

import chisel3._
import chisel3.util._

import tools._


//alu + load + branch
class AluPool extends Module with CoreParameters{
  val io = IO(new Bundle {
    val redirect = Input (new Redirect) 
    val poor     = Output(Bool()) 
    val pidOut   = Output(Vec(PoolInNum,  UInt(PidWidth.W) ))
    val allocate = Input (Vec(PoolInNum, new MicroOp))
    val ienpool  = Input (Vec(PoolInNum,  new MicroOp      ))  
    val regRdReq = Output(Vec(RFReadNum,  new MicroOp))
    val regRdRsp = Input (Vec(RFReadNum,  UInt(64.W)       ))
    val iexe     = Output(Vec(PoolOutNum, new MicroOp      ))  
    val wakeOut  = Output(Vec(PoolOutNum, new MicroOp      ))  
    val wakeIn   = Input (Vec(WbNum,      new MicroOp      ))  
    val dataIn   = Input (Vec(WbNum,      UInt(64.W)       ))
    val wakeAck  = Output(Vec(WbNum,      Bool()))
    val wakeReq  = Input (Vec(WbNum,      new MicroOp      ))  
    val wakeData = Input (Vec(WbNum,      UInt(64.W)       ))
    val iexeBr   = Output(Vec(PoolOutNum, new MicroOp      ))  
    val ldReqRdy = Input (Vec(PoolOutNum, Bool()           ))
    val ldReq    = Output(Vec(PoolOutNum, new LdReqBundle  ))  
    val ldRsp    = Input (Vec(PoolOutNum, new LdRspBundle  ))  
  })  

  //input port
  val allocate = io.allocate
  val ienpool = io.ienpool
  val regRdRsp = io.regRdRsp
  val wakeIn = Wire(Vec(WbNum, new MicroOp))
  val dataIn = Wire(Vec(WbNum, UInt(64.W)))
  val redirect = io.redirect
  val ldReqRdy = io.ldReqRdy
  val ldRsp = io.ldRsp
  //output port
  val poor = RegInit(false.B)
  val pidOut = RegInit(VecInit((0 until PoolInNum).map(i => if(i > 3) ((8 - i)*NumEntriesIn-1).U else (i*NumEntriesIn).U)))
  val regRdReq = Wire(Vec(RFReadNum, new MicroOp))
  val iexe = Wire(Vec(PoolOutNum, new MicroOp))
  val wakeOut = Wire(Vec(PoolOutNum, new MicroOp))
  val iexeBr = Wire(Vec(PoolOutNum, new MicroOp))
  val ldReq = Wire(Vec(PoolOutNum, new LdReqBundle))
  io.poor := poor
  io.pidOut := pidOut
  io.regRdReq := regRdReq
  io.iexe := iexe
  io.wakeOut := wakeOut
  io.iexeBr := iexeBr
  io.ldReq := ldReq
  io.wakeAck := VecInit((io.wakeIn.map(! _.valid)))



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
  val isLd         = RegInit(VecInit.fill(NumEntries)(false.B))
  val ldSent       = RegInit(VecInit.fill(NumEntries)(false.B))
  val ldDRdy       = RegInit(VecInit.fill(NumEntries)(false.B))
  val isBr         = RegInit(VecInit.fill(NumEntries)(false.B))
  val op           = Reg    (Vec    (NumEntries, FuOpType()))
  val iid          = Reg    (Vec    (NumEntries, UInt(IidWidth.W)))
  val lid          = Reg    (Vec    (NumEntries, UInt(LidWidth.W)))
  val bid          = Reg    (Vec    (NumEntries, UInt(BidWidth.W)))
  val src1PReg     = Reg    (Vec    (NumEntries, UInt(PRegWidth.W)))
  val src2PReg     = Reg    (Vec    (NumEntries, UInt(PRegWidth.W)))
  val dstVld       = RegInit(VecInit.fill(NumEntries)(false.B))
  val pReg         = Reg    (Vec    (NumEntries, UInt(PRegWidth.W)))

  val src1Rdy      = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1DRdy     = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1FwdVld_0 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1FwdVld_1 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src1FwdSrc_0 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src1FwdSrc_1 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src1Data     = Reg    (Vec    (NumEntries, UInt(64.W)))

  val src2Rdy      = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2DRdy     = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2FwdVld_0 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2FwdVld_1 = RegInit(VecInit.fill(NumEntries)(false.B))
  val src2FwdSrc_0 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src2FwdSrc_1 = RegInit(VecInit.fill(NumEntries)(0.U(WbNum.W)))
  val src2Data     = Reg    (Vec    (NumEntries, UInt(64.W)))


  val select       = Wire   (Vec(PoolOutNum, new MicroOp))
  val issueVld     = RegInit(VecInit.fill(PoolOutNum)(false.B))
  val issuePid     = Reg    (Vec    (PoolOutNum, UInt(PidWidth.W)))
  val issueDstAddr = Reg    (Vec    (PoolOutNum, UInt(PRegWidth.W)))
  val issue        = Wire   (Vec(PoolOutNum, new MicroOp))
  val exe          = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))


  val selectBr   = Wire   (Vec(PoolOutNum, new MicroOp))
  val issueVldBr = RegInit(VecInit.fill(PoolOutNum)(false.B))
  val issuePidBr = Reg    (Vec    (PoolOutNum, UInt(PidWidth.W)))
  val issueBr    = Wire   (Vec(PoolOutNum, new MicroOp))
  val exeBr      = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))

  val selectLd   = Wire   (Vec(PoolOutNum, new MicroOp))
  val issueVldLd = RegInit(VecInit.fill(PoolOutNum)(false.B))
  val issuePidLd = Reg    (Vec    (PoolOutNum, UInt(PidWidth.W)))
  val issueLd    = Wire   (Vec(PoolOutNum, new MicroOp))
  val exeLd      = RegInit(VecInit.fill(PoolOutNum)(0.U.asTypeOf(new MicroOp)))


//function
//---------------------------------------------------------------------------------
  def outSelect[T <: Data](datas:Vec[T], ptr:UInt, i:Int) : T = {
    if (i/2 == 1) {
      //VecInit(datas.slice(NumEntriesOut, NumEntriesOut*2).reverse)(ptr)
      VecInit(datas.slice(NumEntriesOut, NumEntriesOut*2))(ptr)
    } else {
      //VecInit(datas.slice(0, NumEntriesOut).reverse)(ptr)
      VecInit(datas.slice(0, NumEntriesOut))(ptr)
    }
  }
//---------------------------------------------------------------------------------

  poor := (0 until PoolInNum/2).map( i =>
              PopCount(validNxt.asUInt(( i +1)*NumEntriesIn-1, i * NumEntriesIn)) > (NumEntriesIn-2).U
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
       allocate(inId0).valid && allocate( inId0).toAl && UIntToOH(allocate(inId0).pid)(i) ||
       allocate(inId1).valid && allocate( inId1).toAl && UIntToOH(allocate(inId1).pid)(i)) &&
     !(issue(outId0).valid && UIntToOH(issue(outId0).pid)(i) ||
       issue(outId1).valid && UIntToOH(issue(outId1).pid)(i) ||
       issueBr(outId0).valid && UIntToOH(issueBr(outId0).pid)(i) ||
       issueBr(outId1).valid && UIntToOH(issueBr(outId1).pid)(i) ||
       ienpool(inId0).valid && ienpool( inId0).toAl && UIntToOH(ienpool(inId0).pid)(i) && (redirect.brRBK || redirect.ldRBK) ||
       ienpool(inId1).valid && ienpool( inId1).toAl && UIntToOH(ienpool(inId1).pid)(i) && (redirect.brRBK || redirect.ldRBK) ||
       redirect.brRBK && redirect.flushedBid(bid(i))  ||
       redirect.ldRBK && redirect.flushedLid(lid(i))  ||
       redirect.flush)
    valid(i) := validNxt(i)
    validD1(i) := valid(i)
    ldSent(i) :=
       (ldSent(i) ||
       issueLd(outId0).valid && UIntToOH(issueLd(outId0).pid)(i) ||
       issueLd(outId1).valid && UIntToOH(issueLd(outId1).pid)(i) ) &&
     !(ienpool(inId0).valid && ienpool(inId0).toAl && UIntToOH(ienpool(inId0).pid)(i) ||
       ienpool(inId1).valid && ienpool(inId1).toAl && UIntToOH(ienpool(inId1).pid)(i) )
    ldDRdy(i) :=
       (ldDRdy(i) ||
       ldRsp(outId0).valid && UIntToOH(ldRsp(outId0).pid)(i) && ldSent(i) ||
       ldRsp(outId1).valid && UIntToOH(ldRsp(outId1).pid)(i) && ldSent(i)) &&
     !(ienpool(inId0).valid && ienpool(inId0).toAl && UIntToOH(ienpool(inId0).pid)(i) ||
       ienpool(inId1).valid && ienpool(inId1).toAl && UIntToOH(ienpool(inId1).pid)(i) )
  }


  for (i <- 0 until NumEntries) {
    val inId0 = i/NumEntriesIn
    val inId1 = PoolInNum - 1 - i/NumEntriesIn
    when (ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U) {
      iid(i)       := ienpool(inId0).iid
      bid(i)       := ienpool(inId0).bid
      lid(i)       := ienpool(inId0).lid
      src1PReg(i)  := ienpool(inId0).src1PReg
      src2PReg(i)  := ienpool(inId0).src2PReg
      pReg(i)      := ienpool(inId0).pReg
      dstVld(i)    := ienpool(inId0).dstVld
      isLd(i)      := ienpool(inId0).isLd
      isBr(i)      := ienpool(inId0).isBr

    }.elsewhen( ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U) {
      iid(i)       := ienpool(inId1).iid
      bid(i)       := ienpool(inId1).bid
      lid(i)       := ienpool(inId1).lid
      src1PReg(i)  := ienpool(inId1).src1PReg
      src2PReg(i)  := ienpool(inId1).src2PReg
      pReg(i)      := ienpool(inId1).pReg
      dstVld(i)    := ienpool(inId1).dstVld
      isLd(i)      := ienpool(inId1).isLd
      isBr(i)      := ienpool(inId1).isBr
    }

    val ldId0 = i / NumEntriesOut * 2
    val ldId1 = i / NumEntriesOut * 2 + 1
    when (ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U) {
      op(i)        := ienpool(inId0).fuOp
    }.elsewhen( ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U) {
      op(i)        := ienpool(inId1).fuOp
    }.elsewhen(ldRsp(ldId0).valid && ldRsp(ldId0).pid === i.asUInt && ldSent(i)) {
      op(i)        := 0.U //AluOpType.move
    }.elsewhen(ldRsp(ldId1).valid && ldRsp(ldId1).pid === i.asUInt && ldSent(i)) {
      op(i)        := 0.U //AluOpType.move
    }
  }

  //state update
  val inSrc1WakeupVec = Wire(Vec(PoolInNum, UInt(WbNum.W)))
  val inSrc2WakeupVec = Wire(Vec(PoolInNum, UInt(WbNum.W)))
  val inSrc1Wakeup = Wire(Vec(PoolInNum, Bool()))
  val inSrc2Wakeup = Wire(Vec(PoolInNum, Bool()))
  for (i <- 0 until PoolInNum) {
    inSrc1WakeupVec(i) := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === ienpool(i).src1PReg && ienpool(i).src1Type===SrcType.rs && !ienpool(i).src1Rdy && !ienpool(i).src1x0 && ienpool(i).valid && ienpool(i).toAl)).asUInt
    inSrc2WakeupVec(i) := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === ienpool(i).src2PReg && ienpool(i).src2Type===SrcType.rs && !ienpool(i).src2Rdy && !ienpool(i).src2x0 && ienpool(i).valid && ienpool(i).toAl)).asUInt
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
    entrySrc1WakeupVec := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === src1PReg(i) && !src1Rdy(i) && valid(i) && validD1(i))).asUInt
    entrySrc1Wakeup    := entrySrc1WakeupVec.orR
    entrySrc2WakeupVec := VecInit((0 until WbNum).map(j => wakeIn(j).valid && wakeIn(j).pReg === src2PReg(i) && !src2Rdy(i) && valid(i) && validD1(i))).asUInt
    entrySrc2Wakeup    := entrySrc2WakeupVec.orR

    when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src1Rdy     (i) := ienpool(inId0).src1Type=/=SrcType.rs || ienpool(inId0).src1Rdy || inSrc1Wakeup(inId0) || ienpool(inId0).src1x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
      src1Rdy     (i) := ienpool(inId1).src1Type=/=SrcType.rs || ienpool(inId1).src1Rdy || inSrc1Wakeup(inId1) || ienpool(inId1).src1x0
    }.elsewhen(entrySrc1Wakeup){
      src1Rdy     (i) := true.B
    }

    when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src1DRdy    (i) := ienpool(inId0).src1Type=/=SrcType.rs || ienpool(inId0).src1x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
      src1DRdy    (i) := ienpool(inId1).src1Type=/=SrcType.rs || ienpool(inId1).src1x0
    }.elsewhen(entrySrc1Wakeup){
      src1DRdy    (i) := true.B
    }.elsewhen(regRdReqD1(dId1).valid && regRdReqD1(dId1).pid === i.asUInt){
      src1DRdy    (i) := true.B
    }


    when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src1FwdVld_0(i) := inSrc1Wakeup(inId0)
      src1FwdSrc_0(i) := inSrc1WakeupVec(inId0)
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
      src1FwdVld_0(i) := inSrc1Wakeup(inId1)
      src1FwdSrc_0(i) := inSrc1WakeupVec(inId1)
    }.elsewhen(entrySrc1Wakeup){
      src1FwdVld_0(i) := true.B
      src1FwdSrc_0(i) := entrySrc1WakeupVec
    }.otherwise{
      src1FwdVld_0(i) := false.B
      src1FwdSrc_0(i) := 0.U
    }

    when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src2Rdy     (i) := ienpool(inId0).src2Type=/=SrcType.rs || ienpool(inId0).src2Rdy || inSrc2Wakeup(inId0) || ienpool(inId0).src2x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
      src2Rdy     (i) := ienpool(inId1).src2Type=/=SrcType.rs || ienpool(inId1).src2Rdy || inSrc2Wakeup(inId1) || ienpool(inId1).src2x0
    }.elsewhen(entrySrc2Wakeup){
      src2Rdy     (i) := true.B
    }

    when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src2DRdy    (i) := ienpool(inId0).src2Type=/=SrcType.rs || ienpool(inId0).src2x0
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
      src2DRdy    (i) := ienpool(inId1).src2Type=/=SrcType.rs || ienpool(inId1).src2x0
    }.elsewhen(entrySrc2Wakeup){
      src2DRdy    (i) := true.B
    }.elsewhen(regRdReqD1(dId2).valid && regRdReqD1(dId2).pid === i.asUInt){
      src2DRdy    (i) := true.B
    }

   when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src2FwdVld_0(i) := inSrc2Wakeup(inId0)
      src2FwdSrc_0(i) := inSrc2WakeupVec(inId0)
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
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
  regRdReq(0).valid := r0ReqVld.orR
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
     src1Rdy.asUInt(NumEntries-1, NumEntries/2) &
    ~src1DRdy.asUInt(NumEntries-1, NumEntries/2) &
    ~Mux(regRdReqD1(2).valid, UIntToOH(regRdReqD1(2).pid)(NumEntries-1, NumEntries/2), 0.U)
  regRdReq(2) := DontCare
  regRdReq(2).valid := r2ReqVld.orR
  regRdReq(2).pid  := PriorityEncoder(r2ReqVld) + (NumEntries/2).asUInt
  regRdReq(2).pReg := PriorityMux    (r2ReqVld, src1PReg.slice(NumEntries/2, NumEntries))
  regRdReqD1(2) := regRdReq(2)

  val r3ReqVld =
     src2Rdy.asUInt(NumEntries-1, NumEntries/2) &
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

    when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src1Data(i) := ienpool(inId0).src1Data
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
      src1Data(i) := ienpool(inId1).src1Data
    }.elsewhen(src1FwdVld_1(i)){
      src1Data(i) := Mux1H(src1FwdSrc_1(i), dataIn)
    }.elsewhen(regRdReqD1(dId1).valid && regRdReqD1(dId1).pid === i.asUInt){
      src1Data(i) := regRdRsp(dId1)
    }

    when(ienpool(inId0).valid && ienpool(inId0).toAl && ienpool(inId0).pid === i.U){
      src2Data(i) := ienpool(inId0).src2Data
    }.elsewhen(ienpool(inId1).valid && ienpool(inId1).toAl && ienpool(inId1).pid === i.U){
      src2Data(i) := ienpool(inId1).src2Data
    }.elsewhen(src2FwdVld_1(i)) {
      src2Data(i) := Mux1H(src2FwdSrc_1(i), dataIn)
    }.elsewhen(regRdReqD1(dId2).valid && regRdReqD1(dId2).pid === i.asUInt){
      src2Data(i) := regRdRsp(dId2)
    }.elsewhen(ldRsp(ldId0).valid && ldRsp(ldId0).pid === i.asUInt && ldSent(i)) {
      src2Data(i) := ldRsp(ldId0).data
    }.elsewhen(ldRsp(ldId1).valid && ldRsp(ldId1).pid === i.asUInt && ldSent(i)) {
      src2Data(i) := ldRsp(ldId1).data
    }
  }

  for(i <- 0 until PoolOutNum) {
    val MsbNum = NumEntriesOut*(i/2+1)-1
    val LsbNum = NumEntriesOut*(i/2)
    val selectRdy = Wire(UInt(NumEntriesOut.W))
    selectRdy :=
      valid.asUInt(MsbNum, LsbNum) &
      validD1.asUInt(MsbNum, LsbNum) &
      dstVld.asUInt(MsbNum, LsbNum) &
     (isLd.asUInt(MsbNum, LsbNum) & ldDRdy.asUInt(MsbNum, LsbNum) | ~isLd.asUInt(MsbNum, LsbNum)) &
      src1DRdy.asUInt(MsbNum, LsbNum) &
      src2DRdy.asUInt(MsbNum, LsbNum) &
      VecInit((LsbNum to MsbNum).map(pReg(_)(0) === (i%2).U)).asUInt  &
     ~VecInit((LsbNum to MsbNum).map(i => redirect.flush || redirect.ldRBK && redirect.flushedLid(lid(i)) || redirect.brRBK && redirect.flushedBid(bid(i)))).asUInt  &
     ~(Mux(issueVld(i), UIntToOH(issuePid(i))(MsbNum, LsbNum), 0.U))

    select(i)       := DontCare
    select(i).valid := selectRdy.orR
    select(i).pid   := PriorityEncoder(selectRdy) + (i/2*NumEntriesOut).asUInt

    issueVld(i)       := select(i).valid
    issuePid(i)       := select(i).pid
    issueDstAddr(i)   := outSelect(pReg, select(i).pid, i)

    issue(i)          := DontCare
    issue(i).valid    := issueVld(i) && !(redirect.flush || redirect.ldRBK && redirect.flushedLid(issue(i).lid) || redirect.brRBK && redirect.flushedBid(issue(i).bid))
    issue(i).pid      := issuePid(i)
    issue(i).lid      := outSelect(lid, issuePid(i), i)
    issue(i).bid      := outSelect(bid, issuePid(i), i)
    issue(i).pReg     := issueDstAddr(i)
    issue(i).fuOp     := outSelect(op, issuePid(i), i)
    issue(i).src1Data := Mux(outSelect(src1FwdVld_1, issuePid(i), i),
                             Mux1H(outSelect(src1FwdSrc_1, issuePid(i), i), dataIn),
                             outSelect(src1Data, issuePid(i), i) )
    issue(i).src2Data := Mux(outSelect(src2FwdVld_1, issuePid(i), i),
                             Mux1H(outSelect(src2FwdSrc_1, issuePid(i), i), dataIn),
                             outSelect(src2Data, issuePid(i), i) )
    issue(i).iid      := outSelect(iid, issuePid(i), i)

    exe(i) := issue(i)

    wakeOut(i) := DontCare
    wakeOut(i).valid  := issue(i).valid
    wakeOut(i).pReg := issue(i).pReg

    iexe(i) := exe(i)
    iexe(i).valid := exe(i).valid && !(redirect.flush || redirect.ldRBK && redirect.flushedLid(exe(i).lid) || redirect.brRBK && redirect.flushedBid(exe(i).bid))
  }

  for(i <- 0 until PoolOutNum) {
    val MsbNum = NumEntriesOut*(i/2+1)-1
    val LsbNum = NumEntriesOut*(i/2)
    val selectRdy = Wire(UInt(NumEntriesOut.W))
    selectRdy :=
      valid.asUInt(MsbNum, LsbNum) &
      validD1.asUInt(MsbNum, LsbNum) &
      isBr.asUInt(MsbNum, LsbNum) &
      src1DRdy.asUInt(MsbNum, LsbNum) &
      src2DRdy.asUInt(MsbNum, LsbNum) &
      VecInit((LsbNum to MsbNum).map(pReg(_)(0)=== (i%2).asUInt)).asUInt &
     ~(Mux(issueVldBr(i), UIntToOH(issuePidBr(i))(MsbNum, LsbNum), 0.U))


    selectBr(i)         := DontCare
    selectBr(i).valid   := selectRdy.orR
    selectBr(i).pid     := PriorityEncoder(selectRdy) + (i/2*NumEntriesOut).asUInt

    issueVldBr(i)       := selectBr(i).valid
    issuePidBr(i)       := selectBr(i).pid

    issueBr(i)          := DontCare
    issueBr(i).valid    := issueVldBr(i)
    issueBr(i).pid      := issuePidBr(i)
    issueBr(i).bid      := outSelect(bid    , issueBr(i).pid, i)
    issueBr(i).iid      := outSelect(iid    , issueBr(i).pid, i)
    issueBr(i).fuOp     := outSelect(op     , issueBr(i).pid, i)
    issueBr(i).pReg  := outSelect(pReg, issueBr(i).pid, i)
    issueBr(i).src1Data := Mux(outSelect(src1FwdVld_1, issueBr(i).pid, i),
                               Mux1H(outSelect(src1FwdSrc_1, issueBr(i).pid, i), dataIn),
                               outSelect(src1Data, issueBr(i).pid, i) )
    issueBr(i).src2Data := Mux(outSelect(src2FwdVld_1, issueBr(i).pid, i),
                               Mux1H(outSelect(src2FwdSrc_1, issueBr(i).pid, i), dataIn),
                               outSelect(src2Data, issueBr(i).pid, i) )

    exeBr(i) := issueBr(i)

    iexeBr(i) := DontCare
    iexeBr(i).valid := exeBr(i).valid
    iexeBr(i).bid := exeBr(i).bid
    iexeBr(i).iid := exeBr(i).iid
    iexeBr(i).brTaken :=
      (exeBr(i).fuOp === BrOpType.beq  && exeBr(i).src1Data === exeBr(i).src2Data) ||
      (exeBr(i).fuOp === BrOpType.bne  && exeBr(i).src1Data =/= exeBr(i).src2Data) ||
      (exeBr(i).fuOp === BrOpType.blt  && exeBr(i).src1Data.asSInt  <  exeBr(i).src2Data.asSInt) ||
      (exeBr(i).fuOp === BrOpType.bge  && exeBr(i).src1Data.asSInt  >=  exeBr(i).src2Data.asSInt) ||
      (exeBr(i).fuOp === BrOpType.bltu && exeBr(i).src1Data  <  exeBr(i).src2Data) ||
      (exeBr(i).fuOp === BrOpType.bgeu && exeBr(i).src1Data  >=  exeBr(i).src2Data)
  }

  for(i <- 0 until PoolOutNum) {
    val MsbNum = NumEntriesOut*(i/2+1)-1
    val LsbNum = NumEntriesOut*(i/2)
    val selectRdy = Wire(UInt(NumEntriesOut.W))
    selectRdy :=
      valid.asUInt(MsbNum, LsbNum) &
      validD1.asUInt(MsbNum, LsbNum) &
      isLd.asUInt(MsbNum, LsbNum) &
     ~ldSent.asUInt(MsbNum, LsbNum) &
      src1DRdy.asUInt(MsbNum, LsbNum) &
      VecInit((LsbNum to MsbNum).map(pReg(_)(0)=== (i%2).asUInt)).asUInt &
     ~(Mux(issueVldLd(i), UIntToOH(issuePidLd(i))(MsbNum, LsbNum), 0.U))


    selectLd(i)         := DontCare
    selectLd(i).valid   := selectRdy.orR
    selectLd(i).pid     := PriorityEncoder(selectRdy) + (i/2*NumEntriesOut).asUInt

    issueVldLd(i)       := Mux(io.ldReqRdy(i), selectLd(i).valid, issueVldLd(i))
    issuePidLd(i)       := Mux(io.ldReqRdy(i), selectLd(i).pid, issuePidLd(i))

    issueLd(i)          := DontCare
    issueLd(i).valid    := issueVldLd(i)
    issueLd(i).pid      := issuePidLd(i)
    issueLd(i).lid      := outSelect(lid    , issueLd(i).pid, i)
    issueLd(i).fuOp     := outSelect(op     , issueLd(i).pid, i)
    issueLd(i).src1Data := Mux(outSelect(src1FwdVld_1, issueLd(i).pid, i),
                               Mux1H(outSelect(src1FwdSrc_1, issueLd(i).pid, i), dataIn),
                               outSelect(src1Data, issueLd(i).pid, i) )
    issueLd(i).src2Data := Mux(outSelect(src2FwdVld_1, issueLd(i).pid, i),
                               Mux1H(outSelect(src2FwdSrc_1, issueLd(i).pid, i), dataIn),
                               outSelect(src2Data, issueLd(i).pid, i) )

    exeLd(i) := Mux(io.ldReqRdy(i), issueLd(i), exeLd(i))

    ldReq(i).valid := exeLd(i).valid
    ldReq(i).mask  := DontCare
    ldReq(i).pid   := exeLd(i).pid
    ldReq(i).lid   := exeLd(i).lid
    ldReq(i).op    := exeLd(i).fuOp
    ldReq(i).addr  := exeLd(i).src1Data + exeLd(i).src2Data
  }
}

