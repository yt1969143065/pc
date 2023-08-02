package scheduler

import chisel3._
import chisel3.util._

import tools._

class Bid extends Module with CoreParameters {
  val io = IO(new Bundle {
    val poor        = Output(Bool())

    val bidOut      = Output(Vec(PoolInNum, UInt(BidWidth.W)))
    val allocate    = Input (Vec(PoolInNum, new MicroOp))

    val brEx        = Input (Vec(PoolOutNum, new MicroOp)) 
    val jrEx        = Input (Vec(PoolOutNum, new MicroOp))

    val flush       = Input (Bool()) 
    val ldRbk       = Input (new MicroOp) 
    val redirect    = Output(new Redirect)
    val pcChange    = Output(new MicroOp)

    val lidSafe     = Input (UInt(LidNum.W))
    val bidSafe     = Output(UInt(BidNum.W))
    
    val brWb        = Output(Vec(4, new MicroOp) )
    val bidRlsVec   = Output(UInt(BidNum.W))

    val bpUpVld     = Output(UInt(8.W)) 
    val bpUpDir     = Output(UInt(8.W))
  })  
  
  //input
  val allocate = Wire(Vec(PoolInNum, new MicroOp))
  val brEx = io.brEx
  val jrEx = io.jrEx
  val flush = io.flush
  val ldRbk = io.ldRbk
  val lidSafe = io.lidSafe
  allocate := io.allocate
  //output
  val poor = RegInit(false.B)
  val bidOut = Wire(Vec(PoolInNum, UInt(BidWidth.W)))
  val redirect = Wire(new Redirect)
  val pcChange = Wire(new MicroOp)
  val bidSafe = Wire(UInt(BidNum.W))
  val brWb = RegInit(VecInit.fill(4)(0.U.asTypeOf(new MicroOp)))
  io.poor := poor
  io.bidOut := bidOut
  io.redirect := redirect
  io.pcChange := pcChange
  io.bidSafe := bidSafe
  //bidSafe := ~0.U(BidNum.W) //FIXME
  io.brWb := brWb

  val brRbk = RegInit(0.U.asTypeOf(new MicroOp))

  val tailPtr   = RegInit(VecInit((0 until 5).map(_.U(BidWidth.W))))
  val headPtr   = RegInit(VecInit((0 until PoolInNum).map(_.U(BidWidth.W))))
  val valid     = RegInit(0.U(BidNum.W))
  val loc       = Reg    (Vec(BidNum, UInt(3.W)))
  val lid       = Reg    (Vec(BidNum, UInt(LidWidth.W)))
  val iid       = Reg    (Vec(BidNum, UInt(IidWidth.W)))
  val preTarget = Reg    (Vec(BidNum, UInt(VAddrWidth.W)))
  val preDir    = RegInit(VecInit.fill(BidNum)(false.B))
  val bakTarget = Reg    (Vec(BidNum, UInt(VAddrWidth.W)))
  val tarRight  = RegInit(0.U(BidNum.W))
  val dirRight  = RegInit(0.U(BidNum.W))
  val wbVld     = RegInit(0.U(BidNum.W))
  val chkVld    = RegInit(0.U(BidNum.W))
  val flushed   = RegInit(0.U(BidNum.W))

  val isBr      = RegInit(VecInit.fill(BidNum)(false.B))

  val allocateVld = Wire(UInt(4.W))
  val releaseVld  = Wire(UInt(PoolInNum.W)) 
  allocateVld := VecInit((0 until 4).map(i => PopCount(allocate.map(i => i.valid & (i.isBr || i.isJ || i.isJr))) > i.U)).asUInt

  for(i <- 0 until 5) {
    when(allocateVld(0)) {
      tailPtr(i) := tailPtr(i) + PopCount(allocateVld) 
    }   
  }

  for(i <- 0 until PoolInNum) {
    when(releaseVld(0)) {
      headPtr(i) := headPtr(i) + PopCount(releaseVld) 
    }
  }

  val counter = RegInit(0.U((BidWidth+1).W))
  val counterNxt = Wire(UInt((BidWidth+1).W))
  counterNxt := counter - PopCount(releaseVld) +  PopCount(allocateVld)
  counter := counterNxt
  poor :=  counterNxt > (BidNum - 4).U

  val isBranchVld = VecInit(allocate.map(i => i.valid &&(i.isBr || i.isJ || i.isJr))).asUInt
  for(i <- 0 until PoolInNum) {
    bidOut(i) := Mux(PopCount(isBranchVld(i, 0)) > 3.U, Mux(isBranchVld(i), tailPtr(3), tailPtr(4)),
                 Mux(PopCount(isBranchVld(i, 0)) > 2.U, Mux(isBranchVld(i), tailPtr(2), tailPtr(3)),
                 Mux(PopCount(isBranchVld(i, 0)) > 1.U, Mux(isBranchVld(i), tailPtr(1), tailPtr(2)),
                 Mux(PopCount(isBranchVld(i, 0)) > 0.U, Mux(isBranchVld(i), tailPtr(0), tailPtr(1)),
                 tailPtr(0)))))
  }


  val validIn  = (0 until 4        ).map(i => Mux(allocateVld(i), UIntToOH(tailPtr(i)), 0.U)).reduce(_ | _)
  val validOut = (0 until PoolInNum).map(i => Mux(releaseVld(i) , UIntToOH(headPtr(i)), 0.U)).reduce(_ | _)
  valid := (valid | validIn) & ~validOut
  io.bidRlsVec := validOut

  for(i <- 0 until BidNum) {
    val upVld = VecInit(allocate.map(j => j.valid && (j.isBr || j.isJ || j.isJr) && j.bid === i.U)).asUInt
    when(upVld.orR) {
      preTarget(i) := Mux1H(upVld, allocate.map(_.preTarget))
      loc(i)       := Mux1H(upVld,  (0 until 8).map(_.U))
      lid(i)       := Mux1H(upVld, allocate.map(_.lid))
      iid(i)       := Mux1H(upVld, allocate.map(_.iid))
      isBr(i)      := Mux1H(upVld, allocate.map(_.isBr))
      preDir(i)    := Mux1H(upVld, allocate.map(_.preDir))
    }

    val jrExVld = VecInit(jrEx.map(j => j.valid && j.bid === i.U && j.isJr)).asUInt
    when(upVld.orR) {
      bakTarget(i) := Mux1H(upVld, allocate.map(_.bakTarget))
    }.elsewhen(jrExVld.orR) {
      bakTarget(i) := Mux1H(jrExVld, jrEx.map(j => (j.src1Data + j.src3Data) & (~1.U(VAddrWidth.W))))
    }
  }

  val brExVld   =     brEx.map(i => Mux(i.valid, UIntToOH(i.bid), 0.U)).reduce(_ | _)
  val jrExVld   =     jrEx.map(i => Mux(i.valid, UIntToOH(i.bid), 0.U)).reduce(_ | _)
  wbVld := (wbVld | brExVld | jrExVld) & ~validOut & ~validIn


  val allocTarRight = allocate.map(i => Mux(i.valid && i.tarRight, UIntToOH(i.bid), 0.U)).reduce(_ | _)
  val brExTarRight = brEx.map(i => Mux(i.valid && (i.brTaken === preDir(i.bid)) , UIntToOH(i.bid), 0.U)).reduce(_ | _)
  //val jrExTarRight = jrEx.map(i => Mux(i.valid && (i.src1Data === preTarget(i.bid)) , UIntToOH(i.bid), 0.U)).reduce(_ | _)
  tarRight := (tarRight | allocTarRight | brExTarRight) & ~validOut & ~(validIn & ~allocTarRight)

  val allocDirRight = allocate.map(i => Mux(i.valid && i.dirRight, UIntToOH(i.bid), 0.U)).reduce(_ | _)
  val brExDirRight = brEx.map(i => Mux(i.valid && (i.brTaken === preDir(i.bid)) , UIntToOH(i.bid), 0.U)).reduce(_ | _)
  dirRight := (dirRight | allocDirRight | brExDirRight) & ~validOut & ~(validIn & ~allocDirRight)


  //brRbk
  val correctPtr = RegInit(VecInit((0 until 4).map(_.U(BidWidth.W))))
  val ldFlushVec = Mux(ldRbk.valid, VecInit(lid.map(ldRbk.lidVec(_))).asUInt, 0.U)
  val brFlushVec = Mux(brRbk.valid, brRbk.bidVec, 0.U)
  val excpFlushVec = Mux(flush, valid, 0.U)
  val candidateVec = valid & (wbVld | flushed | ldFlushVec | brFlushVec | excpFlushVec)
  val neeCorrectVec = valid & wbVld & ~(flushed | ldFlushVec | brFlushVec | excpFlushVec) & ~tarRight
  flushed := (flushed | ldFlushVec | brFlushVec | excpFlushVec) & ~validOut & ~validIn

  val checked = Wire(Vec(4, Bool()))
  val checkedIid = Wire(Vec(4, UInt(IidWidth.W)))
  for(i <- 0 until 4) {
    if(i == 0) {
      checked(i) := candidateVec(correctPtr(i))
    } else {
      checked(i) := candidateVec(correctPtr(i))  && checked(i-1)
    }
    checkedIid(i) := iid(correctPtr(i))
    brWb(i) := DontCare
    brWb(i).valid := checked(i) && isBr(correctPtr(i))
    brWb(i).iid   := checkedIid(i)

    correctPtr(i) := correctPtr(i) + PopCount(checked)
  }

  val newCheck = VecInit((0 until 4).map(i => Mux(checked(i), UIntToOH(correctPtr(i)), 0.U))).reduce(_ | _)
  chkVld := (chkVld | newCheck) & ~validOut & ~validIn


  bidSafe := ~(~chkVld & valid) & ~UIntToOH(tailPtr(0)) | UIntToOH(correctPtr(0))

  val correctVec = VecInit(correctPtr.map(neeCorrectVec(_))).asUInt & checked.asUInt
  val correctBid = PriorityMux(correctVec, correctPtr)
  val brRbkBidVec =  VecInit((0 until PoolInNum).map( i =>
    Mux(allocate(i).valid, UIntToOH(allocate(i).bid), 0.U)
  )).reduce(_ | _)
  brRbk.valid  := correctVec.orR
  brRbk.bid    := correctBid
  brRbk.bidVec := QGtBitsByTailInclude(correctBid, tailPtr(0)) | brRbkBidVec

  val bpUpVec = VecInit(correctPtr.map(i => checked(i) && !flushed(i) && isBr(i))).asUInt
  val bpUpLoc = VecInit(correctPtr.map(i => loc(i)))
  val bpUpDir = VecInit(correctPtr.map(i => Mux(tarRight(i), preDir(i), !preDir(i)))).asUInt

  io.bpUpVld := VecInit((0 until 8).map(i =>
    (0 until 4).map(j =>
      bpUpVec(j) && bpUpLoc(j) === i.U
    ).reduce(_ || _)
  )).asUInt
  io.bpUpDir := VecInit((0 until 8).map(i =>
    (0 until 4).map(j =>
      bpUpVec(j) && bpUpLoc(j) === i.U && bpUpDir(j)
    ).reduce(_ || _)
  )).asUInt

  //release
  val releaseVldVec = Wire(Vec(PoolInNum, Bool()))
  for(i <- 0 until PoolInNum) {
    if(i == 0) {
      releaseVldVec(i) := chkVld(headPtr(i)) && lidSafe(lid(headPtr(i)))
    } else {
      releaseVldVec(i) := chkVld(headPtr(i)) && lidSafe(lid(headPtr(i))) && releaseVldVec(i-1)
    }
  }
  releaseVld := releaseVldVec.asUInt

  redirect.flush := flush
  redirect.brRBK := brRbk.valid
  redirect.flushedBid := brRbk.bidVec
  redirect.ldRBK := ldRbk.valid
  redirect.flushedLid := ldRbk.lidVec
  redirect.rbkLid := ldRbk.lid

  pcChange := DontCare
  pcChange.valid := brRbk.valid | ldRbk.valid
  pcChange.pc    := Mux(brRbk.valid, bakTarget(brRbk.bid), ldRbk.pc)

}

