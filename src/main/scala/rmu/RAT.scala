package rmu

import chisel3._
import chisel3.util._

import tools._


class RAT(isFP : Boolean = false) extends Module with CoreParameters {
  val io = IO(new Bundle {
    val redirect     = Input (new Redirect)
    val pRegFlushVec = Output(UInt(PRegNum.W))
    val dstRdAddr    = Input (Vec(PoolInNum, UInt(5.W)))
    val dstRdData    = Output(Vec(PoolInNum, UInt(PRegWidth.W)))
    val srcRdAddr0   = Input (Vec(2, Vec(PoolInNum, UInt(5.W))))
    val srcRdData0   = Output(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
    val srcRdAddr    = Input (Vec(2, Vec(PoolInNum, UInt(5.W))))
    val srcRdData    = Output(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
    val dstUpdate    = Input (Vec(PoolInNum, new MicroOp))
    val pReg         = Output(Vec(32, UInt(PRegWidth.W)))
    val lidRlsVec    = Input(UInt(LidNum.W))
    val bidRlsVec    = Input(UInt(BidNum.W))
  })

  //input
  val redirect  = io.redirect
  val dstRdAddr = io.dstRdAddr
  val srcRdAddr0= io.srcRdAddr0
  val srcRdAddr = io.srcRdAddr
  val dstUpdate = io.dstUpdate
  //output
  val dstRdData = Wire(Vec(PoolInNum, UInt(PRegWidth.W)))
  val srcRdData0 = Wire(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
  val srcRdData = Wire(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
  val pRegFlushVec = Wire(UInt(PRegNum.W))
  io.dstRdData := dstRdData
  io.srcRdData0 := srcRdData0
  io.srcRdData := srcRdData
  io.pRegFlushVec := pRegFlushVec 

  val pReg = RegInit(VecInit((0 until 32).map(i => if(isFP) (32 + i).U(PRegWidth.W) else i.U(PRegWidth.W)))) 
  val bid  = RegInit(VecInit(Seq.fill(32)(0.U(BidWidth.W))))
  val lid  = RegInit(VecInit(Seq.fill(32)(0.U(LidWidth.W))))

  for(i <- 0 until 32) {
    val upVld = VecInit((0 until PoolInNum).map(j => dstUpdate(j).valid && dstUpdate(j).dstReg === i.asUInt && dstUpdate(j).dstVld && !dstUpdate(j).dstx0)).asUInt
    assert(PopCount(upVld) <= 1.U, "rat dstUpdate is not onehot")
    when(upVld.orR) {
      pReg(i) := Mux1H(upVld, dstUpdate.map(_.pReg))
      bid(i)  := Mux1H(upVld, dstUpdate.map(_.bid)) 
      lid(i)  := Mux1H(upVld, dstUpdate.map(_.lid)) 
    } 
  }

  for(i <- 0 until PoolInNum) {
    srcRdData0(0)(i) := pReg(srcRdAddr0(0)(i))
    srcRdData0(1)(i) := pReg(srcRdAddr0(1)(i))
    dstRdData(i)     := pReg(dstRdAddr(i))
    srcRdData(0)(i)  := pReg(srcRdAddr(0)(i))
    srcRdData(1)(i)  := pReg(srcRdAddr(1)(i))
  }

  val valid = RegInit(0.U(32.W))
  val validSet = (0 until 8).map(i => Mux(dstUpdate(i).valid && dstUpdate(i).dstVld, UIntToOH(dstUpdate(i).dstReg), 0.U)).reduce(_ | _) 
  val validClrByLid = VecInit(lid.map(io.lidRlsVec(_))).asUInt 
  val validClrByBid = VecInit(bid.map(io.bidRlsVec(_))).asUInt 
  valid := (valid & ~validClrByLid & ~validClrByBid) | validSet
  val brFlushVec = (0 until 32).map(i => 
      Mux(redirect.brRBK && redirect.flushedBid(bid(i)) && valid(i), UIntToOH(pReg(i)), 0.U)
      ).reduce(_ | _) 
  val ldFlushVec = (0 until 32).map(i => 
      Mux(redirect.ldRBK && redirect.flushedLid(lid(i)) && valid(i), UIntToOH(pReg(i)), 0.U)  
      ).reduce(_ | _) 
  pRegFlushVec := brFlushVec | ldFlushVec
  
  io.pReg := pReg
}

