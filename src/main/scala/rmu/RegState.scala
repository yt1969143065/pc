package rmu 

import chisel3._
import chisel3.util._

import tools._


class RegState extends Module with CoreParameters{
  val io = IO(new Bundle {
    val flush    = Input (UInt(PRegNum.W)) 
    val allocate = Input (Vec(PoolInNum, new MicroOp))  
    val wb       = Input (Vec(WbNum, new MicroOp))  
    val srcReq0  = Input (Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))  //MaxSrcNum = 2
    val srcRsp0  = Output(Vec(2, Vec(PoolInNum, Bool()))) 
    val srcReq   = Input (Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))  //MaxSrcNum = 2
    val srcRsp   = Output(Vec(2, Vec(PoolInNum, Bool()))) 
    val dstReq   = Input (Vec(PoolInNum, UInt(PRegWidth.W)))
    val dstRsp   = Output(Vec(PoolInNum, Bool()))
  })  

  //input
  val allocate = io.allocate
  val wb = io.wb
  val srcReq0 = io.srcReq0 
  val srcReq = io.srcReq 
  val dstReq = io.dstReq
  val flush = io.flush
  //output
  val srcRsp0 = Wire((Vec(2, Vec(PoolInNum, Bool()))))
  val srcRsp = Wire((Vec(2, Vec(PoolInNum, Bool()))))
  val dstRsp = Wire(Vec(PoolInNum, Bool()))
  io.srcRsp0 := srcRsp0
  io.srcRsp := srcRsp
  io.dstRsp := dstRsp

  val rdy = RegInit(VecInit(Seq.tabulate(PRegNum)(i => if (i > 0 && i < 32) true.B else false.B)).asUInt)

  val regAlloc = Wire(UInt(PRegNum.W))  
  val regWb    = Wire(UInt(PRegNum.W))  

  regAlloc := (0 until PoolInNum).map(i => Mux(allocate(i).valid && allocate(i).dstVld && !allocate(i).dstx0, UIntToOH(allocate(i).pReg), 0.U)).reduce(_ | _)
  regWb    := (0 until WbNum).map(i => Mux(wb(i).valid, UIntToOH(wb(i).pReg), 0.U)).reduce(_ | _)

  rdy := (rdy | regWb) & ~regAlloc & ~flush

  for(i <- 0 until 2) {
    for(j <- 0 until PoolInNum) {
      srcRsp0(i)(j) := (rdy | regWb)(srcReq0(i)(j)) 
      srcRsp (i)(j) := (rdy        )(srcReq (i)(j)) 
    }   
  }

  for(i <- 0 until PoolInNum) {
    dstRsp(i) := rdy(dstReq(i)) 
  }
}

