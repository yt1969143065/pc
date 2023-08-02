package rmu 

import chisel3._
import chisel3.util._

import tools._


class RMU extends Module with CoreParameters{
  val io = IO(new Bundle {
    val rmuStall          = Output(Bool())
    val redirect          = Input (new Redirect) 
    val freePReg          = Output(Vec(PoolInNum, UInt(PRegWidth.W)))
    val allocate          = Input (Vec(PoolInNum, new MicroOp))
    val dstRdAddr         = Input (Vec(PoolInNum, UInt(5.W)))
    val dstRdPReg         = Output(Vec(PoolInNum, UInt(PRegWidth.W)))
    val dstRdRdy          = Output(Vec(PoolInNum, Bool()))
    val dstUpdate         = Input (Vec(PoolInNum, new MicroOp))
    val enpool            = Input (Vec(PoolInNum, new MicroOp))
    val srcRdAddr0        = Input (Vec(2, Vec(PoolInNum, UInt(5.W))))
    val srcRdPReg0        = Output(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
    val srcRdRdy0         = Output(Vec(2, Vec(PoolInNum, Bool()))) 
    val srcRdAddr         = Input (Vec(2, Vec(PoolInNum, UInt(5.W))))
    val srcRdPReg         = Output(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
    val srcRdRdy          = Output(Vec(2, Vec(PoolInNum, Bool()))) 
    val wb                = Input (Vec(WbNum, new MicroOp))  
    val iidRls            = Input (Vec(PoolInNum, new MicroOp)) 
    val regRdAck          = Input (Vec(PoolInNum, Bool()))
    val regRdReq          = Output(Vec(PoolInNum, new MicroOp))
    val regRdRsp          = Input (Vec(PoolInNum, UInt(64.W))) 
    val regWrAck          = Input (Vec(PoolInNum, Bool()))
    val regWrReq          = Output(Vec(PoolInNum, new MicroOp))
    val wakeAck           = Input (Vec(PoolInNum, Bool()))
    val wakeReq           = Output(Vec(PoolInNum, new MicroOp))
    val wakeData          = Output(Vec(PoolInNum, UInt(64.W))) 
    val lidRlsVec         = Input (UInt(LidNum.W))
    val bidRlsVec         = Input (UInt(BidNum.W))
  })  

  //input
  val redirect = io.redirect
  val dstRdAddr = io.dstRdAddr
  val allocate = io.allocate
  val enpool = io.enpool
  val dstUpdate = io.dstUpdate
  val srcRdAddr0 = io.srcRdAddr0
  val srcRdAddr = io.srcRdAddr
  val wb = io.wb
  val iidRls = io.iidRls
  val regRdAck = io.regRdAck
  val regRdRsp = io.regRdRsp
  val regWrAck = io.regWrAck
  val wakeAck = io.wakeAck

  //output
  val rmuStall = Wire(Bool())
  val dstRdPReg = Wire(Vec(PoolInNum, UInt(PRegWidth.W)))
  val dstRdRdy = Wire(Vec(PoolInNum, Bool()))
  val srcRdPReg0 = Wire(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
  val srcRdPReg = Wire(Vec(2, Vec(PoolInNum, UInt(PRegWidth.W))))
  val srcRdRdy0 = Wire(Vec(2, Vec(PoolInNum, Bool()))) 
  val srcRdRdy = Wire(Vec(2, Vec(PoolInNum, Bool()))) 
  val freePReg = Wire(Vec(PoolInNum, UInt(PRegWidth.W)))
  val regRdReq = Wire(Vec(PoolInNum, new MicroOp))
  val regWrReq = Wire(Vec(PoolInNum, new MicroOp))
  val wakeReq = Wire(Vec(PoolInNum, new MicroOp))
  val wakeData = Wire(Vec(PoolInNum, UInt(64.W)))
  io.rmuStall := rmuStall
  io.dstRdPReg := dstRdPReg
  io.dstRdRdy := dstRdRdy
  io.srcRdPReg0 := srcRdPReg0
  io.srcRdPReg := srcRdPReg
  io.srcRdRdy0 := srcRdRdy0
  io.srcRdRdy := srcRdRdy
  io.freePReg := freePReg
  io.regRdReq := regRdReq
  io.regWrReq := regWrReq
  io.wakeReq := wakeReq
  io.wakeData := wakeData
  
  val regState    = Module (new RegState)
  val iRAT        = Module (new RAT)
  val freeList    = Module (new FreeList)
  val releaseList = Module (new ReleaseList)

  //regState
  regState.io.allocate := allocate
  regState.io.srcReq0  := iRAT.io.srcRdData0
  regState.io.srcReq   := iRAT.io.srcRdData
  regState.io.dstReq   := iRAT.io.dstRdData
  regState.io.wb       := VecInit((0 until 8).map(i => Mux(wb(i).valid, wb(i), wakeReq(i))))
  regState.io.flush    := iRAT.io.pRegFlushVec
  //iRAT
  iRAT.io.redirect  := redirect
  iRAT.io.dstRdAddr := dstRdAddr
  iRAT.io.dstUpdate := dstUpdate
  iRAT.io.srcRdAddr0 := srcRdAddr0
  iRAT.io.srcRdAddr := srcRdAddr
  iRAT.io.lidRlsVec := io.lidRlsVec
  iRAT.io.bidRlsVec := io.bidRlsVec
  //freeList
  freeList.io.release     := releaseList.io.release
  freeList.io.allocate    := allocate
  //releaseList
  releaseList.io.redirect := redirect
  releaseList.io.allocate := enpool
  releaseList.io.iidRls   := iidRls
  releaseList.io.regRdAck := regRdAck
  releaseList.io.regRdRsp := regRdRsp
  releaseList.io.regWrAck := regWrAck
  releaseList.io.wakeAck  := wakeAck
  releaseList.io.pReg     := iRAT.io.pReg


  srcRdPReg0 := iRAT.io.srcRdData0
  srcRdRdy0  := regState.io.srcRsp0
  dstRdPReg  := iRAT.io.dstRdData
  dstRdRdy   := regState.io.dstRsp

  rmuStall  := freeList.io.poor || releaseList.io.stopNewRename

  srcRdPReg0 := iRAT.io.srcRdData0
  srcRdPReg := iRAT.io.srcRdData
  srcRdRdy0  := regState.io.srcRsp0
  srcRdRdy  := regState.io.srcRsp

  freePReg  := freeList.io.freePReg

  regRdReq  := releaseList.io.regRdReq
  regWrReq  := releaseList.io.regWrReq
  wakeReq   := releaseList.io.wakeReq
  wakeData  := releaseList.io.wakeData

}

