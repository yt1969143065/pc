package scheduler

import chisel3._
import chisel3.util._

import tools._

class Sid extends Module with CoreParameters {
  val io = IO(new Bundle {
    val poor     = Output(Bool())
    val free     = Output(Bool())
    val stall    = Input(Bool())
    val allocate = Input (Vec(PoolInNum, new MicroOp))
    val sidOut   = Output(Vec(PoolInNum, UInt(SidWidth.W)))
    val release  = Input (UInt(PoolInNum.W))
    val stQtailPtr = Output(UInt(LidWidth.W))
  })  
  //input
  val stall = io.stall
  val allocate = io.allocate
  val release = io.release
  //output
  val poor = RegInit(false.B)
  val free = Wire(Bool())
  val sidOut = Wire(Vec(PoolInNum, UInt(SidWidth.W)))
  io.poor := poor
  io.free := free
  io.sidOut := sidOut

  
  val counter   = RegInit(0.U((SidWidth+1).W))
  val tailPtr   = RegInit(VecInit((0 until 5).map(_.U(SidWidth.W))))
  val headPtr   = RegInit(VecInit((0 until PoolInNum).map(_.U(SidWidth.W))))

  val allocateVld = Wire(UInt(4.W))
  val counterNxt = Wire(UInt((SidWidth+1).W))
  val allocateVldVec = VecInit(allocate.map(i => i.valid && i.isSt)).asUInt
  allocateVld := VecInit((0 until 4).map(i => PopCount(allocateVldVec) > i.U)).asUInt

  counterNxt := counter - PopCount(release) + PopCount(allocateVld)
  counter := counterNxt
  poor :=  counterNxt > (SidNum - 4).U

  free := counter === 0.U 

  for(i <- 0 until 5) {
    when(allocateVld(0)) {
      tailPtr(i) := tailPtr(i) + PopCount(allocateVldVec) 
    }   
  }
  for(i <- 0 until PoolInNum) {
    when(release(0)) {
      headPtr(i) := headPtr(i) + PopCount(release) 
    }   
  }


  for(i <- 0 until PoolInNum) {
    sidOut(i) := Mux(PopCount(allocateVldVec(i, 0)) > 3.U, Mux(allocateVldVec(i), tailPtr(3), tailPtr(4)),
                 Mux(PopCount(allocateVldVec(i, 0)) > 2.U, Mux(allocateVldVec(i), tailPtr(2), tailPtr(3)),
                 Mux(PopCount(allocateVldVec(i, 0)) > 1.U, Mux(allocateVldVec(i), tailPtr(1), tailPtr(2)),
                 Mux(PopCount(allocateVldVec(i, 0)) > 0.U, Mux(allocateVldVec(i), tailPtr(0), tailPtr(1)),
                 tailPtr(0)))))
  }

  io.stQtailPtr := tailPtr(0)

}


