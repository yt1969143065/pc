package scheduler

import chisel3._
import chisel3.util._

import tools._

class Lid extends Module with CoreParameters {
  val io = IO(new Bundle {
    val poor     = Output(Bool())
    val stall    = Input(Bool())
    val allocate = Input (Vec(PoolInNum, new MicroOp))
    val lidOut   = Output(Vec(PoolInNum, UInt(LidWidth.W)))
    val release  = Input (UInt(PoolInNum.W))
    val lidSafe  = Output(UInt(LidNum.W))
    val ldQtailPtr = Output(UInt(LidWidth.W))
    val lidRlsVec = Output(UInt(LidNum.W))
  })  
  //input
  val stall = io.stall
  val allocate = io.allocate
  val release = io.release
  //output
  val poor = RegInit(false.B)
  val lidOut = Wire(Vec(PoolInNum, UInt(LidWidth.W)))
  val lidSafe = Wire(UInt(LidNum.W))
  val lidRlsVec = Wire(UInt(LidNum.W))
  io.poor := poor
  io.lidOut := lidOut
  io.lidSafe := lidSafe
  io.lidRlsVec := lidRlsVec

  
  val counter   = RegInit(0.U((LidWidth+1).W))
  val valid     = RegInit(0.U(LidNum.W))
  val tailPtr   = RegInit(VecInit((0 until 5).map(_.U(LidWidth.W))))
  val headPtr   = RegInit(VecInit((0 until PoolInNum).map(_.U(LidWidth.W))))

  val allocateVld = Wire(UInt(4.W))
  val counterNxt = Wire(UInt((LidWidth+1).W))
  val allocateVldVec = VecInit(allocate.map(i => i.valid && i.isLd)).asUInt
  allocateVld := VecInit((0 until 4).map(i => PopCount(allocateVldVec) > i.U)).asUInt

  counterNxt := counter - PopCount(release) + PopCount(allocateVld)
  counter := counterNxt
  poor :=  counterNxt > (LidNum - 4).U

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

  val validIn  = (0 until 4        ).map(i => Mux(allocateVld(i), UIntToOH(tailPtr(i)), 0.U)).reduce(_ | _)
  val validOut = (0 until PoolInNum).map(i => Mux(release(i), UIntToOH(headPtr(i)), 0.U)).reduce(_ | _)
  valid := (valid | validIn) & ~validOut
  lidSafe := ~(valid & ~UIntToOH(headPtr(0)))
  lidRlsVec := validOut

  for(i <- 0 until PoolInNum) {
    lidOut(i) := Mux(PopCount(allocateVldVec(i, 0)) > 3.U, Mux(allocateVldVec(i), tailPtr(3), tailPtr(4)),
                 Mux(PopCount(allocateVldVec(i, 0)) > 2.U, Mux(allocateVldVec(i), tailPtr(2), tailPtr(3)),
                 Mux(PopCount(allocateVldVec(i, 0)) > 1.U, Mux(allocateVldVec(i), tailPtr(1), tailPtr(2)),
                 Mux(PopCount(allocateVldVec(i, 0)) > 0.U, Mux(allocateVldVec(i), tailPtr(0), tailPtr(1)),
                 tailPtr(0)))))
  }

  io.ldQtailPtr := tailPtr(0)

}

