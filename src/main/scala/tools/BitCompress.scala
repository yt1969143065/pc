package tools

import chisel3._
import chisel3.util._

object BitCompress {
  def apply(inBits : UInt) : (Vec[Bool], Vec[UInt])  = {
    val CompNum = inBits.getWidth
    val outBitVec = Wire(Vec(CompNum, Bool()))
    val outIdxVec = Wire(Vec(CompNum, UInt(log2Up(CompNum).W)))
    for(i <- 0 until CompNum) {
      if(i == 0) {
        outBitVec(i) := inBits.orR 
        outIdxVec(i) := PriorityEncoder(inBits) 
      } else {
        outBitVec(i) := PopCount(inBits) > i.U 
        outIdxVec(i) := Mux(outBitVec(i-1), PriorityEncoder(inBits & (~0.U(CompNum.W) << outIdxVec(i-1))), 0.U )
      }
  }
  (outBitVec, outIdxVec)
  }
}

