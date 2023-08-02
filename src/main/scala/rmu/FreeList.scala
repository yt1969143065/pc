package rmu 

import chisel3._
import chisel3.util._

import tools._


class FreeList extends Module with CoreParameters {
  val io = IO(new Bundle {
    val poor     = Output(Bool())
    val freePReg = Output(Vec(PoolInNum, UInt(PRegWidth.W)))
    val allocate = Input (Vec(PoolInNum, new MicroOp ))
    val release  = Input (Vec(PoolInNum, new MicroOp ))
  })  

  //input
  val allocate = io.allocate
  val release = io.release
  //output
  val poor = RegInit(false.B)
  val freePReg = RegInit(VecInit((0 until PoolInNum).map(i => if(i<4) (32+i).U(PRegWidth.W) else (131-i).U(PRegWidth.W)))) 

  //debug===============================================================
  /*  
  when(allocate.map(i => i.valid && i.dstVld && !i.dstx0).reduce(_ || _)){
    val pc = allocate(0).pc
    printf("%x: ", pc)
    for(i <- 0 until 8){
      val pReg = freePReg(i)
      when(allocate(i).valid && allocate(i).dstVld && !allocate(i).dstx0){
        printf(p"$pReg ")
      }.otherwise{
        printf("--- ")
      }
    }
    printf("\n")
  }
  when(release.map(_.valid).reduce(_ || _)){
    printf("release:  ")
    for(i <- 0 until 8){
      val pReg = release(i).pReg
      when(release(i).valid){
        printf(p"$pReg ")
      }.otherwise{
        printf("--- ")
      }
    }
    printf("\n")
  }
  */
  //==================================================================


  io.poor := poor
  io.freePReg := freePReg

  val freeListInitValue = VecInit((0 until PRegNum).map(i => if(i < 32) false.B else true.B)).asUInt
  val freeListNxt = Wire(Vec(PRegNum, Bool()))

  val freeList = RegInit(freeListInitValue)


  poor := (0 until PoolInNum/2).map( i =>  
          PopCount((0 until PRegNum).filter(_%4==i).map(freeList(_)===1.U)) < 2.U 
          ).reduce(_ || _)

  for(i <- 0 until PoolInNum/2) {
    val freeBit = (0 until PRegNum).filter(_%4==i).map(freeListNxt(_))
    val freeReg =  (0 until PRegNum).filter(_%4==i).map(_.asUInt) 
    freePReg(i) := PriorityMux(freeBit, freeReg)
    freePReg(PoolInNum - 1 - i) := PriorityMux(freeBit.reverse, freeReg.reverse)
  }

  for(i <- 0 until PRegNum) {
    val allocIdx0   =   i % (PoolInNum/2)  
    val allocIdx1   = -(i % (PoolInNum/2)) + 7 
    freeListNxt(i) := (freeList(i) ||
      release(0).valid && UIntToOH(release(0).pReg)(i) ||
      release(1).valid && UIntToOH(release(1).pReg)(i) ||
      release(2).valid && UIntToOH(release(2).pReg)(i) ||
      release(3).valid && UIntToOH(release(3).pReg)(i) ||
      release(4).valid && UIntToOH(release(4).pReg)(i) ||
      release(5).valid && UIntToOH(release(5).pReg)(i) ||
      release(6).valid && UIntToOH(release(6).pReg)(i) ||
      release(7).valid && UIntToOH(release(7).pReg)(i) ) &&
    ~(allocate(allocIdx0).valid && allocate(allocIdx0).dstVld && !allocate(allocIdx0).dstx0 && UIntToOH(freePReg(allocIdx0))(i)  ||
      allocate(allocIdx1).valid && allocate(allocIdx1).dstVld && !allocate(allocIdx1).dstx0 && UIntToOH(freePReg(allocIdx1))(i) )
  }

  freeList := freeListNxt.asUInt
}

