package scheduler

import chisel3._
import chisel3.util._

import tools._
import difftest._

class Iid extends Module with CoreParameters {
  val io = IO(new Bundle {
    val poor        = Output(Bool())
    val free        = Output(Bool())
    val iidOut      = Output(Vec(PoolInNum, UInt(IidWidth.W)))
    val allocate    = Input (Vec(PoolInNum, new MicroOp))
    val release     = Output(Vec(PoolInNum, new MicroOp))
    val redirect    = Input (new Redirect)
    val iidWb       = Input (Vec(13, new MicroOp))
    val iidExcp     = Input (Vec(4, new MicroOp)) 
    val flush       = Output(Bool())
    val excp        = Output(new ExcpBundle)
    val envRegRdAddr = if(EnableDifftest) Some(Output(Vec(32, UInt(PRegWidth.W)))) else None
    val envRegRdData = if(EnableDifftest) Some(Input (Vec(32, UInt(64.W)))) else None
  })  

  //input
  val allocate = io.allocate
  val redirect = io.redirect
  val iidWb = io.iidWb
  val iidExcp = io.iidExcp
  //output
  val poor = RegInit(false.B)
  val free = Wire(Bool())
  val iidOut = Wire(Vec(PoolInNum, UInt(IidWidth.W)))
  val release = Wire(Vec(PoolInNum, new MicroOp))
  val flush = RegInit(false.B)
  val excp = RegInit(0.U.asTypeOf(new ExcpBundle))
  io.poor := poor
  io.free := free
  io.iidOut := iidOut
  io.release := release
  io.flush := flush
  io.excp := excp

  //debug==========================================
  val releaseD2 = RegNext(RegNext(release))
  when(releaseD2.map(_.valid).reduce(_ || _)){
    val pc = releaseD2(0).pc
    printf("%x:  ", pc) 
    for(i <- 0 until 8){ 
      val iid = releaseD2(i).iid
      when(releaseD2(i).valid && releaseD2(i).flush){
        printf("\\033[31m%d \\033[0m", iid)
      }.elsewhen(releaseD2(i).valid){
        printf(p"$iid ")
      }.otherwise{
        printf("... ")
      }   
    }   
    printf("\n")
  }
  //===============================================

  val tailPtr  = RegInit(VecInit((0 until PoolInNum).map(_.U(IidWidth.W))))
  val headPtr  = RegInit(VecInit((0 until PoolInNum).map(_.U(IidWidth.W))))
  val valid    = RegInit(0.U(IidNum.W))
  val wbVld    = RegInit(0.U(IidNum.W))
  val flushed  = RegInit(0.U(IidNum.W))
  val excpVld  = RegInit(0.U(IidNum.W))
  val excpCode = Reg    (Vec(IidNum, UInt(ExcpWidth.W)))
  val iidEntries = RegInit(VecInit.fill(IidNum)(0.U.asTypeOf(new MicroOp)))

  free := !valid.orR

  for(i <- 0 until PoolInNum) {
    when(allocate.map(_.valid).reduce(_ || _)) {
      tailPtr(i) := tailPtr(i) + PopCount(VecInit(allocate.map(_.valid)).asUInt) 
    }   
    when(release(0).valid) {
      headPtr(i) := headPtr(i) + PopCount(VecInit(release.map(_.valid)).asUInt) 
    }   
  }
  val counter = RegInit(0.U(IidWidth.W))
  val counterNxt = Wire(UInt(IidWidth.W))
  counterNxt := counter - PopCount(VecInit(release.map(_.valid)).asUInt) + PopCount(VecInit(allocate.map(_.valid)).asUInt)
  counter := counterNxt
  poor :=  counterNxt > (IidNum - 8).U

  val shiftCnt = FirstSet(VecInit(allocate.map(_.valid)).asUInt)
  iidOut := BarrelShifter.rightShift(tailPtr, shiftCnt)


  val validIn  = (0 until PoolInNum).map(i => Mux(allocate(i).valid, UIntToOH(allocate(i).iid), 0.U)).reduce(_ | _)
  val validOut = (0 until PoolInNum).map(i => Mux(release (i).valid, UIntToOH(release (i).iid), 0.U)).reduce(_ | _)
  valid := (valid | validIn) & ~validOut

  for(i <- 0 until IidNum) {
    val upVld = VecInit((0 until PoolInNum).map(j => allocate(j).valid && allocate(j).iid === i.U)).asUInt
    when(upVld.orR) {
      iidEntries(i) := Mux1H(upVld, allocate)
    }
  }

  val wbVec = (0 until 13).map(i => Mux(iidWb(i).valid, UIntToOH(iidWb(i).iid), 0.U)).reduce(_ | _)
  val inWbVld  = (0 until PoolInNum).map(i => Mux(allocate(i).valid && allocate(i).wbVld, UIntToOH(allocate(i).iid), 0.U)).reduce(_ | _)
  wbVld := (wbVld | wbVec | inWbVld) & ~validOut & ~(validIn & ~inWbVld)

  val excpVec = (0 until 4).map(i => Mux(iidExcp(i).valid, UIntToOH(iidExcp(i).iid), 0.U)).reduce(_ | _)
  val excpIn = (0 until 8).map(i => Mux(allocate(i).valid && allocate(i).wbVld, UIntToOH(allocate(i).iid), 0.U)).reduce(_ | _)
  excpVld := (excpVld | excpIn | excpVec) & ~(validIn & ~excpIn) & ~validOut
  for(i <- 0 until IidNum) {
    val alcVld = VecInit((0 until 8).map(j => allocate(j).valid && allocate(j).excpVld && allocate(j).iid === i.U)).asUInt
    val upVld = VecInit((0 until 4).map(j => iidExcp(j).valid && iidExcp(j).iid === i.U)).asUInt
    when(upVld.orR) {
      excpCode(i) := Mux1H(upVld, iidExcp.map(_.excpCode))
    }.elsewhen(alcVld.orR){
      excpCode(i) := Mux1H(alcVld, allocate.map(_.excpCode))
    }
  }

  for(i <- 0 until PoolInNum) {
    release(i) := iidEntries(headPtr(i))
    if(i == 0) {
      release(i).valid := valid(headPtr(i)) && (wbVld(headPtr(i)) || flushed(headPtr(i)) || flush)
    } else {
      release(i).valid := release(i-1).valid && valid(headPtr(i)) && !excpVld((headPtr(i))) && (wbVld(headPtr(i)) && !(flushed(headPtr(i-1))) || flushed(headPtr(i)) || flush)
    }
    release(i).flush := flushed(headPtr(i)) || flush
    release(i).iid := headPtr(i)
  }

  //when (release(0).valid && excpVld(headPtr(0)) && excpCode(headPtr(0)) === 2.U) {
  //  flush := true.B
  //}.otherwise {
  //  flush := false.B
  //}
  flush := 0.U

  val rls0excpVld = excpVld(headPtr(0))
  when (release(0).valid && rls0excpVld) {
    excp.valid := true.B
  }.otherwise {
    excp.valid := false.B
  }

  when (release(0).valid && rls0excpVld) {
    excp.ecode := excpCode(headPtr(0))
    excp.epc   := release(0).pc
  }

  excp.tval := 0.U

  val brFlushVec   = Mux(redirect.brRBK, VecInit((0 until IidNum).map(i => redirect.flushedBid(iidEntries(i).bid) && valid(i))).asUInt, 0.U)
  val ldFlushVec   = Mux(redirect.ldRBK, VecInit((0 until IidNum).map(i => (redirect.flushedLid(iidEntries(i).lid) || iidEntries(i).isLd && iidEntries(i).lid === redirect.rbkLid) && valid(i))).asUInt, 0.U)
  val excpFlushVec = Mux(flush, valid, 0.U)
  flushed := (flushed | brFlushVec | ldFlushVec | excpFlushVec) & ~validIn & ~validOut

  val pReg  = RegInit(VecInit((0 until 32).map(i => i.U(PRegWidth.W))))
  val pReg1 = RegInit(VecInit((0 until 32).map(i => i.U(PRegWidth.W))))
  val pReg1NeedUp = (VecInit(release.map(_.valid)).asUInt & ~ VecInit(release.map(_.flush)).asUInt) =/= 0.U

  for(i <- 0 until 32) {
    val upVld = VecInit((0 until 8).map(j => release(j).valid && release(j).dstVld && release(j).dstReg === i.U)).asUInt
    when(upVld.orR){
      pReg(i) := PriorityMux(Reverse(upVld), release.map(_.pReg).reverse)
    }
    val upVld1 = VecInit((0 until 8).map(j => release(j).valid && !release(j).flush && release(j).dstVld && release(j).dstReg === i.U)).asUInt
    when(pReg1NeedUp) {
      pReg1(i) := Mux(upVld1.orR, PriorityMux(Reverse(upVld1), release.map(_.pReg).reverse), pReg(i))
    }
  }
  val hasFLushRls = RegNext((VecInit(release.map(_.valid)).asUInt ^ VecInit(release.map(_.flush)).asUInt) =/= VecInit(release.map(_.valid)).asUInt)
  io.envRegRdAddr.get := Mux(hasFLushRls,  pReg1, pReg)

  if(EnableDifftest){
    for(i <- 0 until 8){
      val difftest = Module(new DifftestInstrCommit)
      difftest.io.clock  := clock
      difftest.io.valid  := RegNext(RegNext(release(i).valid && !release(i).flush ))
      difftest.io.index  := i.U
      difftest.io.wdest  := RegNext(RegNext(release(i).dstReg                     ))
      difftest.io.wpdest := RegNext(RegNext(release(i).pReg                       ))
      difftest.io.rfwen  := RegNext(RegNext(release(i).dstVld && !release(i).dstx0))
      difftest.io.pc     := RegNext(RegNext(release(i).pc                         ))
      difftest.io.instr  := RegNext(RegNext(release(i).inst                       ))
    }
  }
  if(EnableDifftest){
    val difftest = Module(new DifftestTrapEvent)
      difftest.io.clock := clock
  }
  if(EnableDifftest){
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock := clock
    difftest.io.gpr    := io.envRegRdData.get
  }
}

