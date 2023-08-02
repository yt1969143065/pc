package rmu 

import chisel3._
import chisel3.util._

import tools._


class ReleaseList extends Module with CoreParameters {
  val io = IO(new Bundle {
    val redirect       = Input (new Redirect)
    val stopNewRename  = Output(Bool())
    val allocate       = Input (Vec(PoolInNum, new MicroOp))
    val release        = Output(Vec(PoolInNum, new MicroOp))
    val iidRls         = Input (Vec(PoolInNum, new MicroOp)) 
    val regRdAck       = Input (Vec(PoolInNum, Bool()))
    val regRdReq       = Output(Vec(PoolInNum, new MicroOp))
    val regRdRsp       = Input (Vec(PoolInNum, UInt(64.W))) 
    val regWrAck       = Input (Vec(PoolInNum, Bool()))
    val regWrReq       = Output(Vec(PoolInNum, new MicroOp))
    val wakeAck        = Input (Vec(PoolInNum, Bool()))
    val wakeReq        = Output(Vec(PoolInNum, new MicroOp))
    val wakeData       = Output(Vec(PoolInNum,  UInt(64.W)))
    val pReg           = Input (Vec(32, UInt(PRegWidth.W)))
  })  

  //input
  val redirect = io.redirect.ldRBK || io.redirect.brRBK
  val allocate = io.allocate
  val iidRls = io.iidRls
  val regRdAck = io.regRdAck
  val regRdRsp = io.regRdRsp
  val regWrAck = io.regWrAck
  val regChainRealCurPReg = io.pReg
  //output
  val stopNewRename = RegInit(false.B)
  val release       = Wire   (Vec(PoolInNum, new MicroOp))
  val regRdReq      = Wire   (Vec(PoolInNum, new MicroOp))
  val regWrReq      = Wire   (Vec(PoolInNum, new MicroOp))
  val wakeReq = Wire(Vec(WbNum, new MicroOp))
  val wakeData = Wire(Vec(WbNum, UInt(64.W)))
  io.stopNewRename := stopNewRename
  io.release := release
  io.regRdReq := regRdReq
  io.regWrReq := regWrReq
  io.wakeReq := wakeReq
  io.wakeData := wakeData

  val confirmed = Wire(UInt(PRegNum.W))
  val flushed   = Wire(UInt(PRegNum.W))

  val headPtr       = RegInit(VecInit((0 until PoolInNum).map(_.U(PRegWidth.W) ))) 
  val tailPtr       = RegInit(VecInit((0 until PoolInNum).map(_.U(PRegWidth.W) ))) 
  val valid = RegInit(0.U(PRegNum.W))
  val releaseList          = RegInit(VecInit.fill(PRegNum)(0.U.asTypeOf(new MicroOp)))
  val releaseListFlushed   = RegInit(0.U(PRegNum.W))
  val releaseListConfirmed = RegInit(0.U(PRegNum.W))

  val validIn   = Wire(UInt(PRegNum.W))
  val validOut  = Wire(UInt(PRegNum.W))  

  val select          = Wire(Vec(PoolInNum, new MicroOp))
  val selectConfirmed = Wire(Vec(PoolInNum, Bool()))
  val selectFlushed   = Wire(Vec(PoolInNum, Bool()))
  val rls             = RegInit(VecInit.fill(PoolInNum)(0.U.asTypeOf(new MicroOp)))

  class QBundle extends Bundle with CoreParameters{
    val curPReg  = Vec(32, UInt(PRegWidth.W)) 
    val markTail = UInt(PRegWidth.W)
  }   
  val curPRegQ = Module(new Queue(new QBundle, 2)) 

  val regChainCurPReg = curPRegQ.io.deq.bits.curPReg
  val regChainOldPReg = RegInit(VecInit((0 until 32).map(_.U(PRegWidth.W))))
  val regChainData = Reg(Vec(32, UInt(64.W)))
  val idle :: waitRd :: waitWr :: waitWake :: noAction :: Nil = Enum(5) //5 sate
  val regChainState = RegInit(VecInit.fill(32)(idle))

  stopNewRename := !curPRegQ.io.enq.ready
  curPRegQ.io.enq.valid := redirect
  curPRegQ.io.enq.bits.curPReg := regChainRealCurPReg
  curPRegQ.io.enq.bits.markTail := tailPtr(0) + PopCount(VecInit(allocate.map(i => i.valid && i.dstVld && !i.dstx0)).asUInt)

  val waitRdVec = VecInit(regChainState.map(_ === waitRd)).asUInt
  val waitWrVec = VecInit(regChainState.map(_ === waitWr)).asUInt
  val waitWakeVec = VecInit(regChainState.map(_ === waitWake)).asUInt

  val markTailPtr = curPRegQ.io.deq.bits.markTail
  val stopNewConfirm = markTailPtr === headPtr(0) && (waitRdVec.orR || waitWrVec.orR || waitWakeVec.orR)
  curPRegQ.io.deq.ready := markTailPtr === headPtr(0) && !waitRdVec.orR && !waitWrVec.orR && !waitWakeVec.orR

  //releaseList
  val (inRealVld, inRealIdx) = BitCompress(VecInit(allocate.map(i => i.valid && i.dstVld && !i.dstx0)).asUInt)

  for(i <- 0 until PoolInNum) {
    when(inRealVld(i)) {
      releaseList(tailPtr(i)) := allocate(inRealIdx(i))
    }
    when(inRealVld.reduce(_ || _)) {
      tailPtr(i) := tailPtr(i) + PopCount(VecInit(allocate.map(i => i.valid && i.dstVld && !i.dstx0)).asUInt)
    }
  }

  confirmed := VecInit((0 until PRegNum).map(i =>
    (0 until PoolInNum).map(j => iidRls(j).valid && iidRls(j).iid === releaseList(i).iid && valid(i) && !iidRls(j).flush).reduce(_ || _)
    )).asUInt
  flushed := VecInit((0 until PRegNum).map(i =>
    (0 until PoolInNum).map(j => iidRls(j).valid && iidRls(j).iid === releaseList(i).iid && valid(i) && iidRls(j).flush).reduce(_ || _)
    )).asUInt

  releaseListFlushed   := (releaseListFlushed   | flushed  ) & ~validOut
  releaseListConfirmed := (releaseListConfirmed | confirmed) & ~validOut

  validIn  := (0 until PoolInNum).map(i => Mux(inRealVld(i), UIntToOH(tailPtr(i)), 0.U)).reduce(_ | _)
  validOut := (0 until PoolInNum).map(i => Mux(select(i).valid, UIntToOH(headPtr(i)), 0.U)).reduce(_ | _)
  valid := (valid | validIn) & ~validOut

  //regChain
  for(i <- 0 until PoolInNum) {
    if(i == 0) {
      selectFlushed(i) := releaseListFlushed(headPtr(i))
      selectConfirmed(i) := releaseListConfirmed(headPtr(i))
      select(i) :=  releaseList(headPtr(i))
      select(i).valid :=  valid(headPtr(i)) && (selectFlushed(i) || selectConfirmed(i)) && (!stopNewConfirm  || headPtr(i) =/= markTailPtr)
    } else {
      selectFlushed(i) := releaseListFlushed(headPtr(i))
      selectConfirmed(i) := releaseListConfirmed(headPtr(i))
      select(i) :=  releaseList(headPtr(i))
      select(i).valid :=  select(i-1).valid && valid(headPtr(i)) && (selectFlushed(i) || selectConfirmed(i)) && headPtr(i) =/= markTailPtr
    }

    when(select(0).valid) {
      headPtr(i) := headPtr(i) + PopCount(VecInit(select.map(_.valid)).asUInt)
    }
  }

  rls := select

  for(i <- 0 until PoolInNum) {
    release(i) := DontCare
    release(i).valid := rls(i).valid
    release(i).pReg := rls(i).oldPReg
  }
  val rdReqVec  = Wire(UInt(32.W))
  val rdReqPReg0 = Wire(UInt(32.W))
  val rdReqAck   = io.regRdAck
  val rdReqVld   = Wire(Vec(PoolInNum, Bool()))
  val rdReqLReg  = Wire(Vec(PoolInNum, UInt(5.W)))
  val rdReqPReg  = Wire(Vec(PoolInNum, UInt(PRegWidth.W)))
  val rdReqVldD  = RegInit(VecInit(Seq.fill(PoolInNum)(false.B)))
  val rdReqLRegD = Reg(Vec(PoolInNum, UInt(5.W)))
  val rdReqPRegD = Reg(Vec(PoolInNum, UInt(PRegWidth.W)))
  rdReqVec := waitRdVec
  rdReqPReg0 := VecInit(regChainOldPReg.map(_(0))).asUInt

  val wrReqVec  = Wire(UInt(32.W))
  val wrReqPReg0 = Wire(UInt(PRegNum.W))
  val wrReqAck  = io.regWrAck
  val wrReqVld  = Wire(Vec(PoolInNum, Bool()))
  val wrReqLReg = Wire(Vec(PoolInNum, UInt(5.W)))
  val wrReqPReg = Wire(Vec(PoolInNum, UInt(PRegWidth.W)))
  //val wrReqAckD  = RegInit(VecInit.fill(PoolInNum)(false.B))
  val wrReqVldD  = RegInit(VecInit(Seq.fill(PoolInNum)(false.B)))
  val wrReqLRegD = Reg(Vec(PoolInNum, UInt(5.W)))
  val wrReqPRegD = Reg(Vec(PoolInNum, UInt(PRegWidth.W)))
  wrReqVec := waitWrVec
  wrReqPReg0 := VecInit(regChainCurPReg.map(_(0))).asUInt

  val wakeReqVec  = Wire(UInt(32.W))
  val wakeReqPReg0 = Wire(UInt(PRegNum.W))
  val wakeReqAck  = io.wakeAck
  val wakeReqVld  = Wire(Vec(PoolInNum, Bool()))
  val wakeReqLReg = Wire(Vec(PoolInNum, UInt(5.W)))
  val wakeReqPReg = Wire(Vec(PoolInNum, UInt(PRegWidth.W)))
  //val wakeReqAckD  = RegInit(VecInit.fill(PoolInNum)(false.B))
  val wakeReqVldD  = RegInit(VecInit(Seq.fill(PoolInNum)(false.B)))
  val wakeReqLRegD = Reg(Vec(PoolInNum, UInt(5.W)))
  val wakeReqLRegD2 = Reg(Vec(PoolInNum, UInt(5.W)))
  val wakeReqPRegD = Reg(Vec(PoolInNum, UInt(PRegWidth.W)))
  wakeReqVec := waitWakeVec
  wakeReqPReg0 := VecInit(regChainCurPReg.map(_(0))).asUInt

  for(i <- 0 until 32) {
    val rlsUpVld = VecInit((0 until PoolInNum).map(j => select(j).valid && select(j).dstReg === i.U && selectConfirmed(j))).asUInt
    when(rlsUpVld.orR) {
      regChainOldPReg(i) := VecInit(select.map(_.pReg))(LastSet(rlsUpVld))
    }.elsewhen(curPRegQ.io.deq.fire && regChainState(i) === noAction){
      regChainOldPReg(i) := regChainCurPReg(i)
    }

    val rdDone   = VecInit((0 until PoolInNum).map(j => rdReqVldD(j) && rdReqLRegD(j) === i.U)).asUInt
    val wrDone   = VecInit((0 until PoolInNum).map(j => wrReqVldD(j) && wrReqLRegD(j) === i.U)).asUInt
    val wakeDone   = VecInit((0 until PoolInNum).map(j => wakeReqVldD(j) && wakeReqLRegD(j) === i.U)).asUInt
    val flushVld = VecInit((0 until PoolInNum).map(j => select(j).valid && select(j).dstReg === i.U && selectFlushed(j))).asUInt

    switch(regChainState(i)) {
      is (idle) {
        when(flushVld.orR) {
          regChainState(i) := waitRd
        }
      }
      is (waitRd) {
        when(rdDone.orR) {
          regChainState(i) := waitWr
        }
      }
      is (waitWr) {
        when(wrDone.orR) {
          regChainState(i) := waitWake
        }
      }
      is (waitWake) {
        when(wakeDone.orR) {
          regChainState(i) := noAction
        }
      }
      is (noAction) {
        when(markTailPtr === headPtr(0) && !stopNewConfirm && !flushVld.orR) {
          regChainState(i) := idle
        }.elsewhen(markTailPtr === headPtr(0) && !stopNewConfirm && flushVld.orR){
          regChainState(i) := waitRd
        }
      }
    }
    when (rdDone.orR) {
      regChainData(i) := Mux1H(rdDone, regRdRsp)
    }
  }
  for(i <- 0 until PoolInNum) {
    val MSB = 64/PoolInNum * (i/2+1) - 1
    val LSB = 64/PoolInNum * (i/2)
    val allRdReqVec = Wire(UInt(8.W))
    allRdReqVec  := rdReqVec(MSB, LSB) & ~Mux(rdReqVldD(i/2*2  ), UIntToOH(rdReqLRegD(i/2*2  ))(MSB, LSB), 0.U)  &
                                         ~Mux(rdReqVldD(i/2*2+1), UIntToOH(rdReqLRegD(i/2*2+1))(MSB, LSB), 0.U)
    if(i%2==0) {
      rdReqVld(i)  := allRdReqVec.orR
      rdReqLReg(i) := PriorityEncoder(allRdReqVec) + LSB.U
      rdReqPReg(i) := PriorityMux    (allRdReqVec, regChainOldPReg.slice(LSB, MSB+1))
    }else {
      rdReqVld(i)  := PopCount(allRdReqVec) > 1.U
      rdReqLReg(i) := MSB.U - PriorityEncoder(Reverse(allRdReqVec))
      rdReqPReg(i) := PriorityMux(Reverse(allRdReqVec), regChainOldPReg.slice(LSB, MSB+1).reverse)
    }

    val evenWrReqVec = Wire(UInt(8.W))
    val  oddWrReqVec = Wire(UInt(8.W))
    evenWrReqVec := wrReqVec(MSB, LSB) & ~wrReqPReg0 (MSB, LSB) & ~Mux(wrReqVldD(i), UIntToOH(wrReqLRegD(i))(MSB, LSB), 0.U)
     oddWrReqVec := wrReqVec(MSB, LSB) &  wrReqPReg0 (MSB, LSB) & ~Mux(wrReqVldD(i), UIntToOH(wrReqLRegD(i))(MSB, LSB), 0.U)
    if(i%2==0) {
      wrReqVld(i)  := evenWrReqVec.orR
      wrReqLReg(i) := PriorityEncoder(evenWrReqVec) + LSB.U
      wrReqPReg(i) := PriorityMux    (evenWrReqVec, regChainCurPReg.slice(LSB, MSB+1))
    }else {
      wrReqVld(i)  := oddWrReqVec.orR
      wrReqLReg(i) := PriorityEncoder(oddWrReqVec) + LSB.U
      wrReqPReg(i) := PriorityMux    (oddWrReqVec, regChainCurPReg.slice(LSB, MSB+1))
    }

    regRdReq(i) := DontCare
    regRdReq(i).valid := rdReqVld(i)
    regRdReq(i).pReg  := rdReqPReg(i)
    regWrReq(i) := DontCare
    regWrReq(i).valid  := wrReqVld(i)
    regWrReq(i).pReg   := wrReqPReg(i)
    regWrReq(i).dstData := regChainData(wrReqLReg(i))

    val evenWakeReqVec = Wire(UInt(8.W))
    val  oddWakeReqVec = Wire(UInt(8.W))
    evenWakeReqVec := wakeReqVec(MSB, LSB) & ~wakeReqPReg0 (MSB, LSB) & ~Mux(wakeReqVldD(i), UIntToOH(wakeReqLRegD(i))(MSB, LSB), 0.U)
     oddWakeReqVec := wakeReqVec(MSB, LSB) &  wakeReqPReg0 (MSB, LSB) & ~Mux(wakeReqVldD(i), UIntToOH(wakeReqLRegD(i))(MSB, LSB), 0.U)
    if(i%2==0) {
      wakeReqVld(i)  := evenWakeReqVec.orR
      wakeReqLReg(i) := PriorityEncoder(evenWakeReqVec) + LSB.U
      wakeReqPReg(i) := PriorityMux    (evenWakeReqVec, regChainCurPReg.slice(LSB, MSB+1))
    }else {
      wakeReqVld(i)  := oddWakeReqVec.orR
      wakeReqLReg(i) := PriorityEncoder(oddWakeReqVec) + LSB.U
      wakeReqPReg(i) := PriorityMux    (oddWakeReqVec, regChainCurPReg.slice(LSB, MSB+1))
    }

    regRdReq(i) := DontCare
    regRdReq(i).valid := rdReqVld(i)
    regRdReq(i).pReg  := rdReqPReg(i)

    regWrReq(i) := DontCare
    regWrReq(i).valid  := wrReqVld(i)
    regWrReq(i).pReg   := wrReqPReg(i)
    regWrReq(i).dstData := regChainData(wrReqLReg(i))

    wakeReq(i) := DontCare
    wakeReq(i).valid  := wakeReqVld(i)
    wakeReq(i).pReg   := wakeReqPReg(i)
    wakeData(i) := regChainData(wakeReqLRegD2(i))
  }

  rdReqVldD  := VecInit((0 until WbNum).map(i => rdReqVld(i) && rdReqAck(i)))
  rdReqLRegD := rdReqLReg
  rdReqPRegD := rdReqPReg

  wrReqVldD  := VecInit((0 until WbNum).map(i => wrReqVld(i) && wrReqAck(i)))
  wrReqLRegD := wrReqLReg
  wrReqPRegD := wrReqPReg

  wakeReqVldD  := VecInit((0 until WbNum).map(i => wakeReqVld(i) && wakeReqAck(i)))
  wakeReqLRegD := wakeReqLReg
  wakeReqPRegD := wakeReqPReg

  wakeReqLRegD2 := wakeReqLRegD

}

