package core

import chisel3._
import chisel3.util._

import tools._
import ifu._
import decode._
import scheduler._
import rmu._
import pools._
import exe._
import regfile._
import lsu._
import csr._
import axiif._
import difftest._

class Core extends Module with CoreParameters {
  val io = IO(new Bundle {
    //val ar   = DecoupledIO(new AxiArBundle)
    //val r    = Flipped(DecoupledIO(new AxiRBundle))
    //val aw   = DecoupledIO(new AxiAwBundle)
    //val w    = DecoupledIO(new AxiWBundle)
    //val b    = Flipped(DecoupledIO(new AxiBBundle))
    val axi = new AXI4
  })  

  val u_ifu       = Module(new IFU)
  val u_decode    = Module(new Decode ) 
  val u_scheduler = Module(new Scheduler)
  val u_rmu       = Module(new RMU)
  val u_aluPool   = Module(new AluPool)
  val u_mduPool   = Module(new MduPool)
  val u_lsu       = Module(new LSU)
  val u_alu       = Seq.fill(4)(Module (new ALU)) 
  val u_mdu       = Seq.fill(4)(Module (new MDU)) 
  val u_regfile   = Module(new RegFile)
  val u_csr       = Module(new CSR)
  val u_axiif     = Module(new AXIif)

  //IFU
  u_ifu.io.i_ar              <> u_axiif.io.i_ar
  u_ifu.io.i_r               <> u_axiif.io.i_r
  u_ifu.io.pcChange          := u_scheduler.io.pcChange
  u_ifu.io.stall             := u_decode.io.stallo
  u_ifu.io.bpUpVld           := u_scheduler.io.bpUpVld
  u_ifu.io.bpUpDir           := u_scheduler.io.bpUpDir

  //Decode
  u_decode.io.pcChange       := u_scheduler.io.pcChange
  u_decode.io.idecode        := u_ifu.io.idecode
  u_decode.io.stalli         := u_scheduler.io.stall

  //Scheduler
  u_scheduler.io.iallocate   := u_decode.io.iallocate
  u_scheduler.io.aluPoolPoor := u_aluPool.io.poor 
  u_scheduler.io.aluPidIn    := u_aluPool.io.pidOut
  u_scheduler.io.mduPoolPoor := u_mduPool.io.poor 
  u_scheduler.io.mduPidIn    := u_mduPool.io.pidOut
  u_scheduler.io.rmuStall    := u_rmu.io.rmuStall
  u_scheduler.io.freePReg    := u_rmu.io.freePReg 
  u_scheduler.io.dstRdPReg   := u_rmu.io.dstRdPReg
  u_scheduler.io.dstRdRdy    := u_rmu.io.dstRdRdy
  u_scheduler.io.srcRdPReg0  := u_rmu.io.srcRdPReg0
  u_scheduler.io.srcRdPReg   := u_rmu.io.srcRdPReg
  u_scheduler.io.srcRdRdy0   := u_rmu.io.srcRdRdy0
  u_scheduler.io.srcRdRdy    := u_rmu.io.srcRdRdy
  u_scheduler.io.exWb(0)     := u_alu(0).io.iwb
  u_scheduler.io.exWb(1)     := u_alu(1).io.iwb
  u_scheduler.io.exWb(2)     := u_alu(2).io.iwb
  u_scheduler.io.exWb(3)     := u_alu(3).io.iwb
  u_scheduler.io.exWb(4)     := u_mdu(0).io.iwb
  u_scheduler.io.exWb(5)     := u_mdu(1).io.iwb
  u_scheduler.io.exWb(6)     := u_mdu(2).io.iwb
  u_scheduler.io.exWb(7)     := u_mdu(3).io.iwb
  u_scheduler.io.brEx        := u_aluPool.io.iexeBr
  u_scheduler.io.jrEx        := u_mduPool.io.iexeJr
  u_scheduler.io.stWb        := u_lsu.io.stRsp
  u_scheduler.io.mduExcpWb(0):= Mux(u_mdu(0).io.iwb.valid && u_mdu(0).io.iwb.excpVld, u_mdu(0).io.iwb, 0.U.asTypeOf(new MicroOp))
  u_scheduler.io.mduExcpWb(1):= Mux(u_mdu(1).io.iwb.valid && u_mdu(1).io.iwb.excpVld, u_mdu(1).io.iwb, 0.U.asTypeOf(new MicroOp))
  u_scheduler.io.mduExcpWb(2):= Mux(u_mdu(2).io.iwb.valid && u_mdu(2).io.iwb.excpVld, u_mdu(2).io.iwb, 0.U.asTypeOf(new MicroOp))
  u_scheduler.io.mduExcpWb(3):= Mux(u_mdu(3).io.iwb.valid && u_mdu(3).io.iwb.excpVld, u_mdu(3).io.iwb, 0.U.asTypeOf(new MicroOp))
  u_scheduler.io.ldRbk       := u_lsu.io.ldRbk
  u_scheduler.io.lidRls      := u_lsu.io.lidRls
  u_scheduler.io.sidRls      := u_lsu.io.sidRls
  u_scheduler.io.csrRdIllegal:= u_csr.io.rdIllegal
 u_scheduler.io.csrRdData   := u_csr.io.rdData
  u_scheduler.io.csrinfo     := u_csr.io.csrinfo
  u_scheduler.io.envRegRdData.get :=  u_regfile.io.envRegRdData.get

  //RMU
  u_rmu.io.redirect          := u_scheduler.io.redirect
  u_rmu.io.allocate          := u_scheduler.io.allocate
  u_rmu.io.enpool            := u_scheduler.io.ienpool
  u_rmu.io.dstRdAddr         := u_scheduler.io.dstRdReg
  u_rmu.io.dstUpdate         := u_scheduler.io.allocate
  u_rmu.io.srcRdAddr0        := u_scheduler.io.srcRdReg0
  u_rmu.io.srcRdAddr         := u_scheduler.io.srcRdReg
  u_rmu.io.wb                := u_aluPool.io.wakeOut ++ u_mduPool.io.wakeOut
  u_rmu.io.iidRls            := u_scheduler.io.iidRls
  u_rmu.io.regRdAck          := u_regfile.io.rack(1)
  u_rmu.io.regRdRsp          := u_regfile.io.rdata(1)
  u_rmu.io.regWrAck          := u_regfile.io.wack(1)
  u_rmu.io.wakeAck           := u_aluPool.io.wakeAck
  u_rmu.io.lidRlsVec         := u_scheduler.io.lidRlsVec
  u_rmu.io.bidRlsVec         := u_scheduler.io.bidRlsVec


  //AluPool
  u_aluPool.io.redirect      := u_scheduler.io.redirect
  u_aluPool.io.allocate      := u_scheduler.io.allocate
  u_aluPool.io.ienpool       := u_scheduler.io.ienpool
  u_aluPool.io.regRdRsp      := u_regfile.io.rdata(0).take(4)
  u_aluPool.io.wakeIn        := u_aluPool.io.wakeOut ++ u_mduPool.io.wakeOut
  u_aluPool.io.dataIn(0)     := u_alu(0).io.iwb.dstData
  u_aluPool.io.dataIn(1)     := u_alu(1).io.iwb.dstData
  u_aluPool.io.dataIn(2)     := u_alu(2).io.iwb.dstData
  u_aluPool.io.dataIn(3)     := u_alu(3).io.iwb.dstData
  u_aluPool.io.dataIn(4)     := u_mdu(0).io.iwb.dstData
  u_aluPool.io.dataIn(5)     := u_mdu(1).io.iwb.dstData
  u_aluPool.io.dataIn(6)     := u_mdu(2).io.iwb.dstData
  u_aluPool.io.dataIn(7)     := u_mdu(3).io.iwb.dstData
  u_aluPool.io.ldReqRdy      := u_lsu.io.ldReqRdy
  u_aluPool.io.ldRsp         := u_lsu.io.ldRsp
  u_aluPool.io.wakeReq       := u_rmu.io.wakeReq
  u_aluPool.io.wakeData      := u_rmu.io.wakeData

  //MduPool
  u_mduPool.io.redirect      := u_scheduler.io.redirect
  u_mduPool.io.allocate      := u_scheduler.io.allocate
  u_mduPool.io.ienpool       := u_scheduler.io.ienpool
  u_mduPool.io.regRdRsp      := u_regfile.io.rdata(0).takeRight(4)
  u_mduPool.io.exeDone(0)    := u_mdu(0).io.done
  u_mduPool.io.exeDone(1)    := u_mdu(1).io.done
  u_mduPool.io.exeDone(2)    := u_mdu(2).io.done
  u_mduPool.io.exeDone(3)    := u_mdu(3).io.done
  u_mduPool.io.wakeIn        := u_aluPool.io.wakeOut ++ u_mduPool.io.wakeOut
  u_mduPool.io.dataIn(0)     := u_alu(0).io.iwb.dstData
  u_mduPool.io.dataIn(1)     := u_alu(1).io.iwb.dstData
  u_mduPool.io.dataIn(2)     := u_alu(2).io.iwb.dstData
  u_mduPool.io.dataIn(3)     := u_alu(3).io.iwb.dstData
  u_mduPool.io.dataIn(4)     := u_mdu(0).io.iwb.dstData
  u_mduPool.io.dataIn(5)     := u_mdu(1).io.iwb.dstData
  u_mduPool.io.dataIn(6)     := u_mdu(2).io.iwb.dstData
  u_mduPool.io.dataIn(7)     := u_mdu(3).io.iwb.dstData
  u_mduPool.io.rdySt         := u_lsu.io.stReqRdy
  u_mduPool.io.csrWrIllegal := u_csr.io.wrIllegal
  u_mduPool.io.wakeReq       := u_rmu.io.wakeReq
  u_mduPool.io.wakeData      := u_rmu.io.wakeData

  for(i <- 0 until 4){
  //ALU
  u_alu(i).io.iexe           := u_aluPool.io.iexe(i)
  u_alu(i).io.redirect       := u_scheduler.io.redirect

  //MDU
  u_mdu(i).io.iexe           := u_mduPool.io.iexe(i)
  u_mdu(i).io.redirect       := u_scheduler.io.redirect
  }

  //LSU
  u_lsu.io.d_ar              <> u_axiif.io.d_ar
  u_lsu.io.d_r               <> u_axiif.io.d_r
  u_lsu.io.d_aw              <> u_axiif.io.d_aw
  u_lsu.io.d_w               <> u_axiif.io.d_w
  u_lsu.io.d_b               <> u_axiif.io.d_b
  u_lsu.io.ldQtailPtr        := u_scheduler.io.ldQtailPtr
  u_lsu.io.stQtailPtr        := u_scheduler.io.stQtailPtr
  u_lsu.io.instIn            := u_scheduler.io.allocate
  u_lsu.io.ldReq             := u_aluPool.io.ldReq
  u_lsu.io.stReq             := u_mduPool.io.outSt
  u_lsu.io.redirect          := u_scheduler.io.redirect
  u_lsu.io.bidSafe           := u_scheduler.io.bidSafe

  //RegFile
  u_regfile.io.ren  (0) := VecInit(u_aluPool.io.regRdReq.map(_.valid)) ++ VecInit(u_mduPool.io.regRdReq.map(_.valid))
  u_regfile.io.raddr(0) := VecInit(u_aluPool.io.regRdReq.map(_.pReg))  ++ VecInit(u_mduPool.io.regRdReq.map(_.pReg))
  u_regfile.io.wen  (0) := VecInit(u_alu.map(_.io.iwb.valid  )) ++ VecInit(u_mdu.map(_.io.iwb.valid))
  //u_regfile.io.wen  (0) := VecInit(u_alu.map(_.io.iwb.valid  )) ++ VecInit(u_mdu.map(_.io.iwb.valid))
  u_regfile.io.wen  (0) := VecInit((0 until 4).map(i => u_alu(i).io.iwb.valid && u_alu(i).io.iwb.pReg =/= 0.U)) ++ VecInit((0 until 4).map(i => u_mdu(i).io.iwb.valid && u_mdu(i).io.iwb.pReg =/= 0.U))
  u_regfile.io.waddr(0) := VecInit(u_alu.map(_.io.iwb.pReg   )) ++ VecInit(u_mdu.map(_.io.iwb.pReg))
  u_regfile.io.wdata(0) := VecInit(u_alu.map(_.io.iwb.dstData)) ++ VecInit(u_mdu.map(_.io.iwb.dstData))

  u_regfile.io.ren  (1) := VecInit(u_rmu.io.regRdReq.map(_.valid))
  u_regfile.io.raddr(1) := VecInit(u_rmu.io.regRdReq.map(_.pReg))
  u_regfile.io.wen  (1) := VecInit(u_rmu.io.regWrReq.map(_.valid))
  u_regfile.io.waddr(1) := VecInit(u_rmu.io.regWrReq.map(_.pReg))
  u_regfile.io.wdata(1) := VecInit(u_rmu.io.regWrReq.map(_.dstData))

  u_regfile.io.envRegRdAddr.get := u_scheduler.io.envRegRdAddr.get

  //CSR
  u_csr.io.rdEn       := u_scheduler.io.csrRdEn
  u_csr.io.rdAddr     := u_scheduler.io.csrRdAddr
  u_csr.io.wrEn       := u_mduPool.io.csrWrEn
  u_csr.io.wrCmd      := u_mduPool.io.csrWrCmd
  u_csr.io.wrAddr     := u_mduPool.io.csrWrAddr
  u_csr.io.wrData     := u_mduPool.io.csrWrData
  u_csr.io.excp       := u_scheduler.io.excp

  //AXIif
  u_axiif.io.ar              <> io.axi.ar
  u_axiif.io.r               <> io.axi.r
  u_axiif.io.aw              <> io.axi.aw
  u_axiif.io.w               <> io.axi.w
  u_axiif.io.b               <> io.axi.b
}

object Main extends App {
  emitVerilog(
    new Core,
    Array("--target-dir", "build")
  )
}
