package decode

import chisel3._
import chisel3.util._

import tools._


class Decode extends Module with CoreParameters{
  val io = IO(new Bundle {
    val pcChange  = Input(new MicroOp)

    val stallo    = Output(Bool())
    val idecode   = Input (Vec(PoolInNum, new MicroOp))

    val stalli    = Input (Bool())
    val iallocate = Output(Vec(PoolInNum, new MicroOp)) 
  })  
  //               
  // - idecode - | - iallocate
  //              

  //input
  val idecode = io.idecode
  val stalli = io.stalli
  val pcChange = io.pcChange
  //output
  val stallo = Wire(Bool())
  val iallocate = RegInit(VecInit.fill(PoolInNum)(0.U.asTypeOf(new MicroOp)))
  io.stallo := stallo
  io.iallocate := iallocate

  //instance
  val decoders = Seq.fill(PoolInNum)(Module(new InstDecode))
  val ctrlSigs   = Wire(Vec(PoolInNum, new CtrlSigs))

  for(i <- 0 until PoolInNum) {
    decoders(i).io.inst := idecode(i).inst
    ctrlSigs(i) :=  decoders(i).io.ctrlSigs
  }

  val idecode1  = Wire(Vec(PoolInNum, new MicroOp))
  val instOnly = Wire(Vec(PoolInNum, UInt(32.W)))

  for(i <- 0 until PoolInNum) {
    val isEcall = instOnly(i) === Instructions.ECALL
    val isFence = instOnly(i) === Instructions.FENCE
    instOnly(i) := idecode(i).inst
    idecode1(i) := Mux(pcChange.valid, 0.U.asTypeOf(new MicroOp), idecode(i))
    idecode1(i).src1Reg  := instOnly(i)(19,15) 
    idecode1(i).src2Reg  := instOnly(i)(24,20) 
    idecode1(i).dstReg   := instOnly(i)(11, 7)  
    idecode1(i).excpVld  := !ctrlSigs(i).legal || isEcall
    idecode1(i).excpCode := Mux(!ctrlSigs(i).legal, 2.U, 0.U) | Mux(isEcall, 11.U, 0.U)
    idecode1(i).toAl     := ctrlSigs(i).toAl
    idecode1(i).toMd     := ctrlSigs(i).toMd
    idecode1(i).src1Type := ctrlSigs(i).src1Type
    idecode1(i).src2Type := ctrlSigs(i).src2Type
    idecode1(i).src3Type := ctrlSigs(i).src3Type
    idecode1(i).dstType  := ctrlSigs(i).dstType
    idecode1(i).fuOp     := ctrlSigs(i).fuOp
    idecode1(i).isBr     := ctrlSigs(i).isBr
    idecode1(i).isJ      := ctrlSigs(i).isJ
    idecode1(i).isJr     := ctrlSigs(i).isJr
    idecode1(i).isLd     := ctrlSigs(i).isLd
    idecode1(i).isSt     := ctrlSigs(i).isSt
    idecode1(i).isCsr    := ctrlSigs(i).isCsr
    idecode1(i).isMret   := ctrlSigs(i).isMret
    idecode1(i).isFencei := ctrlSigs(i).isFencei
    idecode1(i).isFence  := isFence
    idecode1(i).isEcall  := isEcall
    idecode1(i).exeCyc   := ctrlSigs(i).exeCyc

    when(ctrlSigs(i).immType === ImmType.immI) {
      idecode1(i).imm := Cat(Fill(52, instOnly(i)(31)), instOnly(i)(31, 20))  
    }.elsewhen(ctrlSigs(i).immType === ImmType.immS){
      idecode1(i).imm := Cat(Fill(52, instOnly(i)(31)), instOnly(i)(31, 25), instOnly(i)(11,8), instOnly(i)(7))  
    }.elsewhen(ctrlSigs(i).immType === ImmType.immB){
      idecode1(i).imm := Cat(Fill(52, instOnly(i)(31)), instOnly(i)(7), instOnly(i)(30,25), instOnly(i)(11,8), 0.U)  
    }.elsewhen(ctrlSigs(i).immType === ImmType.immU){
      idecode1(i).imm := Cat(Fill(32, instOnly(i)(31)), instOnly(i)(31,12), 0.U(12.W))  
    }.elsewhen(ctrlSigs(i).immType === ImmType.sh6){
      idecode1(i).imm := Cat(0.U(58.W), instOnly(i)(25), instOnly(i)(24, 20))  
    }.elsewhen(ctrlSigs(i).immType === ImmType.sh5){
      idecode1(i).imm := Cat(0.U(59.W), instOnly(i)(24, 20))  
    }.otherwise {
      idecode1(i).imm := Cat(Fill(42, instOnly(i)(31)), instOnly(i)(19,12), instOnly(i)(20), instOnly(i)(30,25), instOnly(i)(24,21), 0.U)
    }

    idecode1(i).src1Data := Mux(idecode1(i).src1Type === SrcType.pc,   idecode1(i).pc, 0.U) |
                            Mux(idecode1(i).fuOp === CsrOpType.wri  && idecode1(i).isCsr, Cat(0.U(59.W), idecode1(i).src1Reg), 0.U) |
                            Mux(idecode1(i).fuOp === CsrOpType.seti && idecode1(i).isCsr, Cat(0.U(59.W), idecode1(i).src1Reg), 0.U) |
                            Mux(idecode1(i).fuOp === CsrOpType.clri && idecode1(i).isCsr, Cat(0.U(59.W), idecode1(i).src1Reg), 0.U)
    idecode1(i).src2Data := Mux(idecode1(i).src2Type === SrcType.pc_4, idecode1(i).pc + 4.U, 0.U) |
                            Mux(idecode1(i).src2Type === SrcType.imm,  idecode1(i).imm, 0.U)
    idecode1(i).src3Data := Mux(idecode1(i).src3Type === SrcType.imm,  idecode1(i).imm, 0.U)

    idecode1(i).preTarget := Mux(!idecode(i).preDir,  idecode1(i).pc + 4.U, idecode1(i).pc + idecode1(i).imm)
    idecode1(i).bakTarget := Mux(!idecode(i).preDir,  idecode1(i).pc + idecode1(i).imm, idecode1(i).pc + 4.U)
    idecode1(i).preDir    := Mux(ctrlSigs(i).isJ || ctrlSigs(i).isJr, true.B, idecode(i).preDir)
    idecode1(i).dirRight  := Mux(ctrlSigs(i).isJ || ctrlSigs(i).isJr, true.B, false.B)
    idecode1(i).tarRight  := Mux(ctrlSigs(i).isJ, true.B, false.B)
    idecode1(i).wbVld     := Mux(ctrlSigs(i).isMret || ctrlSigs(i).isFencei || isFence || isEcall, true.B, false.B)

    idecode1(i).src1x0    := idecode1(i).src1Type ===SrcType.rs && idecode1(i).src1Reg === 0.U
    idecode1(i).src2x0    := idecode1(i).src2Type ===SrcType.rs && idecode1(i).src2Reg === 0.U
    idecode1(i).dstx0     := idecode1(i).dstType === DstType.rd && idecode1(i).dstReg === 0.U
    idecode1(i).dstVld    := idecode1(i).dstType === DstType.rd

    when(pcChange.valid){
      iallocate(i) := 0.U.asTypeOf(new MicroOp)
    }.elsewhen(!stalli) {
      iallocate(i) := idecode1(i)
    }
  }


  stallo := stalli
}

