package decode

import chisel3._
import chisel3.util._
//import chisel3.util.experimental.decode._

import tools._
import tools.Instructions._
import tools.BitType._

abstract trait DecodeConstants extends CoreParameters {
  val table: Array[(BitPat, List[BitPat])]
}
object RV64IDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.None, ImmType.immB, BrOpType.bne   , Y, N, N, N, N, N, N, N, ExeCyc.c1),
    BEQ->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.None, ImmType.immB, BrOpType.beq   , Y, N, N, N, N, N, N, N, ExeCyc.c1),
    BLT->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.None, ImmType.immB, BrOpType.blt   , Y, N, N, N, N, N, N, N, ExeCyc.c1),
    BLTU->      List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.None, ImmType.immB, BrOpType.bltu  , Y, N, N, N, N, N, N, N, ExeCyc.c1),
    BGE->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.None, ImmType.immB, BrOpType.bge   , Y, N, N, N, N, N, N, N, ExeCyc.c1),
    BGEU->      List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.None, ImmType.immB, BrOpType.bgeu  , Y, N, N, N, N, N, N, N, ExeCyc.c1),
    JAL->       List(Y, N, Y, SrcType.None, SrcType.pc_4, SrcType.None,  DstType.rd,   ImmType.immJ, MduOpType.move , N, Y, N, N, N, N, N, N, ExeCyc.c1),
    JALR->      List(Y, N, Y, SrcType.rs ,  SrcType.pc_4, SrcType.imm,   DstType.rd,   ImmType.immI, MduOpType.move , N, N, Y, N, N, N, N, N, ExeCyc.c1),
    AUIPC->     List(Y, Y, N, SrcType.pc,   SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immU, AluOpType.add  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    LB->        List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, LdOpType.lb    , N, N, N, Y, N, N, N, N, ExeCyc.c1),
    LH->        List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, LdOpType.lh    , N, N, N, Y, N, N, N, N, ExeCyc.c1),
    LW->        List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, LdOpType.lw    , N, N, N, Y, N, N, N, N, ExeCyc.c1),
    LBU->       List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, LdOpType.lbu   , N, N, N, Y, N, N, N, N, ExeCyc.c1),
    LHU->       List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, LdOpType.lhu   , N, N, N, Y, N, N, N, N, ExeCyc.c1),
    SB->        List(Y, N, Y, SrcType.rs ,  SrcType.rs ,  SrcType.imm,   DstType.None, ImmType.immS, StOpType.sb    , N, N, N, N, Y, N, N, N, ExeCyc.c1),
    SH->        List(Y, N, Y, SrcType.rs ,  SrcType.rs ,  SrcType.imm,   DstType.None, ImmType.immS, StOpType.sh    , N, N, N, N, Y, N, N, N, ExeCyc.c1),
    SW->        List(Y, N, Y, SrcType.rs ,  SrcType.rs ,  SrcType.imm,   DstType.None, ImmType.immS, StOpType.sw    , N, N, N, N, Y, N, N, N, ExeCyc.c1),
    LUI->       List(Y, Y, N, SrcType.None, SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immU, AluOpType.move , N, N, N, N, N, N, N, N, ExeCyc.c1),
    ADDI->      List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, AluOpType.add  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SLTI ->     List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, AluOpType.slt  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SLTIU->     List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, AluOpType.sltu , N, N, N, N, N, N, N, N, ExeCyc.c1),
    ANDI->      List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, AluOpType.and  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    ORI->       List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, AluOpType.or   , N, N, N, N, N, N, N, N, ExeCyc.c1),
    XORI->      List(Y, Y, N, SrcType.rs ,  SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, AluOpType.xor  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    ADD->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.add  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SUB->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.sub  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SLT->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.slt  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SLTU->      List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.sltu , N, N, N, N, N, N, N, N, ExeCyc.c1),
    AND->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.and  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    OR->        List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.or   , N, N, N, N, N, N, N, N, ExeCyc.c1),
    XOR->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.xor  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SLL->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.sll  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRL->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.srl  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRA->       List(Y, Y, N, SrcType.rs ,  SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.sra  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    FENCE->     List(Y, N, N, SrcType.None, SrcType.None, SrcType.None,  DstType.None, ImmType.DC,   AluOpType.sll  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    ECALL->     List(Y, N, N, SrcType.None, SrcType.None, SrcType.None,  DstType.None,  ImmType.DC,  AluOpType.sll  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    //EBREAK->    List(Y,N,Y,),
    MRET->      List(Y, N, N, SrcType.None, SrcType.None, SrcType.None,  DstType.None, ImmType.DC,   AluOpType.sll  , N, N, N, N, N, N, Y, N, ExeCyc.c1),
    FENCE_I->   List(Y, N, N, SrcType.None, SrcType.None, SrcType.None,  DstType.None,  ImmType.DC,  AluOpType.sll  , N, N, N, N, N, N, N, Y, ExeCyc.c1),
    //WFI->       List(Y,N,Y,),
    //CEASE->     List(Y,N,Y,),
    CSRRW->     List(Y, N, Y, SrcType.rs , SrcType.None, SrcType.imm ,  DstType.rd,   ImmType.immI, CsrOpType.wr    , N, N, N, N, N, Y, N, N, ExeCyc.c1),
    CSRRS->     List(Y, N, Y, SrcType.rs , SrcType.None, SrcType.imm ,  DstType.rd,   ImmType.immI, CsrOpType.set   , N, N, N, N, N, Y, N, N, ExeCyc.c1),
    CSRRC->     List(Y, N, Y, SrcType.rs , SrcType.None, SrcType.imm ,  DstType.rd,   ImmType.immI, CsrOpType.clr   , N, N, N, N, N, Y, N, N, ExeCyc.c1),
    CSRRWI->    List(Y, N, Y, SrcType.imm, SrcType.None, SrcType.imm ,  DstType.rd,   ImmType.immI, CsrOpType.wri   , N, N, N, N, N, Y, N, N, ExeCyc.c1),
    CSRRSI->    List(Y, N, Y, SrcType.imm, SrcType.None, SrcType.imm ,  DstType.rd,   ImmType.immI, CsrOpType.seti  , N, N, N, N, N, Y, N, N, ExeCyc.c1),
    CSRRCI->    List(Y, N, Y, SrcType.imm, SrcType.None, SrcType.imm ,  DstType.rd,   ImmType.immI, CsrOpType.clri  , N, N, N, N, N, Y, N, N, ExeCyc.c1),
    LD->        List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, LdOpType.ld     , N, N, N, Y, N, N, N, N, ExeCyc.c1),
    LWU->       List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, LdOpType.lwu    , N, N, N, Y, N, N, N, N, ExeCyc.c1),
    SD->        List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.imm,   DstType.None, ImmType.immS, StOpType.sd     , N, N, N, N, Y, N, N, N, ExeCyc.c1),
    SLLI->      List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.sh6,  AluOpType.sll   , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRLI->      List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.sh6,  AluOpType.srl   , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRAI->      List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.sh6,  AluOpType.sra   , N, N, N, N, N, N, N, N, ExeCyc.c1),
    ADDIW->     List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.immI, AluOpType.addw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SLLIW->     List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.sh5,  AluOpType.sllw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRLIW->     List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.sh5,  AluOpType.srlw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRAIW->     List(Y, Y, N, SrcType.rs , SrcType.imm,  SrcType.None,  DstType.rd,   ImmType.sh5,  AluOpType.sraw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    ADDW->      List(Y, Y, N, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.addw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SUBW->      List(Y, Y, N, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.subw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SLLW->      List(Y, Y, N, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.sllw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRLW->      List(Y, Y, N, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.srlw  , N, N, N, N, N, N, N, N, ExeCyc.c1),
    SRAW->      List(Y, Y, N, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   AluOpType.sraw  , N, N, N, N, N, N, N, N, ExeCyc.c1))
}

object RV64MDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.mul   , N, N, N, N, N, N, N, N, ExeCyc.c2),
    MULH->      List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.mulh  , N, N, N, N, N, N, N, N, ExeCyc.c2),
    MULHU->     List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.mulhu , N, N, N, N, N, N, N, N, ExeCyc.c2),
    MULHSU->    List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.mulhsu, N, N, N, N, N, N, N, N, ExeCyc.c2),
    DIV->       List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.div   , N, N, N, N, N, N, N, N, ExeCyc.NA),
    DIVU->      List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.divu  , N, N, N, N, N, N, N, N, ExeCyc.NA),
    REM->       List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.rem   , N, N, N, N, N, N, N, N, ExeCyc.NA),
    REMU->      List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.remu  , N, N, N, N, N, N, N, N, ExeCyc.NA),
    MULW->      List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.mulw  , N, N, N, N, N, N, N, N, ExeCyc.c2),
    DIVW->      List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.divw  , N, N, N, N, N, N, N, N, ExeCyc.NA),
    DIVUW->     List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.divuw , N, N, N, N, N, N, N, N, ExeCyc.NA),
    REMW->      List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.remw  , N, N, N, N, N, N, N, N, ExeCyc.NA),
    REMUW->     List(Y, N, Y, SrcType.rs , SrcType.rs ,  SrcType.None,  DstType.rd,   ImmType.DC,   MduOpType.remuw , N, N, N, N, N, N, N, N, ExeCyc.NA))
}

class InstDecode extends Module with CoreParameters{
  val io = IO(new Bundle {
    val inst     = Input(UInt(32.W))
    val ctrlSigs = Output(new CtrlSigs)
  })

  val decodeTable =  RV64IDecode.table ++ RV64MDecode.table

  //input
  val inst = io.inst
  //output
  val ctrlSigs = Wire(new CtrlSigs()).decode(inst, decodeTable)
  io.ctrlSigs := ctrlSigs

}

