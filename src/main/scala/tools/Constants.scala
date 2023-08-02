package tools

import chisel3._
import chisel3.util._

object BitType {
   def X = BitPat("b?")
   def Y = BitPat("b1")
   def N = BitPat("b0")
}

object SrcType {
  def None= BitPat(0.U(3.W))
  def rs  = BitPat(1.U(3.W))
  def imm = BitPat(2.U(3.W))
  def csr = BitPat(3.U(3.W))
  def pc  = BitPat(4.U(3.W))
  def pc_4= BitPat(5.U(3.W))
}

object DstType {
  def None= BitPat(0.U(2.W))
  def rd  = BitPat(1.U(2.W))
  def csr = BitPat(2.U(2.W))
}
object ImmType {
  def DC    = BitPat("b???")
  def immI  = BitPat(0.U(3.W))
  def immS  = BitPat(1.U(3.W))
  def immB  = BitPat(2.U(3.W))
  def immU  = BitPat(3.U(3.W))
  def immJ  = BitPat(4.U(3.W))
  def sh6   = BitPat(5.U(3.W))
  def sh5   = BitPat(6.U(3.W))
}

object FuOpType {
  def apply() = UInt(5.W)
  def X = BitPat("b?????")
}

object AluOpType {
  def move = BitPat( 0.U(5.W))
  def xor  = BitPat( 1.U(5.W))
  def or   = BitPat( 2.U(5.W))
  def and  = BitPat( 3.U(5.W))
  def add  = BitPat( 4.U(5.W))
  def addw = BitPat( 5.U(5.W))
  def sub  = BitPat( 6.U(5.W))
  def subw = BitPat( 7.U(5.W))
  def slt  = BitPat( 8.U(5.W))
  def sltu = BitPat( 9.U(5.W))
  def sll  = BitPat(10.U(5.W))
  def sllw = BitPat(11.U(5.W))
  def srl  = BitPat(12.U(5.W))
  def srlw = BitPat(13.U(5.W))
  def sra  = BitPat(14.U(5.W))
  def sraw = BitPat(15.U(5.W))
}

object MduOpType {
  def mul    = BitPat( 0.U(5.W))
  def mulh   = BitPat( 1.U(5.W))
  def mulhsu = BitPat( 2.U(5.W))
  def mulhu  = BitPat( 3.U(5.W))

  def div    = BitPat( 4.U(5.W))
  def divu   = BitPat( 5.U(5.W))
  def rem    = BitPat( 6.U(5.W))
  def remu   = BitPat( 7.U(5.W))

  def mulw   = BitPat( 8.U(5.W))
  def move   = BitPat( 9.U(5.W))

  def divw   = BitPat(12.U(5.W))
  def divuw  = BitPat(13.U(5.W))
  def remw   = BitPat(14.U(5.W))
  def remuw  = BitPat(15.U(5.W))

}


object BrOpType {
  def beq  = BitPat(0.U(5.W))
  def bne  = BitPat(1.U(5.W))
  def blt  = BitPat(2.U(5.W))
  def bge  = BitPat(3.U(5.W))
  def bltu = BitPat(4.U(5.W))
  def bgeu = BitPat(5.U(5.W))
}

object LdOpType {
  def lb   = BitPat(0.U(5.W))
  def lbu  = BitPat(1.U(5.W))
  def lh   = BitPat(2.U(5.W))
  def lhu  = BitPat(3.U(5.W))
  def lw   = BitPat(4.U(5.W))
  def lwu  = BitPat(5.U(5.W))
  def ld   = BitPat(6.U(5.W))
}

object StOpType {
  def sb   = BitPat(0.U(5.W))
  def sh   = BitPat(2.U(5.W))
  def sw   = BitPat(4.U(5.W))
  def sd   = BitPat(6.U(5.W))
}

object CsrOpType {
  def wr   = BitPat(0.U(5.W))
  def set  = BitPat(1.U(5.W))
  def clr  = BitPat(2.U(5.W))
  def wri  = BitPat(3.U(5.W))
  def seti = BitPat(4.U(5.W))
  def clri = BitPat(5.U(5.W))
}


object ExeCyc {
  def c1   = BitPat(0.U(2.W))
  def c2   = BitPat(1.U(2.W))
  def c3   = BitPat(2.U(2.W))
  def NA   = BitPat(3.U(2.W))
}
