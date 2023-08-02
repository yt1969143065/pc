package tools

import chisel3._
import chisel3.util._

object GeBits {
  def apply(inPtr : UInt) : UInt  = {
    val Width = inPtr.getWidth
    val Num = 1 << Width
    ~(0.U(Num.W)) << inPtr
  }
}

object GtBits {
  def apply(inPtr : UInt) : UInt  = {
    val Width = inPtr.getWidth
    val WidthP1 = Width + 1
    val Num = 1 << Width
    ~(0.U(Num.W)) << (inPtr + 1.U(WidthP1.W))
  }
}

object LeBits {
  def apply(inPtr : UInt) : UInt  = {
    val Width = inPtr.getWidth
    val Num = 1 << Width
    ~(0.U(Num.W)) >> (Num.U - 1.U - inPtr)
  }
}

object LtBits {
  def apply(inPtr : UInt) : UInt  = {
    val Width = inPtr.getWidth
    val Num = 1 << Width
    ~(0.U(Num.W)) >> (Num.U - inPtr)
  }
}


object QGeBitsByTail {
  def apply(inPtr : UInt, tailPtr : UInt) : UInt = {
    Mux(inPtr < tailPtr,
        GeBits(inPtr) & LtBits(tailPtr),
        GeBits(inPtr) | LtBits(tailPtr),
    )
  }
}

object QGtBitsByTail {
  def apply(inPtr : UInt, tailPtr : UInt) : UInt = {
    Mux(inPtr < tailPtr,
        GtBits(inPtr) & LtBits(tailPtr),
        GtBits(inPtr) | LtBits(tailPtr),
    )
  }
}
object QGtBitsByTailInclude {
  def apply(inPtr : UInt, tailPtr : UInt) : UInt = {
    Mux(inPtr < tailPtr,
        GtBits(inPtr) & LeBits(tailPtr),
        GtBits(inPtr) | LeBits(tailPtr),
    )
  }
}



object QLeBitsByHead {
  def apply(inPtr : UInt, headPtr : UInt) : UInt = {
    Mux(inPtr > headPtr,
        LeBits(inPtr) & GeBits(headPtr),
        LeBits(inPtr) | GeBits(headPtr),
    )
  }
}

object QLtBitsByHead {
  def apply(inPtr : UInt, headPtr : UInt) : UInt = {
    Mux(inPtr > headPtr,
        LtBits(inPtr) & GeBits(headPtr),
        LtBits(inPtr) | GeBits(headPtr),
    )
  }
}

