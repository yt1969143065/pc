package tools

import chisel3._
import chisel3.util._

object FirstSet {
  def apply(inBits : UInt) : UInt  = { 
    PriorityEncoder(inBits)
  }
}

object QFirstSetByTail {
  def apply(inBits : UInt, tailPtr : UInt) : UInt  = { 
    val Num = inBits.getWidth
    val geTailBits =  (~(0.U(Num.W)) << tailPtr) & inBits
    Mux(geTailBits.orR, FirstSet(geTailBits), FirstSet(inBits))
  }
}

object QFirstSetByHead {
  def apply(inBits : UInt, headPtr : UInt) : UInt  = { 
    val Num = inBits.getWidth
    val geHeadBits =  (~(0.U(Num.W)) << headPtr) & inBits
    Mux(geHeadBits.orR, FirstSet(geHeadBits), FirstSet(inBits))
  }
}



object LastSet {
  def apply(inBits : UInt) : UInt  = { 
    (inBits.getWidth - 1).U - PriorityEncoder(Reverse(inBits))
  }
}

object QLastSetByTail {
  def apply(inBits : UInt, tailPtr : UInt) : UInt  = { 
    val Num = inBits.getWidth
    val geTailBits =  (~(0.U(Num.W)) << tailPtr) & inBits
    Mux(geTailBits.orR,  LastSet(inBits & ~geTailBits), LastSet(inBits))
  }
}

object QLastSetByHead {
  def apply(inBits : UInt, headPtr : UInt) : UInt  = { 
    val Num = inBits.getWidth
    val ltHeadBits =  (~(0.U(Num.W)) >> (Num.U - headPtr)) & inBits
    Mux(ltHeadBits.orR, LastSet(ltHeadBits), LastSet(inBits))
  }
}

