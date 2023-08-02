package tools

import chisel3._
import chisel3.util._

object MaskExpand {
  def apply(m: UInt, maskWidth: Int = 8): UInt = Cat(m.asBools.map(Fill(maskWidth, _)).reverse)
  def apply(m: Seq[Bool], maskWidth: Int): Vec[UInt] = VecInit(m.map(Fill(maskWidth, _)))
}


