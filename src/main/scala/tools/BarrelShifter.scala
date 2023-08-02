package tools

import chisel3._
import chisel3.util._

object BarrelShifter {
  private trait ShiftType

  private object LeftShift extends ShiftType

  private object RightShift extends ShiftType

  private object LeftRotate extends ShiftType

  private object RightRotate extends ShiftType

  private def apply[T <: Data](
    inputs:           Vec[T],
    shiftInput:       UInt,
    shiftType:        ShiftType,
    shiftGranularity: Int = 1
  ): Vec[T] = {
    require(shiftGranularity > 0)
    val elementType: T = chiselTypeOf(inputs.head)
    shiftInput.asBools
      .grouped(shiftGranularity)
      .map(VecInit(_).asUInt)
      .zipWithIndex
      .foldLeft(inputs) {
        case (prev, (shiftBits, layer)) =>
          Mux1H(
            UIntToOH(shiftBits),
            Seq.tabulate(1 << shiftBits.getWidth)(i => {
              // For each layer of barrel shifter, it needs to
              // Mux between shift 0 and i * 2^(depthOfLayer*granularity)
              //
              // e.g, when depth = 2 and granu = 1, the first layer Mux between 0 and 1
              // while the second layer Mux between 0 and 2, thus Vec.shift(UInt(2.W))
              //
              // e.g, when depth = 2 and granu = 2, the first layer Mux between 0, 1, 2 and 3
              // while the second layer Mux between 0, 4, 8, and 12, thus achieving Vec.shift(UInt(4.W))
              //
              // Also, shift no more than inputs length since prev.drop will not warn about overflow
              // this is for Vec with length not the exponential of 2, e.g. 13
              val layerShift: Int = (i * (1 << (layer * shiftGranularity))).min(prev.length)
              VecInit(shiftType match {
                case LeftRotate =>
                  prev.drop(layerShift) ++ prev.take(layerShift)
                case LeftShift =>
                  prev.drop(layerShift) ++ Seq.fill(layerShift)(0.U.asTypeOf(elementType))
                case RightRotate =>
                  prev.takeRight(layerShift) ++ prev.dropRight(layerShift)
                case RightShift =>
                  Seq.fill(layerShift)(0.U.asTypeOf(elementType)) ++ prev.dropRight(layerShift)
              })
            })
          )
      }
  }

  def leftShift[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, LeftShift, layerSize)

  def rightShift[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, RightShift, layerSize)

  def leftRotate[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, LeftRotate, layerSize)

  def rightRotate[T <: Data](inputs: Vec[T], shift: UInt, layerSize: Int = 1): Vec[T] =
    apply(inputs, shift, RightRotate, layerSize)
}

