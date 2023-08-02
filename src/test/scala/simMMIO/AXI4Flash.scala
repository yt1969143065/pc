package top

import chisel3._
import chisel3.util._
import chisel3.experimental._

import tools._

class FlashHelper extends HasBlackBoxPath{
  val io = IO(new Bundle{
    val clk = Input(Clock())
    val ren = Input(Bool())
    val data = Output(UInt(64.W))
    val addr = Input(UInt(32.W))
  })
  addPath("./src/test/vsrc/FlashHelper.v")
}

class AXI4Flash extends Module with CoreParameters {
  val io = IO(new Bundle{
    val axi = Flipped(new AXI4)
  })
  val flash = Module(new FlashHelper)
  io.axi.ar.ready := true.B
  flash.io.clk := clock
  flash.io.ren := io.axi.ar.fire
  flash.io.addr := io.axi.ar.bits.addr

  io.axi.r.valid := io.axi.ar.valid
  io.axi.r.bits.id := io.axi.ar.bits.id
  io.axi.r.bits.data := flash.io.data
  io.axi.r.bits.resp := 0.U
  io.axi.r.bits.last := true.B

  io.axi.aw.ready := false.B
  io.axi.w.ready := false.B
  io.axi.b.valid := false.B
  io.axi.b.bits := DontCare
}

