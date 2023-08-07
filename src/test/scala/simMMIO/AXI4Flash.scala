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
  flash.io.clk := clock
  flash.io.ren := io.axi.ar.fire
  flash.io.addr := io.axi.ar.bits.addr - FlashBase.U

  val rspQ  = Module(new Queue(new AxiRBundle, 4))
  io.axi.ar.ready := rspQ.io.enq.ready
  rspQ.io.enq.valid := io.axi.ar.valid
  rspQ.io.enq.bits.id :=  io.axi.ar.bits.id
  rspQ.io.enq.bits.data :=  flash.io.data << (io.axi.ar.bits.addr(4, 0) << 3.U)
  rspQ.io.enq.bits.resp := 0.U
  rspQ.io.enq.bits.last := true.B
  io.axi.r <> rspQ.io.deq

  io.axi.aw.ready := false.B
  io.axi.w.ready := false.B
  io.axi.b.valid := false.B
  io.axi.b.bits := DontCare
}

