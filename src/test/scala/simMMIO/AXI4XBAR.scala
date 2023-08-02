package top

import chisel3._
import chisel3.util._

import tools._

class AXI4XBar extends Module with CoreParameters {
  val io = IO(new Bundle{
    val axi_m       = Flipped(new AXI4)
    val axi_s       = Vec(2, new AXI4)
  })

  //read
  val rinFlashRange = inFlasRange(io.axi_m.ar.bits.addr)
  val rinRamRange = inRamRange(io.axi_m.ar.bits.addr)

  io.axi_m.ar.ready := true.B

  io.axi_s(0).ar.valid := io.axi_m.ar.valid && rinFlashRange
  io.axi_s(0).ar.bits  := io.axi_m.ar.bits
  io.axi_s(0).r.ready  := true.B

  io.axi_s(1).ar.valid := io.axi_m.ar.valid && rinRamRange
  io.axi_s(1).ar.bits  := io.axi_m.ar.bits
  io.axi_s(1).r.ready  := true.B

  io.axi_m.r.valid := RegNext(io.axi_m.ar.valid)
  io.axi_m.r.bits := Mux(io.axi_s(0).r.fire, io.axi_s(0).r.bits, io.axi_s(1).r.bits)

  //write
  val winFlashRange = inFlasRange(io.axi_m.aw.bits.addr)
  val winRamRange = inFlasRange(io.axi_m.aw.bits.addr)

  io.axi_m.aw.ready := io.axi_m.w.valid
  io.axi_m.w.ready := io.axi_m.aw.valid
  
  io.axi_s(0).aw.valid := io.axi_m.aw.valid &&  io.axi_m.w.valid && winFlashRange
  io.axi_s(0).aw.bits  := io.axi_m.aw.bits
  io.axi_s(0).w.valid  := io.axi_m.aw.valid &&  io.axi_m.w.valid && winFlashRange
  io.axi_s(0).w.bits   := io.axi_m.w.bits
  io.axi_s(0).b.ready  := true.B

  io.axi_s(1).aw.valid := io.axi_m.aw.valid &&  io.axi_m.w.valid && winRamRange
  io.axi_s(1).aw.bits  := io.axi_m.aw.bits
  io.axi_s(1).w.valid  := io.axi_m.aw.valid &&  io.axi_m.w.valid && winRamRange
  io.axi_s(1).w.bits   := io.axi_m.w.bits
  io.axi_s(1).b.ready  := true.B

  io.axi_m.b.valid := RegNext(io.axi_m.aw.valid &&  io.axi_m.w.valid)
  io.axi_m.b.bits := Mux(io.axi_s(0).b.fire, io.axi_s(0).b.bits, io.axi_s(1).b.bits)
}

