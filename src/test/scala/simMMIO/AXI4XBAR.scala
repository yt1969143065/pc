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
  val arQ = Module(new Queue(new AxiArBundle, 4)) 
  val  rQ = Module(new Queue(new AxiRBundle, 4)) 

  arQ.io.enq <> io.axi_m.ar 

  val rinFlashRange = inFlasRange(arQ.io.deq.bits.addr)
  val rinRamRange = inRamRange(arQ.io.deq.bits.addr)
  val rinVacantRange = !rinFlashRange && !rinRamRange && arQ.io.deq.valid

  arQ.io.deq.ready := ((0 until 2).map(io.axi_s(_).r.fire).reduce(_ || _) || rinVacantRange) && rQ.io.enq.ready
   
  io.axi_s(0).ar.valid := arQ.io.deq.valid && rinFlashRange
  io.axi_s(1).ar.valid := arQ.io.deq.valid && rinRamRange
  for(i <- 0 until 2) {
    io.axi_s(i).ar.bits := arQ.io.deq.bits
  }

  rQ.io.enq.valid := (0 until 2).map(io.axi_s(_).r.valid).reduce(_ || _) || rinVacantRange
  rQ.io.enq.bits := Mux1H((0 until 2).map(i => io.axi_s(i).r.valid -> io.axi_s(i).r.bits))

  for(i <- 0 until 2) {
    io.axi_s(i).r.ready := rQ.io.enq.ready
  }

  io.axi_m.r <> rQ.io.deq
  

  //write
  val awQ = Module(new Queue(new AxiAwBundle, 4))
  val  wQ = Module(new Queue(new AxiWBundle, 4))
  val  bQ = Module(new Queue(new AxiBBundle, 4))

  awQ.io.enq <> io.axi_m.aw
   wQ.io.enq <> io.axi_m.w

  val winFlashRange = inFlasRange(awQ.io.deq.bits.addr)
  val winRamRange = inFlasRange(awQ.io.deq.bits.addr)
  val winVacantRange = !winFlashRange && !winRamRange && awQ.io.deq.valid

  awQ.io.deq.ready :=  ((0 until 2).map(io.axi_s(_).aw.fire).reduce(_ || _) || winVacantRange) && bQ.io.enq.ready
   wQ.io.deq.ready :=  ((0 until 2).map(io.axi_s(_).w.fire).reduce(_ || _) || winVacantRange) && bQ.io.enq.ready
  
  io.axi_s(0).aw.valid := awQ.io.deq.valid && wQ.io.deq.valid && winFlashRange
  io.axi_s(1).aw.valid := awQ.io.deq.valid && wQ.io.deq.valid && winRamRange
  io.axi_s(0).w.valid  := awQ.io.deq.valid && wQ.io.deq.valid && winFlashRange
  io.axi_s(1).w.valid  := awQ.io.deq.valid && wQ.io.deq.valid && winRamRange

  for(i <- 0 until 2) {
    io.axi_s(i).aw.bits := awQ.io.deq.bits
    io.axi_s(i).w.bits := wQ.io.deq.bits
  }

  bQ.io.enq.valid := (0 until 2).map(io.axi_s(_).b.valid).reduce(_ || _) || winVacantRange
  bQ.io.enq.bits := Mux1H((0 until 2).map(i => io.axi_s(i).b.valid -> io.axi_s(i).b.bits))

  for(i <- 0 until 2) {
    io.axi_s(i).b.ready := bQ.io.enq.ready
  }

  io.axi_m.b <> bQ.io.deq
}

