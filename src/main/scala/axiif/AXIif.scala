package axiif

import chisel3._
import chisel3.util._

import tools._

class AXIif extends Module{
  val io = IO(new Bundle {
    val i_ar = Flipped(DecoupledIO(new AxiArBundle))
    val i_r  = DecoupledIO(new AxiRBundle)

    val d_ar = Flipped(DecoupledIO(new AxiArBundle))
    val d_r  = DecoupledIO(new AxiRBundle)
    val d_aw = Flipped(DecoupledIO(new AxiAwBundle))
    val d_w  = Flipped(DecoupledIO(new AxiWBundle))
    val d_b  = DecoupledIO(new AxiBBundle)

    val ar   = DecoupledIO(new AxiArBundle)
    val r    = Flipped(DecoupledIO(new AxiRBundle))
    val aw   = DecoupledIO(new AxiAwBundle)
    val w    = DecoupledIO(new AxiWBundle)
    val b    = Flipped(DecoupledIO(new AxiBBundle))
  })  


  //read
  val arbRdAxi = Module(new Arbiter((new AxiArBundle), 2)) //dRd.ar have higher priority
  arbRdAxi.io.in(0) <> io.d_ar
  arbRdAxi.io.in(1) <> io.i_ar
  io.ar <> arbRdAxi.io.out
  io.ar.bits.id := Mux(arbRdAxi.io.in(1).ready, 
                      "b10000000".U | arbRdAxi.io.out.bits.id,
                      "b01111111".U & arbRdAxi.io.out.bits.id) 
  io.d_r <> io.r
  io.d_r.valid := io.r.valid && !io.r.bits.id(7)
  io.i_r <> io.r
  io.i_r.bits.id := io.r.bits.id & "b01111111".U
  io.i_r.valid := io.r.valid &&  io.r.bits.id(7)
  io.r.ready := Mux(io.r.valid && !io.r.bits.id(7), io.d_r.ready, io.i_r.ready)
  //write
  io.aw <> io.d_aw
  io.w  <> io.d_w
  io.b  <> io.d_b 
}

