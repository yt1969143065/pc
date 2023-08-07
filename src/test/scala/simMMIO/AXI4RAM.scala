package top

import chisel3._
import chisel3.util._
import chisel3.experimental._

import tools._

class RAMHelper extends HasBlackBoxPath{
  val io = IO(new Bundle{
    val clk = Input(Clock())
    val en = Input(Bool())
    val wen = Input(Bool())
    val rIdx = Input(UInt(64.W))
    val rdata = Output(UInt(64.W))
    val wIdx = Input(UInt(64.W))
    val wdata = Input(UInt(64.W))  
    val wmask = Input(UInt(64.W))
  })
  addPath("./src/test/vsrc/RAMHelper.v")
}
 

class AXI4RAM extends Module with CoreParameters {
  val io = IO(new Bundle{
    val axi = Flipped(new AXI4)
  })
  
  val rIdx = (io.axi.ar.bits.addr - RAMBase.U)(RAMIdxWidth-1, 5) 
  val wIdx = (io.axi.aw.bits.addr - RAMBase.U)(RAMIdxWidth-1, 5)
  val rdata = Wire(Vec(4, UInt(64.W)))
  for(i <- 0 until 4){
    val ram = Module(new RAMHelper)
    ram.io.clk := clock
    ram.io.en  := io.axi.ar.fire || io.axi.aw.fire
    ram.io.wen := io.axi.aw.fire
    ram.io.rIdx := (rIdx << 2.U) + i.U
    ram.io.wIdx := (wIdx << 2.U) + i.U
    ram.io.wdata := io.axi.w.bits.data((i + 1) * 64 - 1, i * 64)
    ram.io.wmask := MaskExpand(io.axi.w.bits.strb((i + 1) * 8 - 1, i * 8))
    rdata(i) := ram.io.rdata
  }

  val r_rspQ  = Module(new Queue(new AxiRBundle, 4))
  io.axi.ar.ready := r_rspQ.io.enq.ready
  r_rspQ.io.enq.valid := io.axi.ar.valid
  r_rspQ.io.enq.bits.id := io.axi.ar.bits.id 
  r_rspQ.io.enq.bits.data := Cat(rdata.reverse)
  r_rspQ.io.enq.bits.resp := 0.U
  r_rspQ.io.enq.bits.last := true.B
  io.axi.r <>  r_rspQ.io.deq

  val b_rspQ  = Module(new Queue(new AxiBBundle, 4))
  io.axi.aw.ready := b_rspQ.io.enq.ready && io.axi.w.valid
  io.axi.w.ready := b_rspQ.io.enq.ready && io.axi.aw.valid
  b_rspQ.io.enq.valid := io.axi.aw.valid && io.axi.w.valid
  b_rspQ.io.enq.bits.id := io.axi.aw.bits.id
  b_rspQ.io.enq.bits.resp := 0.U
  io.axi.b <>  b_rspQ.io.deq
}


