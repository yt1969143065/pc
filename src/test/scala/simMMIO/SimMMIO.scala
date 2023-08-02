package top

import chisel3._
import chisel3.util._

import tools._

class SimMMIO extends Module with CoreParameters {
  val io = IO(new Bundle{
    val axi = Flipped(new AXI4)
  })
  val axi4xbar = Module(new AXI4XBar)
  val flash    = Module(new AXI4Flash)
  val ram      = Module(new AXI4RAM)

        io.axi <> axi4xbar.io.axi_m
  flash.io.axi <> axi4xbar.io.axi_s(0)
    ram.io.axi <> axi4xbar.io.axi_s(1)
}

