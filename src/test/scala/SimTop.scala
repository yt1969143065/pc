package top 

import chisel3._
import chisel3.util._

import tools._
import core._

class SimTop extends Module {
  val simMMIO = Module(new SimMMIO)
  val core = Module(new Core)
  simMMIO.io <> core.io
  
}


object Main extends App {
  emitVerilog(
    new SimTop,
    Array("--target-dir", "build")
  )
}

