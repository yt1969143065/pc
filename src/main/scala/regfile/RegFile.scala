package regfile

import chisel3._
import chisel3.util._

import tools._


class RF extends Module with CoreParameters{
  val io = IO(new Bundle {
    val ren   = Input (Vec(8, Bool()))
    val raddr = Input (Vec(8, UInt(PRegWidth.W)))
    val rdata = Output(Vec(8, UInt(64.W)))
    val wen   = Input (Vec(4, Bool()))
    val waddr = Input (Vec(4, UInt(PRegWidth.W)))
    val wdata = Input (Vec(4, UInt(64.W)))
    val envRegRdAddr = if(EnableDifftest) Some(Input (Vec(32, UInt(PRegWidth.W)))) else None
    val envRegRdData = if(EnableDifftest) Some(Output(Vec(32, UInt(64.W)))) else None
  })  
  //input
  val raddr = io.raddr
  val wen = io.wen
  val waddr = io.waddr
  val wdata = io.wdata
  //output
  val rdata = Wire(Vec(8, UInt(64.W)))
  io.rdata := rdata

  val rf = SyncReadMem(PRegNum/2, UInt(64.W))

  for(i <- 0 until 8){ 
    rdata(i) :=  rf(raddr(i)(PRegWidth-1, 1)) 
  }
  for(i <- 0 until 4){ 
    when(wen(i)){
      rf(waddr(i)(PRegWidth-1, 1)) := wdata(i)
    }   
  }
  for(i <- 0 until 32){
    io.envRegRdData.get(i) :=  rf(io.envRegRdAddr.get(i)(PRegWidth-1, 1)) 
  }
}

class RegFile extends Module with CoreParameters{
  val io = IO(new Bundle {
    val rack  = Output(Vec(2, Vec(8, Bool())))
    val ren   = Input (Vec(2, Vec(8, Bool())))
    val raddr = Input (Vec(2, Vec(8, UInt(PRegWidth.W))))
    val rdata = Output(Vec(2, Vec(8, UInt(64.W))))
    val wack  = Output(Vec(2, Vec(8, Bool())))
    val wen   = Input (Vec(2, Vec(8, Bool())))
    val waddr = Input (Vec(2, Vec(8, UInt(PRegWidth.W))))
    val wdata = Input (Vec(2, Vec(8, UInt(64.W))))
    val envRegRdAddr = if(EnableDifftest) Some(Input(Vec(32, UInt(PRegWidth.W)))) else None
    val envRegRdData = if(EnableDifftest) Some(Output (Vec(32, UInt(64.W)))) else None
  })  
  //input
  val ren = io.ren
  val raddr = io.raddr
  val wen = io.wen
  val waddr = io.waddr
  val wdata = io.wdata
  //output
  val rack  = Wire(Vec(2, Vec(8, Bool())))
  val rdata = Wire(Vec(2, Vec(8, UInt(64.W))))
  val wack  = Wire(Vec(2, Vec(8, Bool())))
  io.rack  := rack
  io.rdata := rdata
  io.wack  := wack

  val rfs = Seq.fill(2)(Module(new RF))  

  val rfrack    = Wire(Vec(2, Vec(2, Vec(8, Bool())))) //even/odd, pool/regchain, port0-7
  val rfrackReg = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(2)(VecInit(Seq.fill(8)(false.B)))))))
  
  val arbRfRd = Seq.fill(2)(Seq.fill(8)(Module(new Arbiter(UInt(), 2))))

  for(rfnum <- 0 until 2){ 
    for(i <- 0 until 8){ 
      rfrack(rfnum)(0)(i) := arbRfRd(rfnum)(i).io.in(0).fire
      arbRfRd(rfnum)(i).io.in(0).valid := ren(0)(i) &&  (raddr(0)(i)(0) === rfnum.U)
      arbRfRd(rfnum)(i).io.in(0).bits  := raddr(0)(i)

      rfrack(rfnum)(1)(i) := arbRfRd(rfnum)(i).io.in(1).fire
      arbRfRd(rfnum)(i).io.in(1).valid := ren(1)(i) &&  (raddr(1)(i)(0) === rfnum.U)
      arbRfRd(rfnum)(i).io.in(1).bits  := raddr(1)(i)

      arbRfRd(rfnum)(i).io.out.ready := true.B
      rfs(rfnum).io.ren(i) := arbRfRd(rfnum)(i).io.out.valid
      rfs(rfnum).io.raddr(i) := arbRfRd(rfnum)(i).io.out.bits
    }
  }
  rfrackReg := rfrack

  for(src <- 0 until 2){
    for(i <- 0 until 8){
      rack (src)(i) := rfrack(0)(src)(i) || rfrack(1)(src)(i)
      rdata(src)(i) := Mux(rfrackReg(0)(src)(i), rfs(0).io.rdata(i), rfs(1).io.rdata(i))
    }
  }

  val rfwack  = Wire(Vec(2, Vec(2, Vec(4, Bool())))) //even/odd, pool/regchain, port0-7
  val rfwen   = Wire(Vec(2, Vec(2, Vec(4, Bool()))))
  val rfwaddr = Wire(Vec(2, Vec(2, Vec(4, UInt(PRegWidth.W)))))
  val rfwdata = Wire(Vec(2, Vec(2, Vec(4, UInt(64.W)))))

  for(rfnum <- 0 until 2){
    for(src <- 0 until 2){
      for(i <- 0 until 4){
        rfwen  (rfnum)(src)(i) := wen  (src)(i*2+rfnum)
        rfwaddr(rfnum)(src)(i) := waddr(src)(i*2+rfnum)
        rfwdata(rfnum)(src)(i) := wdata(src)(i*2+rfnum)
      }
    }
  }

  val arbRfWr = Seq.fill(2)(Seq.fill(4)(Module(new Arbiter(UInt(), 2))))

  for(rfnum <- 0 until 2){
    for(i <- 0 until 4){
      rfwack(rfnum)(0)(i) := arbRfWr(rfnum)(i).io.in(0).ready
      arbRfWr(rfnum)(i).io.in(0).valid := rfwen(rfnum)(0)(i)
      arbRfWr(rfnum)(i).io.in(0).bits  := Cat(rfwaddr(rfnum)(0)(i), rfwdata(rfnum)(0)(i))

      rfwack(rfnum)(1)(i) := arbRfWr(rfnum)(i).io.in(1).ready
      arbRfWr(rfnum)(i).io.in(1).valid := rfwen(rfnum)(1)(i)
      arbRfWr(rfnum)(i).io.in(1).bits  := Cat(rfwaddr(rfnum)(1)(i), rfwdata(rfnum)(1)(i))

      arbRfWr(rfnum)(i).io.out.ready := true.B
      rfs(rfnum).io.wen  (i) := arbRfWr(rfnum)(i).io.out.valid
      rfs(rfnum).io.waddr(i) := arbRfWr(rfnum)(i).io.out.bits(PRegWidth+63, 64)
      rfs(rfnum).io.wdata(i) := arbRfWr(rfnum)(i).io.out.bits(63, 0)
    }
  }

  for(src <- 0 until 2){
    for(i <- 0 until 8){
       if(i%2==0) {
         wack (src)(i) := rfwack(0)(src)(i/2)
       } else {
         wack (src)(i) := rfwack(1)(src)(i/2)
       }
    }
  }
  rfs(0).io.envRegRdAddr.get := io.envRegRdAddr.get
  rfs(1).io.envRegRdAddr.get := io.envRegRdAddr.get
  val select = RegNext(VecInit(io.envRegRdAddr.get.map(_(0))).asUInt)
  io.envRegRdData.get := VecInit((0 until 32).map(i => if (i == 0) 0.U else Mux(select(i), rfs(1).io.envRegRdData.get(i), rfs(0).io.envRegRdData.get(i))))
}

