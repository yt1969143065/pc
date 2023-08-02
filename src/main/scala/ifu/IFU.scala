package ifu 

import chisel3._
import chisel3.util._

import tools._


class IFU extends Module with CoreParameters{
  val io = IO(new Bundle {
    val i_ar    = DecoupledIO(new(AxiArBundle))
    val i_r     = Flipped(DecoupledIO(new(AxiRBundle)))

    val pcChange= Input(new MicroOp)
    val stall   = Input (Bool())
    val idecode = Output(Vec(PoolInNum, new MicroOp)) //ordered
    val bpUpVld = Input(UInt(8.W)) 
    val bpUpDir = Input(UInt(8.W))
  })  

  //input
  val stall = io.stall 
  val pcChange = io.pcChange

  //output
  val idecode = Wire(Vec(PoolInNum, new MicroOp))
  io.idecode := idecode

  //channel
  val i_ar = Wire(Decoupled(new AxiArBundle)) //instruction read
  val i_r  = Wire(Decoupled(new AxiRBundle)) 
  i_ar <> io.i_ar
  i_r <> io.i_r


//function
//---------------------------------------------------------------------------------
  def isFlash(addr:UInt) : Bool = { 
    addr(VAddrWidth-1, 28) === 1.U 
  }
//---------------------------------------------------------------------------------

  val IDWidth = 7 
  val IDNum = 2 << IDWidth
  val idReqVld = RegInit(0.U(IDNum.W))
  val idPtr    = RegInit(0.U(IDWidth.W))

  class QBundle extends Bundle with CoreParameters{
    val isFlashAddr = Bool()
    val id          = UInt(IDWidth.W)
    val addr        = UInt(VAddrWidth.W) 
    val data        = UInt(256.W)
    val bp          = Vec(8, UInt(2.W))
  }   

  //============================================================
  val pc            = RegInit("h10000000".U(VAddrWidth.W))
  val pcIsFlashAddr = RegInit(true.B)
  val pcStallByJr   = RegInit(false.B)
  val pcStallByMret = RegInit(false.B)
  val pcStallByEcall = RegInit(false.B)
  val pcStallByFencei = RegInit(false.B)
  val pcStallByExcp = RegInit(false.B)

  val pcForFencei  = Reg(UInt(VAddrWidth.W))

  val reqQ     = Module(new Queue(new QBundle,  8, hasFlush = true))
  val reqAddrQ = Module(new Queue(new QBundle, 16))
  val rspQ     = Module(new Queue(new QBundle,  2, hasFlush = true))
 
  val bp = RegInit(VecInit.fill(8) (1.U(2.W)))
  //============================================================

  val pcChangeByBJ = Wire(new MicroOp)
  val stallByJr = Wire(Bool())
  val stallByMret = Wire(Bool())
  val stallByEcall = Wire(Bool())
  val stallByFencei = Wire(Bool())
  val stallByExcp = Wire(Bool())
  val pcPlus8 = Cat(pc(VAddrWidth-1, 3) + 1.U, 0.U(3.W))
  val pcPlus32 = Cat(pc(VAddrWidth-1, 5) + 1.U, 0.U(5.W))

  when(pcChange.valid){
    pc := Mux(pcChange.fuOp === 1.U, pcForFencei, pcChange.pc)
  }.elsewhen(pcChangeByBJ.valid){
    pc := pcChangeByBJ.pc
  }.elsewhen(reqQ.io.enq.fire && !pcIsFlashAddr){
    pc := pcPlus32
  }.elsewhen(reqQ.io.enq.fire && pcIsFlashAddr){
    pc := pcPlus8
  }
  assert(pc =/= 0.U)

  when(pcChange.valid){
    pcIsFlashAddr := isFlash(pcChange.pc)
  }.elsewhen(pcChangeByBJ.valid){
    pcIsFlashAddr := isFlash(pcChangeByBJ.pc)
  }.elsewhen(reqQ.io.enq.fire && !pcIsFlashAddr){
    pcIsFlashAddr := isFlash(pcPlus32)
  }.elsewhen(reqQ.io.enq.fire && pcIsFlashAddr){
    pcIsFlashAddr := isFlash(pcPlus8)
  }

  val stallJrNxt = RegNext(stallByJr)
  when(stallByJr){
    pcStallByJr := true.B
  }.elsewhen(stallJrNxt && !idecode.map(i => i.valid && i.inst === Instructions.JALR).reduce(_ || _)){
    pcStallByJr := false.B
  }.elsewhen(pcChange.valid){
    pcStallByJr := false.B
  }

  when(stallByMret){
    pcStallByMret := true.B
  }.elsewhen(pcChange.valid){
    pcStallByMret := false.B
  }

  when(stallByEcall){
    pcStallByEcall := true.B
  }.elsewhen(pcChange.valid){
    pcStallByEcall := false.B
  }

  when(stallByFencei){
    pcStallByFencei := true.B
  }.elsewhen(pcChange.valid){
    pcStallByFencei := false.B
  }

  val fenceiVec = VecInit((0 until 8).map(i => idecode(i).valid && idecode(i).inst === Instructions.FENCE_I)).asUInt
  when(fenceiVec.orR){
    pcForFencei := PriorityMux(fenceiVec, VecInit(idecode.map(_.pc + 4.U)))
  }

  when(stallByExcp){
    pcStallByExcp := true.B
  }.elsewhen(pcChange.valid){
    pcStallByExcp := false.B
  }
  stallByExcp := pcChange.valid && (pcChange.pc(1, 0) =/= 0.U)

  for(i <- 0 until 8){
    when(io.bpUpVld(i)){
      when(io.bpUpDir(i) && !(bp(i)===3.U)){
        bp(i) := bp(i) + 1.U
      }.elsewhen(!io.bpUpDir(i) && !(bp(i)===0.U)){
        bp(i) := bp(i) - 1.U
      }
    }
  }

  //instruction read request
  reqQ.io.enq.valid            := !pcStallByJr && !pcStallByExcp && !pcStallByMret && !pcStallByFencei && !pcStallByEcall
  reqQ.io.enq.bits             := DontCare
  reqQ.io.enq.bits.isFlashAddr := pcIsFlashAddr
  reqQ.io.enq.bits.id          := idPtr
  reqQ.io.enq.bits.addr        := pc
  reqQ.io.flush.get            := pcChange.valid || pcChangeByBJ.valid || stallByJr || stallByMret || stallByFencei || stallByEcall

  reqQ.io.deq.ready            := i_ar.ready && reqAddrQ.io.enq.ready && rspQ.io.enq.ready

  i_ar.valid      := reqQ.io.deq.valid && rspQ.io.enq.ready
  i_ar.bits.addr  := reqQ.io.deq.bits.addr
  i_ar.bits.id    := Cat("b0".U, reqQ.io.deq.bits.id)
  i_ar.bits.len   := "b0".U
  i_ar.bits.size  := "b101".U
  i_ar.bits.burst := "b01".U
  i_ar.bits.lock  := 0.U
  i_ar.bits.cache := 0.U
  i_ar.bits.prot  := 0.U
  i_ar.bits.qos   := 0.U

  reqAddrQ.io.enq.valid := reqQ.io.deq.valid && i_ar.ready && rspQ.io.enq.ready
  reqAddrQ.io.enq.bits  := reqQ.io.deq.bits

  //instruction read response
  i_r.ready := !idReqVld(i_r.bits.id(IDWidth-1, 0)) || rspQ.io.enq.ready
  reqAddrQ.io.deq.ready := i_r.fire

  rspQ.io.enq.valid     := i_r.valid && idReqVld(i_r.bits.id(IDWidth-1, 0))
  rspQ.io.enq.bits      := reqAddrQ.io.deq.bits
  rspQ.io.enq.bits.data := i_r.bits.data
  rspQ.io.enq.bits.bp   := bp
  rspQ.io.flush.get     := pcChange.valid

  val rspInstVld = Wire(UInt(8.W))
  val rspInst    = Wire(Vec(8, UInt(32.W)))
  val rspInstPc  = Wire(Vec(8, UInt(VAddrWidth.W)))
  val rspInstImmJ = Wire(Vec(8, UInt(VAddrWidth.W)))
  val rspInstImmB = Wire(Vec(8, UInt(VAddrWidth.W)))

  val rspInstIdx     = reqAddrQ.io.deq.bits.addr(4, 2)
  val rspIsFlashAddr = reqAddrQ.io.deq.bits.isFlashAddr

  rspInstVld := Mux(rspQ.io.enq.fire, Mux(rspIsFlashAddr, "b00000011".U, "b11111111".U) << rspInstIdx, 0.U)
  for(i <- 0 until 8) {
    rspInst(i)    := i_r.bits.data((i+1)*32-1,  i*32)
    rspInstPc(i)  := Cat(reqAddrQ.io.deq.bits.addr(VAddrWidth-1, 5), i.U(3.W), "b00".U(2.W))
    rspInstImmJ(i) := Cat(Fill(VAddrWidth-20, rspInst(i)(31)), rspInst(i)(19, 12), rspInst(i)(20), rspInst(i)(30, 25), rspInst(i)(24, 21), 0.U)
    rspInstImmB(i) := Cat(Fill(VAddrWidth-12, rspInst(i)(31)), rspInst(i)(7), rspInst(i)(30, 25), rspInst(i)(11, 8), 0.U)
  }

  val brt   = VecInit((0 until 8).map(i => Mux(rspInstVld(i), rspInst(i)(6, 0) === "b1100011".U && bp(i)(1), false.B))).asUInt
  val brtPc = VecInit((0 until 8).map(i => rspInstPc(i) + rspInstImmB(i)))
  val jal   = VecInit((0 until 8).map(i => Mux(rspInstVld(i), rspInst(i) === Instructions.JAL, false.B))).asUInt
  val jalPc = VecInit((0 until 8).map(i => rspInstPc(i) + rspInstImmJ(i)))

  pcChangeByBJ := DontCare
  pcChangeByBJ.valid := (brt | jal).orR
  pcChangeByBJ.pc :=
      PriorityMux(brt | jal, (0 until 8).map(i => Mux(jal(i), jalPc(i), brtPc(i))))

  stallByJr := VecInit((0 until 8).map(i =>
      Mux(rspInstVld(i), rspInst(i) === Instructions.JALR, false.B)
    )).reduce(_ || _)

  stallByMret := VecInit((0 until 8).map(i =>
      Mux(rspInstVld(i), rspInst(i) === Instructions.MRET, false.B)
    )).reduce(_ || _)

  stallByEcall := VecInit((0 until 8).map(i =>
      Mux(rspInstVld(i), rspInst(i) === Instructions.ECALL, false.B)
    )).reduce(_ || _)

  stallByFencei := VecInit((0 until 8).map(i =>
      Mux(rspInstVld(i), rspInst(i) === Instructions.FENCE_I, false.B)
    )).reduce(_ || _)


  rspQ.io.deq.ready := !stall
  val shitCnt = rspQ.io.deq.bits.addr(4, 2)
  val addrMask = Mux(rspQ.io.deq.bits.isFlashAddr, "b00000011".U, "b11111111".U) << shitCnt
  val jMask      = VecInit((0 until 8).map(i => Mux(idecode(i).inst === Instructions.JAL     && addrMask(i), "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  val jrMask     = VecInit((0 until 8).map(i => Mux(idecode(i).inst === Instructions.JALR    && addrMask(i), "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  val mretMask   = VecInit((0 until 8).map(i => Mux(idecode(i).inst === Instructions.MRET    && addrMask(i), "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  val ecallMask   = VecInit((0 until 8).map(i => Mux(idecode(i).inst === Instructions.ECALL    && addrMask(i), "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  val fenceiMask = VecInit((0 until 8).map(i => Mux(idecode(i).inst === Instructions.FENCE_I && addrMask(i), "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  val brtMask = VecInit((0 until 8).map(i => Mux(idecode(i).inst(6, 0) === "b1100011".U && rspQ.io.deq.bits.bp(i)(1) && addrMask(i), "b11111111".U >> (7-i).U, "b11111111".U))).reduce(_ & _)
  for(i <- 0 until PoolInNum) {
    idecode(i) := DontCare
    idecode(i).valid := rspQ.io.deq.bits.addr(4, 2) <= i.U && rspQ.io.deq.valid && addrMask(i) && jMask(i) && jrMask(i) && mretMask(i) && fenceiMask(i) && ecallMask(i) && brtMask(i)
    idecode(i).pc    := Cat(rspQ.io.deq.bits.addr(VAddrWidth - 1, 5), i.U(3.W), "b00".U(2.W))
    idecode(i).inst  := rspQ.io.deq.bits.data(32*(i+1) - 1, 32*i)
    idecode(i).preDir := rspQ.io.deq.bits.bp(i)(1)
  }


  //flush logic
  idPtr := Mux(reqQ.io.enq.fire, idPtr+1.U, idPtr)
  idReqVld := Mux(pcChange.valid || pcChangeByBJ.valid || stallByJr || stallByMret || stallByFencei || stallByEcall,
              0.U,
              Mux(reqQ.io.enq.fire, UIntToOH(idPtr) | idReqVld, idReqVld))
}
