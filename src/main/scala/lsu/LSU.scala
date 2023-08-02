package lsu 

import chisel3._
import chisel3.util._

import tools._

class LSU extends Module with CoreParameters{
  val io = IO(new Bundle {
    val d_ar     = DecoupledIO(new AxiArBundle)
    val d_r      = Flipped(DecoupledIO(new AxiRBundle))
    val d_aw     = DecoupledIO(new AxiAwBundle)
    val d_w      = DecoupledIO(new AxiWBundle)
    val d_b      = Flipped(DecoupledIO(new AxiBBundle))

    val instIn   = Input(Vec(PoolInNum, new MicroOp))
    val ldQtailPtr = Input(UInt(LidWidth.W))
    val stQtailPtr = Input(UInt(SidWidth.W))

    val ldReqRdy = Output(Vec(LdNum, Bool()           ))
    val ldReq    = Input (Vec(LdNum, new LdReqBundle  ))  
    val ldRsp    = Output(Vec(LdNum, new LdRspBundle  ))  

    val stReqRdy = Output(Vec(StNum, Bool()           ))
    val stReq    = Input (Vec(StNum, new StReqBundle  ))  
    val stRsp    = Output(new MicroOp)

    val ldExcpWb = Output(Vec(LdNum, new MicroOp))
    val stExcpWb = Output(Vec(StNum, new MicroOp))

    val ldRbk    = Output(new MicroOp)
    val redirect = Input (new Redirect)

    val bidSafe  = Input (UInt(LidNum.W))

    val lidRls   = Output(UInt(PoolInNum.W))
    val sidRls   = Output(UInt(PoolInNum.W))
  })  

  //input
  val instIn = io.instIn
  val ldReq = io.ldReq
  val stReq = io.stReq
  val redirect = io.redirect
  val bidSafe = io.bidSafe
  val ldQtailPtr = io.ldQtailPtr
  val stQtailPtr = io.stQtailPtr
  //output
  val ldReqRdy = Wire(Vec(LdNum, Bool()))
  val ldRsp = Wire(Vec(LdNum, new LdRspBundle))
  val stReqRdy = Wire(Vec(StNum, Bool()))
  val stRsp = RegInit(0.U.asTypeOf(new MicroOp))
  val ldExcpWb = Wire(Vec(4, new MicroOp))
  val stExcpWb = Wire(Vec(4, new MicroOp))
  val lidRls = Wire(UInt(PoolInNum.W))
  val sidRls = Wire(UInt(PoolInNum.W))
  val ldRbkR = RegInit(0.U.asTypeOf(new MicroOp))
  val ldRbk = Wire(new MicroOp)
  io.ldReqRdy := ldReqRdy
  io.ldRsp := ldRsp
  io.stReqRdy := stReqRdy
  io.stRsp := stRsp
  io.ldExcpWb := ldExcpWb
  io.stExcpWb := stExcpWb
  io.lidRls := lidRls
  io.sidRls := sidRls
  io.ldRbk := ldRbk
  //channel
  val d_ar = Wire(Decoupled(new(AxiArBundle))) 
  val d_r  = Wire(Decoupled(new(AxiRBundle)))
  val d_aw = Wire(Decoupled(new(AxiAwBundle)))
  val d_w  = Wire(Decoupled(new(AxiWBundle)))
  val d_b  = Wire(Decoupled(new(AxiBBundle)))
  io.d_ar <> d_ar
  io.d_r <> d_r 
  io.d_aw <> d_aw
  io.d_w <> d_w 
  io.d_b <> d_b 

  class QBundle extends Bundle with CoreParameters{
    val op    = UInt(FuOpType.X.getWidth.W)
    val addr  = UInt(VAddrWidth.W) 
    val data  = UInt(64.W)
    val lid   = UInt(LidWidth.W)
    val pid   = UInt(PidWidth.W)
    val sid   = UInt(SidWidth.W)
    val iid   = UInt(IidWidth.W)
    val mask  = UInt(8.W)
  }

  val ldReqQ   = Seq.fill(LdNum)(Module(new Queue(new QBundle, 4)))
  val ldRsp0Q  = Seq.fill(LdNum)(Module(new Queue(new QBundle, 2))) //fwdSt
  val dRdQ     = Seq.fill(LdNum)(Module(new Queue(new QBundle, 4)))
  val dRdSentQ = Seq.fill(LdNum)(Module(new Queue(new QBundle, 4)))
  val ldRsp1Q  = Seq.fill(LdNum)(Module(new Queue(new QBundle, 2))) //fromMem

  val stReqQ   = Seq.fill(StNum)(Module(new Queue(new QBundle, 4)))
  val dWrQaw   =                 Module(new Queue(new QBundle, 4))
  val dWrQw    =                 Module(new Queue(new QBundle, 4))


  val ldQvld     = RegInit(0.U(LidNum.W))
  val ldQflushed = RegInit(0.U(LidNum.W))
  val ldQcome    = RegInit(0.U(LidNum.W))
  val ldQgone    = RegInit(0.U(LidNum.W))
  val ldQaddr    = Reg(Vec(LidNum, UInt(VAddrWidth.W)))
  val ldQmask    = Reg(Vec(LidNum, UInt(16.W)))
  val ldQpc      = Reg(Vec(LidNum, UInt(VAddrWidth.W)))
  val ldQbid     = Reg(Vec(LidNum, UInt(BidWidth.W)))
  val ldQsid     = Reg(Vec(LidNum, UInt(SidWidth.W)))
  val ldQheadPtr = RegInit(VecInit((0 until PoolInNum).map(_.U(LidWidth.W))))
  val ldQrlsVld  = Wire(Vec(PoolInNum, Bool()))

  val stQvld     = RegInit(0.U(SidNum.W))
  val stQflushed = RegInit(0.U(SidNum.W))
  val stQcome    = RegInit(0.U(SidNum.W))
  val stQconfirm = RegInit(0.U(SidNum.W))
  val stQ2wrQ    = RegInit(0.U(SidNum.W))
  val stQgoneA   = RegInit(0.U(SidNum.W))
  val stQgoneD   = RegInit(0.U(SidNum.W))
  val stQaddr    = Reg(Vec(SidNum, UInt(VAddrWidth.W)))
  val stQmask    = Reg(Vec(SidNum, UInt(16.W)))
  val stQdata    = Reg(Vec(SidNum, UInt(128.W)))
  val stQbid     = Reg(Vec(SidNum, UInt(BidWidth.W)))
  val stQlid     = Reg(Vec(SidNum, UInt(SidWidth.W)))
  val stQop      = Reg(Vec(SidNum, FuOpType()))
  val stQheadPtr = RegInit(VecInit((0 until PoolInNum).map(_.U(SidWidth.W))))
  val stQconfirmPtr = RegInit(VecInit((0 until PoolInNum).map(_.U(SidWidth.W))))
  val stQ2wrQPtr = RegInit(0.U(SidWidth.W))
  val stQrlsVld  = Wire(Vec(PoolInNum, Bool()))

  //-----------------------------------------------------------------
  // LOAD
  //-----------------------------------------------------------------
  val ldQvldIn = (0 until PoolInNum).map(i => Mux(instIn(i).valid && instIn(i).isLd, UIntToOH(instIn(i).lid), 0.U)).reduce(_ | _)
  val ldQvldOut = (0 until PoolInNum).map(i => Mux(ldQrlsVld(i), UIntToOH(ldQheadPtr(i)), 0.U)).reduce(_ | _)
  ldQvld := (ldQvld | ldQvldIn) & ~ldQvldOut

  for(i <- 0 until LidNum) {
    val upVld = VecInit((0 until PoolInNum).map(j => instIn(j).valid && instIn(j).isLd && instIn(j).lid === i.U)).asUInt
    when(upVld.orR){
      ldQpc (i) := Mux1H(upVld, instIn.map(_.pc))
      ldQbid(i) := Mux1H(upVld, instIn.map(_.bid))
      ldQsid(i) := Mux1H(upVld, instIn.map(_.sid))
    }
  }

  val excpFlushLd = Mux(redirect.flush, ldQvld, 0.U)
  val brRbkFlushLd = VecInit((0 until LidNum).map(i => ldQvld(i) && redirect.brRBK && redirect.flushedBid(ldQbid(i)))).asUInt
  val ldRbkFlushLd = VecInit((0 until LidNum).map(i => ldQvld(i) && ldRbk.valid    && (ldRbk.lidVec(i) || ldRbk.lid === i.U))).asUInt
  ldQflushed := (ldQflushed | excpFlushLd | brRbkFlushLd | ldRbkFlushLd) & ~ldQvldOut & ~ldQvldIn

  for(i <- 0 until LdNum) {
    ldReqRdy(i) := ldReqQ(i).io.enq.ready
    ldReqQ(i).io.enq.valid := ldReq(i).valid
    ldReqQ(i).io.enq.bits  := DontCare
    ldReqQ(i).io.enq.bits.op  := ldReq(i).op
    ldReqQ(i).io.enq.bits.lid := ldReq(i).lid
    ldReqQ(i).io.enq.bits.pid := ldReq(i).pid
    ldReqQ(i).io.enq.bits.addr:= ldReq(i).addr
  }

  for(i <- 0 until LdNum) {
    ldReqQ(i).io.deq.ready := dRdQ(i).io.enq.ready && ldRsp0Q(i).io.enq.ready
  }

  val reqLdDeq = Wire(Vec(LdNum, new LdReqBundle))
  for(i <- 0 until LdNum) {
    reqLdDeq(i).valid := ldReqQ(i).io.deq.valid
    reqLdDeq(i).lid   := ldReqQ(i).io.deq.bits.lid
    reqLdDeq(i).op    := ldReqQ(i).io.deq.bits.op
    reqLdDeq(i).addr  := ldReqQ(i).io.deq.bits.addr
    reqLdDeq(i).pid   := ldReqQ(i).io.deq.bits.pid
    reqLdDeq(i).mask  :=
      Mux(reqLdDeq(i).op === LdOpType.lb , "b00000001".U << reqLdDeq(i).addr(3, 0), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lbu, "b00000001".U << reqLdDeq(i).addr(3, 0), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lh , "b00000011".U << reqLdDeq(i).addr(3, 0), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lhu, "b00000011".U << reqLdDeq(i).addr(3, 0), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lw , "b00001111".U << reqLdDeq(i).addr(3, 0), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lwu, "b00001111".U << reqLdDeq(i).addr(3, 0), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.ld , "b11111111".U << reqLdDeq(i).addr(3, 0), 0.U)
  }

  for(i <- 0 until LidNum) {
    val upVld = VecInit((0 until LdNum).map(j => reqLdDeq(j).valid && reqLdDeq(j).lid === i.U)).asUInt
    when(upVld.orR){
      ldQaddr(i) := Mux1H(upVld, reqLdDeq.map(_.addr))
      ldQmask(i) := Mux1H(upVld, reqLdDeq.map(_.mask))
    }
  }
  val ldQcomeIn = (0 until LdNum).map(i => Mux(reqLdDeq(i).valid, UIntToOH(reqLdDeq(i).lid), 0.U)).reduce(_ | _)
  ldQcome := (ldQcome | ldQcomeIn) & ~ldQvldOut  & ~ldQvldIn

  val stReqDeq = Wire(new StReqBundle)
  val rspLdFwdVld0 = Wire(Vec(LdNum, Bool()))
  val rspLdFwdVld = Wire(Vec(LdNum, Bool()))
  val rspLdFwdSid = Wire(Vec(LdNum, UInt(SidWidth.W)))
  val rspLdFwdDataOrg = Wire(Vec(LdNum, UInt(64.W)))
  val rspLdFwdData = Wire(Vec(LdNum, UInt(64.W)))

  for(i <- 0 until LdNum) {
    rspLdFwdVld0(i) :=
      reqLdDeq(i).valid &&
     !ldQflushed(reqLdDeq(i).lid) &&
      stReqDeq.valid &&
      stReqDeq.addr(VAddrWidth-1, 4) === reqLdDeq(i).addr(VAddrWidth-1, 4) &&
     (stReqDeq.mask & reqLdDeq(i).mask) === reqLdDeq(i).mask &&
      ~QGtBitsByTailInclude(reqLdDeq(i).lid, ldQtailPtr)(stReqDeq.lid)

    val ldFwdVldVec = Wire(UInt(SidNum.W))
    ldFwdVldVec := VecInit((0 until SidNum).map(j =>
      reqLdDeq(i).valid &&
     !ldQflushed(reqLdDeq(i).lid) &&
      stQvld(j) &&
      stQcome(j) &&
     !stQflushed(j) &&
      stQaddr(j)(VAddrWidth-1, 4) === reqLdDeq(i).addr(VAddrWidth-1, 4) &&
     (stQmask(j) & reqLdDeq(i).mask) === reqLdDeq(i).mask &&
     ~QGtBitsByTailInclude(reqLdDeq(i).lid, ldQtailPtr)(stQlid(j))
    )).asUInt |
      Mux(rspLdFwdVld0(i), UIntToOH(stReqDeq.sid), 0.U)

    rspLdFwdVld(i) := ldFwdVldVec.orR
    rspLdFwdSid(i) := QLastSetByHead(ldFwdVldVec, stQheadPtr(0))
    rspLdFwdDataOrg(i) := Mux((rspLdFwdSid(i) === stReqDeq.sid) && stReqDeq.valid, stReqDeq.data, stQdata(rspLdFwdSid(i)) >> (reqLdDeq(i).addr(3,0) << 3.U))
    rspLdFwdData(i) :=
      Mux(reqLdDeq(i).op === LdOpType.lb , Cat(Fill(56, rspLdFwdDataOrg(i)( 7)), rspLdFwdDataOrg(i)( 7,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lbu, Cat(Fill(56, "b0".U                ), rspLdFwdDataOrg(i)( 7,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lh , Cat(Fill(48, rspLdFwdDataOrg(i)(15)), rspLdFwdDataOrg(i)(15,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lhu, Cat(Fill(48, "b0".U                ), rspLdFwdDataOrg(i)(15,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lw , Cat(Fill(32, rspLdFwdDataOrg(i)(31)), rspLdFwdDataOrg(i)(31,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lwu, Cat(Fill(32, "b0".U                ), rspLdFwdDataOrg(i)(31,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.ld ,                                       rspLdFwdDataOrg(i)(63,0) , 0.U)

    ldRsp0Q(i).io.enq.valid := rspLdFwdVld(i)
    ldRsp0Q(i).io.enq.bits  := DontCare
    ldRsp0Q(i).io.enq.bits.data  := rspLdFwdData(i)
    ldRsp0Q(i).io.enq.bits.lid   := reqLdDeq(i).lid
    ldRsp0Q(i).io.enq.bits.pid   := reqLdDeq(i).pid
  }

  val rspLdFwdPartialVld0 = Wire(Vec(LdNum, Bool()))
  val rspLdFwdPartialVld = Wire(Vec(LdNum, Bool()))
  val rspLdFwdPartialSid = Wire(Vec(LdNum, UInt(SidWidth.W)))
  val rspLdFwdPartialDataOrg = Wire(Vec(LdNum, UInt(128.W)))
  val rspLdFwdPartialData = Wire(Vec(LdNum, UInt(64.W)))
  val rspLdFwdPartialMask = Wire(Vec(LdNum, UInt(8.W)))

  for(i <- 0 until LdNum) {
    rspLdFwdPartialVld0(i) :=
      reqLdDeq(i).valid &&
     !ldQflushed(reqLdDeq(i).lid) &&
      stReqDeq.valid &&
      stReqDeq.addr(VAddrWidth-1, 4) === reqLdDeq(i).addr(VAddrWidth-1, 4) &&
     (stReqDeq.mask & reqLdDeq(i).mask) =/=  reqLdDeq(i).mask &&
     (stReqDeq.mask & reqLdDeq(i).mask).orR &&
      ~QGtBitsByTailInclude(reqLdDeq(i).lid, ldQtailPtr)(stReqDeq.lid)

    val ldFwdPartialVldVec = Wire(UInt(SidNum.W))
    ldFwdPartialVldVec := VecInit((0 until SidNum).map(j =>
      reqLdDeq(i).valid &&
     !ldQflushed(reqLdDeq(i).lid) &&
      stQvld(j) &&
      stQcome(j) &&
     !stQflushed(j) &&
      stQaddr(j)(VAddrWidth-1, 4) === reqLdDeq(i).addr(VAddrWidth-1, 4) &&
      (stQmask(j) & reqLdDeq(i).mask).orR &&
      (stQmask(j) & reqLdDeq(i).mask) =/= reqLdDeq(i).mask &&
     ~QGtBitsByTailInclude(reqLdDeq(i).lid, ldQtailPtr)(stQlid(j))
    )).asUInt |
      Mux(rspLdFwdPartialVld0(i), UIntToOH(stReqDeq.sid), 0.U)
    rspLdFwdPartialVld(i) := ldFwdPartialVldVec.orR
    rspLdFwdPartialSid(i) := QLastSetByHead(ldFwdPartialVldVec, stQheadPtr(0))
    rspLdFwdPartialDataOrg(i) := stQdata(rspLdFwdPartialSid(i)) >> (reqLdDeq(i).addr(3,0) << 3.U)
    rspLdFwdPartialData(i) :=
      Mux(reqLdDeq(i).op === LdOpType.lb , Cat(Fill(56, rspLdFwdDataOrg(i)( 7)), rspLdFwdPartialDataOrg(i)( 7,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lbu, Cat(Fill(56, "b0".U                ), rspLdFwdPartialDataOrg(i)( 7,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lh , Cat(Fill(48, rspLdFwdDataOrg(i)(15)), rspLdFwdPartialDataOrg(i)(15,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lhu, Cat(Fill(48, "b0".U                ), rspLdFwdPartialDataOrg(i)(15,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lw , Cat(Fill(32, rspLdFwdDataOrg(i)(31)), rspLdFwdPartialDataOrg(i)(31,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.lwu, Cat(Fill(32, "b0".U                ), rspLdFwdPartialDataOrg(i)(31,0)), 0.U) |
      Mux(reqLdDeq(i).op === LdOpType.ld ,                                       rspLdFwdPartialDataOrg(i)(63,0) , 0.U)
    rspLdFwdPartialMask(i) := (reqLdDeq(i).mask & ~(stQmask(rspLdFwdPartialSid(i)))) >> reqLdDeq(i).addr(3, 0)
  }

  val rspLdNoFwdVld = Wire(Vec(LdNum, Bool()))
  val rspLdNoFwdMask = Wire(Vec(LdNum, UInt(8.W)))

  for(i <- 0 until LdNum) {
    rspLdNoFwdVld(i) :=
      reqLdDeq(i).valid &&
     !ldQflushed(reqLdDeq(i).lid) &&
     !rspLdFwdVld(i) &&
     !rspLdFwdPartialVld(i)
    rspLdNoFwdMask(i) := reqLdDeq(i).mask >> reqLdDeq(i).addr(3, 0)
  }

  for(i <- 0 until LdNum) {
    val mask = Mux(rspLdFwdPartialVld(i), rspLdFwdPartialMask(i), rspLdNoFwdMask(i))
    dRdQ(i).io.enq.valid := rspLdNoFwdVld(i) || rspLdFwdPartialVld(i)
    dRdQ(i).io.enq.bits  :=  DontCare
    dRdQ(i).io.enq.bits.op   := reqLdDeq(i).op
    dRdQ(i).io.enq.bits.lid  := reqLdDeq(i).lid
    dRdQ(i).io.enq.bits.pid  := reqLdDeq(i).pid
    dRdQ(i).io.enq.bits.mask := mask
    dRdQ(i).io.enq.bits.addr := reqLdDeq(i).addr
    dRdQ(i).io.enq.bits.data := rspLdFwdPartialData(i)
  }


  val arbRdAxi = Module(new RRArbiter(new QBundle, LdNum))
  for(i <- 0 until LdNum) {
    arbRdAxi.io.in(i) <> dRdQ(i).io.deq
    arbRdAxi.io.in(i).valid := dRdQ(i).io.deq.valid && dRdSentQ(i).io.enq.ready
    dRdSentQ(i).io.enq <> dRdQ(i).io.deq
    dRdSentQ(i).io.enq.valid :=  dRdQ(i).io.deq.valid && d_ar.ready && arbRdAxi.io.chosen === i.U
  //dRdQ(i).io.deq.ready := d_ar.ready && dRdSentQ(i).io.enq.ready && arbRdAxi.io.in(i).ready 
    dRdQ(i).io.deq.ready := d_ar.ready && dRdSentQ(i).io.enq.ready && arbRdAxi.io.chosen === i.U
  }

  arbRdAxi.io.out.ready := d_ar.ready
  d_ar.valid := arbRdAxi.io.out.valid
  d_ar.bits.id    := arbRdAxi.io.chosen
  d_ar.bits.addr  := arbRdAxi.io.out.bits.addr
  d_ar.bits.len   := 0.U
  d_ar.bits.size  := 5.U
  d_ar.bits.burst := 1.U
  d_ar.bits.lock  := 0.U
  d_ar.bits.cache := 0.U
  d_ar.bits.prot  := 0.U
  d_ar.bits.qos   := 0.U

  d_r.ready := VecInit(ldRsp1Q.map(_.io.enq.ready))(d_r.bits.id)
  for(i <- 0 until LdNum) {
    dRdSentQ(i).io.deq.ready := ldRsp1Q(i).io.enq.ready &&  d_r.valid && d_r.bits.id === i.U

    ldRsp1Q(i).io.enq.valid := d_r.valid && d_r.bits.id === i.U
    val memdata = Wire(UInt(64.W))
    memdata := d_r.bits.data >> (VecInit(dRdSentQ.map(_.io.deq.bits.addr(4, 0) << 3))(d_r.bits.id))
    val stdata = VecInit(dRdSentQ.map(_.io.deq.bits.data))(d_r.bits.id)
    val mask = VecInit(dRdSentQ.map(_.io.deq.bits.mask))(d_r.bits.id)
    val datatmp = Cat(
          Mux(mask(7), memdata(63, 56), stdata(63, 56)),
          Mux(mask(6), memdata(55, 48), stdata(55, 48)),
          Mux(mask(5), memdata(47, 40), stdata(47, 40)),
          Mux(mask(4), memdata(39, 32), stdata(39, 32)),
          Mux(mask(3), memdata(31, 24), stdata(31, 24)),
          Mux(mask(2), memdata(23, 16), stdata(23, 16)),
          Mux(mask(1), memdata(15,  8), stdata(15,  8)),
          Mux(mask(0), memdata( 7,  0), stdata( 7,  0))
        )
    val op = VecInit(dRdSentQ.map(_.io.deq.bits.op))(d_r.bits.id)
    val data = Mux(dRdSentQ(i).io.deq.bits.op === LdOpType.lb , Cat(Fill(56,datatmp( 7)), datatmp( 7, 0)), 0.U) |
               Mux(dRdSentQ(i).io.deq.bits.op === LdOpType.lbu, Cat(Fill(56,"b0".U     ), datatmp( 7, 0)), 0.U) |
               Mux(dRdSentQ(i).io.deq.bits.op === LdOpType.lh , Cat(Fill(48,datatmp(15)), datatmp(15, 0)), 0.U) |
               Mux(dRdSentQ(i).io.deq.bits.op === LdOpType.lhu, Cat(Fill(48,"b0".U     ), datatmp(15, 0)), 0.U) |
               Mux(dRdSentQ(i).io.deq.bits.op === LdOpType.lw , Cat(Fill(32,datatmp(31)), datatmp(31, 0)), 0.U) |
               Mux(dRdSentQ(i).io.deq.bits.op === LdOpType.lwu, Cat(Fill(32,"b0".U     ), datatmp(31, 0)), 0.U) |
               Mux(dRdSentQ(i).io.deq.bits.op === LdOpType.ld , datatmp, 0.U)
    val lid = VecInit(dRdSentQ.map(_.io.deq.bits.lid))(d_r.bits.id)
    val pid = VecInit(dRdSentQ.map(_.io.deq.bits.pid))(d_r.bits.id)
    ldRsp1Q(i).io.enq.bits := DontCare
    ldRsp1Q(i).io.enq.bits.data := data
    ldRsp1Q(i).io.enq.bits.lid  := lid
    ldRsp1Q(i).io.enq.bits.pid  := pid
  }


  val ldRspArb = Seq.fill(LdNum)(Module(new RRArbiter(new QBundle, 2)))
  val rspLdLid = Wire(Vec(LdNum, UInt(LidWidth.W)))
  for(i <- 0 until LdNum) {
    ldRspArb(i).io.in(0) <> ldRsp0Q(i).io.deq
    ldRspArb(i).io.in(1) <> ldRsp1Q(i).io.deq

    ldRsp0Q(i).io.deq.ready := UIntToOH(ldRspArb(i).io.chosen)(0)
    ldRsp1Q(i).io.deq.ready := UIntToOH(ldRspArb(i).io.chosen)(1)

    ldRspArb(i).io.out.ready := true.B
    ldRsp(i).valid := ldRspArb(i).io.out.valid
    ldRsp(i).pid   := ldRspArb(i).io.out.bits.pid
    ldRsp(i).data  := ldRspArb(i).io.out.bits.data
    rspLdLid(i)    := ldRspArb(i).io.out.bits.lid
  }
  val ldQgoneIn = (0 until LdNum).map(i => Mux(ldRspArb(i).io.out.fire, UIntToOH(rspLdLid(i)), 0.U)).reduce(_ | _)
  ldQgone := (ldQgone | ldQgoneIn) &  ~ldQvldOut  & ~ldQvldIn

  //-----------------------------------------------------------------
  // STORE
  //-----------------------------------------------------------------
  val stQvldIn = (0 until PoolInNum).map(i => Mux(instIn(i).valid && instIn(i).isSt, UIntToOH(instIn(i).sid), 0.U)).reduce(_ | _)
  val stQvldOut = (0 until PoolInNum).map(i => Mux(stQrlsVld(i), UIntToOH(stQheadPtr(i)), 0.U)).reduce(_ | _)
  stQvld := (stQvld | stQvldIn) & ~stQvldOut

  for(i <- 0 until LidNum) {
    val upVld = VecInit((0 until PoolInNum).map(j => instIn(j).valid && instIn(j).isSt && instIn(j).sid === i.U)).asUInt
    when(upVld.orR){
      stQbid(i) := Mux1H(upVld, instIn.map(_.bid))
      stQlid(i) := Mux1H(upVld, instIn.map(_.lid))
    }
  }

  val excpFlushSt = Mux(redirect.flush, stQvld, 0.U)
  val brRbkFlushSt = VecInit((0 until LidNum).map(i => stQvld(i) && !stQconfirm(i) && redirect.brRBK && redirect.flushedBid(stQbid(i)))).asUInt
  val ldRbkFlushSt = VecInit((0 until LidNum).map(i => stQvld(i) && !stQconfirm(i) && ldRbk.valid    && ldRbk.lidVec(stQlid(i)))).asUInt
  stQflushed := (stQflushed | excpFlushSt | brRbkFlushSt | ldRbkFlushSt) & ~stQvldOut & ~stQvldIn

  for(i <- 0 until StNum) {
    stReqRdy(i) := stReqQ(i).io.enq.ready
    stReqQ(i).io.enq.valid := stReq(i).valid
    stReqQ(i).io.enq.bits  := DontCare
    stReqQ(i).io.enq.bits.op   := stReq(i).op
    stReqQ(i).io.enq.bits.sid  := stReq(i).sid
    stReqQ(i).io.enq.bits.lid  := stReq(i).lid
    stReqQ(i).io.enq.bits.iid  := stReq(i).iid
    stReqQ(i).io.enq.bits.addr := stReq(i).addr
    stReqQ(i).io.enq.bits.data := stReq(i).data
  }

  val arbStReqDeq = Module(new RRArbiter(new QBundle, StNum))
  for(i <- 0 until StNum ) {
    arbStReqDeq.io.in(i) <> stReqQ(i).io.deq
  }

  arbStReqDeq.io.out.ready := true.B
  stReqDeq.valid := arbStReqDeq.io.out.valid
  stReqDeq.sid   := arbStReqDeq.io.out.bits.sid
  stReqDeq.lid   := arbStReqDeq.io.out.bits.lid
  stReqDeq.iid   := arbStReqDeq.io.out.bits.iid
  stReqDeq.op    := arbStReqDeq.io.out.bits.op
  stReqDeq.addr  := arbStReqDeq.io.out.bits.addr
  stReqDeq.mask  :=
    Mux(stReqDeq.op === StOpType.sb, "b00000001".U << stReqDeq.addr(3, 0), 0.U) |
    Mux(stReqDeq.op === StOpType.sh, "b00000011".U << stReqDeq.addr(3, 0), 0.U) |
    Mux(stReqDeq.op === StOpType.sw, "b00001111".U << stReqDeq.addr(3, 0), 0.U) |
    Mux(stReqDeq.op === StOpType.sd, "b11111111".U << stReqDeq.addr(3, 0), 0.U)
  stReqDeq.data := arbStReqDeq.io.out.bits.data

  val stReqMergeFromVldVec = Wire(UInt(SidNum.W))
  val stReqMergeFromVld = Wire(Bool())
  val stReqMergeFromSid = Wire(UInt(SidWidth.W))
  val stReqMergeFromData = Wire(UInt(128.W))
  val stReqMergeFromMask = Wire(UInt(16.W))

  stReqMergeFromVldVec := VecInit((0 until SidNum).map( i =>
    stReqDeq.valid &&
    stQvld(i) &&
    stQcome(i) &&
   !stQflushed(i) &&
    stQaddr(i)(VAddrWidth-1, 4) === stReqDeq.addr(VAddrWidth-1, 4) &&
    QLtBitsByHead(stReqDeq.sid, stQheadPtr(0))(i)
  )).asUInt
  stReqMergeFromVld  := stReqMergeFromVldVec.orR
  stReqMergeFromSid  := QLastSetByHead(stReqMergeFromVldVec, stQheadPtr(0))
  stReqMergeFromData := stQdata(stReqMergeFromSid)
  stReqMergeFromMask := stQmask(stReqMergeFromSid)

  val stReqData         = Wire(UInt(128.W))
  val stReqDataMerged   = Wire(UInt(128.W))
  val stReqMaskMerged   = Wire(UInt(16.W))
  stReqData := stReqDeq.data << (stReqDeq.addr(3, 0) << 3.U)
  stReqDataMerged := Cat(
    Mux(stReqDeq.mask(15), stReqData(127, 120), stReqMergeFromData(127, 120)),
    Mux(stReqDeq.mask(14), stReqData(119, 112), stReqMergeFromData(119, 112)),
    Mux(stReqDeq.mask(13), stReqData(111, 104), stReqMergeFromData(111, 104)),
    Mux(stReqDeq.mask(12), stReqData(103,  96), stReqMergeFromData(103,  96)),
    Mux(stReqDeq.mask(11), stReqData( 95,  88), stReqMergeFromData( 95,  88)),
    Mux(stReqDeq.mask(10), stReqData( 87,  80), stReqMergeFromData( 87,  80)),
    Mux(stReqDeq.mask( 9), stReqData( 79,  72), stReqMergeFromData( 79,  72)),
    Mux(stReqDeq.mask( 8), stReqData( 71,  64), stReqMergeFromData( 71,  64)),
    Mux(stReqDeq.mask( 7), stReqData( 63,  56), stReqMergeFromData( 63,  56)),
    Mux(stReqDeq.mask( 6), stReqData( 55,  48), stReqMergeFromData( 55,  48)),
    Mux(stReqDeq.mask( 5), stReqData( 47,  40), stReqMergeFromData( 47,  40)),
    Mux(stReqDeq.mask( 4), stReqData( 39,  32), stReqMergeFromData( 39,  32)),
    Mux(stReqDeq.mask( 3), stReqData( 31,  24), stReqMergeFromData( 31,  24)),
    Mux(stReqDeq.mask( 2), stReqData( 23,  16), stReqMergeFromData( 23,  16)),
    Mux(stReqDeq.mask( 1), stReqData( 15,   8), stReqMergeFromData( 15,   8)),
    Mux(stReqDeq.mask( 0), stReqData(  7,   0), stReqMergeFromData(  7,   0))
  )
  stReqMaskMerged := Mux(stReqMergeFromVld, stReqMergeFromMask | stReqDeq.mask, stReqDeq.mask)

  val stQcomeIn = Mux(stReqDeq.valid, UIntToOH(stReqDeq.sid), 0.U)
  stQcome := (stQcome | stQcomeIn) & ~stQvldOut & ~stQvldIn

  when(stReqDeq.valid){
    stQaddr(stReqDeq.sid) := stReqDeq.addr
    stQop  (stReqDeq.sid) := stReqDeq.op
    stQmask(stReqDeq.sid) := stReqMaskMerged
    stQdata(stReqDeq.sid) := stReqDataMerged
  }

  val stReqMergeToVldVec = Wire(UInt(SidNum.W))
  stReqMergeToVldVec :=  VecInit((0 until SidNum).map( i =>
    stReqDeq.valid &&
    stQvld(i) &&
    stQcome(i) &&
   !stQflushed(i) &&
    stQaddr(i)(VAddrWidth-1, 4) === stReqDeq.addr(VAddrWidth-1, 4) &&
   ~(QLeBitsByHead(stReqDeq.sid, stQheadPtr(0)))(i)
  )).asUInt

  for(i <- 0 until SidNum) {
    val mergedData = Cat(
      Mux(stQmask(i)(15), stQdata(i)(127, 120), stReqData(127, 120)),
      Mux(stQmask(i)(14), stQdata(i)(119, 112), stReqData(119, 112)),
      Mux(stQmask(i)(13), stQdata(i)(111, 104), stReqData(111, 104)),
      Mux(stQmask(i)(12), stQdata(i)(103,  96), stReqData(103,  96)),
      Mux(stQmask(i)(11), stQdata(i)( 95,  88), stReqData( 95,  88)),
      Mux(stQmask(i)(10), stQdata(i)( 87,  80), stReqData( 87,  80)),
      Mux(stQmask(i)( 9), stQdata(i)( 79,  72), stReqData( 79,  72)),
      Mux(stQmask(i)( 8), stQdata(i)( 71,  64), stReqData( 71,  64)),
      Mux(stQmask(i)( 7), stQdata(i)( 63,  56), stReqData( 63,  56)),
      Mux(stQmask(i)( 6), stQdata(i)( 55,  48), stReqData( 55,  48)),
      Mux(stQmask(i)( 5), stQdata(i)( 47,  40), stReqData( 47,  40)),
      Mux(stQmask(i)( 4), stQdata(i)( 39,  32), stReqData( 39,  32)),
      Mux(stQmask(i)( 3), stQdata(i)( 31,  24), stReqData( 31,  24)),
      Mux(stQmask(i)( 2), stQdata(i)( 23,  16), stReqData( 23,  16)),
      Mux(stQmask(i)( 1), stQdata(i)( 15,   8), stReqData( 15,   8)),
      Mux(stQmask(i)( 0), stQdata(i)(  7,   0), stReqData(  7,   0))
    )
    when(stReqMergeToVldVec(i)){
      stQdata(i) := mergedData
      stQmask(i) := stQmask(i) | stReqDeq.mask
    }
  }

  stRsp       := DontCare
  stRsp.valid := stReqDeq.valid && !stQflushed(stReqDeq.sid)
  stRsp.iid   := stReqDeq.iid

  val ldRbkVldVec0 = Wire(UInt(LidNum.W))
  val ldRbkVldVec1 = Wire(UInt(LidNum.W))
  val ldRbkVldVec  = Wire(UInt(LidNum.W))

  //ld coming
  ldRbkVldVec0:=  VecInit((0 until 4).map( i => Mux(
    stReqDeq.valid &&
    ldReqQ(i).io.deq.fire &&
   !ldQflushed(reqLdDeq(i).lid) &&
    reqLdDeq(i).addr(VAddrWidth-1, 4) === stReqDeq.addr(VAddrWidth-1, 4) &&
    (reqLdDeq(i).mask & stReqDeq.mask).orR &&
    ~(QLeBitsByHead(stReqDeq.sid, stQheadPtr(0)))(ldQsid(reqLdDeq(i).lid)),
    UIntToOH(reqLdDeq(i).lid), 0.U)
  )).reduce(_ | _)

  //ldQ
  ldRbkVldVec1:=  VecInit((0 until LidNum).map( i =>
    stReqDeq.valid &&
    ldQvld(i) &&
    ldQcome(i) &&
   !ldQflushed(i) &&
    ldQaddr(i)(VAddrWidth-1, 4) === stReqDeq.addr(VAddrWidth-1, 4) &&
    (ldQmask(i) & stReqDeq.mask).orR &&
    ~(QLeBitsByHead(stReqDeq.sid, stQheadPtr(0)))(ldQsid(i))
  )).asUInt

  //ld allocating
  val ldRbkLidVec = Wire(UInt(LidNum.W))
  ldRbkLidVec :=  VecInit((0 until PoolInNum).map( i =>
    Mux(instIn(i).valid, UIntToOH(instIn(i).lid), 0.U)
  )).reduce(_ | _)

  ldRbkVldVec := ldRbkVldVec0 | ldRbkVldVec1

  val ldRbkLid = QFirstSetByHead(ldRbkVldVec, ldQheadPtr(0))
  ldRbkR        := DontCare
  ldRbkR.valid  := ldRbkVldVec.orR
  ldRbkR.lid    := ldRbkLid
  ldRbkR.lidVec := QGtBitsByTailInclude(ldRbkLid, ldQtailPtr)  | ldRbkLidVec
  ldRbkR.pc     := ldQpc(ldRbkLid)
  val stReqDeqD1 = RegNext(stReqDeq)
  ldRbk := ldRbkR
  ldRbk.valid := ldRbkR.valid && !(stReqDeqD1.valid && stQflushed(stReqDeqD1.sid)) && !(ldQflushed(ldRbkR.lid))

  val lidSafe = Wire(UInt(LidNum.W))
  lidSafe := ~ldQvld & ~UIntToOH(ldQtailPtr) | UIntToOH(ldQheadPtr(0))

  val stConfirmVld = Wire(Vec(PoolInNum, Bool()))
  val stFlushed = VecInit((0 until PoolInNum).map(i => stQflushed(stQconfirmPtr(i)))).asUInt

  for(i <- 0 until PoolInNum){
    if(i == 0) {
      stConfirmVld(i) :=
        stQvld(stQconfirmPtr(i)) &&
        (stQflushed(stQconfirmPtr(i)) || stQcome(stQconfirmPtr(i))) &&
        bidSafe(stQbid(stQconfirmPtr(i))) &&
        lidSafe(stQlid(stQconfirmPtr(i)))
    }else{
      stConfirmVld(i) :=
        stConfirmVld(i-1) &&
        stQvld(stQconfirmPtr(i)) &&
        PopCount(~stFlushed(i-1, 0)) < 2.U  &&
        (stQflushed(stQconfirmPtr(i)) || stQcome(stQconfirmPtr(i))) &&
        bidSafe(stQbid(stQconfirmPtr(i))) &&
        lidSafe(stQlid(stQconfirmPtr(i)))
    }
  }

  val stQconfirmIn = (0 until PoolInNum).map(i => Mux(stConfirmVld(i), UIntToOH(stQconfirmPtr(i)), 0.U)).reduce(_ | _)
  stQconfirm := (stQconfirm & ~stQvldOut & ~stQvldIn) | stQconfirmIn


  for(i <- 0 until PoolInNum) {
    when(stConfirmVld(0)) {
      stQconfirmPtr(i) := stQconfirmPtr(i) + PopCount(stConfirmVld.asUInt)
    }
  }

  val stWrMem = stQvld & stQconfirm & ~stQflushed & ~stQ2wrQ

  val dWrQenqVld = stWrMem.orR
  val dWrQenqSid = QFirstSetByHead(stWrMem, stQ2wrQPtr)
  val dWrQenqOp  = stQop(dWrQenqSid)
  val dWrQenqAddr = stQaddr(dWrQenqSid)
  val dWrQenqData = Wire(UInt(64.W))
  dWrQenqData := stQdata(dWrQenqSid) >> (dWrQenqAddr(3, 0) << 3.U)
  val dWrQenqMask = Wire(UInt(8.W))
  dWrQenqMask :=
    Mux(dWrQenqOp === StOpType.sb, "b00000001".U, 0.U) |
    Mux(dWrQenqOp === StOpType.sh, "b00000011".U, 0.U) |
    Mux(dWrQenqOp === StOpType.sw, "b00001111".U, 0.U) |
    Mux(dWrQenqOp === StOpType.sd, "b11111111".U, 0.U)

  dWrQaw.io.enq.valid := dWrQenqVld && dWrQw.io.enq.ready
  dWrQaw.io.enq.bits := DontCare
  dWrQaw.io.enq.bits.sid  := dWrQenqSid
  dWrQaw.io.enq.bits.addr := dWrQenqAddr

  dWrQw.io.enq.valid := dWrQenqVld && dWrQaw.io.enq.ready
  dWrQw.io.enq.bits := DontCare
  dWrQw.io.enq.bits.sid  := dWrQenqSid
  dWrQw.io.enq.bits.mask := dWrQenqMask
  dWrQw.io.enq.bits.addr := dWrQenqAddr
  dWrQw.io.enq.bits.data := dWrQenqData

  when(dWrQaw.io.enq.fire) {
    stQ2wrQPtr := dWrQenqSid
  }
  val stQ2wrQIn = Mux(dWrQaw.io.enq.fire, UIntToOH(dWrQenqSid), 0.U)
  stQ2wrQ := (stQ2wrQ | stQ2wrQIn) &  ~stQvldOut &  ~stQvldIn

  dWrQaw.io.deq.ready := d_aw.ready
  d_aw.valid := dWrQaw.io.deq.valid
  d_aw.bits.id    := dWrQaw.io.deq.bits.sid
  d_aw.bits.addr  := dWrQaw.io.deq.bits.addr
  d_aw.bits.len   := 0.U
  d_aw.bits.size  := 5.U
  d_aw.bits.burst := 1.U
  d_aw.bits.lock  := 0.U
  d_aw.bits.cache := 0.U
  d_aw.bits.prot  := 0.U
  d_aw.bits.qos   := 0.U

  dWrQw.io.deq.ready := d_w.ready
  d_w.valid  := dWrQw.io.deq.valid
  d_w.bits.data   := dWrQw.io.deq.bits.data << (dWrQw.io.deq.bits.addr(4, 0) << 3.U)
  d_w.bits.strb   := dWrQw.io.deq.bits.mask << dWrQw.io.deq.bits.addr(4, 0)
  d_w.bits.last   := true.B

  d_b.ready  := true.B

  val stQgoneInA = Mux(d_aw.fire, UIntToOH(dWrQaw.io.deq.bits.sid), 0.U)
  stQgoneA := (stQgoneA | stQgoneInA) &  ~stQvldOut &  ~stQvldIn
  val stQgoneInD = Mux(d_w.fire, UIntToOH(dWrQw.io.deq.bits.sid), 0.U)
  stQgoneD := (stQgoneD | stQgoneInD) &  ~stQvldOut &  ~stQvldIn

  //-----------------------------------------------------------------
  // ID release
  //-----------------------------------------------------------------
  val sidSafe = Wire(UInt(SidNum.W))
  sidSafe := ~(~stQconfirm & stQvld) & ~UIntToOH(stQtailPtr) | UIntToOH(stQconfirmPtr(0))

  for(i <- 0 until PoolInNum){
    if(i == 0) {
      ldQrlsVld(i) :=
        ldQvld(ldQheadPtr(i)) &&
        (ldQflushed(ldQheadPtr(i)) || ldQgone(ldQheadPtr(i))) &&
        sidSafe(ldQsid(ldQheadPtr(i)))  &&
        bidSafe(ldQbid(ldQheadPtr(i)))
      stQrlsVld(i) :=
        stQvld(stQheadPtr(i)) &&
        (stQflushed(stQheadPtr(i)) && stQconfirm(stQheadPtr(i)) || (stQgoneA(stQheadPtr(i)) && stQgoneD(stQheadPtr(i)))) &&
        lidSafe(stQlid(stQheadPtr(i)))  &&
        bidSafe(stQbid(stQheadPtr(i)))
    } else {
      ldQrlsVld(i) :=
        ldQrlsVld(i-1) &&
        ldQvld(ldQheadPtr(i)) &&
        (ldQflushed(ldQheadPtr(i)) || ldQgone(ldQheadPtr(i))) &&
        sidSafe(ldQsid(ldQheadPtr(i)))  &&
        bidSafe(ldQbid(ldQheadPtr(i)))
      stQrlsVld(i) :=
        stQrlsVld(i-1) &&
        stQvld(stQheadPtr(i)) &&
        (stQflushed(stQheadPtr(i)) && stQconfirm(stQheadPtr(i)) || (stQgoneD(stQheadPtr(i)) && stQgoneD(stQheadPtr(i)))) &&
        lidSafe(stQlid(stQheadPtr(i)))  &&
        bidSafe(stQbid(stQheadPtr(i)))
    }
  }

  for(i <- 0 until PoolInNum) {
    when(ldQrlsVld(0)) {
      ldQheadPtr(i) := ldQheadPtr(i) + PopCount(ldQrlsVld.asUInt)
    }
    when(stQrlsVld(0)) {
      stQheadPtr(i) := stQheadPtr(i) + PopCount(stQrlsVld.asUInt)
    }
  }

  lidRls := ldQrlsVld.asUInt
  sidRls := stQrlsVld.asUInt

  ldExcpWb := DontCare
  stExcpWb := DontCare

  dontTouch(d_ar)
  dontTouch(d_aw)
  dontTouch(d_b)
}

