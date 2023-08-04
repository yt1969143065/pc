package difftest

import chisel3._
import chisel3.util._
import Chisel.BlackBox
import chisel3.experimental.{DataMirror, ExtModule}

trait DifftestParameter {
}

trait DifftestWithClock {
  val clock  = Input(Clock())
}

trait DifftestWithIndex {
  val index = Input(UInt(8.W))
}

abstract class DifftestBundle extends Bundle
  with DifftestParameter
  with DifftestWithClock

class DiffBasicInstrCommitIO extends DifftestBundle with DifftestWithIndex {
  val valid    = Input(Bool())
  val rfwen    = Input(Bool())
  val fpwen    = Input(Bool())
  val wpdest   = Input(UInt(32.W))
  val wdest    = Input(UInt(8.W))
}

class DiffInstrCommitIO extends DiffBasicInstrCommitIO {
  val pc       = Input(UInt(64.W))
  val instr    = Input(UInt(32.W))
}

class DiffBasicTrapEventIO extends DifftestBundle {
  val valid    = Input(Bool())
}

class DiffTrapEventIO extends DiffBasicTrapEventIO {
  val code     = Input(UInt(3.W))
  val pc       = Input(UInt(64.W))
}

class DiffArchIntRegStateIO extends DifftestBundle {
  val gpr = Input(Vec(32, UInt(64.W)))
}

class DiffCSRStateIO extends DifftestBundle {
  val priviledgeMode = Input(UInt(2.W))
  val mstatus = Input(UInt(64.W))
  val sstatus = Input(UInt(64.W))
  val mepc = Input(UInt(64.W))
  val sepc = Input(UInt(64.W))
  val mtval = Input(UInt(64.W))
  val stval = Input(UInt(64.W))
  val mtvec = Input(UInt(64.W))
  val stvec = Input(UInt(64.W))
  val mcause = Input(UInt(64.W))
  val scause = Input(UInt(64.W))
  val satp = Input(UInt(64.W))
  val mip = Input(UInt(64.W))
  val mie = Input(UInt(64.W))
  val mscratch = Input(UInt(64.W))
  val sscratch = Input(UInt(64.W))
  val mideleg = Input(UInt(64.W))
  val medeleg = Input(UInt(64.W))
}

class DiffArchEventIO extends DifftestBundle {
  val intrNO = Input(UInt(32.W))
  val cause = Input(UInt(32.W))
  val exceptionPC = Input(UInt(64.W))
  val exceptionInst = Input(UInt(32.W))
}

abstract class DifftestModule[T <: DifftestBundle] extends ExtModule with HasExtModuleInline
{
  val io: T

  def getDirectionString(data: Data): String = {
    if (DataMirror.directionOf(data) == ActualDirection.Input) "input " else "output"
  }

  def getDPICArgString(argName: String, data: Data): String = {
    val directionString = getDirectionString(data)
    val typeString = data.getWidth match {
      case 1                                  => "bit"
      case width if width > 1  && width <= 8  => "byte"
      case width if width > 8  && width <= 32 => "int"
      case width if width > 32 && width <= 64 => "longint"
      case _ => s"unsupported io type of width ${data.getWidth}!!\n"
    }
    val argString = Seq(directionString, f"${typeString}%7s", argName)
    argString.mkString(" ")
  }

  def getModArgString(argName: String, data: Data): String = {
    val widthString = if (data.getWidth == 1) "      " else f"[${data.getWidth - 1}%2d:0]"
    val argString = Seq(getDirectionString(data), widthString, s"$argName")
    argString.mkString(" ")
  }

  def moduleName = this.getClass.getSimpleName
  def moduleBody: String = {
    // ExtModule implicitly adds io_* prefix to the IOs (because the IO val is named as io).
    // This is different from BlackBoxes.
    val interfaces = io.elements.toSeq.reverse.flatMap{ case (name, data) =>
      data match {
        case vec: Vec[Data] => vec.zipWithIndex.map{ case (v, i) => (s"io_${name}_$i", v) }
        case _ => Seq((s"io_$name", data))
      }
    }
    // (1) DPI-C function prototype
    val dpicInterfaces = interfaces.filterNot(_._1 == "io_clock")
    val dpicName = s"v_difftest_${moduleName.replace("Difftest", "")}"
    val dpicDecl =
      s"""
         |import "DPI-C" function void $dpicName (
         |${dpicInterfaces.map(ifc => getDPICArgString(ifc._1, ifc._2)).mkString(",\n")}
         |);
         |""".stripMargin
    // (2) module definition
    val modPorts = interfaces.map(i => getModArgString(i._1, i._2)).mkString(",\n")
    val modDef =
      s"""
         |module $moduleName(
         |  $modPorts
         |);
         |`ifndef SYNTHESIS
         |`ifdef DIFFTEST
         |$dpicDecl
         |  always @(posedge io_clock) begin
         |    $dpicName (${dpicInterfaces.map(_._1).mkString(",")});
         |  end
         |`endif
         |`endif
         |endmodule
         |""".stripMargin
    modDef
  }
  def instantiate(): Unit = setInline(s"$moduleName.v", moduleBody)
}

class DifftestBaseModule[T <: DifftestBundle](gen: T) extends DifftestModule[T] {
  val io = IO(gen)
  instantiate()
}

class DifftestInstrCommit extends DifftestBaseModule(new DiffInstrCommitIO)
class DifftestTrapEvent extends DifftestBaseModule(new DiffTrapEventIO)
class DifftestArchIntRegState extends DifftestBaseModule(new DiffArchIntRegStateIO)
class DifftestArchEvent extends DifftestBaseModule(new DiffArchEventIO)
class DifftestCSRState extends DifftestBaseModule(new DiffCSRStateIO)

