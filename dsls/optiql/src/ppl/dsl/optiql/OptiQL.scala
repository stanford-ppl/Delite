package ppl.dsl.optiql

import scala.reflect.RefinedManifest

import ops._
import scala.virtualization.lms.common._
import ppl.delite.framework.ops._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.opencl.TargetOpenCL
import ppl.delite.framework.codegen.cpp.TargetCpp
import ppl.delite.framework.{Config, DeliteApplication}
import java.io.{FileWriter, BufferedWriter, File}
import ppl.delite.framework.codegen.{Utils, Target}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.transform.MultiloopSoATransformWithReduceExp
import ppl.delite.framework.{DeliteInteractive, DeliteInteractiveRunner, DeliteRestageOps, DeliteRestageOpsExp, DeliteRestageRunner}
import ppl.delite.framework.codegen.restage.{DeliteCodeGenRestage,LMSCodeGenRestage,TargetRestage}


/**
 * These are the lifted scala constructs that only operate on the Rep world. These are usually safe to mix in
 */
trait OptiQLScalaOpsPkg extends Base with MiscOps with BooleanOps with OrderingOps with PrimitiveOps with ObjectOps with StringOps with TupleOps with StructOps with NumericOps with ArrayOps with DeliteArrayOps with IfThenElse with Equal with CastingOps

/**
 * This trait adds the Ops that are specific to OptiQL
 */
trait OptiQL extends OptiQLScalaOpsPkg with TableOps with QueryableOps with DateOps with OptiQLMiscOps with InputReaderOps with Types
  with DeliteArrayOps with DeliteArrayBufferOps with DeliteMapOps {
  this: OptiQLApplication =>
}

trait OptiQLInteractive extends OptiQLApplication with DeliteInteractive
trait OptiQLInteractiveRunner[R] extends OptiQLApplicationRunner with DeliteInteractiveRunner[R]

trait OptiQLLower extends OptiQLApplication with DeliteRestageOps
trait OptiQLLowerRunner[R] extends OptiQLApplicationRunner with DeliteRestageRunner[R]

object OptiQL_ {
  def apply[R](b: => R) = new Scope[OptiQLLower, OptiQLLowerRunner[R], R](b)
}

/**
 * These are the lifted scala constructs, which convert a concrete type to a Rep type.
 * These can be dangerous if you mix them in to the wrong place
 */
trait OptiQLLift extends LiftString with LiftPrimitives with LiftNumeric {
  this: OptiQL =>
}

/**
 * Scala IR nodes
 */
trait OptiQLScalaOpsPkgExp extends OptiQLScalaOpsPkg with MiscOpsExp with IOOpsExp with SeqOpsExp with OrderingOpsExp with BooleanOpsExp with EqualExp with MathOpsExp
  with PrimitiveOpsExp with ObjectOpsExp with StringOpsExp with TupleOpsExp with StructExp with NumericOpsExp with ArrayOpsExp with ArrayBufferOpsExp with IfThenElseExp
  with StructFatExpOptCommon with CastingOpsExp with RangeOpsExp with HashMapOpsExp

/**
 * Ops available only to the compiler, and not our applications
 */
trait OptiQLCompiler extends OptiQL with IOOps with SeqOps with Variables with While with DeliteArrayOps with DeliteArrayBufferCompilerOps with DeliteFileReaderOps {
  this: OptiQLApplication with OptiQLExp =>
}

/**
 * This trait comprises the IR nodes for OptiQL and the code required to instantiate code generators
 */
trait OptiQLExp extends OptiQLCompiler with OptiQLScalaOpsPkgExp with TableOpsExp with DateOpsExp with DateImplOpsStandard with QueryableOpsExpOpt with OptiQLMiscOpsExp
  with InputReaderOpsExp with InputReaderImplOpsStandard with DeliteCollectionOpsExp with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp with DeliteMapOpsExp with DeliteFileReaderOpsExp with DSArrayOpsExp 
  with DeliteRestageOpsExp with DeliteAllOverridesExp with MultiloopSoATransformWithReduceExp {

  this: DeliteApplication with OptiQLApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiQLExp.this.type}) : GenericFatCodegen{val IR: OptiQLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiQLCodeGenScala{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _:TargetCuda => new OptiQLCodeGenCuda{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _:TargetOpenCL => new OptiQLCodeGenOpenCL{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _:TargetCpp => new OptiQLCodeGenC{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _:TargetRestage => new OptiQLCodeGenRestage{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _ => throw new RuntimeException("OptiQL does not support this target")
    }
  }

}

/**
 * Codegen traits
 */
trait OptiQLScalaCodeGenPkg extends ScalaGenMiscOps with ScalaGenIOOps with ScalaGenSeqOps with ScalaGenOrderingOps with ScalaGenBooleanOps with ScalaGenEqual with ScalaGenVariables
  with ScalaGenPrimitiveOps with ScalaGenObjectOps with ScalaGenStringOps with ScalaGenTupleOps with ScalaGenNumericOps with ScalaGenArrayOps with ScalaGenArrayBufferOps with ScalaGenIfThenElseFat with ScalaGenImplicitOps with ScalaGenCastingOps {
  val IR: OptiQLScalaOpsPkgExp
}

trait OptiQLCudaCodeGenPkg extends CudaGenMiscOps with CudaGenIOOps with CudaGenSeqOps with CudaGenOrderingOps with CudaGenBooleanOps with CudaGenEqual with CudaGenVariables
  with CudaGenPrimitiveOps with CudaGenObjectOps with CudaGenStringOps with CudaGenTupleOps with CudaGenNumericOps with CudaGenArrayOps with CudaGenIfThenElseFat with CudaGenImplicitOps with CudaGenCastingOps {
  val IR: OptiQLScalaOpsPkgExp
}

trait OptiQLOpenCLCodeGenPkg extends OpenCLGenMiscOps with OpenCLGenIOOps with OpenCLGenSeqOps with OpenCLGenOrderingOps with OpenCLGenBooleanOps with OpenCLGenEqual with OpenCLGenVariables
  with OpenCLGenPrimitiveOps with OpenCLGenObjectOps with OpenCLGenStringOps with OpenCLGenTupleOps with OpenCLGenNumericOps with OpenCLGenArrayOps with OpenCLGenIfThenElseFat with OpenCLGenImplicitOps with OpenCLGenCastingOps {
  val IR: OptiQLScalaOpsPkgExp
}

trait OptiQLCCodeGenPkg extends CGenMiscOps with CGenIOOps with CGenSeqOps with CGenOrderingOps with CGenBooleanOps with CGenEqual with CGenVariables
  with CGenPrimitiveOps with CGenObjectOps with CGenStringOps with CGenTupleOps with CGenNumericOps with CGenArrayOps with CGenIfThenElseFat with CGenImplicitOps with CGenCastingOps {
  val IR: OptiQLScalaOpsPkgExp
}

trait OptiQLCodeGenBase extends GenericFatCodegen {
  val IR: DeliteApplication with OptiQLExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  def dsmap(line: String) = line

  //TODO HC: This is copied and pasted from OptiML, need to be refactored
  override def emitDataStructures(path: String) {
    val s = File.separator
    val dsRoot = Config.homeDir + s+"dsls"+s+"optiql"+s+"src"+s+"ppl"+s+"dsl"+s+"optiql"+s+"datastruct"+s + this.toString
    emitDSHelper(path, dsRoot)
  }

  def emitDSHelper(path:String, dsRoot:String) {
    val s = File.separator
    val dsDir = new File(dsRoot)
    if (!dsDir.exists) return
    val outDir = new File(path)
    outDir.mkdirs()

    for (f <- dsDir.listFiles) {
      val outFile = new File(path + f.getName)
      if(f.isDirectory) {
        emitDSHelper(path + f.getName + s, dsRoot + s + f.getName)
      } else {
        val out = new BufferedWriter(new FileWriter(outFile))
        for (line <- scala.io.Source.fromFile(f).getLines) {
          out.write(dsmap(line) + "\n")
        }
        out.close()
      }

    }
  }
}

trait OptiQLCodeGenRestage extends OptiQLScalaCodeGenPkg with DeliteCodeGenRestage with LMSCodeGenRestage { 
  val IR: DeliteApplication with OptiQLExp
  import IR._

  // we shouldn't need this if we have a proper lowering stage (i.e. transformation)
  override def remap[A](m: Manifest[A]): String = {    
    m match {
      case m if m.erasure.getSimpleName == "Date" => "Int"
      case _ => super.remap(m)
    }   
  }
}

trait OptiQLCodeGenScala extends OptiQLCodeGenBase with OptiQLScalaCodeGenPkg with ScalaGenOptiQLMiscOps with ScalaGenQueryableOps
  with ScalaGenDeliteCollectionOps with ScalaGenDeliteOps with ScalaGenDeliteStruct with ScalaGenDeliteArrayOps with ScalaGenDeliteArrayBufferOps with ScalaGenDeliteMapOps with ScalaGenDeliteFileReaderOps with ScalaGenDSArrayOps with DeliteScalaGenAllOverrides {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = m match {
    case m if m.erasure.getSimpleName == "Date" => "Int"
    case _ => dsmap(super.remap(m))
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optiql.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res
  }
}

trait OptiQLCodeGenCuda extends OptiQLCodeGenBase with OptiQLCudaCodeGenPkg
  with CudaGenDeliteCollectionOps with CudaGenDeliteOps with CudaGenDeliteStruct with CudaGenDeliteArrayOps with CudaGenDSArrayOps with DeliteCudaGenAllOverrides with DeliteCppHostTransfer with DeliteCudaDeviceTransfer {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = {
    m match {
      case m if m.erasure.getSimpleName == "Date" => "int32_t"
      case _ => super.remap(m)
    }
  }
}

trait OptiQLCodeGenOpenCL extends OptiQLCodeGenBase with OptiQLOpenCLCodeGenPkg
  with OpenCLGenDeliteCollectionOps with OpenCLGenDeliteOps with OpenCLGenDeliteStruct with OpenCLGenDeliteArrayOps with OpenCLGenDSArrayOps with DeliteOpenCLGenAllOverrides {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = {
    m match {
      case m if m.erasure.getSimpleName == "Date" => "int32_t"
      case _ => super.remap(m)
    }
  }

}

trait OptiQLCodeGenC extends OptiQLCodeGenBase with OptiQLCCodeGenPkg with CGenDeliteFileReaderOps
  with CGenDeliteCollectionOps with CGenDeliteOps with CGenDeliteArrayOps with CGenDeliteStruct with DeliteCGenAllOverrides with DeliteCppHostTransfer {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = {
    m match {
      case m if m.erasure.getSimpleName == "Date" => "int32_t"
      case _ => super.remap(m)
    }
  }
}

/**
 * Traits for running applications
 */
// ex. trait TPCH extends OptiQLApplication
trait OptiQLApplication extends OptiQL with OptiQLLift {
  var args: Rep[Array[String]]
  def main(): Unit 
}

// ex. object TPCHRunner extends OptiQLApplicationRunner with  TPCH
trait OptiQLApplicationRunner extends OptiQLApplication with DeliteApplication with OptiQLExp
