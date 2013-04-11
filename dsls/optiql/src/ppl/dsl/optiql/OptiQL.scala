package ppl.dsl.optiql

import scala.reflect.RefinedManifest

import ops._
import scala.virtualization.lms.common._
import ppl.delite.framework.ops._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.opencl.TargetOpenCL
import ppl.delite.framework.{Config, DeliteApplication}
import java.io.{FileWriter, BufferedWriter, File}
import ppl.delite.framework.codegen.{Utils, Target}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.transform.MultiloopSoATransformExp
import ppl.delite.framework.{DeliteInteractive, DeliteInteractiveRunner, DeliteRestageOps, DeliteRestageOpsExp, DeliteRestageRunner}
import ppl.delite.framework.codegen.restage.{DeliteCodeGenRestage,TargetRestage}


/**
 * These are the lifted scala constructs that only operate on the Rep world. These are usually safe to mix in
 */
trait OptiQLScalaOpsPkg extends Base with MiscOps with BooleanOps with OrderingOps with PrimitiveOps with ObjectOps with StringOps with TupleOps with StructOps with NumericOps with ArrayOps with DeliteArrayOps with IfThenElse with Equal

/**
 * This trait adds the Ops that are specific to OptiQL
 */
trait OptiQL extends OptiQLScalaOpsPkg with TableOps with QueryableOps with DateOps with OptiQLMiscOps with InputReaderOps with Types {
  this: OptiQLApplication =>
}

trait OptiQLInteractive extends OptiQLApplication with DeliteInteractive
trait OptiQLInteractiveRunner extends OptiQLApplicationRunner with DeliteInteractiveRunner

trait OptiQLLower extends OptiQLApplication with DeliteRestageOps
trait OptiQLLowerRunner extends OptiQLApplicationRunner with DeliteRestageRunner

object OptiQL_ {
  def apply[R](b: => R) = new Scope[OptiQLLower, OptiQLLowerRunner, R](b)
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
  with PrimitiveOpsExp with ObjectOpsExp with StringOpsExp with TupleOpsExp with StructExp with NumericOpsExp with ArrayOpsExp with ArrayBufferOpsExp with IfThenElseExp with StructFatExpOptCommon with CastingOpsExp

/**
 * Ops available only to the compiler, and not our applications
 */
trait OptiQLCompiler extends OptiQL with IOOps with SeqOps with Variables with While with DeliteArrayOps with DeliteArrayBufferCompilerOps {
  this: OptiQLApplication with OptiQLExp =>
}

/**
 * This trait comprises the IR nodes for OptiQL and the code required to instantiate code generators
 */
trait OptiQLExp extends OptiQLCompiler with OptiQLScalaOpsPkgExp with TableOpsExp with DateOpsExp with DateImplOpsStandard with QueryableOpsExpOpt with OptiQLMiscOpsExp
  with InputReaderOpsExp with InputReaderImplOpsStandard with DeliteCollectionOpsExp with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp with DSArrayOpsExp 
  with DeliteRestageOpsExp with DeliteAllOverridesExp 
  with MultiloopSoATransformExp {

  this: DeliteApplication with OptiQLApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiQLExp.this.type}) : GenericFatCodegen{val IR: OptiQLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiQLCodeGenScala{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _:TargetCuda => new OptiQLCodeGenCuda{val IR: OptiQLExp.this.type = OptiQLExp.this}
      case _:TargetOpenCL => new OptiQLCodeGenOpenCL{val IR: OptiQLExp.this.type = OptiQLExp.this}
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
  with CudaGenPrimitiveOps with CudaGenObjectOps with CudaGenStringOps /*with CudaGenTupleOps*/ with CudaGenNumericOps with CudaGenArrayOps with CudaGenIfThenElseFat with CudaGenImplicitOps with CudaGenCastingOps {
  val IR: OptiQLScalaOpsPkgExp
}

trait OptiQLOpenCLCodeGenPkg extends OpenCLGenMiscOps with OpenCLGenIOOps with OpenCLGenSeqOps with OpenCLGenOrderingOps with OpenCLGenBooleanOps with OpenCLGenEqual with OpenCLGenVariables
  with OpenCLGenPrimitiveOps with OpenCLGenObjectOps with OpenCLGenStringOps /*with OpenCLGenTupleOps*/ with OpenCLGenNumericOps with OpenCLGenArrayOps with OpenCLGenIfThenElseFat with OpenCLGenImplicitOps with OpenCLGenCastingOps {
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

trait OptiQLCodeGenRestage extends OptiQLScalaCodeGenPkg with DeliteCodeGenRestage { 
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
  with ScalaGenDeliteCollectionOps with ScalaGenDeliteOps with ScalaGenDeliteStruct with ScalaGenDeliteArrayOps with ScalaGenDeliteArrayBufferOps with ScalaGenDSArrayOps with DeliteScalaGenAllOverrides {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = {    
    m match {
      case m if m.erasure.getSimpleName == "Date" => "Int"
      case m if m.toString == "scala.Tuple2[Char, Char]" => "Int"
      case m if m.toString.startsWith("scala.collection.immutable.Map") // HACK-ish, maybe use a DSL type instead
        && remap(m.typeArguments(0)) == "Int" => "generated.scala.container.HashMapImpl[" + remap(m.typeArguments(0)) + "]"

      case _ => dsmap(super.remap(m))
    }   
  }

  override def emitNode(sym: IR.Sym[Any], rhs: IR.Def[Any]) = rhs match {
    case IR.Struct(tag, elems) if sym.tp.toString == "scala.Tuple2[Char, Char]" =>
      emitValDef(sym, "("+ quote(elems(0)._2) + ".toInt << 16) + " + quote(elems(1)._2))
    case f@IR.FieldApply(s, index) if s.tp.toString == "scala.Tuple2[Char, Char]" && index == "_1" =>
      emitValDef(sym, "((" + quote(s) + " & 0xffff0000) >>> 16).toChar")
    case f@IR.FieldApply(s, index) if s.tp.toString == "scala.Tuple2[Char, Char]" && index == "_2" =>
      emitValDef(sym, "(" + quote(s) + " & 0xffff).toChar")
    case _ => super.emitNode(sym, rhs)
  }

  override def dsmap(line: String) : String = {
    var res = line.replaceAll("ppl.dsl.optiql.datastruct", "generated")
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res
  }
}

trait OptiQLCodeGenCuda extends OptiQLCodeGenBase with OptiQLCudaCodeGenPkg
  with CudaGenDeliteCollectionOps with CudaGenDeliteOps with CudaGenDeliteArrayOps /*with CudaGenDeliteArrayBufferOps*/ with CudaGenDSArrayOps with DeliteCudaGenAllOverrides with DeliteCppHostTransfer with DeliteCudaDeviceTransfer {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = {
    m match {
      case m if m.erasure.getSimpleName == "Date" => "int"
      case m if m.toString == "scala.Tuple2" => "int" //HACK
      case m if m.toString == "scala.Tuple2[Char, Char]" => "int"
      case m if m.toString == "scala.Tuple2[Boolean, Boolean]" => "int"
      case _ => super.remap(m)
    }
  }

	override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.toString
	}

}

trait OptiQLCodeGenOpenCL extends OptiQLCodeGenBase with OptiQLOpenCLCodeGenPkg
  with OpenCLGenDeliteCollectionOps with OpenCLGenDeliteOps with OpenCLGenDeliteArrayOps /*with OpenCLGenDeliteArrayBufferOps*/ with OpenCLGenDSArrayOps with DeliteOpenCLGenAllOverrides {
  val IR: DeliteApplication with OptiQLExp

  override def remap[A](m: Manifest[A]): String = {
    m match {
      case m if m.erasure.getSimpleName == "Date" => "int"
      case m if m.toString == "scala.Tuple2" => "int" //HACK
      case m if m.toString == "scala.Tuple2[Char, Char]" => "int"
      case m if m.toString == "scala.Tuple2[Boolean, Boolean]" => "int"
      case _ => super.remap(m)
    }
  }

	override def getDSLHeaders: String = {
    val out = new StringBuilder
    out.toString
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
