package ppl.delite.framework

import java.io._
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp
import ppl.delite.framework.codegen.opencl.TargetOpenCL
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.codegen.restage.DeliteILScalaGenExternal
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

trait DeliteILApplicationRunner extends DeliteILApplication with DeliteApplication with DeliteILExp

trait DeliteILApplication extends DeliteIL with DeliteILLift {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait DeliteILLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric with LiftPrimitives {
  this: DeliteIL =>
}

trait DeliteILScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with TupledFunctions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with CastingOps with ObjectOps with IOOps with HashMapOps
  with ArrayOps with ExceptionOps with MathOps with NumericOps with FractionalOps

trait DeliteILScalaOpsPkgExp extends DeliteILScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with TupledFunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp  with NumericOpsExp with FractionalOpsExp
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp with ArrayBufferOpsExp with ExceptionOpsExp 

trait DeliteILScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenTupleOps
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps  with ScalaGenNumericOps with ScalaGenFractionalOps
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps with ScalaGenArrayBufferOps with ScalaGenExceptionOps
  { val IR: DeliteILScalaOpsPkgExp  }

trait DeliteIL extends DeliteILScalaOpsPkg with DeliteILOps with DeliteCollectionOps with DeliteArrayOps with StructOps {

  this: DeliteILApplication =>
}


trait DeliteILExp extends DeliteIL with DeliteILScalaOpsPkgExp with DeliteILOpsExp with DeliteOpsExp with DeliteArrayFatExp with DeliteArrayBufferOpsExp with DeliteMapOpsExp with StructExp
  with FunctionBlocksExp with ExpressionsOpt 
  with DeliteTransform with MultiloopSoATransformExp with DeliteAllOverridesExp {

  this: DeliteApplication with DeliteILApplication  => 

  def getCodeGenPkg(t: Target{val IR: DeliteILExp.this.type}) : GenericFatCodegen{val IR: DeliteILExp.this.type} = {
    t match {
      case _:TargetScala => new DeliteILCodeGenScala{val IR: DeliteILExp.this.type = DeliteILExp.this}
      case _ => throw new RuntimeException("DeliteIL does not support this target")
    }
  }    
}

trait DeliteILCodeGenBase extends GenericFatCodegen with SchedulingOpt {

  val IR: DeliteApplication with DeliteILExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  def dsmap(line: String) = line

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteCollection" => IR.structName(m)
    case s if s.contains("$") => s // due to synthetically generated domain-specific types in the restaged program
    case _ => super.remap(m)
  }
    
  val specialize = Set[String]()
  val specialize2 = Set[String]()
  def genSpec(f: File, outPath: String) = {}

  def getFiles(d: File): Array[File] = {
    d.listFiles flatMap { f => if (f.isDirectory()) getFiles(f) else Array(f) }
  }  
}

trait DeliteILCodeGenScala extends DeliteILCodeGenBase with DeliteILScalaCodeGenPkg with DeliteILScalaGenExternal with ScalaGenDeliteILOps
  with ScalaGenDeliteOps with ScalaGenDeliteCollectionOps with ScalaGenDeliteStruct with ScalaGenDeliteArrayOps with ScalaGenDeliteArrayBufferOps with ScalaGenDeliteMapOps with ScalaGenTupledFunctions  
  with DeliteScalaGenAllOverrides { 
  
  val IR: DeliteApplication with DeliteILExp
}
