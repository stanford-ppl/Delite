package feattest

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp

import ppl.delite.framework.{Config, ExpressionsOpt, SchedulingOpt, DeliteApplication}
import ppl.delite.framework.{Interfaces,InterfacesExp}
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

// --- Basic LMS ops (mimics Scala syntax)
trait LMSOps extends Base
  with IfThenElse           with While                with OrderingOps          with MiscOps
  with TupledFunctions      with TupleOps             with CastingOps           with ExceptionOps
  with Equal                with Variables            with StringOps            with ImplicitOps
  with BooleanOps           with PrimitiveOps         with MathOps              with ObjectOps
  with RangeOps

trait LMSLift extends Base
  with LiftEquals           with LiftVariables        with LiftString
  with LiftBoolean          with LiftPrimitives       with LiftNumeric {
  this: LMSOps =>
}

// TODO: Are DSLOps really needed here?
trait LMSOpsExp extends LMSOps
  with EqualExp             with IfThenElseExp        with VariablesExp         with WhileExp
  with MathOpsExp           with ImplicitOpsExp       with OrderingOpsExp       with StringOpsExp
  with BooleanOpsExp        with PrimitiveOpsExpOpt   with MiscOpsExp           with TupledFunctionsExp
  with TupleOpsExp          with CastingOpsExp        with ExceptionOpsExp      with ObjectOpsExpOpt
  with RangeOpsExp          with DSLOpsExp

// --- Basic LMS Code Generators
trait ScalaGenLMS extends ScalaGenBase
  with ScalaGenEqual        with ScalaGenIfThenElse   with ScalaGenVariables    with ScalaGenWhile
  with ScalaGenMathOps      with ScalaGenImplicitOps  with ScalaGenOrderingOps  with ScalaGenStringOps
  with ScalaGenBooleanOps   with ScalaGenPrimitiveOps with ScalaGenMiscOps      with ScalaGenTupledFunctions
  with ScalaGenTupleOps     with ScalaGenCastingOps   with ScalaGenExceptionOps with ScalaGenObjectOps
  with ScalaGenRangeOps     with ScalaGenDSLOps
  { val IR: LMSOpsExp }

trait CudaGenLMS extends CudaGenBase
  with CudaGenEqual         with CudaGenIfThenElse    with CudaGenVariables     with CudaGenWhile
  with CudaGenMathOps       with CudaGenImplicitOps   with CudaGenOrderingOps   with CudaGenStringOps
  with CudaGenBooleanOps    with CudaGenPrimitiveOps  with CudaGenMiscOps       /*with CudaGenFunctions*/
  with CudaGenTupleOps      with CudaGenCastingOps    with CudaGenExceptionOps  with CudaGenObjectOps
  with CudaGenRangeOps      with CudaGenDSLOps
  { val IR: LMSOpsExp }

trait CGenLMS extends CGenBase
  with CGenEqual            with CGenIfThenElse       with CGenVariables        with CGenWhile
  with CGenMathOps          with CGenImplicitOps      with CGenOrderingOps      with CGenStringOps
  with CGenBooleanOps       with CGenPrimitiveOps     with CGenMiscOps          with CGenTupledFunctions
  with CGenTupleOps         with CGenCastingOps       with CGenExceptionOps     with CGenObjectOps
  with CGenRangeOps         with CGenDSLOps
  { val IR: LMSOpsExp }

// --- Delite Transformers
trait DeliteDSLTransformExp extends DeliteTransform
  //with MultiloopSoATransformWithReduceExp -- SOA transform for reduce needs to be fixed
  with MultiloopSoATransformExp

// --- Delite DSL Ops
trait DeliteDSLOps extends LMSOps
  with StructOps            with DeliteArrayOps      with DeliteMapOps with DeliteArrayBufferOps
  with DeliteAnalysesOps    with DeliteFileReaderOps {
  this: DeliteDSLApplication =>
}

trait DeliteDSLCompilerOps extends DeliteDSLOps
  with DeliteArrayCompilerOps with DeliteArrayBufferCompilerOps {
  this: DeliteDSLApplication with DeliteDSLOpsExp =>
}

trait DeliteDSLOpsExp extends DeliteDSLCompilerOps with LMSOpsExp with DeliteDSLTransformExp
  with DeliteStructsExp       with DeliteArrayFatExp       with DeliteMapOpsExp   with DeliteArrayBufferOpsExp
  with DeliteLMSForwarderExp  with DeliteFileReaderOpsExp
  with FunctionBlocksExp      with DeliteOpsExp            with ExpressionsOpt    with DeliteAllOverridesExp {

  this: DeliteApplication with DeliteDSLApplication =>

  def getCodeGenPkg(t: Target{val IR: DeliteDSLOpsExp.this.type}): GenericFatCodegen{val IR: DeliteDSLOpsExp.this.type} = t match {
    case _:TargetScala => new ScalaGenDeliteDSL{val IR: DeliteDSLOpsExp.this.type = DeliteDSLOpsExp.this}
    case _:TargetCuda => new CudaGenDeliteDSL{val IR: DeliteDSLOpsExp.this.type = DeliteDSLOpsExp.this}
    case _:TargetCpp => new CGenDeliteDSL{val IR: DeliteDSLOpsExp.this.type = DeliteDSLOpsExp.this}
    case _ => throw new Exception("DeliteDSL does not support this target")
  }
}

// --- Delite DSL Code Generators
trait DeliteDSLCodeGenBase extends GenericFatCodegen with SchedulingOpt {
  val IR: DeliteApplication with DeliteDSLOpsExp
  // Very strange things happen during codegen if this line isn't here!
  override def initialDefs = IR.deliteGenerator.availableDefs
}

trait ScalaGenDeliteDSL extends DeliteDSLCodeGenBase with ScalaGenLMS
  with ScalaGenDeliteStruct   with ScalaGenDeliteArrayOps with ScalaGenDeliteMapOps
  with ScalaGenDeliteFileReaderOps
  with ScalaGenDeliteOps      with DeliteScalaGenAllOverrides
  { val IR: DeliteApplication with DeliteDSLOpsExp }

trait CudaGenDeliteDSL extends DeliteDSLCodeGenBase with CudaGenLMS
  with CudaGenDeliteStruct with CudaGenDeliteArrayOps     /*with CudaGenDeliteMapOps*/
  with CudaGenDeliteOps    with DeliteCudaGenAllOverrides with DeliteCppHostTransfer with DeliteCudaDeviceTransfer
  { val IR: DeliteApplication with DeliteDSLOpsExp }

trait CGenDeliteDSL extends DeliteDSLCodeGenBase with CGenLMS
  with CGenDeliteStruct    with CGenDeliteArrayOps with CGenDeliteMapOps
  with CGenDeliteFileReaderOps
  with CGenDeliteOps       with DeliteCGenAllOverrides
  with DeliteCppHostTransfer
  { val IR: DeliteApplication with DeliteDSLOpsExp }

// --- Stubs for DSLs to extend
trait DeliteDSLCompiler extends DeliteDSLOpsExp with DeliteApplication with DeliteDSLApplication
trait DeliteDSLApplication extends DeliteDSLOps with LMSLift {
  var args: Rep[Array[String]]
  def main(): Unit
}
