package ppl.dsl.assignment2

import java.io.{BufferedWriter, FileWriter}
import scala.virtualization.lms.common._
import ppl.delite.framework.{Config, DeliteApplication}
import scala.tools.nsc.io._
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.delite.overrides._
import scala.virtualization.lms.internal.GenericFatCodegen

/**
 * Packages
 */

//the portions of Scala I want to include in my SimpleVector DSL:
//Reps version
trait SimpleVectorScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with NumericOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with ArrayOps
  with DeliteArrayOps

//Exps version
trait SimpleVectorScalaOpsPkgExp extends SimpleVectorScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with NumericOpsExp with OrderingOpsExp with StringOpsExp
  with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with MathOpsExp with CastingOpsExp with ObjectOpsExp with ArrayOpsExp with RangeOpsExp
  with DeliteArrayFatExp with DeliteArrayBufferOpsExp with DeliteOpsExp with StructExp
  
//Scala codegen version
trait SimpleVectorScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenOrderingOps with ScalaGenStringOps
  with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenObjectOps with ScalaGenArrayOps with ScalaGenRangeOps
  with ScalaGenDeliteArrayOps with ScalaGenDeliteArrayBufferOps with ScalaGenDeliteStruct
  { val IR: SimpleVectorScalaOpsPkgExp }

trait SimpleVectorCudaCodeGenPkg extends CudaGenDSLOps
  with CudaGenEqual with CudaGenIfThenElse with CudaGenVariables with CudaGenWhile with CudaGenFunctions
  with CudaGenImplicitOps with CudaGenNumericOps with CudaGenOrderingOps with CudaGenStringOps
  with CudaGenBooleanOps with CudaGenPrimitiveOps with CudaGenMiscOps
  with CudaGenMathOps with CudaGenCastingOps with CudaGenArrayOps with CudaGenRangeOps
  { val IR: SimpleVectorScalaOpsPkgExp }

trait SimpleVectorCCodeGenPkg extends CGenDSLOps
  with CGenEqual with CGenIfThenElse with CGenVariables with CGenWhile with CGenFunctions
  with CGenImplicitOps with CGenNumericOps with CGenOrderingOps with CGenStringOps
  with CGenBooleanOps with CGenPrimitiveOps with CGenMiscOps
  with CGenArrayOps with CGenRangeOps
  { val IR: SimpleVectorScalaOpsPkgExp }

/**
 * add SimpleVector functionality
 */

trait SimpleVector extends SimpleVectorScalaOpsPkg with VectorOps { this: SimpleVectorApplication => }

//additional functionality I want available in the compiler, but not in the applications
trait SimpleVectorCompiler extends SimpleVector with RangeOps {
  this: SimpleVectorApplication with SimpleVectorExp =>
}

trait SimpleVectorExp extends SimpleVectorCompiler with SimpleVectorScalaOpsPkgExp with VectorOpsExp with VectorImplOpsStandard with DeliteOpsExp with DeliteAllOverridesExp {
  this: DeliteApplication with SimpleVectorApplication =>

  def getCodeGenPkg(t: Target{val IR: SimpleVectorExp.this.type}) : GenericFatCodegen{val IR: SimpleVectorExp.this.type} = {
    t match {
      case _:TargetScala => new SimpleVectorCodegenScala{val IR: SimpleVectorExp.this.type = SimpleVectorExp.this}
      case _:TargetCuda => new SimpleVectorCodegenCuda{val IR: SimpleVectorExp.this.type = SimpleVectorExp.this}
      case _ => throw new RuntimeException("simple vector does not support this target")
    }
  }
}

trait SimpleVectorLift extends LiftVariables with LiftEquals with LiftString with LiftNumeric with LiftBoolean {
  this: SimpleVector =>
}

//the trait all SimpleVector applications must extend
trait SimpleVectorApplication extends SimpleVector with SimpleVectorLift {
  var args: Rep[Array[String]]
  def main()
}

//the runner for SimpleVector applications
trait SimpleVectorApplicationRunner extends SimpleVectorApplication with DeliteApplication with SimpleVectorExp


trait SimpleVectorCodegenBase extends GenericFatCodegen {
  val IR: DeliteApplication with SimpleVectorExp
  override def initialDefs = IR.deliteGenerator.availableDefs
  
}

trait SimpleVectorCodegenScala extends SimpleVectorCodegenBase with SimpleVectorScalaCodeGenPkg
  with ScalaGenDeliteOps with ScalaGenDeliteCollectionOps with DeliteScalaGenAllOverrides {

  val IR: DeliteApplication with SimpleVectorExp
}

trait SimpleVectorCodegenCuda extends SimpleVectorCodegenBase with SimpleVectorCudaCodeGenPkg
  with CudaGenDeliteOps with DeliteCudaGenAllOverrides {

  val IR: DeliteApplication with SimpleVectorExp
}

trait SimpleVectorCodegenC extends SimpleVectorCodegenBase with SimpleVectorCCodeGenPkg
  with CGenDeliteOps with DeliteCGenAllOverrides {

  val IR: DeliteApplication with SimpleVectorExp
}
