
package ppl.dsl.opticvx

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.codegen
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
//import ppl.delite.framework.codegen.cuda.TargetCuda
//import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._

import ppl.dsl.optila.{OptiLAApplication}
import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift}
import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenC}

/**
 * DSL specific
 */
// import ppl.dsl.optiml.io._
// import ppl.dsl.optiml.vector._
// import ppl.dsl.optiml.matrix._
// import ppl.dsl.optiml.graph._
// import ppl.dsl.optiml.stream._
// import ppl.dsl.optiml.library.cluster._
// import ppl.dsl.optiml.application._

/**
 * These separate CVX applications from the Exp world.
 */

// ex. object GDARunner extends OptiMLApplicationRunner with GDA
trait OptiCVXApplicationRunner extends OptiCVXApplication with DeliteApplication with OptiCVXExp

// ex. trait GDA extends OptiMLApplication
//trait OptiMLApplication extends OptiLAApplication with OptiML with OptiMLLift with OptiMLLibrary {
trait OptiCVXApplication extends OptiLAApplication with OptiCVX with OptiCVXLift {
  var args: Rep[Array[String]]
  def main(): Unit
}


/**
 * These are the portions of Scala imported into OptiML's scope.
 */
//trait OptiMLLift extends OptiLALift {
trait OptiCVXLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric {
  this: OptiCVX =>
}

trait OptiCVXScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with IOOps
  // only included because of args. TODO: investigate passing args as a vector
  with ArrayOps
  
trait OptiCVXScalaOpsPkgExp extends OptiCVXScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp

  
trait OptiCVXScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps
  { val IR: OptiCVXScalaOpsPkgExp  }
/**
 * This is the trait that every OptiML application must extend.
 */
// trait OptiML extends OptiLA with OptiMLScalaOpsPkg with LanguageOps with ApplicationOps with LBPOps // TODO: LBPOpsshould be auto-generated with ApplicationOps
//   with MLInputReaderOps with MLOutputWriterOps
//   with VectorOps with OptiMLDenseVectorOps with OptiMLVectorViewOps with OptiMLRangeVectorOps
//   with MatrixOps with IndexVectorOps with IndexVectorDenseOps with IndexVectorRangeOps with IndexVector2Ops 
//   with StreamOps with StreamRowOps
//   with GraphOps with VerticesOps with EdgeOps with VertexOps with MessageEdgeOps with MessageVertexOps with VSetOps
//   with LabelsOps with TrainingSetOps with ImageOps with GrayscaleImageOps {
// 
//   this: OptiMLApplication =>
// }
trait OptiCVX extends OptiLA
  with OptiCVXScalaOpsPkg
  with OptVarOps
  with ExprShapeOps
  with ExprOps
  with FunctionOps
  with ConstraintOps
  with VectorOps
  with ObjectiveOps
  with ConstExprOps
  with SolverOps
  with MatlabCVXOps
  with AbstractMatrixOps
  with ConeOps
  //with OptVarOps with ExprOps with ConstraintOps
  //with ObjectiveOps with FunctionOps
  //awith ConstExprOps
  with DeliteCollectionOps
{
  this: OptiCVXApplication =>
}


// these ops are only available to the compiler (they are restricted from application use)
//trait OptiMLCompiler extends OptiLACompiler with OptiML {
trait OptiCVXCompiler extends OptiCVX {
  this: OptiCVXApplication with OptiCVXExp =>
}


/**
 * These are the corresponding IR nodes for OptiML.
 */
trait OptiCVXExp extends OptiLAExp
  with OptiCVXCompiler with OptiCVXScalaOpsPkgExp 
  with ScalaOpsPkgExp
  with DeliteOpsExp
  with OptVarOpsExp
  with ExprShapeOpsExp
  with ExprOpsExp
  with FunctionOpsExp
  with ConstraintOpsExp
  with VectorOpsExp
  with ObjectiveOpsExp
  with ConstExprOpsExp
  with SolverOpsExp
  with MatlabCVXOpsExp
  with AbstractMatrixOpsExp
  with ConeOpsExp
  //with OptVarOpsExp with ExprOpsExp with ConstraintOpsExp
  //with ObjectiveOpsExp with FunctionOpsExp
  //with ConstExprOpsExp
  with DeliteAllOverridesExp {

  // this: OptiMLApplicationRunner => why doesn't this work?
  this: DeliteApplication with OptiCVXApplication with OptiCVXExp => // can't be OptiMLApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  override def getCodeGenPkg(t: Target{val IR: OptiCVXExp.this.type}) : GenericFatCodegen{val IR: OptiCVXExp.this.type} = {
    t match {
      case _:TargetScala => new OptiCVXCodeGenScala{val IR: OptiCVXExp.this.type = OptiCVXExp.this}
      case _ => throw new RuntimeException("OptiCVX does not support this target")
    }
  }

}


/**
 * CVX code generators
 */
trait OptiCVXCodeGenBase extends OptiLACodeGenBase { //with GenericFatCodegen with codegen.Utils {

  val IR: DeliteApplication with OptiCVXExp
  override def initialDefs = IR.deliteGenerator.availableDefs

  override def remap[A](m: Manifest[A]): String = dsmap(super.remap(m))

  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
  }
}

// insert code generators here
trait OptiCVXCodeGenScala extends OptiLACodeGenScala
  with OptiCVXCodeGenBase
  with ScalaCodeGenPkg
  with ScalaGenDeliteOps
  with ScalaGenOptVarOps
  with ScalaGenExprShapeOps
  with ScalaGenExprOps
  with ScalaGenFunctionOps
  with ScalaGenConstraintOps
  with ScalaGenVectorOps
  with ScalaGenObjectiveOps
  with ScalaGenConstExprOps
  with ScalaGenSolverOps
  with ScalaGenMatlabCVXOps
  with ScalaGenAbstractMatrixOps
  with ScalaGenConeOps
  //with ScalaGenOptVarOps with ScalaGenExprOps with ScalaGenConstraintOps
  //with ScalaGenObjectiveOps with ScalaGenFunctionOps
  //with ScalaGenConstExprOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with OptiCVXExp

  override def dsmap(line: String) : String = {
    var res = super.dsmap(line)
    res.replaceAll("ppl.dsl.opticvx.datastruct", "generated")
    res = res.replaceAll("import ppl.dsl.optila.datastruct.scala._", "")     
    res = res.replaceAll("ppl.delite.framework.datastruct", "generated")
    res = res.replaceAll("ppl.dsl.opticvx", "generated.scala")      
    res
  }

}
