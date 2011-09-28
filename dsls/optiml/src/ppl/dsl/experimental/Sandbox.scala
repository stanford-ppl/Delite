package ppl.dsl.experimental

import java.io._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}
import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.overrides.{DeliteCudaGenAllOverrides, DeliteCGenAllOverrides, DeliteScalaGenAllOverrides, DeliteAllOverridesExp}
import ppl.delite.framework.ops._


// TODO: temp: move to base
trait InterfaceOps  
trait Interface[T] {
  val ops: InterfaceOps
}

/**
 * Sandbox types 
 */

trait Vector[T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T]
trait DenseVector[T] extends Vector[T]

/**
 * These separate Sandbox applications from the Exp world.
 */

// ex. object GDARunner extends SandboxApplicationRunner with GDA
trait SandboxApplicationRunner extends SandboxApplication with DeliteApplication with SandboxExp

// ex. trait GDA extends SandboxApplication
trait SandboxApplication extends Sandbox with SandboxLift  {
  var args: Rep[Array[String]]
  def main(): Unit
}


/**
 * These are the portions of Scala imported into Sandbox's scope.
 */
trait SandboxLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftNumeric {
  this: Sandbox =>
}

trait SandboxScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with IOOps
  // only included because of args. TODO: investigate passing args as a vector
  with ArrayOps

trait SandboxScalaOpsPkgExp extends SandboxScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with OrderingOpsExp with StringOpsExp with RangeOpsExp with IOOpsExp
  with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with ListOpsExp with SeqOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp
  with SynchronizedArrayBufferOpsExp with HashMapOpsExp with IterableOpsExp

trait SandboxScalaCodeGenPkg extends ScalaGenDSLOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenOrderingOps with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps
  with ScalaGenArrayOps with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenListOps with ScalaGenSeqOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps with ScalaGenObjectOps
  with ScalaGenSynchronizedArrayBufferOps with ScalaGenHashMapOps with ScalaGenIterableOps
  { val IR: SandboxScalaOpsPkgExp  }


/**
 * This the trait that every Sandbox application must extend.
 */
trait Sandbox extends SandboxScalaOpsPkg
  with ArithOps with VectorOps with DenseVectorOps  {

  this: SandboxApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait SandboxCompiler extends Sandbox with DeliteCollectionOps with RangeOps with IOOps with SeqOps with SetOps
  with ListOps with HashMapOps with IterableOps {
    
  this: SandboxApplication with SandboxExp =>
}


/**
 * These are the corresponding IR nodes for Sandbox.
 */
trait SandboxExp extends SandboxCompiler with SandboxScalaOpsPkgExp with DeliteOpsExp 
  with VectorOpsExp with DenseVectorOpsExp 
  with DeliteAllOverridesExp {

  // this: SandboxApplicationRunner => why doesn't this work?
  this: DeliteApplication with SandboxApplication with SandboxExp => // can't be SandboxApplication right now because code generators depend on stuff inside DeliteApplication (via IR)

  def getCodeGenPkg(t: Target{val IR: SandboxExp.this.type}) : GenericFatCodegen{val IR: SandboxExp.this.type} = {
    t match {
      case _:TargetScala => new SandboxCodeGenScala{val IR: SandboxExp.this.type = SandboxExp.this}
      case _ => throw new RuntimeException("sandbox does not support this target")
    }
  }

}


/**
 * Sandbox code generators
 */
trait SandboxCodeGenBase extends GenericFatCodegen {

  val IR: DeliteApplication with SandboxExp
  override def initialDefs = IR.deliteGenerator.availableDefs


  def dsmap(line: String) = line
}

trait SandboxCodeGenScala extends SandboxCodeGenBase with SandboxScalaCodeGenPkg with ScalaGenDeliteOps
  with ScalaGenVectorOps with ScalaGenDenseVectorOps
  with DeliteScalaGenAllOverrides { //with ScalaGenMLInputReaderOps {
  
  val IR: DeliteApplication with SandboxExp

}