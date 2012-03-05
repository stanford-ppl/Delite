package ppl.dsl.optisdr

import scala.virtualization.lms.common._
import ppl.delite.framework.{Config, DeliteApplication}

import ppl.dsl.optisdr.primitive._

trait OptiSDRApplicationRunner extends OptiSDRApplication with DeliteApplication with OptiSDRExp

trait OptiSDRApplication extends OptiSDR with OptiSDRLift {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait OptiSDRScalaOpsPkg extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with BooleanOps with PrimitiveOps with TupleOps
  
trait OptiSDRScalaOpsPkgExp extends OptiSDRScalaOpsPkg with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with BooleanOpsExp with PrimitiveOpsExp

trait OptiSDRLift extends LiftVariables with LiftEquals with LiftString with LiftBoolean with LiftPrimitives {
  this: OptiSDR =>
}

/**
 * This the trait that every OptiSDR application must extend.
 */
trait OptiSDR extends Variables with OptiSDRScalaOpsPkg
  with ComplexOps {

  this: OptiSDRApplication =>
}

// these ops are only available to the compiler (they are restricted from application use)
trait OptiSDRCompiler extends OptiSDR {
   
  this: OptiSDRApplication with OptiSDRExp =>
}

trait OptiSDRExp extends OptiSDRCompiler with OptiSDRScalaOpsPkgExp
  with ComplexOpsExp {
  this: DeliteApplication with OptiSDRApplication with OptiSDRExp => // can't be OptiSDRApplication right now because code generators depend on stuff inside DeliteApplication (via IR)
}