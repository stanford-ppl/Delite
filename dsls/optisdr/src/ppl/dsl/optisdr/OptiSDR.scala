package ppl.dsl.optisdr

import scala.virtualization.lms.common._
import ppl.delite.framework.{Config, DeliteApplication}

import ppl.dsl.optila.{OptiLAApplication}
import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift, OptiLAUtilities}
import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLAOpenCLCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenOpenCL, OptiLACodeGenC}

import ppl.dsl.optisdr.capabilities._
import ppl.dsl.optisdr.primitive._
import ppl.dsl.optisdr.vector._

trait OptiSDRApplicationRunner extends OptiSDRApplication with DeliteApplication with OptiSDRExp

trait OptiSDRApplication extends OptiLAApplication with OptiSDR with OptiSDRLift {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait OptiSDRScalaOpsPkg extends OptiLAScalaOpsPkg
  with Equal with IfThenElse with Variables with While with Functions
  with BooleanOps with PrimitiveOps with TupleOps
  
trait OptiSDRScalaOpsPkgExp extends OptiLAScalaOpsPkgExp with OptiSDRScalaOpsPkg

trait OptiSDRScalaCodeGenPkg extends OptiLAScalaCodeGenPkg 
  { val IR: OptiSDRScalaOpsPkgExp  }

trait OptiSDRCudaCodeGenPkg extends OptiLACudaCodeGenPkg
  { val IR: OptiSDRScalaOpsPkgExp  }

trait OptiSDROpenCLCodeGenPkg extends OptiLAOpenCLCodeGenPkg
  { val IR: OptiSDRScalaOpsPkgExp  }

trait OptiSDRCCodeGenPkg extends OptiLACCodeGenPkg
  { val IR: OptiSDRScalaOpsPkgExp  }

/**
 * These are the portions of Scala imported into OptiML's scope.
 */
trait OptiSDRLift extends OptiLALift {
  this: OptiSDR =>
}

/**
 * This the trait that every OptiSDR application must extend.
 */
trait OptiSDR extends OptiSDRScalaOpsPkg with OptiLA
  with ComplexOps with ComplexIntOps with UIntOps with SoftBitOps
  with SDRArithOps
  with SDRVectorOps {
  this: OptiSDRApplication =>

  type Real = Double
  // Int is already a defined type
}

trait OptiSDRExp extends OptiLAExp with OptiSDRCompiler with OptiSDRScalaOpsPkgExp
  with ComplexOpsExpOpt with ComplexIntOpsExpOpt with UIntOpsExpOpt with SoftBitOpsExp
  with SDRArithOpsExp
  with SDRVectorOpsExpOpt {
  this: DeliteApplication with OptiSDRApplication with OptiSDRExp => // can't be OptiSDRApplication right now because code generators depend on stuff inside DeliteApplication (via IR)
}

// these ops are only available to the compiler (they are restricted from application use)
trait OptiSDRCompiler extends OptiLACompiler with OptiSDR {
   
  this: OptiSDRApplication with OptiSDRExp =>
}