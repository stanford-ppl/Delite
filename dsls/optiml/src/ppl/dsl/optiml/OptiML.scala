package ppl.dsl.optiml

import scala.virtualization.lms.internal.GenericCodegen

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import scala.virtualization.lms.common.embedded.scala.{ScalaOpsPkgExp, ScalaOpsPkg, ScalaCodeGenPkg}

trait OptiML extends ScalaOpsPkg with VectorOps with MatrixOps with MLInputReaderOps {
  this: DeliteApplication =>
}

trait OptiMLExp extends OptiML with ScalaOpsPkgExp with VectorOpsExp with VectorViewOpsExp with MatrixOpsExp with MLInputReaderOpsExp
  with VectorImplOpsStandard with VectorViewImplOpsStandard with MatrixImplOpsStandard with MLInputReaderImplOpsStandard {
  this: DeliteApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _ => throw new RuntimeException("optiml does not support this target")
    }
  }
}

trait OptiMLCodeGenScala extends ScalaCodeGenPkg with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps //with ScalaGenMLInputReaderOps {
  { val IR: DeliteApplication with OptiMLExp }
