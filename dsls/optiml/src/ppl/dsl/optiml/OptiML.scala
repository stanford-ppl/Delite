package ppl.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import scala.virtualization.lms.common.{ScalaOpsPkgExp, ScalaOpsPkg, ScalaCodeGenPkg}
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericCodegen}
import ppl.delite.framework.codegen.delite.DeliteCodeGenOverridesScala

trait OptiML extends ScalaOpsPkg with VectorOps with MatrixOps with MLInputReaderOps {
  this: DeliteApplication =>
}

trait OptiMLExp extends OptiML with ScalaOpsPkgExp with VectorOpsExp with VectorViewOpsExp with MatrixOpsExp with MLInputReaderOpsExp
  with VectorImplOpsStandard with VectorViewImplOpsStandard with MatrixImplOpsStandard with MLInputReaderImplOpsStandard {
  this: DeliteApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiMLExp.this.type}) : GenericNestedCodegen{val IR: OptiMLExp.this.type} = {
    t match {
      case _:TargetScala => new OptiMLCodeGenScala{val IR: OptiMLExp.this.type = OptiMLExp.this}
      case _ => throw new RuntimeException("optiml does not support this target")
    }
  }
}

trait OptiMLCodeGenScala extends ScalaCodeGenPkg with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps
  with DeliteCodeGenOverridesScala { //with ScalaGenMLInputReaderOps {

  val IR: DeliteApplication with OptiMLExp

  override def remap[A](m: Manifest[A]) : String = m.toString match {
    // TODO: make more robust
    case "ppl.dsl.optiml.Vector[Double]" => "ppl.dsl.optiml.VectorImpl[Double]"
    case "ppl.dsl.optiml.Vector[Boolean]" => "ppl.dsl.optiml.VectorImpl[Boolean]"
    case "ppl.dsl.optiml.Matrix[Double]" => "ppl.dsl.optiml.MatrixImpl[Double]"
    case _ => super.remap(m)
  }

}
