package ppl.dsl.optiml

import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.embedded.scala.ScalaCodeGenPkg
import ppl.delite.framework.codegen.Target
import scala.virtualization.lms.internal.GenericCodegen
import ppl.delite.framework.codegen.scala.TargetScala

trait OptiML extends VectorOps with MatrixOps with MLInputReaderOps {
  self: DeliteApplication =>

  // TODO: how does the DSL separate its Scala, C, etc. code gens?
  //type DSLCodeGenPkg = OptiMLCodeGen

//  def getCodeGenPkg(t: Target{val IR: self.type}) : GenericCodegen{val IR: self.type} = {
////    if (t.isInstanceOf[TargetScala]) new OptiMLCodeGen{val IR: self.type = self}
////    else throw new RuntimeException("optiml does not support this target")
//    t match {
//      case _:TargetScala => new OptiMLCodeGen{val IR: self.type = self}
//      case _ => throw new RuntimeException("optiml does not support this target")
//    }
//  }
}

trait OptiMLExp extends OptiML with VectorOpsExp with MatrixOpsExp with MLInputReaderOpsExp
  with VectorImplOpsStandard with MatrixImplOpsStandard with MLInputReaderImplOpsStandard {

  this: DeliteApplication =>
}

//trait OptiMLCodeGen extends ScalaCodeGenPkg with ScalaGenVectorOps with ScalaGenVectorViewOps with ScalaGenMatrixOps //with ScalaGenMLInputReaderOps
