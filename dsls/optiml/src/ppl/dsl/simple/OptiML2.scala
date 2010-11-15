package ppl.dsl.simple

import scala.virtualization.lms.common.{CCodeGenPkg, ScalaCodeGenPkg, ScalaOpsPkgExp}
import scala.virtualization.lms.internal.GenericCodegen

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.c.TargetC
import ppl.delite.framework.codegen.delite.{DeliteCodeGenPkg, TargetDelite}

trait OptiML2 extends ScalaOpsPkgExp with VectorOpsExp2 with MatrixOpsExp2 {
  this: DeliteApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiML2.this.type}) : GenericCodegen{val IR: OptiML2.this.type} = {
    t match {
      case _:TargetScala => new OptiML2CodeGenScala{val IR: OptiML2.this.type = OptiML2.this}
      case _:TargetC => new OptiML2CodeGenC{val IR: OptiML2.this.type = OptiML2.this}
      case _ => throw new RuntimeException("optiml_simple does not support this target")
    }
  }
}

trait OptiML2CodeGenScala extends ScalaCodeGenPkg with ScalaGenVectorOps2 with ScalaGenMatrixOps2
  { val IR: DeliteApplication with OptiML2 }

trait OptiML2CodeGenC extends CCodeGenPkg with CGenVectorOps2 with CGenMatrixOps2
  { val IR: DeliteApplication with OptiML2 }