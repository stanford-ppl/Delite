package ppl.dsl.simple

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericCodegen}
import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.codegen.delite.overrides.{DeliteScalaGenAllOverrides, DeliteAllOverridesExp}

/**
 * Imports from the Scala language
 */
trait OptiML2ScalaOpsPkg extends Base with OrderingOps with MiscOps with IfThenElse with RangeOps with PrimitiveOps with ImplicitOps with While

trait OptiML2ScalaOpsPkgExp extends OptiML2ScalaOpsPkg with OrderingOpsExp with MiscOpsExp with IfThenElseExp with RangeOpsExp with PrimitiveOpsExp with ImplicitOpsExp with WhileExp

trait OptiML2ScalaCodeGenPkg extends ScalaGenOrderingOps
  with ScalaGenMiscOps with ScalaGenIfThenElse with ScalaGenRangeOps
  with ScalaGenPrimitiveOps with ScalaGenWhile {
  val IR: OptiML2ScalaOpsPkgExp
}

trait OptiML2 extends OptiML2ScalaOpsPkg with VectorOps2 with MatrixOps2 {
  this: DeliteApplication =>
}

trait OptiML2Exp extends OptiML2ScalaOpsPkgExp with VectorOpsExp2 with MatrixOpsExp2
  with DeliteOpsExp with DeliteAllOverridesExp  { this: DeliteApplication =>

  def getCodeGenPkg(t: Target{val IR: OptiML2Exp.this.type}) : GenericNestedCodegen{val IR: OptiML2Exp.this.type} = {
    t match {
      case _:TargetScala => new OptiML2CodeGenScala{val IR: OptiML2Exp.this.type = OptiML2Exp.this}
      //case _:TargetC => new OptiML2CodeGenC{val IR: OptiML2.this.type = OptiML2.this}
      case _ => throw new RuntimeException("optiml_simple does not support this target")
    }
  }
}

trait OptiML2CodeGenBase extends GenericCodegen {
  // this is where you put your Data structure emission code
}



trait OptiML2CodeGenScala extends OptiML2CodeGenBase with OptiML2ScalaCodeGenPkg with ScalaGenVectorOps2
with ScalaGenMatrixOps2 with DeliteScalaGenAllOverrides
  { val IR: DeliteApplication with OptiML2Exp }