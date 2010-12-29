package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.VariablesExp
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.DeliteApplication

// you can pick and choose your overrides, these are provided for convenience
trait DeliteAllOverridesExp extends DeliteVariablesExp with DeliteIfThenElseExp with DeliteRangeOpsExp with DeliteWhileExp {
  this: DeliteOpsExp =>
}

trait DeliteScalaGenAllOverrides extends DeliteScalaGenVariables with DeliteScalaGenIfThenElse with DeliteScalaGenRange with DeliteScalaGenWhile  {
  val IR: DeliteApplication with DeliteAllOverridesExp
}


