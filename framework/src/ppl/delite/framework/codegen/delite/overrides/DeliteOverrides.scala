package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.VariablesExp
import ppl.delite.framework.ops.DeliteOpsExp

trait DeliteOverridesExp extends VariablesExp with DeliteIfThenElseExp {
  this: DeliteOpsExp =>
}