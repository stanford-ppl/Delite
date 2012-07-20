package ppl.delite.framework.codegen.delite.overrides

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.transform._

// you can pick and choose your overrides, these are provided for convenience
trait DeliteAllOverridesExp extends DeliteIfThenElseExp /*with DeliteOpMap*/ with DeliteWhileExp {
  this: DeliteApplication with DeliteOpsExp =>
}

trait DeliteScalaGenAllOverrides extends DeliteScalaGenVariables with DeliteScalaGenIfThenElse /*with DeliteScalaGenRange*/ with DeliteScalaGenWhile  {
  val IR: DeliteApplication with DeliteAllOverridesExp
}

trait DeliteCudaGenAllOverrides extends DeliteCudaGenVariables with DeliteCudaGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteCudaGenWhile {
  val IR: DeliteApplication with DeliteAllOverridesExp
}

trait DeliteOpenCLGenAllOverrides extends DeliteOpenCLGenVariables with DeliteOpenCLGenIfThenElse /*with DeliteCudaGenRange*/ with DeliteOpenCLGenWhile {
  val IR: DeliteApplication with DeliteAllOverridesExp
}

trait DeliteCGenAllOverrides extends DeliteCGenVariables /*with DeliteCGenIfThenElse with DeliteCGenRange with DeliteCGenWhile */ {
  val IR: DeliteApplication with DeliteAllOverridesExp
}
