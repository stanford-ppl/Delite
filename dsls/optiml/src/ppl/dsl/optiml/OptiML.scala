package ppl.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.embedded.scala.{ScalaOpsPkg3, ScalaOpsPkgExp3}

trait OptiML extends ScalaOpsPkg3 with VectorOps with MatrixOps with MLInputReaderOps { 
  this: DeliteApplication =>
}

trait OptiMLExp extends OptiML with ScalaOpsPkgExp3 with VectorOpsExp with MatrixOpsExp with MLInputReaderOpsExp
  with VectorImplOpsStandard with MatrixImplOpsStandard with MLInputReaderImplOpsStandard {

  this: DeliteApplication =>
}
