package ppl.dsl.optiml

import ppl.delite.framework.DeliteApplication

trait OptiML extends VectorOps with MatrixOps with MLInputReaderOps {
  this: DeliteApplication =>
}

trait OptiMLExp extends OptiML with VectorOpsExp with MatrixOpsExp with MLInputReaderOpsExp
  with VectorImplOpsStandard with MatrixImplOpsStandard with MLInputReaderImplOpsStandard {

  this: DeliteApplication =>
}

//trait OptiMLCodeGen extends OptiMLExp with ScalaGenVector with ScalaGenMatrix with ScalaGenMLInputReader
