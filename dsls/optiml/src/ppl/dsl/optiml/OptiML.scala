package ppl.dsl.optiml

trait OptiML extends VectorOps with MatrixOps with MLInputReaderOps

trait OptiMLExp extends OptiML with VectorOpsRepExp with MatrixOpsRepExp with MLInputReaderOpsRepExp
  with VectorImplOpsStandard with MatrixImplOpsStandard with MLInputReaderImplOpsStandard

trait OptiMLCodeGen extends OptiMLExp with ScalaGenVector with ScalaGenMatrix with ScalaGenMLInputReader
