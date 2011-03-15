package ppl.dsl.optiml

import datastruct.scala.{Vector, Matrix, Stream}

trait StreamImplOps { this: OptiML =>

}

trait StreamImplOpsStandard extends StreamImplOps {
  this: OptiMLCompiler with OptiMLLift =>

}
