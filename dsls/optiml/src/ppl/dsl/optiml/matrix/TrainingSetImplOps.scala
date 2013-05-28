package ppl.dsl.optiml

trait TrainingSetImplOps { this: OptiML =>
}

trait TrainingSetImplOpsStandard extends TrainingSetImplOps {
  this: OptiMLCompiler with OptiMLLift =>

}
