package ppl.dsl.optigraph

trait LanguageImplOps { 
  this: OptiGraph =>

  // ...
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>
  
  //....
}
