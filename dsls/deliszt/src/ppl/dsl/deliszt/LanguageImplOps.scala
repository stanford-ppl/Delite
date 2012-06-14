package ppl.dsl.deliszt

import ppl.dsl.deliszt.datastruct.scala._

trait LanguageImplOps {
  this: DeLisztExp =>
  
  def print_impl(as : Seq[Exp[Any]]) {
    for(a <- as) {
      print(a)
    }
    
    print(unit("""\n"""))
  }
}

trait LanguageImplOpsStandard extends LanguageImplOps {
  this: DeLisztCompiler with DeLisztExp =>
}
