package ppl.delite.framework.codegen.c

import collection.mutable.ListBuffer
import ppl.delite.framework.codegen.{CodeGenerator, Target}

trait TargetC extends Target with EmitterC {
  import intermediate._

  val name = "C"

  val generators = new ListBuffer[CodeGenerator{val intermediate: TargetC.this.intermediate.type}]
  //generators += new CodeGeneratorCMisc{val intermediate: TargetC.this.intermediate.type = TargetC.this.intermediate}

}