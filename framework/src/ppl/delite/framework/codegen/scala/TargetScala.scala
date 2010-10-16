package ppl.delite.framework.codegen.scala

import collection.mutable.ListBuffer
import ppl.delite.framework.codegen.{CodeGenerator, Target}


trait TargetScala extends Target with EmitterScala {

  import intermediate._

  val name = "Scala"

  val generators = new ListBuffer[CodeGenerator{val intermediate: TargetScala.this.intermediate.type}]
  //generators += new CodeGeneratorScalaMisc{val intermediate: TargetScala.this.intermediate.type = TargetScala.this.intermediate}

}