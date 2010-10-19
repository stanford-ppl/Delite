package ppl.delite.framework.codegen.scala

import java.io.PrintWriter
import collection.mutable.ListBuffer
import ppl.delite.framework.codegen.{CodeGenerator, Target}
import ppl.delite.framework.embedded.scala.CodeGeneratorScalaMisc


trait TargetScala extends Target {

  import intermediate._

  val name = "Scala"

  def applicationGenerator = {
    if (_applicationGenerator == null){
      _applicationGenerator = new CodeGeneratorScalaApplication { val intermediate: TargetScala.this.intermediate.type = TargetScala.this.intermediate }
    }
    _applicationGenerator
  }
  
  val generators = new ListBuffer[CodeGenerator{val intermediate: TargetScala.this.intermediate.type}]
  //generators += new CodeGeneratorScalaMisc{val intermediate: TargetScala.this.intermediate.type = TargetScala.this.intermediate}

}