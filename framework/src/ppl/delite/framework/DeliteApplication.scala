package ppl.delite.framework

import codegen.c.CodeGeneratorCBase
import codegen.CodeGenerator
import codegen.scala.CodeGeneratorScalaBase
import scala.virtualization.lms.ppl.{ScalaOpsPkgExp}
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, ScalaCompile}
import collection.mutable.ListBuffer

trait DeliteApplication extends ScalaOpsPkgExp {

  var args: Rep[Array[String]] = _

  var generators = new ListBuffer[CodeGenerator{val intermediate: DeliteApplication.this.type}]

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    this.args = args;
    println("Running the main function to extract the AST")
    val main_m = {x: Rep[Any] => liftedMain()}
    println("******MY GENs*********")
    for(cg <- generators) {
      //resetting
      println("Using Generator: " + cg.name)
      globalDefs = List()
      cg.emitSource(main_m,"Application", new PrintWriter(System.out))
    }


  }

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main(): Unit

  def liftedMain(): Rep[Unit] = main


  //so that our main doesn't itself get lifted
  private def println(s:String) = System.out.println(s)
}
