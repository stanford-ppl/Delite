package ppl.delite.framework

import codegen.CodeGenerator
import codegen.scala.CodeGeneratorScala
import scala.virtualization.lms.ppl.{ScalaGenScalaOpsPkg, ScalaOpsPkgExp}
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, ScalaCompile}

trait DeliteApplication extends ScalaGenScalaOpsPkg with GenericNestedCodegen {

  var args: Rep[Array[String]] = _

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    this.args = args;
    println("Running the main function to extract the AST")
    val main_m = {x: Exp[Any] => liftedMain()}
    println("******Usual Gen******")
    emitScalaSource(main_m, "Application", new PrintWriter(System.out))
    //resetting
    globalDefs = List()
    println("******MY GEN*********")
    val cg = new CodeGeneratorScala {
      val intermediate: this.type = this
    }
    //cg.emitSource(this,main_m,"Application", new PrintWriter(System.out))


  }

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main(): Unit

  def liftedMain(): Exp[Unit] = main


  //so that our main doesn't itself get lifted
  private def println(s:String) = System.out.println(s)
}
