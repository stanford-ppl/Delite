package ppl.delite.framework

import codegen.{Target, CodeGenerator}
import scala.virtualization.lms.ppl.{ScalaOpsPkgExp}
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, ScalaCompile}
import collection.mutable.{HashMap, ListBuffer}

trait DeliteApplication extends ScalaOpsPkgExp {

  var args: Rep[Array[String]] = _

  val targets = new HashMap[String,Target]
  val dsls2generate = new ListBuffer[DSLTypeRepresentation]

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    this.args = args;
    val main_m = {x: Rep[Any] => liftedMain()}
    println("******Adding Requested Target*******")
    //todo this should be implemented via some option parsing framework
    targets += "Scala" -> new TargetScala
    targets += "C" -> new TargetC
    //targets += "CUDA" -> new TargetCuda
    println("******Generating the program*********")
    //todo need to also add a way for the dsls to be imported to the application
    for(cg <- generators) {
      //resetting
      println("Using Generator: " + cg.name)
      globalDefs = List()
      cg.emitSource(main_m,"Application", new PrintWriter(System.out))
    }
    println("******Generating the DSLs used*********")
    for(dsl <- dsls2generate) {
      println("Generating DSL: ")
    }

  }

  def registerDSLType(name: String): DSLTypeRepresentation = nop

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main(): Unit

  def liftedMain(): Rep[Unit] = main


  //so that our main doesn't itself get lifted
  private def println(s:String) = System.out.println(s)

  private def nop = throw new RuntimeException("not implemented yet")
}
