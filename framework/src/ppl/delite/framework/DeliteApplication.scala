package ppl.delite.framework

import codegen.c.TargetC
import codegen.scala.TargetScala
import codegen.{Target, CodeGenerator}
import embedded.scala.{CodeGeneratorCMisc, CodeGeneratorScalaMisc}
import scala.virtualization.lms.ppl.{ScalaOpsPkgExp}
import java.io.PrintWriter
import collection.mutable.{HashMap, ListBuffer}

trait DeliteApplication extends ScalaOpsPkgExp {

  var args: Rep[Array[String]] = _
  val targets = new HashMap[String, Target{val intermediate: DeliteApplication.this.type}]
  val targetCodeGenerators = new HashMap[String, ListBuffer[CodeGenerator{val intermediate: DeliteApplication.this.type}]]
  val dsls2generate = new ListBuffer[DSLTypeRepresentation]

  println("******Adding Requested Target*******")
  //todo this should be implemented via some option parsing framework
  val stgt = addTarget(new TargetScala{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
  val sgenlist = new ListBuffer[CodeGenerator{val intermediate: DeliteApplication.this.type}]
  sgenlist += new CodeGeneratorScalaMisc{val intermediate: DeliteApplication.this.type = DeliteApplication.this}
  targetCodeGenerators += stgt.name ->  sgenlist
  //todo move this repetition into a function
  val ctgt = addTarget(new TargetC{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
  val cgenlist = new ListBuffer[CodeGenerator{val intermediate: DeliteApplication.this.type}]
  cgenlist += new CodeGeneratorCMisc{val intermediate: DeliteApplication.this.type = DeliteApplication.this}
  targetCodeGenerators += ctgt.name ->  cgenlist

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    this.args = args;
    val main_m = {x: Rep[Any] => liftedMain()}


    //addTarget(new TargetC{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
    //targets += "CUDA" -> new TargetCuda
    println("******Generating the program*********")
    for(e <- targets) {
      val tgt = e._2
      val gens = targetCodeGenerators.get(tgt.name).getOrElse{
        throw new RuntimeException("No Generators list found for target: " + tgt.name)
      }
      globalDefs = List()
      tgt.emitSource(main_m, "Application", new PrintWriter(System.out), gens)
    }

  }

  def addTarget(tgt: Target{val intermediate:DeliteApplication.this.type}) = {
    targets += tgt.name -> tgt
    tgt
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
