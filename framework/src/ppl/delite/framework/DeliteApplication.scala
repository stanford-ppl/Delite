package ppl.delite.framework

import codegen.c.TargetC
import codegen.scala.{CodeGeneratorScalaApplication, TargetScala}
import codegen.{Target, CodeGenerator}
import embedded.scala._
import java.io.PrintWriter
import collection.mutable.{HashMap, ListBuffer}
import scala.virtualization.lms.common.{EffectExp, BaseExp}

trait DeliteApplication extends EffectExp { 
//trait DeliteApplication extends ScalaOpsPkgExp3 {

  type DeliteApplicationTarget = Target{val intermediate: DeliteApplication.this.type}

  var args: Rep[Array[String]] = _
  private var _targets: HashMap[String, DeliteApplicationTarget] = _
  //val targetCodeGenerators = new HashMap[String, ListBuffer[CodeGenerator{val intermediate: DeliteApplication.this.type}]]
//  val dsls2generate = new ListBuffer[DSLTypeRepresentation]

  println("******Adding Requested Target*******")
  //todo this should be implemented via some option parsing framework
  //addTarget(new TargetScala{val intermediate: DeliteApplication.this.type = DeliteApplication.this})

  //val sgenlist = new ListBuffer[CodeGenerator{val intermediate: DeliteApplication.this.type}]
  //sgenlist += new CodeGeneratorScalaMisc{val intermediate: DeliteApplication.this.type = DeliteApplication.this}
  //targetCodeGenerators += stgt.name ->  sgenlist
//  //todo move this repetition into a function
//  val ctgt = addTarget(new TargetC{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
//  val cgenlist = new ListBuffer[CodeGenerator{val intermediate: DeliteApplication.this.type}]
//  cgenlist += new CodeGeneratorCMisc{val intermediate: DeliteApplication.this.type = DeliteApplication.this}
//  targetCodeGenerators += ctgt.name ->  cgenlist

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    this.args = fresh //args;
    val main_m = {x: Rep[Array[String]] => liftedMain()}


    println("******Generating the program*********")
    for(e <- targets) {
      val tgt = e._2
      globalDefs = List()
      tgt.applicationGenerator.emitSource(this.args, main_m, "Application", new PrintWriter(System.out))
    }

  }

  // temporary, for code gen: we need one copy of these globals shared between code generators and targets
  //var shallow = false

  var scope: List[TP[_]] = Nil


  def addTarget(tgt: DeliteApplicationTarget) = {
    targets += tgt.name -> tgt
    tgt
  }

  def targets : HashMap[String, DeliteApplicationTarget] = {
    if (_targets == null) {
      _targets = new HashMap[String, DeliteApplicationTarget]

      addTarget(new TargetScala{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
      //addTarget(new TargetC{val intermediate: DeliteApplication.this.type = DeliteApplication.this})
    }
    _targets
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
