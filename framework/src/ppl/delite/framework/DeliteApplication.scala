package ppl.delite.framework

import codegen.c.TargetC
import codegen.scala.TargetScala
import codegen.Target
import java.io.PrintWriter
import collection.mutable.{HashMap, ListBuffer}
import scala.virtualization.lms.common.embedded.scala.ScalaOpsPkgExp
import scala.virtualization.lms.common.{Base, EffectExp, BaseExp}
import scala.virtualization.lms.internal.GenericCodegen

trait DeliteApplication extends ScalaOpsPkgExp {
  type DeliteApplicationTarget = Target{val IR: DeliteApplication.this.type}
  
  def getCodeGenPkg(t: Target{val IR: DeliteApplication.this.type}) : GenericCodegen{val IR: DeliteApplication.this.type}

  lazy val targets = ListBuffer[DeliteApplicationTarget](
                       //new TargetScala{val IR: DeliteApplication.this.type = DeliteApplication.this},
                       //new TargetC{val IR: DeliteApplication.this.type = DeliteApplication.this}
                     )
                                                                 
  
  var args: Rep[Array[String]] = _
  //private var _targets: HashMap[String, DeliteApplicationTarget] = _

  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    val main_m = {x: Rep[Array[String]] => this.args = x; liftedMain()}

    println("******Generating the program*********")
    // TODO: this loop blows up scalac somewhere/somehow
//    for(tgt <- targets) {
//      //val tgt = e._2
//      globalDefs = List()
//      tgt.generator.emitSource(main_m, "Application", new PrintWriter(System.out))
//    }

    // temp until scalac issue can be resolved
    val scalaTarget = new TargetScala{val IR: DeliteApplication.this.type = DeliteApplication.this}
    //val cTarget = new TargetC{val IR: DeliteApplication.this.type = DeliteApplication.this}
    //scalaTarget.generator.emitSource(main_m, "Application", new PrintWriter(System.out))
    getCodeGenPkg(scalaTarget).emitSource(main_m, "Application", new PrintWriter(System.out))
    //getCodeGenPkg(cTarget).emitSource(main_m, "Application", new PrintWriter(System.out))
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
