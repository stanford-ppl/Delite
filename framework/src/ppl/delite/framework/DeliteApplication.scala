package ppl.delite.framework

import codegen.c.TargetC
import codegen.scala.TargetScala
import codegen.Target
import java.io.PrintWriter
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.virtualization.lms.internal.{ScalaCompile, GenericCodegen}

trait DeliteApplication extends BaseExp /*with ScalaCompile*/ {
  type DeliteApplicationTarget = Target{val IR: DeliteApplication.this.type}

  def getCodeGenPkg(t: DeliteApplicationTarget) : GenericCodegen{val IR: DeliteApplication.this.type}

  lazy val targets = List[DeliteApplicationTarget](
                       new TargetScala{val IR: DeliteApplication.this.type = DeliteApplication.this}//,
                       //new TargetC{val IR: DeliteApplication.this.type = DeliteApplication.this}
                     )
                                                                   
  var args: Rep[Array[String]] = _
  
  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    val main_m = {x: Rep[Array[String]] => this.args = x; liftedMain()}

    println("******Generating the program*********")
    for(tgt <- targets) {
      globalDefs = List()
      getCodeGenPkg(tgt).emitSource(main_m, "Application", new PrintWriter(System.out))
    }
  }

//  final def execute(args: Array[String]) {
//    println("Delite Application Being Executed:[" + this.getClass.getSimpleName + "]")
//    val main_m = {x: Rep[Array[String]] => this.args = x; liftedMain()}
//
//    println("******Executing the program*********")
//    for(tgt <- targets) {
//      globalDefs = List()
//      val g = compile(main_m)
//      g(args)
//    }
//  }

  def registerDSLType(name: String): DSLTypeRepresentation = nop

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main(): Unit

  def liftedMain(): Rep[Unit] = main

  private def nop = throw new RuntimeException("not implemented yet")
}
