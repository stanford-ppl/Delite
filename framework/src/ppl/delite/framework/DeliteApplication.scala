package ppl.delite.framework

import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.scala.TargetScala
import codegen.Target
import ops.DeliteOpsExp
import scala.virtualization.lms.common.{BaseExp, Base}
import java.io.{FileWriter, File, PrintWriter}
import scala.virtualization.lms.internal.{GenericNestedCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

trait DeliteApplication extends DeliteOpsExp with ScalaCompile {
  type DeliteApplicationTarget = Target{val IR: DeliteApplication.this.type}

  def getCodeGenPkg(t: DeliteApplicationTarget) : GenericNestedCodegen{val IR: DeliteApplication.this.type}

  lazy val scalaTarget = new TargetScala{val IR: DeliteApplication.this.type = DeliteApplication.this}
  lazy val cudaTarget = new TargetCuda{val IR: DeliteApplication.this.type = DeliteApplication.this}
  lazy val cTarget = new TargetC{val IR: DeliteApplication.this.type = DeliteApplication.this}

  // TODO: this should be handled via command line options
  lazy val targets = List[DeliteApplicationTarget](scalaTarget , cudaTarget /*, cTarget*/)
  val generators: List[GenericNestedCodegen{ val IR: DeliteApplication.this.type }] = targets.map(getCodeGenPkg(_))

  // TODO: refactor, this is from ScalaCompile trait
  lazy val codegen: ScalaCodegen { val IR: DeliteApplication.this.type } = 
    getCodeGenPkg(scalaTarget).asInstanceOf[ScalaCodegen { val IR: DeliteApplication.this.type }]

  var args: Rep[Array[String]] = _
  
  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")
    val main_m = {x: Rep[Array[String]] => this.args = x; liftedMain()}                                   

    println("******Generating the program*********")

    val deliteGenerator = new DeliteCodeGenPkg { val IR : DeliteApplication.this.type = DeliteApplication.this;
                                                 val generators = DeliteApplication.this.generators }

    //clean up the code gen directory
    Util.deleteDirectory(new File(Config.build_dir))

    val stream =
      if (Config.deg_filename == ""){
        new PrintWriter(System.out)
      }
      else {
        new PrintWriter(new FileWriter(Config.deg_filename))
      }

    for (g <- generators) {
      g.emitDataStructures()
    }
    
    //codegen.emitSource(main_m, "Application", stream) // whole scala application (for testing)
    deliteGenerator.emitSource(main_m, "Application", stream)
  }

  final def execute(args: Array[String]) {
    println("Delite Application Being Executed:[" + this.getClass.getSimpleName + "]")
    val main_m = {x: Rep[Array[String]] => this.args = x; liftedMain()}

    println("******Executing the program*********")
    globalDefs = List()
    val g = compile(main_m)
    g(args)
  }

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
