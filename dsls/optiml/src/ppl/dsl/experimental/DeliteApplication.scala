package ppl.dsl.experimental

import java.io.{FileWriter, File, PrintWriter}
import scala.tools.nsc.io._
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import ppl.delite.framework.codegen.delite.{DeliteCodegen, TargetDelite}
import ppl.delite.framework._

trait SandboxDeliteCodeGenPkg extends SandboxDeliteGenTaskGraph

trait SandboxDeliteApplication extends SandboxDeliteOpsExp with ScalaCompile {
  type DeliteApplicationTarget = SandboxTarget{val IR: SandboxDeliteApplication.this.type}

  def getCodeGenPkg(t: DeliteApplicationTarget) : GenericFatCodegen{val IR: SandboxDeliteApplication.this.type}

  lazy val scalaTarget = new TargetScala{val IR: SandboxDeliteApplication.this.type = SandboxDeliteApplication.this}
  lazy val cudaTarget = new TargetCuda{val IR: SandboxDeliteApplication.this.type = SandboxDeliteApplication.this}
  lazy val cTarget = new TargetC{val IR: SandboxDeliteApplication.this.type = SandboxDeliteApplication.this}

  // TODO: this should be handled via command line options
  lazy val targets = List[DeliteApplicationTarget](scalaTarget/*, cudaTarget, cTarget*/)
  val generators: List[GenericFatCodegen{ val IR: SandboxDeliteApplication.this.type }] = targets.map(getCodeGenPkg(_))

  // TODO: refactor, this is from ScalaCompile trait
  lazy val codegen: ScalaCodegen { val IR: SandboxDeliteApplication.this.type } = 
    getCodeGenPkg(scalaTarget).asInstanceOf[ScalaCodegen { val IR: SandboxDeliteApplication.this.type }]

  // generators created by getCodeGenPkg will use the 'current' scope of the deliteGenerator as global scope
  val deliteGenerator = new SandboxDeliteCodeGenPkg { val IR : SandboxDeliteApplication.this.type = SandboxDeliteApplication.this;
                                                      val generators = SandboxDeliteApplication.this.generators }
                                               
  var args: Rep[Array[String]] = _
  
  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getSimpleName + "]")

    println("******Generating the program******")

    //clean up the code gen directory
    Directory(Path(Config.buildDir)).deleteRecursively()

    val stream =
      if (Config.degFilename == ""){
        new PrintWriter(System.out)
      }
      else {
        new PrintWriter(new FileWriter(Config.degFilename))
      }

    def writeModules(baseDir: String) {
      Directory(Path(baseDir)).createDirectory()
      val writer = new FileWriter(baseDir + "modules.dm")
      writer.write("datastructures:\n")
      writer.write("kernels:datastructures\n")
      writer.close()
    }

    deliteGenerator.emitDataStructures(Config.buildDir + File.separator)

    for (g <- generators) {
      val baseDir = Config.buildDir + File.separator + g.toString + File.separator
      writeModules(baseDir)
      g.emitDataStructures(baseDir + "datastructures" + File.separator)
      g.initializeGenerator(baseDir + "kernels" + File.separator)
    }

    if (Config.degFilename.endsWith(".deg")) {
      val streamScala = new PrintWriter(new FileWriter(Config.degFilename.replace(".deg",".scala")))
      codegen.emitSource(liftedMain, "Application", streamScala) // whole scala application (for testing)
      // TODO: dot output
      reset
    }
    deliteGenerator.initializeGenerator(Config.buildDir)
    deliteGenerator.emitSource(liftedMain, "Application", stream)    
    deliteGenerator.finalizeGenerator()
    
    generators foreach { _.finalizeGenerator()}
  }

  final def generateScalaSource(name: String, stream: PrintWriter) = {
    reset
    stream.println("object "+name+"Main {"/*}*/)
    stream.println("def main(args: Array[String]) {"/*}*/)
    stream.println("val o = new "+name)
    stream.println("o.apply(args)")
    stream.println("ppl.delite.runtime.profiler.PerformanceTimer.print(\"app\")")
    stream.println(/*{*/"}")
    stream.println(/*{*/"}")
    codegen.emitSource(liftedMain, name, stream)
  }


  final def execute(args: Array[String]) {
    println("Delite Application Being Executed:[" + this.getClass.getSimpleName + "]")

    println("******Executing the program*********")
    globalDefs = List()
    val g = compile(liftedMain)
    g(args)
  }

  /**
   * this is the entry method for our applications, user implement this method. Note, that it is missing the
   * args parameter, args are now accessed via the args field. This basically hides the notion of Reps from
   * user code
   */
  def main(): Unit

  def liftedMain(x: Rep[Array[String]]) = { this.args = x; val y = main(); this.args = null; unit(y) }
  

  private def nop = throw new RuntimeException("not implemented yet")
}
