package ppl.delite.framework

import java.io.{FileWriter, File, PrintWriter}
import scala.collection.mutable.{Map => MMap}
import scala.tools.nsc.io._
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.virtualization.lms.internal.{GenericFatCodegen, ScalaCompile, GenericCodegen, ScalaCodegen}

import analysis.{MockStream, TraversalAnalysis}
import codegen.c.TargetC
import codegen.cuda.TargetCuda
import codegen.delite.{DeliteCodeGenPkg, DeliteCodegen, TargetDelite}
import codegen.opencl.TargetOpenCL
import codegen.scala.TargetScala
import codegen.Target
import ops.DeliteOpsExp

trait DeliteApplication extends DeliteOpsExp with ScalaCompile {
  type DeliteApplicationTarget = Target{val IR: DeliteApplication.this.type}

  def getCodeGenPkg(t: DeliteApplicationTarget) : GenericFatCodegen{val IR: DeliteApplication.this.type}

  lazy val scalaTarget = new TargetScala{val IR: DeliteApplication.this.type = DeliteApplication.this}
  lazy val cudaTarget = new TargetCuda{val IR: DeliteApplication.this.type = DeliteApplication.this}
  lazy val cTarget = new TargetC{val IR: DeliteApplication.this.type = DeliteApplication.this}
  lazy val openclTarget = new TargetOpenCL{val IR: DeliteApplication.this.type = DeliteApplication.this}

  var targets = List[DeliteApplicationTarget](scalaTarget)
  if(Config.generateCUDA)
    targets = cudaTarget :: targets
  if(Config.generateC)
    targets = cTarget :: targets
  if(Config.generateOpenCL)
    targets = openclTarget :: targets

  val generators: List[GenericFatCodegen{ val IR: DeliteApplication.this.type }] = targets.reverse.map(getCodeGenPkg(_))


  // TODO: refactor, this is from ScalaCompile trait
  lazy val codegen: ScalaCodegen { val IR: DeliteApplication.this.type } = 
    getCodeGenPkg(scalaTarget).asInstanceOf[ScalaCodegen { val IR: DeliteApplication.this.type }]

  // generators created by getCodeGenPkg will use the 'current' scope of the deliteGenerator as global scope
  val deliteGenerator = new DeliteCodeGenPkg { val IR : DeliteApplication.this.type = DeliteApplication.this;
                                               val generators = DeliteApplication.this.generators }

  lazy val analyses: List[TraversalAnalysis{ val IR: DeliteApplication.this.type }] = List()
  
  // Store and retrieve
  val analysisResults = MMap[String,Any]()

  var args: Rep[Array[String]] = _

  var staticDataMap: Map[String,_] = _

  
  final def main(args: Array[String]) {
    println("Delite Application Being Staged:[" + this.getClass.getName + "]")

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
    
    //System.out.println("Running analysis")
    
    // Run any analyses defined
    for(a <- analyses) {
      val baseDir = Config.buildDir + File.separator + a.toString + File.separator
      a.initializeGenerator(baseDir + "kernels" + File.separator, args, analysisResults)
      a.traverse(liftedMain) match {
        case Some(result) => { analysisResults(a.className) = result }
        case _ =>
      }
    }
    reset
    
    //System.out.println("Staging application")
    
    deliteGenerator.emitDataStructures(Config.buildDir + File.separator)

    for (g <- generators) {
      //TODO: Remove c generator specialization
      val baseDir = Config.buildDir + File.separator + (if(g.toString=="c") "cuda" else g.toString) + File.separator // HACK! TODO: HJ, fix
      writeModules(baseDir)
      g.emitDataStructures(baseDir + "datastructures" + File.separator)
      g.initializeGenerator(baseDir + "kernels" + File.separator, args, analysisResults)
    }

    if (Config.debug) {
      if (Config.degFilename.endsWith(".deg")) {
        val streamScala = new PrintWriter(new FileWriter(Config.degFilename.replace(".deg",".scala")))
        val baseDir = Config.buildDir + File.separator + codegen.toString + File.separator
        codegen.initializeGenerator(baseDir + "kernels" + File.separator, args, analysisResults) // whole scala application (for testing)
        codegen.emitSource(liftedMain, "Application", streamScala) // whole scala application (for testing)
        // TODO: dot output
        reset
      }
    }
    deliteGenerator.initializeGenerator(Config.buildDir, args, analysisResults)
    val sd = deliteGenerator.emitSource(liftedMain, "Application", stream)    
    deliteGenerator.finalizeGenerator()

    if(Config.printGlobals) {
      println("Global definitions")
      for(globalDef <- globalDefs) {
        println(globalDef)
      }
    }

    generators foreach { _.finalizeGenerator()}
    
    staticDataMap = Map() ++ sd map { case (s,d) => (deliteGenerator.quote(s), d) }
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
    println("Delite Application Being Executed:[" + this.getClass.getName + "]")

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
