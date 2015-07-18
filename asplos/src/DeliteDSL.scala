package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.codegen.scala.TargetScala
import ppl.delite.framework.codegen.cuda.TargetCuda
import ppl.delite.framework.codegen.cpp.TargetCpp
import ppl.delite.framework.codegen.hw._

//import ppl.delite.framework.{DeliteInteractive, DeliteInteractiveRunner}
import ppl.delite.framework.{Config, ExpressionsOpt, SchedulingOpt, DeliteApplication}
import ppl.delite.framework.{Interfaces,InterfacesExp}
import ppl.delite.framework.codegen.delite.overrides._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

import org.scalatest._
import ppl.delite.framework.Config
import java.io.{File,FileReader}

// --- Basic LMS ops (mimics Scala syntax)
trait LMSDSLOps extends Base 
  with Equal                with IfThenElse           with Variables            with While 
  with MathOps              with ImplicitOps          with OrderingOps          with StringOps 
  with BooleanOps           with PrimitiveOps         with MiscOps              with TupledFunctions
  with TupleOps             with CastingOps           with ExceptionOps         with ObjectOps

trait LMSDSLLift extends Base 
  with LiftEquals           with LiftVariables        with LiftString
  with LiftBoolean          with LiftPrimitives       with LiftNumeric {
  this: LMSDSLOps =>
}

// TODO: Are DSLOps really needed here?
trait LMSDSLOpsExp extends LMSDSLOps 
  with EqualExp             with IfThenElseExp        with VariablesExp         with WhileExp   
  with MathOpsExp           with ImplicitOpsExp       with OrderingOpsExp       with StringOpsExp 
  with BooleanOpsExp        with PrimitiveOpsExpOpt   with MiscOpsExp           with TupledFunctionsExp 
  with TupleOpsExp          with CastingOpsExp        with ExceptionOpsExp      with ObjectOpsExpOpt
  with DSLOpsExp

// --- Basic LMS Code Generators
trait ScalaGenLMSDSL extends ScalaGenBase
  with ScalaGenEqual        with ScalaGenIfThenElse   with ScalaGenVariables    with ScalaGenWhile
  with ScalaGenMathOps      with ScalaGenImplicitOps  with ScalaGenOrderingOps  with ScalaGenStringOps    
  with ScalaGenBooleanOps   with ScalaGenPrimitiveOps with ScalaGenMiscOps      with ScalaGenTupledFunctions 
  with ScalaGenTupleOps     with ScalaGenCastingOps   with ScalaGenExceptionOps with ScalaGenObjectOps
  with ScalaGenDSLOps
  { val IR: LMSDSLOpsExp }

trait CudaGenLMSDSL extends CudaGenBase 
  with CudaGenEqual         with CudaGenIfThenElse    with CudaGenVariables     with CudaGenWhile
  with CudaGenMathOps       with CudaGenImplicitOps   with CudaGenOrderingOps   with CudaGenStringOps
  with CudaGenBooleanOps    with CudaGenPrimitiveOps  with CudaGenMiscOps       /*with CudaGenFunctions*/
  with CudaGenTupleOps      with CudaGenCastingOps    with CudaGenExceptionOps  with CudaGenObjectOps
  with CudaGenDSLOps
  { val IR: LMSDSLOpsExp }

trait CGenLMSDSL extends CGenBase
  with CGenEqual            with CGenIfThenElse       with CGenVariables        with CGenWhile
  with CGenMathOps          with CGenImplicitOps      with CGenOrderingOps      with CGenStringOps
  with CGenBooleanOps       with CGenPrimitiveOps     with CGenMiscOps          with CGenTupledFunctions
  with CGenTupleOps         with CGenCastingOps       with CGenExceptionOps     with CGenObjectOps
  with CGenDSLOps
  { val IR: LMSDSLOpsExp }

trait HwGenLMSDSL extends HwCodegen
  /*with HwGenEqual*/       /*with HwGenIfThenElse*/  with HwGenVariables       with HwGenWhile
  /*with HwGenMathOps*/     /*with HwGenImplicitOps*/ with HwGenOrderingOps     with HwGenStringOps
  with HwGenBooleanOps      with HwGenPrimitiveOps    /*with HwGenMiscOps*/     /*with HwGenTupledFunctions*/
  /*with HwGenTupleOps*/    /*with HwGenCastingOps*/  /*with HwGenExceptionOps*//*with HwGenObjectOps*/
  with HwGenDSLOps
  { val IR: LMSDSLOpsExp }

// --- Delite Transformers
trait DeliteDSLTransformExp extends DeliteVisit
  //with MultiloopSoATransformWithReduceExp -- SOA transform for reduce needs to be fixed
  with MultiloopSoATransformExp

// --- Delite DSL Ops
trait DeliteDSLOps extends LMSDSLOps
  with StructOps            with DeliteArrayOps      with DeliteMapOps with DeliteArrayBufferOps
  with DeliteAnalysesOps    with DeliteFileReaderOps {
  this: DeliteDSLApplication =>
}

trait DeliteDSLCompilerOps extends DeliteDSLOps 
  with DeliteArrayCompilerOps with DeliteArrayBufferCompilerOps {
  this: DeliteDSLApplication with DeliteDSLOpsExp =>
}

trait DeliteDSLOpsExp extends DeliteDSLCompilerOps with LMSDSLOpsExp with DeliteDSLTransformExp
  with DeliteStructsExp       with DeliteArrayFatExp       with DeliteMapOpsExp   with DeliteArrayBufferOpsExp
  with DeliteLMSForwarderExp  with DeliteFileReaderOpsExp
  with FunctionBlocksExp      with DeliteOpsExp            with ExpressionsOpt    with DeliteAllOverridesExp {

  this: DeliteApplication with DeliteDSLApplication =>

  def getCodeGenPkg(t: Target{val IR: DeliteDSLOpsExp.this.type}) : GenericFatCodegen{val IR: DeliteDSLOpsExp.this.type} = {
    t match {
      case _:TargetScala => new ScalaGenDeliteDSL{val IR: DeliteDSLOpsExp.this.type = DeliteDSLOpsExp.this}
      case _:TargetCuda => new CudaGenDeliteDSL{val IR: DeliteDSLOpsExp.this.type = DeliteDSLOpsExp.this}
      case _:TargetCpp => new CGenDeliteDSL{val IR: DeliteDSLOpsExp.this.type = DeliteDSLOpsExp.this}
      case _:TargetHw => new HwGenDeliteDSL{val IR: DeliteDSLOpsExp.this.type = DeliteDSLOpsExp.this}
      case _ => throw new Exception("DeliteDSL does not support this target")
    }
  }  
}

// --- Delite DSL Code Generators
trait DeliteDSLCodeGenBase extends GenericFatCodegen with SchedulingOpt {
  val IR: DeliteApplication with DeliteDSLOpsExp 
  // Very strange things happen during codegen if this line isn't here!
  override def initialDefs = IR.deliteGenerator.availableDefs
}

trait ScalaGenDeliteDSL extends DeliteDSLCodeGenBase with ScalaGenLMSDSL
  with ScalaGenDeliteStruct   with ScalaGenDeliteArrayOps with ScalaGenDeliteMapOps
  with ScalaGenDeliteFileReaderOps
  with ScalaGenDeliteOps      with DeliteScalaGenAllOverrides 
  { val IR: DeliteApplication with DeliteDSLOpsExp }

trait CudaGenDeliteDSL extends DeliteDSLCodeGenBase with CudaGenLMSDSL 
  with CudaGenDeliteStruct with CudaGenDeliteArrayOps     /*with CudaGenDeliteMapOps*/
  with CudaGenDeliteOps    with DeliteCudaGenAllOverrides with DeliteCppHostTransfer with DeliteCudaDeviceTransfer 
  { val IR: DeliteApplication with DeliteDSLOpsExp }

trait CGenDeliteDSL extends DeliteDSLCodeGenBase with CGenLMSDSL
  with CGenDeliteStruct    with CGenDeliteArrayOps with CGenDeliteMapOps
  with CGenDeliteFileReaderOps
  with CGenDeliteOps       with DeliteCGenAllOverrides 
  with DeliteCppHostTransfer 
  { val IR: DeliteApplication with DeliteDSLOpsExp }

trait HwGenDeliteDSL extends DeliteDSLCodeGenBase with HwGenLMSDSL
  /*with HwGenDeliteStruct*/ with HwGenDeliteArrayOps  /*with HwGenDeliteMapOps*/
  with HwGenDeliteOps       
  with HwGenDeliteInternalOps 
  { val IR: DeliteApplication with DeliteDSLOpsExp }

// --- Stubs for DSLs to extend
trait DeliteDSLCompiler extends DeliteDSLOpsExp with DeliteApplication with DeliteDSLApplication
trait DeliteDSLApplication extends DeliteDSLOps with LMSDSLLift {
  var args: Rep[Array[String]]
  def main(): Unit
}

trait DeliteDSLBenchmarks extends Suite with DeliteDSLCompilation

trait DeliteDSLConfig {
  val propFile = new File("delite.properties")
  val props = new java.util.Properties(System.getProperties)
  if (propFile.exists) props.load(new FileReader(propFile))

  // Parameters
  val verbose = props.getProperty("tests.verbose", "false") != "false"
  val verboseDefs = props.getProperty("tests.verboseDefs", "false") != "false"
  val threads = props.getProperty("tests.threads", "1").split(",").map(_.toInt)
  val cacheSyms = props.getProperty("tests.cacheSyms", "true").toBoolean
  val javaHome = new File(props.getProperty("java.home", ""))
  val scalaHome = new File(props.getProperty("scala.vanilla.home", ""))
  val runtimeClasses = new File(props.getProperty("runtime.classes", ""))
  val runtimeExternalProc = false // javaHome, scalaHome and runtimeClasses only required if runtimeExternalProc is true. should this be configurable? or should we just remove execTestExternal?
  val deliteTestTargets = props.getProperty("tests.targets", "scala").split(",")
  val useBlas = props.getProperty("tests.extern.blas", "false").toBoolean
}

trait DeliteDSLCompilation extends DeliteDSLConfig {
  val javaBin = new File(javaHome, "bin/java")
  val scalaCompiler = new File(scalaHome, "lib/scala-compiler.jar")
  val scalaLibrary = new File(scalaHome, "lib/scala-library.jar")

  def uniqueTestName(app: DeliteDSLCompiler): String = app.getClass.getName.replaceAll("\\$", "")
  def degName(app: DeliteDSLCompiler): String = uniqueTestName(app) + ".deg"

  def validateParameters() {
    if (runtimeExternalProc && !javaBin.exists) throw new TestFailedException("Could not find valid java installation in " + javaHome, 3)
    else if (runtimeExternalProc && !scalaHome.exists) throw new TestFailedException("scala.vanilla.home must be a valid path in delite.proeprties", 3)
    else if (runtimeExternalProc && (!scalaCompiler.exists || !scalaLibrary.exists)) throw new TestFailedException("Could not find valid scala installation in " + scalaHome, 3)
    else if (runtimeExternalProc && !runtimeClasses.exists) throw new TestFailedException("runtime.classes must be a valid path in delite.properties", 3)
  }

  def runBenchmark(app: DeliteDSLCompiler) {
    val args = Array(degName(app))
    
    stageBenchmark(app)

    // Set runtime parameters for targets and execute runtime
    for(target <- deliteTestTargets) {
      for (num <- threads) {
        def runtimeConfig(numScala: Int = 1, numCpp: Int = 0, numCuda: Int = 0, numOpenCL: Int = 0) {
          ppl.delite.runtime.Config.numThreads = numScala
          ppl.delite.runtime.Config.numCpp = numCpp
          ppl.delite.runtime.Config.numCuda = numCuda
          ppl.delite.runtime.Config.testMode = true
        }

        target match {
          case "scala" => runtimeConfig(numScala = num)
          case "cpp" => runtimeConfig(numCpp = num)
          case "cuda" => runtimeConfig(numScala = num, numCpp = 1, numCuda = 1) // C++ kernels launched on GPU host
          case _ => assert(false)
        }
        val outStr = execBenchmark(app, args, target, num)
      }
    }
  }

  def stageBenchmark(app: DeliteDSLCompiler) {
    validateParameters()

    // Enable specified target code generators
    for(t <- deliteTestTargets) { t match {
      case "scala" =>
      case "cuda" => Config.generateCUDA = true; Config.generateCpp = true
      case "cpp" => Config.generateCpp = true
      case _ => println("Unknown test target: " + t)
    }}

    if (useBlas) Config.useBlas = true

    println("STAGING...")
    val saveDeg = Config.degFilename
    val saveBuildDir = Config.buildDir
    val saveCacheSyms = Config.cacheSyms
    val generatedDir = Config.buildDir + java.io.File.separator + uniqueTestName(app)
    try {
      Config.degFilename = degName(app)
      Config.buildDir = generatedDir
      Config.cacheSyms = cacheSyms
      app.main(Array())
    } finally {
      // concurrent access check
      assert(Config.buildDir == generatedDir)
      Config.degFilename = saveDeg
      Config.buildDir = saveBuildDir
      Config.cacheSyms = saveCacheSyms
    }
  }
  
  private def execBenchmark(app: DeliteDSLCompiler, args: Array[String], target: String, threads: Int) = {
    ppl.delite.runtime.Delite.embeddedMain(args, app.staticDataMap)
  }
}
