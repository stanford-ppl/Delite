package ppl.tests.scalatest

import org.scalatest._

import scala.virtualization.lms.common._

import ppl.delite.framework.Config
import ppl.delite.framework.codegen.Target
import ppl.delite.framework.DeliteApplication
import ppl.delite.runtime.graph._
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets

import java.io.{FileWriter, BufferedWriter}
import java.io.{ File, Console => _, _ }
import java.io.FileSystem
import scala.reflect.SourceContext
import scala.collection.mutable.{ ArrayBuffer, SynchronizedBuffer }

trait DeliteTestConfig {
  // something arbitrary that we should never see in any test's output
  val MAGICDELIMETER = "!~x02$758209"

  val propFile = new File("delite.properties")
  val props = new java.util.Properties(System.getProperties)
  if (propFile.exists) props.load(new FileReader(propFile))

  // test parameters
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

  var cppWhiteList = Seq("StaticData", "DeliteTestMkString", "DeliteTestAppend", "DeliteTestStrConcat", "DeliteTestFwNew", //test operations are Scala-only by design
                         "DeliteTestBwNew", "DeliteTestBwWrite", "DeliteTestBwClose", "DeliteTestPrintLn", "scala.collection.mutable.ArrayBuffer",
                         "DeliteArraySeq[scala.virtualization.lms.common.Record{", "Array[scala.virtualization.lms.common.Record{") //C++ doesn't currently support non-Soa'd Array[Record]

}

trait DeliteSuite extends Suite with DeliteTestConfig {
  val javaBin = new File(javaHome, "bin/java")
  val scalaCompiler = new File(scalaHome, "lib/scala-compiler.jar")
  val scalaLibrary = new File(scalaHome, "lib/scala-library.jar")

  def checkMultiLoop = false
  def enforceFullCoverage = true

  def validateParameters() {
    if (runtimeExternalProc && !javaBin.exists) throw new TestFailedException("Could not find valid java installation in " + javaHome, 3)
    else if (runtimeExternalProc && !scalaHome.exists) throw new TestFailedException("scala.vanilla.home must be a valid path in delite.proeprties", 3)
    else if (runtimeExternalProc && (!scalaCompiler.exists || !scalaLibrary.exists)) throw new TestFailedException("Could not find valid scala installation in " + scalaHome, 3)
    else if (runtimeExternalProc && !runtimeClasses.exists) throw new TestFailedException("runtime.classes must be a valid path in delite.properties", 3)
  }

  def uniqueTestName(app: DeliteTestRunner): String = {
    app.getClass.getName.replaceAll("\\$", "")
  }

  def degName(app: DeliteTestRunner): String = {
    uniqueTestName(app) + "-test.deg"
  }

  def compileAndTest(app: DeliteTestRunner, checkMultiLoop: Boolean = checkMultiLoop, enforceFullCoverage: Boolean = enforceFullCoverage) {
    println("=================================================================================================")
    println("TEST: " + app.toString)
    println("=================================================================================================")

    validateParameters()
    val args = Array(degName(app))
    app.resultBuffer = new ArrayBuffer[Boolean] with SynchronizedBuffer[Boolean]

    // Enable specified target code generators
    for(t <- deliteTestTargets) {
      t match {
        case "scala" =>
        case "cuda" => Config.generateCUDA = true; Config.generateCpp = true
        case "cpp" => Config.generateCpp = true
        case "opencl" => Config.generateOpenCL = true; Config.generateCpp = true
        case _ => println("Unknown test target: " + t)
      }
    }

    //enable strict checking that scala and cpp kernels are actually generated
    if (enforceFullCoverage) {
      Config.generationFailedWhitelist += "scala" -> Seq() //no exceptions
      Config.generationFailedWhitelist += "cpp" -> cppWhiteList //exclude ops provided by test suite
    }

    if(useBlas) Config.useBlas = true

    // check if all multiloops in the test app are generated for specified targets
    if(checkMultiLoop) {
      val generateCUDA = Config.generateCUDA
      Config.generateCUDA = true
      stageTest(app)
      val graph = ppl.delite.runtime.Delite.loadDeliteDEG(degName(app))
      val targets = List("scala","cuda") // Add other targets
      for(op <- graph.totalOps if op.isInstanceOf[OP_MultiLoop]) {
        targets foreach { t =>  if(!op.supportsTarget(Targets(t))) sys.error(t + " was unable to generate op " + op) }
      }
      Config.generateCUDA = generateCUDA
    }
    else { // Just stage test
      stageTest(app)
    }

    // Set runtime parameters for targets and execute runtime
    for(target <- deliteTestTargets) {
      for (num <- threads) {
        def runtimeConfig(numScala: Int = 1, numCpp: Int = 0, numCuda: Int = 0, numOpenCL: Int = 0) {
          ppl.delite.runtime.Config.numThreads = numScala
          ppl.delite.runtime.Config.numCpp = numCpp
          ppl.delite.runtime.Config.numCuda = numCuda
          ppl.delite.runtime.Config.numOpenCL = numOpenCL
          ppl.delite.runtime.Config.testMode = true
        }

        target match {
          case "scala" => runtimeConfig(numScala = num)
          case "cpp" => runtimeConfig(numCpp = num)
          case "cuda" => runtimeConfig(numScala = num, numCuda = 1) //scala or cpp (or both) for host?
          case "opencl" => runtimeConfig(numScala = num, numOpenCL = 1)
          case _ => assert(false)
        }
        val outStr = execTest(app, args, target, num)
        checkTest(app, outStr)
      }
    }
  }

  private def stageTest(app: DeliteTestRunner) = {
    println("STAGING...")
    val saveDeg = Config.degFilename
    val saveBuildDir = Config.buildDir
    val saveCacheSyms = Config.cacheSyms
    val generatedDir = Config.buildDir + java.io.File.separator + uniqueTestName(app)
    try {
      Config.degFilename = degName(app)
      Config.buildDir = generatedDir
      Config.cacheSyms = cacheSyms
      val screenOrVoid = if (verbose) System.out else new PrintStream(new ByteArrayOutputStream())
      Console.withOut(screenOrVoid) {
        app.main(Array())
        if (verboseDefs) app.globalDefs.foreach { d => //TR print all defs
          println(d)
          val s = d match { case app.TP(sym,_) => sym; case app.TTP(syms,_,_) => syms(0); case _ => sys.error("unknown Stm type: " + d) }
          val info = s.pos.drop(3).takeWhile(_.methodName != "main")
          println(info.map(s => s.fileName + ":" + s.line).distinct.mkString(","))
        }
      }
    } finally {
      // concurrent access check
      assert(Config.buildDir == generatedDir)
      Config.degFilename = saveDeg
      Config.buildDir = saveBuildDir
      Config.cacheSyms = saveCacheSyms
    }
  }

  private def execTest(app: DeliteTestRunner, args: Array[String], target: String, threads: Int) = {
    println("EXECUTING(" + target + ":" + threads + ")...")
    val name = "test.tmp"
    // Changed mkReport to directly write to a file instead of trying to capture the output stream here.
    // This is to make the C target testing work, because native C stdout is not captured by this.
    val screenOrVoid = if (verbose) System.out else new PrintStream(new ByteArrayOutputStream())
    Console.withOut(screenOrVoid) {
      println("test output for: " + app.toString)
      ppl.delite.runtime.Delite.embeddedMain(args, app.staticDataMap)
    }
    val reportFile = new File(name)
    val buf = new Array[Byte](reportFile.length.toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    reportFile.delete() //we don't want any other test to find this file
    new String(buf)
  }

  private def execTestExternal(args: Array[String]) = {
    println("EXECUTING...")
    //Delite.main(args)
    // need to use a different compiler version to build and run Delite
    var p: Process = null
    val output = new File("test.tmp")
    try {
      val javaProc = javaBin.toString
      val javaArgs = "-server -d64 -XX:+UseCompressedOops -XX:+DoEscapeAnalysis -Xmx16g -Ddelite.threads=" + threads(0) + " -cp " + runtimeClasses + ":" + scalaLibrary + ":" + scalaCompiler
      val cmd = Array(javaProc) ++ javaArgs.split(" ") ++ Array("ppl.delite.runtime.Delite") ++ args
      val pb = new ProcessBuilder(java.util.Arrays.asList(cmd: _*))
      p = pb.start()
    } catch {
      case e: Throwable => e.printStackTrace()
    }

    var exited = 3.14
    val errStream = p.getErrorStream
    val errStreamReader = new BufferedReader(new InputStreamReader(errStream))
    val outStream = p.getInputStream
    val outStreamReader = new BufferedReader(new InputStreamReader(outStream))
    val buf = new StringBuilder()

    // have to read streams and check process exit concurrently, or process can fill its buffers and hang
    while (exited == 3.14 || errStream.available() > 0 || outStream.available() > 0) {
      if (errStream.available() > 0) {
        System.err.println(errStreamReader.readLine())
      }

      if (outStream.available() > 0) {
        var line = outStreamReader.readLine()
        buf append line
        buf append System.getProperty("line.separator")
        if (verbose) System.out.println(line)
      }

      exited =
        try { p.exitValue() }
        catch { case e: Throwable => 3.14 }
    }

    buf.toString
  }

  private def checkTest(app: DeliteTestRunner, outStr: String) {
    println("CHECKING...")
    val resultStr = outStr substring (outStr.indexOf(MAGICDELIMETER) + MAGICDELIMETER.length, outStr.lastIndexOf(MAGICDELIMETER))
    val results = resultStr split ","
    for (i <- 0 until results.length) {
      if (verbose) print("  condition " + i + ": ")
      val passed = results(i).toLowerCase() == "true"
      if (verbose)
        if (passed) println("PASSED") else println("FAILED")
      assert(passed)
    }
  }
}

trait DeliteTestStandaloneRunner extends DeliteTestModule with DeliteTestOpsExp with DeliteApplication {
  def collect(s: Rep[Boolean]) = delite_test_println(s)
  def mkReport() = { }
}

trait DeliteTestRunner extends DeliteTestModule with DeliteTestConfig with DeliteTestOpsExp with DeliteApplication {
  var resultBuffer: ArrayBuffer[Boolean] = _

  def collector: Rep[ArrayBuffer[Boolean]] = staticData(resultBuffer)
  def collect(s: Rep[Boolean]) = delite_test_append(collector, s)

  def mkReport(): Rep[Unit] = {
    val out = delite_test_bw_new(delite_test_fw_new(unit("test.tmp")))
    val s1 = delite_test_strconcat(unit(MAGICDELIMETER), (delite_test_mkstring(collector, unit(","))))
    val s2 = delite_test_strconcat(s1, unit(MAGICDELIMETER))
    delite_test_bw_write(out, s2)
    delite_test_bw_close(out)
  }
  
}

trait DeliteTestModule extends Base {
  def collect(s: Rep[Boolean]): Rep[Unit]
  def mkReport(): Rep[Unit]
}

/*
 * These are the internal nodes required by DeliteTestModule. We implement them here to avoid any
 * dependence on LMS common ops, which would get imported into the application under test and
 * could cause collisions.
 */
trait DeliteTestOps extends Base {
  def delite_test_mkstring[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit pos: SourceContext): Rep[String]
  def delite_test_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def delite_test_strconcat(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext): Rep[String]
  def delite_test_fw_new(s: Rep[String])(implicit pos: SourceContext): Rep[FileWriter]
  def delite_test_bw_new(f: Rep[FileWriter])(implicit pos: SourceContext): Rep[BufferedWriter]
  def delite_test_bw_write(b: Rep[BufferedWriter], s: Rep[String])(implicit pos: SourceContext): Rep[Unit]
  def delite_test_bw_close(b: Rep[BufferedWriter])(implicit pos: SourceContext): Rep[Unit]
  def delite_test_println(s: Rep[Any])(implicit pos: SourceContext): Rep[Unit]
}

trait DeliteTestOpsExp extends DeliteTestOps with EffectExp {
  case class DeliteTestMkString[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class DeliteTestAppend[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]
  case class DeliteTestStrConcat(s: Exp[String], o: Exp[String]) extends Def[String]
  case class DeliteTestFwNew(s: Exp[String]) extends Def[FileWriter]
  case class DeliteTestBwNew(f: Exp[FileWriter]) extends Def[BufferedWriter]
  case class DeliteTestBwWrite(b: Exp[BufferedWriter], s: Exp[String]) extends Def[Unit]
  case class DeliteTestBwClose(b: Exp[BufferedWriter]) extends Def[Unit]
  case class DeliteTestPrintLn(s: Exp[Any]) extends Def[Unit]

  def delite_test_mkstring[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit pos: SourceContext) = reflectEffect(DeliteTestMkString(l, sep), Simple() andAlso Read(List(l.asInstanceOf[Sym[_]])))
  def delite_test_append[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit pos: SourceContext) = reflectEffect(DeliteTestAppend(l, e), Simple() andAlso Write(List(l.asInstanceOf[Sym[_]])))
  def delite_test_strconcat(s: Exp[String], o: Exp[String])(implicit pos: SourceContext) = reflectEffect(DeliteTestStrConcat(s,o))
  def delite_test_fw_new(s: Exp[String])(implicit pos: SourceContext) = reflectEffect(DeliteTestFwNew(s))
  def delite_test_bw_new(f: Exp[FileWriter])(implicit pos: SourceContext) = reflectEffect(DeliteTestBwNew(f))
  def delite_test_bw_write(b: Exp[BufferedWriter], s: Exp[String])(implicit pos: SourceContext) = reflectEffect(DeliteTestBwWrite(b,s))
  def delite_test_bw_close(b: Exp[BufferedWriter])(implicit pos: SourceContext) = reflectEffect(DeliteTestBwClose(b))
  def delite_test_println(s: Rep[Any])(implicit pos: SourceContext): Rep[Unit] = reflectEffect(DeliteTestPrintLn(s))

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case DeliteTestMkString(l,r) => DeliteTestMkString(f(l),f(r))
    case DeliteTestAppend(l,r) => DeliteTestAppend(f(l),f(r))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(DeliteTestStrConcat(a,b), u, es) => reflectMirrored(Reflect(DeliteTestStrConcat(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestFwNew(s), u, es) => reflectMirrored(Reflect(DeliteTestFwNew(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestBwNew(x), u, es) => reflectMirrored(Reflect(DeliteTestBwNew(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestBwWrite(b,s), u, es) => reflectMirrored(Reflect(DeliteTestBwWrite(f(b),f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestBwClose(b), u, es) => reflectMirrored(Reflect(DeliteTestBwClose(f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestPrintLn(s), u, es) => reflectMirrored(Reflect(DeliteTestPrintLn(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

// how do we add our code generators? right now we expect a single codegen package being supplied by the dsl.
// the workaround for now is that the dsl under test must include ScalaGenDeliteTest in its code gen package
trait ScalaGenDeliteTest extends ScalaGenBase {
  val IR: DeliteTestOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteTestMkString(l, sep) => emitValDef(sym, quote(l) + ".mkString(" + quote(sep) + ")")
    case DeliteTestAppend(l, e) => emitValDef(sym, quote(l) + " += " + quote(e))
    case DeliteTestStrConcat(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case DeliteTestBwNew(f) => emitValDef(sym, "new java.io.BufferedWriter(" + quote(f) + ")")
    case DeliteTestFwNew(s) => emitValDef(sym, "new java.io.FileWriter(" + quote(s) + ")")
    case DeliteTestBwWrite(b,s) => emitValDef(sym, quote(b) + ".write(" + quote(s) + ")")
    case DeliteTestBwClose(b) => emitValDef(sym, quote(b) + ".close()")
    case DeliteTestPrintLn(s) => emitValDef(sym, "println(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

