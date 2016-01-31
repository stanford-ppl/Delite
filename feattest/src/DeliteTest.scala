package feattest
import java.io._
import org.scalatest._
import scala.virtualization.lms.common.{Base,EffectExp,ScalaGenBase}
import scala.reflect.SourceContext
import scala.collection.mutable.{ArrayBuffer, SynchronizedBuffer}

import ppl.delite.framework.Config

trait TestConfig {
  val MAGICDELIMETER = "!~x02$758209"

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
  val deliteTestTargets = props.getProperty("tests.targets", "scala").split(",")
  val useBlas = props.getProperty("tests.extern.blas", "false").toBoolean
}

trait TestCompilation extends TestConfig {
  def uniqueTestName(app: DeliteDSLCompiler): String = app.getClass.getName.replaceAll("\\$", "")
  def degName(app: DeliteDSLCompiler): String = uniqueTestName(app) + ".deg"

  private def stageApp(app: DeliteDSLCompiler) {
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

  private def execApp(app: DSLTest, args: Array[String], target: String, threads: Int) = {
    println("EXECUTING(" + target + ":" + threads + ")...")
    println("Test output for: " + app.toString)
    ppl.delite.runtime.Delite.embeddedMain(args, app.staticDataMap)

    val name = "test.tmp"
    val reportFile = new File(name)
    val buf = new Array[Byte](reportFile.length.toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    reportFile.delete() //we don't want any other test to find this file
    new String(buf)
  }

  private def checkTest(app: DSLTest, outStr: String) {
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

  def runTest(app: DSLTest) {
    val args = Array(degName(app))
    app.resultBuffer = new ArrayBuffer[Boolean] with SynchronizedBuffer[Boolean]

    stageApp(app)

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
        val outStr = execApp(app, args, target, num)
        checkTest(app, outStr)
      }
    }
  }
}


trait DeliteTestOps extends Base {
  def delite_test_mkstring[A:Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit pos: SourceContext): Rep[String]
  def delite_test_append[A:Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def delite_test_fw_new(s: Rep[String])(implicit pos: SourceContext): Rep[FileWriter]
  def delite_test_bw_new(f: Rep[FileWriter])(implicit pos: SourceContext): Rep[BufferedWriter]
  def delite_test_bw_write(b: Rep[BufferedWriter], s: Rep[String])(implicit pos: SourceContext): Rep[Unit]
  def delite_test_bw_close(b: Rep[BufferedWriter])(implicit pos: SourceContext): Rep[Unit]
}

trait DeliteTestOpsExp extends DeliteTestOps with EffectExp {
  case class DeliteTestMkString[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class DeliteTestAppend[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]
  case class DeliteTestFwNew(s: Exp[String]) extends Def[FileWriter]
  case class DeliteTestBwNew(f: Exp[FileWriter]) extends Def[BufferedWriter]
  case class DeliteTestBwWrite(b: Exp[BufferedWriter], s: Exp[String]) extends Def[Unit]
  case class DeliteTestBwClose(b: Exp[BufferedWriter]) extends Def[Unit]

  def delite_test_mkstring[A:Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit pos: SourceContext) = reflectEffect(DeliteTestMkString(l, sep), Simple() andAlso Read(List(l.asInstanceOf[Sym[_]])))
  def delite_test_append[A:Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit pos: SourceContext) = reflectEffect(DeliteTestAppend(l, e), Simple() andAlso Write(List(l.asInstanceOf[Sym[_]])))
  def delite_test_fw_new(s: Exp[String])(implicit pos: SourceContext) = reflectEffect(DeliteTestFwNew(s))
  def delite_test_bw_new(f: Exp[FileWriter])(implicit pos: SourceContext) = reflectEffect(DeliteTestBwNew(f))
  def delite_test_bw_write(b: Exp[BufferedWriter], s: Exp[String])(implicit pos: SourceContext) = reflectEffect(DeliteTestBwWrite(b,s))
  def delite_test_bw_close(b: Exp[BufferedWriter])(implicit pos: SourceContext) = reflectEffect(DeliteTestBwClose(b))

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case DeliteTestMkString(l,r) => DeliteTestMkString(f(l),f(r))
    case DeliteTestAppend(l,r) => DeliteTestAppend(f(l),f(r))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(DeliteTestFwNew(s), u, es) => reflectMirrored(Reflect(DeliteTestFwNew(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestBwNew(x), u, es) => reflectMirrored(Reflect(DeliteTestBwNew(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestBwWrite(b,s), u, es) => reflectMirrored(Reflect(DeliteTestBwWrite(f(b),f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DeliteTestBwClose(b), u, es) => reflectMirrored(Reflect(DeliteTestBwClose(f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}


trait ScalaGenDeliteTest extends ScalaGenBase {
  val IR: DeliteTestOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteTestMkString(l, sep) => emitValDef(sym, quote(l) + ".mkString(" + quote(sep) + ")")
    case DeliteTestAppend(l, e) => emitValDef(sym, quote(l) + " += " + quote(e))
    case DeliteTestBwNew(f) => emitValDef(sym, "new java.io.BufferedWriter(" + quote(f) + ")")
    case DeliteTestFwNew(s) => emitValDef(sym, "new java.io.FileWriter(" + quote(s) + ")")
    case DeliteTestBwWrite(b,s) => emitValDef(sym, quote(b) + ".write(" + quote(s) + ")")
    case DeliteTestBwClose(b) => emitValDef(sym, quote(b) + ".close()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait DSLTest extends TestConfig with DeliteDSLCompiler with DeliteTestOpsExp {
  var resultBuffer: ArrayBuffer[Boolean] = _

  def collector: Rep[ArrayBuffer[Boolean]] = staticData(resultBuffer)
  def collect(s: Rep[Boolean]) = delite_test_append(collector, s)

  def mkReport(): Rep[Unit] = {
    val out = delite_test_bw_new(delite_test_fw_new(unit("test.tmp")))
    val s1 = unit(MAGICDELIMETER) + (delite_test_mkstring(collector, unit(",")))
    val s2 = s1 + unit(MAGICDELIMETER)
    delite_test_bw_write(out, s2)
    delite_test_bw_close(out)
  }

  def test(): Unit
  def main() {
    test()
    mkReport
  }
}

trait DSLTestBenchmarks extends Suite with TestCompilation
