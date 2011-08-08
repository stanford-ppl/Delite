package ppl.tests.scalatest

import org.scalatest._
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.Config
import scala.virtualization.lms.common._
import scala.collection.mutable.ArrayBuffer
import java.io.{File, Console => _, _}

trait DeliteTestConfig {
  // something arbitrary that we should never see in any test's output
  val MAGICDELIMETER = "!~x02$758209"

  val propFile = new File("delite.properties")
  if (!propFile.exists) throw new TestFailedException("could not find delite.properties", 3)
  val props = new java.util.Properties()
  props.load(new FileReader(propFile))

  // test parameters
  val verbose = props.getProperty("tests.verbose", "false").toBoolean
  val verboseDefs = props.getProperty("tests.verboseDefs", "false").toBoolean
  val threads = props.getProperty("tests.threads", "1")
  val javaHome = new File(props.getProperty("java.home", ""))
  val scalaHome = new File(props.getProperty("scala.vanilla.home", ""))
  val runtimeClasses = new File(props.getProperty("runtime.classes", ""))
}

trait DeliteSuite extends Suite with DeliteTestConfig {
  val javaBin = new File(javaHome, "bin/java")
  val scalaCompiler = new File(scalaHome, "lib/scala-compiler.jar")
  val scalaLibrary = new File(scalaHome, "lib/scala-library.jar")

  def validateParameters() {
    if (!javaHome.exists) throw new TestFailedException("java.home must be a valid path in delite.properties", 3)
    else if (!javaBin.exists) throw new TestFailedException("Could not find valid java installation in " + javaHome, 3)
    else if (!scalaHome.exists) throw new TestFailedException("scala.vanilla.home must be a valid path in delite.proeprties", 3)
    else if (!scalaCompiler.exists || !scalaLibrary.exists) throw new TestFailedException("Could not find valid scala installation in " + scalaHome, 3)
    else if (!runtimeClasses.exists) throw new TestFailedException("runtime.classes must be a valid path in delite.properties", 3)
  }

  def compileAndTest(app: DeliteTestRunner) {
    println("=================================================================================================")
    println("TEST: " + app.toString)
    println("=================================================================================================")

    validateParameters()
    val args = Array("test.deg")
    stageTest(app, args(0))
    val outStr = execTest(args)
    checkTest(outStr)
  }

  private def stageTest(app: DeliteTestRunner, degName: String) = {
    println("STAGING...")
    val save = Config.degFilename
    try {
      Config.degFilename = degName      
      val screenOrVoid = if (verbose) System.out else new PrintStream(new ByteArrayOutputStream())
      Console.withOut(screenOrVoid) {
        app.main(Array())
        if (verboseDefs) app.globalDefs.foreach { d => //TR print all defs
          println(d)
          val info = d.sym.sourceInfo.drop(3).takeWhile(_.getMethodName!="main")
          println(info.map(s=>s.getFileName+":"+s.getLineNumber).distinct.mkString(","))
        }
        //assert(!app.hadErrors) //TR should enable this check at some time ...
      }
    } finally {
      Config.degFilename = save
    }      
  }

  private def execTest(args: Array[String]) = {
    println("EXECUTING...")
    //Delite.main(args)
    // need to use a different compiler version to build and run Delite
    var p: Process = null
    val output = new File("test.tmp")
    try{
      val javaProc = javaBin.toString
      val javaArgs = "-server -d64 -XX:+UseCompressedOops -XX:+DoEscapeAnalysis -Xmx16g -Ddelite.threads=" + threads + " -cp " + runtimeClasses + ":" + scalaLibrary + ":" + scalaCompiler
      val cmd = Array(javaProc) ++ javaArgs.split(" ") ++ Array("ppl.delite.runtime.Delite") ++ args
      val pb = new ProcessBuilder(java.util.Arrays.asList(cmd: _*))
      p = pb.start()
    } catch {
      case e => e.printStackTrace()
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
        catch { case e => 3.14 }
    }

    buf.toString
  }

  private def checkTest(outStr: String) {
    println("CHECKING...")
    val resultStr = outStr substring (outStr.indexOf(MAGICDELIMETER) + MAGICDELIMETER.length, outStr.lastIndexOf(MAGICDELIMETER))
    val results = resultStr split ","
    for (i <- 0 until results.length) {
      val passed = results(i).toLowerCase() == "true"
      if (verbose) {
        print("  condition " + i + ": ")
        if (passed) println("PASSED") else println("FAILED")
      }
      assert(passed)
    }
  }
}

// how do we add our code generators? right now we expect a single codegen package being supplied by the dsl.
// the workaround for now is that the dsl under test must include ArrayBuffer in its code gen
trait DeliteTestRunner extends DeliteTestModule with DeliteApplication
  with MiscOpsExp with SynchronizedArrayBufferOpsExp with StringOpsExp

// it is not ideal that the test module imports these things into the application under test
trait DeliteTestModule extends DeliteTestConfig
  with MiscOps with SynchronizedArrayBufferOps with StringOps {

  var args: Rep[Array[String]]
  def main(): Unit

  //var collector: Rep[ArrayBuffer[Boolean]] = null.asInstanceOf[Rep[ArrayBuffer[Boolean]]]
//  lazy val ctest = ArrayBuffer[Boolean]()
//
//  def collect(s: Rep[Boolean]) {
//    // interpreted if/then/else!
//    //if (collector == null) collector = ArrayBuffer[Boolean]()
//    ctest append s
//  }
//
//  def mkReport() {
//    println(MAGICDELIMETER + (ctest mkString unit(",")) + MAGICDELIMETER)
//  }

  def collect(s: Rep[Boolean])(implicit c: Rep[ArrayBuffer[Boolean]]) { c += s }

  def mkReport(implicit c: Rep[ArrayBuffer[Boolean]]): Rep[Unit] = {
    println(MAGICDELIMETER + (c mkString unit(",")) + MAGICDELIMETER)
  }


  /*
  def main() = {
    test()
    out
  }

  def test(): Rep[Unit]
  */
}