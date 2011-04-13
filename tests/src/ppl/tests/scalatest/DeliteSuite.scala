package ppl.tests.scalatest

import org.scalatest._
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common._
import scala.collection.mutable.ArrayBuffer
import java.io.{Console => _, _}

trait DeliteTestConfig {
  // something arbitrary that we should never see in any test's output
  val MAGICDELIMETER = "!~x02$758209"
}

trait DeliteSuite extends Suite with DeliteTestConfig {
  // test parameters
  val verbose = System.getProperty("tests.verbose", "false").toBoolean
  val javaHome = System.getProperty("java.home", "") replaceAll ("/", "//")
  val scalaHome = System.getProperty("scala.home", "") replaceAll ("/", "//")
  val runtimeClasses = System.getProperty("runtime.classes", "") replaceAll ("/", "//")

  def validateParameters() {
    if (javaHome == "") throw new TestFailedException("java.home must be specified", 3)
    else if (scalaHome == "") throw new TestFailedException("scala.home must be specified", 3)
    else if (runtimeClasses == "") throw new TestFailedException("runtime.classes must be specified", 3)
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
    System.setProperty("delite.deg.filename", degName)
    val screenOrVoid = if (verbose) System.out else new PrintStream(new ByteArrayOutputStream())
    Console.withOut(screenOrVoid) {
      app.main(Array())
    }
  }

  private def execTest(args: Array[String]) = {
    println("EXECUTING...")
    //Delite.main(args)
    // need to use a different compiler version to build and run Delite
    var p: Process = null
    val output = new File("test.tmp")
    try{
      val javaProc = javaHome + "bin/java"
      val javaArgs = "-server -d64 -XX:+UseCompressedOops -XX:+DoEscapeAnalysis -Xmx16g -cp " + runtimeClasses + ":" + scalaHome + "lib/scala-library.jar:" + scalaHome + "lib/scala-compiler.jar"
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
      print("  condition " + i + ": ")
      val passed = results(i).toLowerCase() == "true"
      if (passed) println("PASSED") else println("FAILED")
      assert(passed)
    }
  }
}

// how do we add our code generators? right now we expect a single codegen package being supplied by the dsl.
// the workaround for now is that the dsl under test must include ArrayBuffer in its code gen
trait DeliteTestRunner extends DeliteTestModule with DeliteApplication
  with MiscOpsExp with ArrayBufferOpsExp with StringOpsExp

// it is not ideal that the test module imports these things into the application under test
trait DeliteTestModule extends DeliteTestConfig
  with MiscOps with ArrayBufferOps with StringOps {

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