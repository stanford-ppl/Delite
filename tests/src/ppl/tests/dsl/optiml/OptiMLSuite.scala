package ppl.tests.dsl.optiml

import org.scalatest._
import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.dsl.optiml.datastruct.scala.Vector
import java.io.{PrintStream, ByteArrayOutputStream, FileOutputStream, FileDescriptor, BufferedReader, InputStream, InputStreamReader}

trait OptiMLTestConfig {
  // something arbitrary that we should never see in any test's output
  val MAGICDELIMETER = "!~x02$758209"
}

trait OptiMLSuite extends Suite with OptiMLTestConfig {
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

  def compileAndTest(app: OptiMLApplicationRunner) {
    println("=================================================================================================")
    println("TEST: " + app.toString)
    println("=================================================================================================")

    validateParameters()
    val args = Array("test.deg")
    stageTest(app, args(0))
    val outStr = execTest(args)

    val resultStr = outStr substring (outStr.indexOf(MAGICDELIMETER) + MAGICDELIMETER.length, outStr.lastIndexOf(MAGICDELIMETER))
    val results = resultStr split ","
    for (i <- 0 until results.length) {
      val passed = results(i).toLowerCase() == "true"
      print("  condition " + i + ": ")
      if (passed) print("PASSED") else print("FAILED")
      println()
      assert(passed)
    }
  }

  private def stageTest(app: OptiMLApplicationRunner, degName: String) = {
    print("STAGING...")
    System.setProperty("delite.deg.filename", degName)
    val void = if (verbose) System.out else new PrintStream(new ByteArrayOutputStream())
    Console.withOut(void) {
      app.main(Array())
    }
    print("DONE"); println()
  }

  private def execTest(args: Array[String]) = {
    print("EXECUTING...")
    //Delite.main(args)
    // need to use a different compiler version to build and run Delite
    var p: Process = null
    try{
      val javaProc = javaHome + "bin/java"
      val javaArgs = "-server -d64 -XX:+UseCompressedOops -XX:+DoEscapeAnalysis -Xmx16g -cp " + runtimeClasses + ":" + scalaHome + "lib/scala-library.jar:" + scalaHome + "lib/scala-compiler.jar"
      val cmd = Array(javaProc) ++ javaArgs.split(" ") ++ Array("ppl.delite.runtime.Delite") ++ args
      val pb = new ProcessBuilder(java.util.Arrays.asList(cmd: _*))
      p = pb.start()
    } catch {
      case e => e.printStackTrace()
    }

    // print any errors
    val errStream = new BufferedReader(new InputStreamReader(p.getErrorStream()))
    var line = errStream.readLine()
    while (line  != null) {
      System.err.println(line);
      line = errStream.readLine()
    }
    print("DONE"); println()

    val outStream = new BufferedReader(new InputStreamReader(p.getInputStream()))
    val s = readerToStr(outStream)
    if (verbose) println(s)
    s
  }

  private def readerToStr(b: BufferedReader) = {
    val buf = new StringBuilder()
    var line = b.readLine()
    while (line != null) {
      buf append line
      buf append System.getProperty("line.separator")
      line = b.readLine()
    }
    buf.toString
  }
}

trait OptiMLTestModule extends OptiMLApplication with OptiMLTestConfig {
  // even having a lazy v defined here caused an NPE inside effects when the test initializes

  def collect(s: Rep[Boolean])(implicit v: Rep[Vector[Boolean]]) { v += s }

  def mkReport(implicit v: Rep[Vector[Boolean]]): Rep[Unit] = {
    println(MAGICDELIMETER + (v mkString ",") + MAGICDELIMETER)
  }

  /*
  def main() = {
    test()
    out
  }

  def test(): Rep[Unit]
  */
}