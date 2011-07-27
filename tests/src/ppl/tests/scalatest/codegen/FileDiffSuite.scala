package ppl.tests.scalatest.codegen

import java.io.{PrintStream,File,FileInputStream,FileOutputStream,ByteArrayOutputStream}
import org.scalatest._


trait FileDiffSuite extends Suite {
  
  def withOutFile(name: String)(func: => Unit): Unit = {
    val file = new File(name)
    file.getParentFile.mkdirs()
    withOutput(new PrintStream(new FileOutputStream(file)))(func)
  }
  def captureOutput(func: => Unit): String = {
    val bstream = new ByteArrayOutputStream
    withOutput(new PrintStream(bstream))(func)
    bstream.toString
  }
  def withOutput(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }
  
  def readFile(name: String): String = {
    val buf = new Array[Byte](new File(name).length().toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    new String(buf)
  }
  def assertFileEqualsCheck(name: String): Unit = {
    assert(readFile(name) == readFile(name+".check"), "File contents do not match: "+name) // TODO: diff output
    new File(name) delete ()
  }
  def assertFileEqualsCheckModulo(name: String)(s: String, r: String): Unit = {
    assert(readFile(name).replaceAll(s,r) == readFile(name+".check").replaceAll(s,r), "File contents do not match: "+name) // TODO: diff output
    //new File(name) delete ()
  }

  def summarizeFile(name: String): Unit = {
    val str = readFile(name)
    val lines = str.split("\n").filter(line => line.contains("{") || line.contains("}")) //.map(line => line.replace)
    var indent = 0
    var opened = false
    for (l <- lines) {
      if (opened) {
        opened = false
        if (l.contains("}")) {
          print(" " + l)
        } else {
          indent += 1
          println()
          print("  "*indent + l)
        }
      } else {
        if (l.contains("}")) indent -= 1
        print("  "*indent + l)
      }
      if (l.contains("{")) { opened = true } else println()
    }
  }

}
