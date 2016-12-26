// See LICENSE.txt for license details.

package utils

import scala.collection.mutable.ArrayBuffer
import scala.util.Properties.envOrElse

object TemplateRunner {
  def apply(templateMap: Map[String, String => Boolean], args: Array[String]): Unit = {
    // Choose the default backend based on what is available.
    lazy val firrtlTerpBackendAvailable: Boolean = {
      try {
        val cls = Class.forName("chisel3.iotesters.FirrtlTerpBackend")
        cls != null
      } catch {
        case e: Throwable => false
      }
    }
    lazy val defaultBackend = if (firrtlTerpBackendAvailable) {
      "firrtl"
    } else {
      ""
    }
    val backendName = envOrElse("TESTER_BACKENDS", defaultBackend).split(" ").head
    val problemsToRun = if(args.isEmpty || args.head == "all" ) {
      templateMap.keys.toSeq.sorted.toArray
    }
    else {
      args
    }

    var successful = 0
    val errors = new ArrayBuffer[String]
    for(testName <- problemsToRun) {
      templateMap.get(testName) match {
        case Some(test) =>
          println(s"Starting template $testName")
          try {
            if(test(backendName)) {
              successful += 1
            }
            else {
              errors += s"Template $testName: test error occurred"
            }
          }
          catch {
            case exception: Exception =>
              exception.printStackTrace()
              errors += s"Template $testName: exception ${exception.getMessage}"
            case t : Throwable =>
              errors += s"Template $testName: throwable ${t.getMessage}"
          }
        case _ =>
          errors += s"Bad template name: $testName"
      }

    }
    if(successful > 0) {
      println(s"Templates passing: $successful")
    }
    if(errors.nonEmpty) {
      println("=" * 80)
      println(s"Errors: ${errors.length}: in the following templates")
      println(errors.mkString("\n"))
      println("=" * 80)
      System.exit(1)
    }
  }
}
