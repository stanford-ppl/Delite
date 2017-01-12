// See LICENSE.txt for license details.

package utils

import scala.collection.mutable.ArrayBuffer
import scala.util.Properties.envOrElse

object AppRunner {
  def apply(templateMap: Map[String, (String,String,Int) => Boolean], args: Array[String]): Unit = {
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
    if(args.isEmpty) {
      println(s"Error: Must specify module to test!")
      System.exit(1)
    }
    val testName = args(0)
    val inputArgs = args.drop(1).mkString(" ")
    val TIMEOUT = 1000000

    val errors = new ArrayBuffer[String]

    val message = templateMap.get(testName) match {
      case Some(test) =>
        println(s"Starting template $testName")
        try {
          if(test(backendName, inputArgs, TIMEOUT)) {
            "Success"
          }
          else {
            s"App $testName: test error occurred"
          }
        }
        catch {
          case exception: Exception =>
            exception.printStackTrace()
            s"App $testName: exception ${exception.getMessage}"
          case t : Throwable =>
            s"App $testName: throwable ${t.getMessage}"
        }
      case _ =>
        s"Bad template name: $testName"
    }

    if(message == "Success") {
      println(s"Module passed!")
    } else {
      println("=" * 80)
      println(s"Error: ${message}")
      println("=" * 80)
      System.exit(1)
    }
  }
}
