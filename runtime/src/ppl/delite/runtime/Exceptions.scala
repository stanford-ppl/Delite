package ppl.delite.runtime

import ppl.delite.runtime.profiler.Profiler
import scala.collection.mutable.HashMap

class DeliteException(message: String, cause: Throwable) extends RuntimeException(message, cause, true, false)

object Exceptions {

  //TODO: using the 'original' line number and a full dump of all SourceContext from the compiler we could provide exception locations for all symbols rather than just DEG symbols
  val sourceInfo = new HashMap[String, (String, Int, String)]()

  def translate(original: Throwable): Throwable = {
    def stripKernel(name: String) = name.stripPrefix("generated.scala.kernel_").takeWhile(_ != '$')
    var translated = worker(original)
    for (elem <- original.getStackTrace) {
      sourceInfo.get(stripKernel(elem.getClassName)) match {
        case Some((fileName, line, opName)) => 
          val message = s"Error within $opName: $fileName:$line"
          if (translated.getMessage != message) translated = new DeliteException(message, translated)
        case _ => //keep looking
      }
    }
    translated
  }

  def worker(cause: Throwable) = new DeliteException(s"Exception in thread ${Thread.currentThread.getName}", cause)

}
