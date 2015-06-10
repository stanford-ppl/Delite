package ppl.delite.runtime

class DeliteException(message: String, cause: Throwable) extends RuntimeException(message, cause, true, false)

object Exceptions {

  //TODO: using the 'original' line number and a full dump of all SourceContext from the compiler we could provide exception locations for all symbols rather than just DEG symbols
  def translate(original: Throwable): Throwable = {
    def stripKernel(name: String) = name.stripPrefix("generated.scala.kernel_").takeWhile(_ != '$')
    var translated = worker(original)
    translated
  }

  def worker(cause: Throwable) = new DeliteException(s"Exception in thread ${Thread.currentThread.getName}", cause)

}
