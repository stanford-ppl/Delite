package ppl.delite.runtime.profiler

class DeliteApplicationException(message: String, cause: Throwable) extends RuntimeException(message, cause, true, false)

object Exceptions {

  //TODO: using the 'original' line number and a full dump of all SourceContext from the compiler we could provide exception locations for all symbols rather than just DEG symbols
  def translate(original: Throwable): Throwable = {
    def stripKernel(name: String) = name.stripPrefix("kernel_").stripSuffix("$")
    var translated = original
    for (elem <- original.getStackTrace) {
      Profiler.sourceInfo.get(stripKernel(elem.getClassName)) match {
        case Some((fileName, line, opName)) => 
          translated = new DeliteApplicationException(s"Error within $opName: $fileName:$line", translated)
        case _ => //keep looking
      }
    }
    translated
  }

}
