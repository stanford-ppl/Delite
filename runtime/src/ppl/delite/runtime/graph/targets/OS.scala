package ppl.delite.runtime.graph.targets

object OS {

  sealed abstract class SupportedOS
  case object Linux extends SupportedOS
  case object Windows extends SupportedOS
  case object Mac extends SupportedOS

  private val theOS = {
    val os = System.getProperty("os.name")
    if (os.contains("Linux")) Linux
    else if (os.contains("Windows")) Windows
    else if (os.contains("Mac")) Mac
    else sys.error("OS " + os + " is not currently supported")
  }

  def currentOS: String = theOS.toString

  def libExt: String = theOS match {
    case Linux => "so"
    case Windows => "dll"
    case Mac => "so"
  }

}
