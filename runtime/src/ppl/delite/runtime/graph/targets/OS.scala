package ppl.delite.runtime.graph.targets

object OS {

  sealed abstract class SupportedOS
  abstract class Unix extends SupportedOS
  case object Linux extends Unix
  case object Mac extends Unix
  case object Solaris extends Unix
  case object Windows extends SupportedOS

  private val theOS = {
    val os = System.getProperty("os.name")
    if (os.contains("Linux")) Linux
    else if (os.contains("Windows")) Windows
    else if (os.contains("Mac")) Mac
    else if (os.contains("SunOS")) Solaris
    else sys.error("OS " + os + " is not currently supported")
  }

  def currentOS: String = theOS.toString

  def libExt: String = theOS match {
    case u:Unix => "so"
    case Windows => "dll"
  }

  def objExt: String = theOS match {
    case u:Unix => "o"
    case Windows => "obj"
  }

}
