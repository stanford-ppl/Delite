package ppl.delite.runtime.graph.targets

object OS extends Enumeration {

  val Win = Value("win")
  val Linux = Value("linux")
  val Mac = Value("mac")

  private val osName = System.getProperty("os.name")

  def currentOS:OS.Value = {
    if (osName.contains("Linux")) OS.Linux
    else if (osName.contains("Windows")) OS.Win
    else if (osName.contains("Mac")) OS.Mac
    else error("OS " + osName + " not currently supported")
  }

  def libExt:String = currentOS match {
    case OS.Linux => "so"
    case OS.Win => "dll"
    case OS.Mac => "dll"
    case _ => error("OS " + currentOS + " not currently supported")
  }

  def jniMD:String = currentOS match {
    case OS.Linux => "linux"
    case OS.Win => "win32"
    case OS.Mac => "mac"    //TODO: Check if correct
    case _ => error("OS " + currentOS + " not currently supported")
  }
}