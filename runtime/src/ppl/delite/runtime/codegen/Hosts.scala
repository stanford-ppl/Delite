package ppl.delite.runtime.codegen.hosts

object Hosts extends Enumeration {

  val Scala = Value("scala")
  val Cpp = Value("cpp")

  def host(s: String): Value = s.toLowerCase() match {
    case "scala" => Scala
    case "cpp" => Cpp
    case _ => throw new IllegalArgumentException("unsupported host: " + s)
  }
}