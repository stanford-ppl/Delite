package ppl.delite.runtime.graph.targets

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 4:16:29 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Targets extends Enumeration {
  val Scala = Value("scala")
  val Cuda = Value("cuda")
  val OpenCL = Value("opencl")
  val Cpp = Value("cpp")

  val GPU = List(Cuda, OpenCL)

  /**
   * Return the value of a target
   */
  def apply(s: String): Value = s.toLowerCase() match {
    case "scala" => Scala
    case "cuda" => Cuda
    case "opencl" => OpenCL
    case "cpp" => Cpp
    case _ => throw new IllegalArgumentException("unsupported target: " + s)
  }

  /**
   * Create a Unit-type Map for the set of targets included in the input Map
   */
  def unitTypes(id: String, targets: Map[Value,Map[String,String]]): Map[Value,Map[String,String]] = {
    var unitMap = Map[Value,Map[String,String]]()
    for (target <- targets.keys) {
      unitMap += target -> Map(id -> unitType(target), "functionReturn" -> unitType(target))
    }
    unitMap
  }

  /**
   * Creates a Unit-type Map for all targets
   */
  def unitTypes(id: String): Map[Value,Map[String,String]] = {
    var unitMap = Map[Value,Map[String,String]]()
    for (target <- values) {
      unitMap += target -> Map(id -> unitType(target), "functionReturn" -> unitType(target))
    }
    unitMap
  }

  /**
   *  Returns the Unit-type for the specified target as a String
   */
  def unitType(target: Value): String = {
    target match {
      case Scala => "Unit"
      case Cuda | OpenCL | Cpp => "void"
    }
  }


  def isPrimitiveType(scalaType: String): Boolean = scalaType match { //should include Target type in determination, but for now everyone agrees
    case "Boolean" | "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" | "Unit" => true
    case _ => false
  }

  def getHostTarget(target: Value): Targets.Value = {
    target match {
      case Targets.Scala => Targets.Scala
      case Targets.Cpp => Targets.Cpp
      case Targets.Cuda => Targets.Cpp
      case Targets.OpenCL => Targets.Cpp
      case _ => throw new IllegalArgumentException("Cannot find a host target for target " + target)
    }
  }

  def getHostTarget(target: String): Targets.Value = getHostTarget(Targets(target))
  
  /*
  def getCompiler(target: Value): CodeCache = {
    target match {
      case Targets.Scala => ScalaCompile
      case Targets.Cuda => CudaCompile
      case Targets.OpenCL => OpenCLCompile
      case Targets.Cpp => CppCompile
      case _ => throw new IllegalArgumentException("Cannot find a compiler for target " + target)
    }
  }
  */

}
