package ppl.delite.runtime.graph.targets

import ppl.delite.runtime.Config

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


  def isPrimitiveType(tpe: String): Boolean = isPrimitiveType(Targets.Scala, tpe)

  def isPrimitiveType(target: Value, tpe: String): Boolean = target match {
    case Scala => 
      tpe match {
        case "Boolean" | "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" | "Unit" => true
        case _ => false 
      }
    case Cpp =>
      tpe match {
        case "bool" | "int8_t" | "uint16_t" | "int16_t" | "int32_t" | "int64_t" | "float" | "double" | "void" => true
        case _ => false 
      }
    case _ => false
  }

  def getClassType(scalaType: String): Class[_] = scalaType match {
    case "Unit" => java.lang.Void.TYPE
    case "Int" => java.lang.Integer.TYPE
    case "Long" => java.lang.Long.TYPE
    case "Float" => java.lang.Float.TYPE
    case "Double" => java.lang.Double.TYPE
    case "Boolean" => java.lang.Boolean.TYPE
    case "Short" => java.lang.Short.TYPE
    case "Char" => java.lang.Character.TYPE
    case "Byte" => java.lang.Byte.TYPE
    case _ => Class.forName(scalaType)
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
  
  def resourceIDs(target: Value): Seq[Int] = {
    target match {
      case Targets.Scala => 0 until Config.numThreads
      case Targets.Cpp => Config.numThreads until Config.numThreads+Config.numCpp
      case Targets.Cuda => Config.numThreads+Config.numCpp until Config.numThreads+Config.numCpp+Config.numCuda
      case Targets.OpenCL => Config.numThreads+Config.numCpp+Config.numCuda until Config.numThreads+Config.numCpp+Config.numCuda+Config.numOpenCL
      case _ => throw new RuntimeException("Cannot find a resource IDs for target " + target)
    }
  }

  def getByLocation(location: Int): Value = {
    if (location < Config.numThreads) Scala
    else if (location < Config.numThreads+Config.numCpp) Cpp
    else if (location < Config.numThreads+Config.numCpp+Config.numCuda) Cuda
    else if (location < Config.numThreads+Config.numCpp+Config.numCuda+Config.numOpenCL) OpenCL
    else throw new RuntimeException("requested location " + location + " is not in the range of available resources.")
  }

  // get the relative location within a resource type
  def getRelativeLocation(location: Int): Int = {
    getByLocation(location) match {
      case Targets.Scala => location
      case Targets.Cpp => location - Config.numThreads
      case Targets.Cuda => location - Config.numThreads - Config.numCpp
      case Targets.OpenCL => location - Config.numThreads - Config.numCpp - Config.numCuda
      case t => throw new RuntimeException("unkown resource type (" + t + ") for location " + location)
    }

  }

}
