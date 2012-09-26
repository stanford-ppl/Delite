package ppl.delite.runtime.graph.targets

sealed abstract class Resource {
  //val location: Int
}

case object Scala extends Resource
case object Cpp extends Resource
case object Cuda extends Resource
case object OpenCL extends Resource
