package ppl.dsl.deliszt.datastruct.scala

import scala.collection.mutable.{Set => MSet, Map}

class ReadWriteSet {
  val read = MSet[MeshObj]()
  val write = MSet[MeshObj]()
}

object StencilCollector {
  type StencilMap = Map[MeshObj,ReadWriteSet]
  val forMap = Map[String,StencilMap]()
}