package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._
import collection.mutable.ArrayBuffer

class Vertex[VD:Manifest,ED:Manifest](
  val _graph: Graph[VD,ED], 
  val _data: VD) {
  val _tasks = new ArrayBuffer[Vertex[VD,ED]]
}