package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class Edge(
  val _graph: Graph,
  val _inData: MessageData, 
  val _outData: MessageData, 
  val _v1: Vertex, 
  val _v2: Vertex
)