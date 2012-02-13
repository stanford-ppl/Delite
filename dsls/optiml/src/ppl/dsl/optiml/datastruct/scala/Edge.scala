package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class MessageEdgeImpl(val graph: Graph[MessageVertex,MessageEdge], inData: MessageData, outData: MessageData, v1: MessageVertex, v2: MessageVertex) extends MessageEdge {
  def in(v: MessageVertex) = if(v == v1) inData else outData
  def out(v: MessageVertex) = if(v == v1) outData else inData
  def target(source: MessageVertex) = if (source == v1) v2 else v1
}