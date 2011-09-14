package ppl.delite.framework.analysis

import scala.virtualization.lms.internal._

import java.io.{PrintWriter, ByteArrayOutputStream}

class MockStream extends ByteArrayOutputStream { 
   override def flush() {}
   override def close() {}
   def print(line: String) {}
}

trait TraversalAnalysis extends GenericFatCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
  implicit val stream: PrintWriter = new PrintWriter(new MockStream())
  val className: String
  
  def init(args: Array[String])
  def traverseNode(sym: Sym[Any], a: Def[Any]) = emitNode(sym, a)(stream)
  def traverseBlock(b: Exp[Any]) = emitBlock(b)(stream)
  def traverse[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]) : Unit = emitSource(f, className, stream)
}