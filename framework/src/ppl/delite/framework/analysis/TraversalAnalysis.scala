package ppl.delite.framework.analysis

import java.io.{PrintWriter, ByteArrayOutputStream}
import scala.virtualization.lms.internal._
import ppl.delite.framework.DeliteApplication

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
  var _result: Option[Any] = None
  
  def traverseNode(sym: Sym[Any], a: Def[Any]) = emitNode(sym, a)(stream)
  def traverseBlock(b: Block[Any]) = emitBlock(b)(stream)
  def traverse[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]) = { emitSource(f, className, stream); result }
  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter) {}
  def result: Option[Any] = _result
  
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val x = fresh[A]
    val y = reifyEffects(f(x))

    traverseBlock(y)
    Nil
  }
}