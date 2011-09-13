import scala.virtualization.lms.internal.GenericFatCodegen

trait TraversalAnalysis extends GenericFatCodegen {
  implicit val stream : PrintWriter
  
  
  
  def traverseNode(a: Exp[_]) = emitNode(a)
  def traverseBlock(b: Exp[_]) = emitBlock(b)
  def traverse = emitSource
}