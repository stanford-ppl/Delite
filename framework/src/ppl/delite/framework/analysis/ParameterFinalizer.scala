package ppl.delite.framework.analysis

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.Traversal

// Used to finalize all parameters in the IR after all other tuning steps are complete
trait ParameterFinalizer extends Traversal {
  import IR._

  override val name = "Parameter Finalizer"
  override val recurse = Always

  def params(x: Any): List[Param[Any]] = x match {
    case p: Param[_] => List(p.asInstanceOf[Param[Any]])
    case p: Product => p.productIterator.toList.flatMap(params(_))
    case _ => Nil
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    params(rhs).foreach(_.fix)
    super.traverse(lhs, rhs)
  }
}
