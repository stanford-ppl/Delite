package ppl.delite.framework.ops

import scala.virtualization.lms.common._

trait RefinementOps extends Base {
  class Refinement extends Row[Rep]
  
  /*
  class ApplyDynamicOps {
    def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
  }
  
  implicit def applyDynamicOps[T <: Refinement](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps
  */
}