package ppl.delite.framework.transform

import ppl.delite.framework.ops.{DeliteOpsExp,BaseDeliteOpsTraversalFat}

import scala.virtualization.lms.common._

trait ForwardPassTransformer extends WorklistTransformer /*with BaseDeliteOpsTraversalFat*/ {
  val IR: DeliteOpsExp
  import IR._
  
  override def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { // no transitive subst
    case Some(y) => y.asInstanceOf[Exp[A]] case _ => x 
  }

  // single pass forward transform  
  var runs = 0
  
  override def isDone = if (runs > 0) true else false
  
  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    runs += 1
    super.runOnce(s)
  }  
}