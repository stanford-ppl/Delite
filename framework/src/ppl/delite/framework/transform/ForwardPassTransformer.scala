package ppl.delite.framework.transform

import scala.virtualization.lms.common._

trait ForwardPassTransformer extends WorklistTransformer {
  val IR: LoopsFatExp with IfThenElseFatExp 
  import IR._
  
  // single pass forward transform  
  var runs = 0
  
  override def isDone = if (runs > 0) true else false
  
  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    runs += 1
    super.runOnce(s)
  }  
}