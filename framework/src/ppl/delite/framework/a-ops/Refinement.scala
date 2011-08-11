package ppl.delite.framework.ops

import scala.virtualization.lms.common._

trait Refinement extends Base {
  override def __new[T](args: (String, Any)*): T = { 
    println("Called my new: ")
    args.map(println(_))
    null.asInstanceOf[T]    
  }
}