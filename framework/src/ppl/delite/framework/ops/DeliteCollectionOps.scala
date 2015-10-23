package ppl.delite.framework.ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.Interfaces

trait DeliteCollection[A]

trait DeliteCollectionOpsExp extends ExceptionOpsExp with BaseFatExp { this: DeliteOpsExp =>

  /**
   * Default delite collection op implementations
   */

  def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (hasConditions) ParSimpleBuffer else ParFlat // default
  }
    
  // -- ParFlat methods

  def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x match {
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    case _ => undefined("dc_size", x)
  }
  
  def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    undefined("dc_apply", x)
  }
  
  def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    undefined("dc_update", x)
  }
  
  // -- ParBuffer methods
   
  def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    undefined("dc_alloc", x)
  }  
  
  /* returns true if the element y can be appended to collection x */
  def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Boolean] = {
    undefined("dc_appendable", x)
  }
  
  /* append the element y to the collection x, returns unit */
  def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    undefined("dc_append", x)
  }

  def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    undefined("dc_alloc", x)
  }  
  
  def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    undefined("dc_copy", src)
  }   

  // - Struct transformation methods
  def dc_data_field(tp: Manifest[_]): String = ""

  def dc_size_field(tp: Manifest[_]): String = ""


  private def undefined[A](method: String, x: Exp[DeliteCollection[A]]) = {
    fatal(unit("no static implementation found for " + method + " on " + x.toString + " of type: " + x.tp))
  }

}
