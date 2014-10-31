package ppl.delite.framework.ops

import java.io.PrintWriter
import scala.reflect.SourceContext
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
    case _ => throw new RuntimeException("no static implementation found for dc_size on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }
  
  def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Exp[A] = {
    throw new RuntimeException("no static implementation found for dc_apply on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }
  
  def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    throw new RuntimeException("no static implementation found for dc_update on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }
  
  // -- ParBuffer methods
   
  def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    throw new RuntimeException("no static implementation found for dc_set_logical_size on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }  
  
  /* returns true if the element y can be appended to collection x */
  def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Boolean] = {
    throw new RuntimeException("no static implementation found for dc_appendable on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }
  
  /* append the element y to the collection x, returns unit */
  def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    throw new RuntimeException("no static implementation found for dc_append on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }

  def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    throw new RuntimeException("no static implementation found for dc_alloc on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }  
  
  def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    throw new RuntimeException("no static implementation found for dc_copy on " + findDefinition(x.asInstanceOf[Sym[DeliteCollection[A]]]).get + " --- x.tp is " + x.tp)
  }   

  // - Struct transformation methods
  def dc_data_field[A:Manifest](x: Exp[DeliteCollection[A]]): String = ""

  def dc_size_field[A:Manifest](x: Exp[DeliteCollection[A]]): String = ""

}
