package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenFat, BaseFatExp, Base}
import ppl.dsl.optiql.{OptiQL, OptiQLExp}
import ppl.delite.framework.ops.{DeliteCollection, DeliteCollectionOpsExp}
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.Util._
import java.io.PrintWriter
import scala.reflect.SourceContext

trait TableOps extends Base { this: OptiQL =>

  // Lifting and Interface Injection
  implicit def tableRepToTableRepOps[T:Manifest](d: Rep[Table[T]]) = new TableRepOps(d)

  object Table {
    def apply[T:Manifest](): Rep[Table[T]] = tableObjectApply()
	  def apply[T:Manifest](initSize: Rep[Int]): Rep[Table[T]] = tableObjectApply(initSize)
	  def apply[T:Manifest](data: Rep[DeliteArray[T]], size: Rep[Int]): Rep[Table[T]] = tableObjectApply(data, size)
  }

  class TableRepOps[T:Manifest](t:Rep[Table[T]]) {
    def apply(i: Rep[Int]): Rep[T] = tableApply(t, i)
    def size() = tableSize(t)
  }


  //implementation method defintions
  def tableApply[T:Manifest](t: Rep[Table[T]], i: Rep[Int]): Rep[T]
  def tableObjectApply[T:Manifest](): Rep[Table[T]]
  def tableObjectApply[T:Manifest](initSize: Rep[Int]): Rep[Table[T]]
  def tableObjectApply[T:Manifest](data: Rep[DeliteArray[T]], initSize: Rep[Int]): Rep[Table[T]]
  def tableSize[T:Manifest](t: Rep[Table[T]]): Rep[Int]

}

trait TableOpsExp extends TableOps with DeliteCollectionOpsExp { this: OptiQLExp =>

  def tableSize[T:Manifest](t: Exp[Table[T]]): Exp[Int] = field[Int](t, "size")
  def tableArray[T:Manifest](t: Exp[Table[T]]): Exp[DeliteArray[T]] = field[DeliteArray[T]](t, "data")

  def tableApply[T:Manifest](t: Exp[Table[T]], i: Exp[Int]): Exp[T] = darray_apply(tableArray(t), i)

  def tableObjectApply[T:Manifest](): Exp[Table[T]] = 
    struct[Table[T]](ClassTag[Table[T]]("Table"), "data" -> DeliteArray(unit(0)), "size" -> unit(0))

  def tableObjectApply[T:Manifest](initSize: Exp[Int]): Exp[Table[T]] = 
    struct[Table[T]](ClassTag[Table[T]]("Table"), "data" -> DeliteArray(initSize), "size" -> initSize)

  def tableObjectApply[T:Manifest](data: Exp[DeliteArray[T]], initSize: Exp[Int]): Exp[Table[T]] = 
    struct[Table[T]](ClassTag[Table[T]]("Table"), "data" -> data, "size" -> initSize)

  
  //delite collection ops  
  def isTable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Table[A]])  
  def asTable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[Table[A]]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isTable(x)) asTable(x).size
    else super.dc_size(x)
  }
  
  /* override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isTable(x)) table_set_size(asTable(x), y)
    else super.dc_set_logical_size(x,y)        
  } */
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isTable(x)) asTable(x).apply(n)
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isTable(x)) tableArray(asTable(x)).update(n, y)
    else super.dc_update(x,n,y)        
  }
  
  /* override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isTable(x)) { asTable(x) += y; unit(true) }
    else super.dc_append(x,i,y)        
  } */
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isTable(x)) {
      val out = Table[A](size)
      out.asInstanceOf[Exp[CA]]
    }
    else super.dc_alloc[A,CA](x,size)
  } 
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isTable(src) && isTable(dst)) {
      darray_unsafe_copy(tableArray(asTable(src)), srcPos, tableArray(asTable(dst)), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }   

}
