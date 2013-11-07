package ppl.dsl.optiql.ops

import scala.virtualization.lms.common.{ScalaGenFat, BaseFatExp, Base, Record}
import ppl.dsl.optiql.{OptiQL, OptiQLExp}
import ppl.delite.framework.ops.{DeliteCollection, DeliteCollectionOpsExp}
import ppl.delite.framework.datastructures.{DeliteArray, DeliteArrayBuffer, DeliteStructsExp}
import ppl.delite.framework.Util._
import java.io.PrintWriter
import scala.reflect.SourceContext

trait TableOps extends Base { this: OptiQL =>

  // Lifting and Interface Injection
  implicit def tableRepToTableRepOps[T:Manifest](d: Rep[Table[T]]) = new TableRepOps(d)

  object Table {
	  def apply[T:Manifest](initSize: Rep[Int]): Rep[Table[T]] = tableObjectApply(initSize)
	  def apply[T:Manifest](data: Rep[DeliteArray[T]], size: Rep[Int]): Rep[Table[T]] = tableObjectApply(data, size)
    def apply[T:Manifest](data: Rep[DeliteArray[T]])(implicit o: Overloaded1): Rep[Table[T]] = tableObjectApply(data, data.length)
    def apply[T:Manifest](elems: Rep[T]*): Rep[Table[T]] = optiql_table_from_seq(elems)
    def fromFile[T<:Record:Manifest](path: Rep[String], separator: Rep[String]): Rep[Table[T]] = optiql_table_input_reader(path, separator)
    def fromFile(path: Rep[String]): Rep[Table[String]] = optiql_table_line_reader(path)
    def fromString[T<:Record:Manifest](data: Rep[String], rowSeparator: Rep[String], columnSeparator: Rep[String]): Rep[Table[T]] = optiql_table_from_string(data, rowSeparator, columnSeparator)
    def range(start: Rep[Int], end: Rep[Int]): Rep[Table[Int]] = tableObjectRange(start, end)
  }

  class TableRepOps[T:Manifest](t:Rep[Table[T]]) {
    def apply(i: Rep[Int]): Rep[T] = tableApply(t, i)
    def size() = tableSize(t)
    def toArray() = tableToArray(t)
  }


  //implementation method defintions
  def tableApply[T:Manifest](t: Rep[Table[T]], i: Rep[Int]): Rep[T]
  def tableObjectApply[T:Manifest](): Rep[Table[T]]
  def tableObjectApply[T:Manifest](initSize: Rep[Int]): Rep[Table[T]]
  def tableObjectApply[T:Manifest](data: Rep[DeliteArray[T]], size: Rep[Int]): Rep[Table[T]]
  def tableSize[T:Manifest](t: Rep[Table[T]]): Rep[Int]
  def tableObjectRange(start: Rep[Int], end: Rep[Int]): Rep[Table[Int]]

  // data exchange
  def tableToArray[T:Manifest](t: Rep[Table[T]]): Rep[DeliteArray[T]]
}

trait TableOpsExp extends TableOps with DeliteCollectionOpsExp with DeliteStructsExp { this: OptiQLExp =>

  def tableRawData[T:Manifest](t: Exp[Table[T]]) = field[DeliteArray[T]](t, "data")
  def tableSize[T:Manifest](t: Exp[Table[T]]): Exp[Int] = field[Int](t, "size")
  def tableApply[T:Manifest](t: Exp[Table[T]], i: Exp[Int]): Exp[T] = tableRawData(t).apply(i)

  def tableObjectApply[T:Manifest](): Exp[Table[T]] = tableObjectApply(unit(16))
  def tableObjectApply[T:Manifest](initSize: Exp[Int]): Exp[Table[T]] = struct(classTag[Table[T]], "data" -> fatal(unit("Table allocation within Delite Op not rewritten")), "size" -> initSize)
  def tableObjectApply[T:Manifest](data: Exp[DeliteArray[T]], size: Exp[Int]): Exp[Table[T]] = struct(classTag[Table[T]], "data" -> data, "size" -> size)

  def tableObjectRange(start: Exp[Int], end: Exp[Int]) = Table(DeliteArray.fromFunction(end-start)(i => i + start))

  def tableToArray[T:Manifest](t: Exp[Table[T]]) = tableRawData(t)
  
  //delite collection ops  
  def isTable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Table[A]])  
  def asTable[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[Table[A]]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isTable(x)) asTable(x).size
    else super.dc_size(x)
  }

  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isTable(x)) asTable(x).apply(n)
    else super.dc_apply(x,n)    
  }

  override def dc_data_field[A:Manifest](x: Exp[DeliteCollection[A]]) = {
    if (isTable(x)) "data"
    else super.dc_data_field(x)
  }

  override def dc_size_field[A:Manifest](x: Exp[DeliteCollection[A]]) = {
    if (isTable(x)) "size"
    else super.dc_size_field(x)
  }

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = {
    val m = manifest[T]
    if (m.erasure == classOf[Table[_]]) Some((classTag(m), List("data" -> darrayManifest(m.typeArguments(0)), "size" -> manifest[Int])))
    else super.unapplyStructType
  }
  
  /* override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isTable(x)) dc_set_logical_size(asTable(x).data, y)
    else super.dc_set_logical_size(x,y)        
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isTable(x)) dc_update(asTable(x).data, n, y)
    else super.dc_update(x,n,y)        
  }
  
  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isTable(x)) dc_append(asTable(x).data, i, y)
    else super.dc_append(x,i,y)        
  }
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isTable(x)) Table[A](size).asInstanceOf[Exp[CA]]
    else super.dc_alloc[A,CA](x,size)
  } 
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isTable(src) && isTable(dst)) {
      dc_copy(asTable(src).data, srcPos, asTable(dst).data, dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  } */

}
