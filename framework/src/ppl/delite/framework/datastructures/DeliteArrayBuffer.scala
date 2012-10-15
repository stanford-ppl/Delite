package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import java.io.PrintWriter
import reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.Config
import ppl.delite.framework.ops._
import ppl.delite.framework.Util._


/*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

trait DeliteArrayBuffer[A] extends DeliteCollection[A]

trait DeliteArrayBufferOps extends Base {

  object DeliteArrayBuffer {
    def apply[A:Manifest]()(implicit ctx: SourceContext) = darray_buffer_new(unit(16))
    def apply[A:Manifest](initSize: Rep[Int])(implicit ctx: SourceContext) = darray_buffer_new(initSize)
    def apply[A:Manifest](data: Rep[DeliteArray[A]], length: Rep[Int])(implicit ctx: SourceContext) = darray_buffer_new_imm(data, length)
  }

  implicit def repDArrayBufferToDArrayBufferOps[A:Manifest](b: Rep[DeliteArrayBuffer[A]]) = new DeliteArrayBufferOpsCls(b)

  class DeliteArrayBufferOpsCls[A:Manifest](d: Rep[DeliteArrayBuffer[A]]) {
    def +=(elem: Rep[A])(implicit ctx: SourceContext) = darray_buffer_append(d,elem)
    def ++=(elems: Rep[DeliteArray[A]])(implicit ctx: SourceContext) = darray_buffer_appendAll(d,elems)
    def result(implicit ctx: SourceContext) = darray_buffer_result(d)
    def apply(idx: Rep[Int])(implicit ctx: SourceContext) = darray_buffer_apply(d,idx)
    def update(idx: Rep[Int], x: Rep[A])(implicit ctx: SourceContext) = darray_buffer_update(d,idx,x)
    def length(implicit ctx: SourceContext) = darray_buffer_length(d)
  }

  def darray_buffer_new[A:Manifest](initSize: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]
  def darray_buffer_new_imm[A:Manifest](data: Rep[DeliteArray[A]], length: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]
  def darray_buffer_apply[A:Manifest](d: Rep[DeliteArrayBuffer[A]], idx: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def darray_buffer_update[A:Manifest](d: Rep[DeliteArrayBuffer[A]], idx: Rep[Int], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_length[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[Int]
  def darray_buffer_append[A:Manifest](d: Rep[DeliteArrayBuffer[A]], elem: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_appendAll[A:Manifest](d: Rep[DeliteArrayBuffer[A]], elems: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_result[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]

}

trait DeliteArrayBufferCompilerOps extends DeliteArrayBufferOps { 
  def darray_buffer_unsafe_result[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
}

trait DeliteArrayBufferOpsExp extends DeliteArrayBufferOps with DeliteCollectionOpsExp with StructExp with NumericOpsExp with PrimitiveOpsExp with VariablesExp {
  this: DeliteArrayOpsExp with DeliteOpsExp =>

  case class DeliteArrayBufferNew[A:Manifest](initSize: Exp[Int]) extends DeliteStruct[DeliteArrayBuffer[A]] {
    val elems = Seq("data" -> var_new(DeliteArray[A](initSize)).e, "length" -> var_new(initSize).e)
    val mA = manifest[A]
  }

  case class DeliteArrayBufferNewImm[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int]) extends DeliteStruct[DeliteArrayBuffer[A]] {
    val elems = Seq("data" -> data, "length" -> length)
    val mA = manifest[A]
  }

  def darray_buffer_new[A:Manifest](initSize: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = reflectMutable(DeliteArrayBufferNew(initSize))

  def darray_buffer_new_imm[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = reflectPure(DeliteArrayBufferNewImm(data, length))

  def darray_buffer_apply[A:Manifest](d: Exp[DeliteArrayBuffer[A]], idx: Exp[Int])(implicit ctx: SourceContext): Exp[A] = darray_buffer_raw_data(d).apply(idx)

  def darray_buffer_update[A:Manifest](d: Exp[DeliteArrayBuffer[A]], idx: Exp[Int], x: Exp[A])(implicit ctx: SourceContext) = darray_buffer_raw_data(d).update(idx,x)

  def darray_buffer_append[A:Manifest](d: Exp[DeliteArrayBuffer[A]], elem: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    darray_buffer_insert(d, d.length, elem)
  }

  def darray_buffer_appendAll[A:Manifest](d: Exp[DeliteArrayBuffer[A]], elems: Exp[DeliteArray[A]])(implicit ctx: SourceContext): Exp[Unit] = {
    darray_buffer_insertAll(d, d.length, elems)
  }

  def darray_buffer_result[A:Manifest](d: Exp[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Exp[DeliteArray[A]] = {
    val data = darray_buffer_raw_data(d)
    val result = DeliteArray[A](d.length)
    darray_copy(data, unit(0), result, unit(0), d.length)
    result.unsafeImmutable
  }

  def darray_buffer_unsafe_result[A:Manifest](d: Exp[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Exp[DeliteArray[A]] = {
    darray_buffer_raw_data(d).unsafeImmutable
  }

  protected def darray_buffer_raw_data[A:Manifest](d: Exp[DeliteArrayBuffer[A]]): Exp[DeliteArray[A]] = field[DeliteArray[A]](d, "data")
  protected def darray_buffer_set_raw_data[A:Manifest](d: Exp[DeliteArrayBuffer[A]], data: Exp[DeliteArray[A]]) = field_update[DeliteArray[A]](d, "data", data)

  def darray_buffer_length[A:Manifest](d: Exp[DeliteArrayBuffer[A]])(implicit ctx: SourceContext) = field[Int](d, "length")
  protected def darray_buffer_set_length[A:Manifest](d: Exp[DeliteArrayBuffer[A]], len: Exp[Int]) = field_update[Int](d, "length", len)

  protected def darray_buffer_insert[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    darray_buffer_insertspace(d,pos,unit(1))
    darray_buffer_update(d,pos,x)
  }

  protected def darray_buffer_insertAll[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], xs: Exp[DeliteArray[A]])(implicit ctx: SourceContext): Exp[Unit] = {
    darray_buffer_insertspace(d,pos,xs.length)
    darray_buffer_copyfrom(d, pos, xs)
  }

  protected def darray_buffer_copyfrom[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], xs: Exp[DeliteArray[A]]): Exp[Unit] = {
    val i = var_new(unit(0))
    val data = darray_buffer_raw_data(d)
    while (i < xs.length) {
      darray_unsafe_update(data,pos+readVar(i),xs(i))
      i += 1
    }
  }

  protected def darray_buffer_insertspace[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], len: Exp[Int]): Exp[Unit] = {
    darray_buffer_ensureextra(d,len)
    val data = darray_buffer_raw_data(d)
    darray_unsafe_copy(data, pos, data, pos + len, d.length - pos)
    darray_buffer_set_length(d, d.length + len)
  }

  protected def darray_buffer_ensureextra[A:Manifest](d: Exp[DeliteArrayBuffer[A]], extra: Exp[Int]): Exp[Unit] = {
    val data = darray_buffer_raw_data(d)
    if (data.length - d.length < extra) {
      darray_buffer_realloc(d, d.length + extra)
    }
  }

  protected def darray_buffer_realloc[A:Manifest](d: Exp[DeliteArrayBuffer[A]], minLen: Exp[Int]): Exp[Unit] = {  
    val oldData = darray_buffer_raw_data(d)
    val doubleLength = oldData.length * unit(2)
    val n = var_new(if (unit(4) > doubleLength) unit(4) else doubleLength)
    while (n < minLen) n = n * unit(2)
    val newData = DeliteArray[A](n)
    darray_unsafe_copy(oldData, unit(0), newData, unit(0), d.length)
    darray_buffer_set_raw_data(d, newData.unsafeImmutable)
  }

  /////////////////////
  // delite collection
    
  def isDeliteArrayBuffer[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[DeliteArrayBuffer[A]])  
  def asDeliteArrayBuffer[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DeliteArrayBuffer[A]]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDeliteArrayBuffer(x)) asDeliteArrayBuffer(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDeliteArrayBuffer(x)) asDeliteArrayBuffer(x).apply(n)
    else super.dc_apply(x,n)
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArrayBuffer(x)) asDeliteArrayBuffer(x).update(n,y)
    else super.dc_update(x,n,y)        
  }
  
  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDeliteArrayBuffer(x)) darray_buffer_set_length(asDeliteArrayBuffer(x), y)
    else super.dc_set_logical_size(x,y)        
  }
  
  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArrayBuffer(x)) { asDeliteArrayBuffer(x) += y; unit(true) }
    else super.dc_append(x,i,y)
  }
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isDeliteArrayBuffer(x)) DeliteArrayBuffer[A](size).asInstanceOf[Exp[CA]]
    else super.dc_alloc[A,CA](x,size)
  } 
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isDeliteArrayBuffer(src) && isDeliteArrayBuffer(dst)) {
      darray_unsafe_copy(darray_buffer_raw_data(asDeliteArrayBuffer(src)), srcPos, darray_buffer_raw_data(asDeliteArrayBuffer(dst)), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteArrayBufferNewImm(d,l) => reflectPure(new {override val original = Some(f,e) } with DeliteArrayBufferNewImm(f(d),f(l))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteArrayBufferNew(l), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferNew(f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenDeliteArrayBufferOps extends ScalaGenEffect {
  val IR: DeliteArrayBufferOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArrayBuffer" => structName(m)
    case _ => super.remap(m)
  }

}
