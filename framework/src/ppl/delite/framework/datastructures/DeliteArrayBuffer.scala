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
    def fromFunction[A:Manifest](size: Rep[Int])(func: Rep[Int] => Rep[A])(implicit ctx: SourceContext) = darray_buffer_from_function(size, func)
  }

  implicit def repDArrayBufferToDArrayBufferOps[A:Manifest](b: Rep[DeliteArrayBuffer[A]]) = new DeliteArrayBufferOpsCls(b)

  class DeliteArrayBufferOpsCls[A:Manifest](d: Rep[DeliteArrayBuffer[A]]) {
    def +=(elem: Rep[A])(implicit ctx: SourceContext) = darray_buffer_append(d,elem)
    def ++=(elems: Rep[DeliteArray[A]])(implicit ctx: SourceContext) = darray_buffer_appendAll(d,elems)
    def insert(pos: Rep[Int], elem: Rep[A])(implicit ctx: SourceContext) = darray_buffer_insert(d,pos,elem)
    def result(implicit ctx: SourceContext) = darray_buffer_result(d)
    def apply(idx: Rep[Int])(implicit ctx: SourceContext) = darray_buffer_apply(d,idx)
    def update(idx: Rep[Int], x: Rep[A])(implicit ctx: SourceContext) = darray_buffer_update(d,idx,x)
    def length(implicit ctx: SourceContext) = darray_buffer_length(d)
    def mutable(implicit ctx: SourceContext) = darray_buffer_mutable(d)
    def immutable(implicit ctx: SourceContext) = darray_buffer_immutable(d)

    def map[B:Manifest](func: Rep[A] => Rep[B])(implicit ctx: SourceContext) = darray_buffer_map(d,func)
    def filter(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = darray_buffer_filter(d,pred)
    def zip[B:Manifest,R:Manifest](that: Rep[DeliteArrayBuffer[B]])(func: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext) = darray_buffer_zip(d,that,func)
    def reduce(func: (Rep[A],Rep[A]) => Rep[A])(zero: Rep[A])(implicit ctx: SourceContext) = darray_buffer_reduce(d,func,zero)
    def foreach(func: Rep[A] => Rep[Unit])(implicit ctx: SourceContext) = darray_buffer_foreach(d,func)
    def forIndices(func: Rep[Int] => Rep[Unit])(implicit ctx: SourceContext) = darray_buffer_forIndices(d,func)
    def groupBy[K:Manifest](key: Rep[A] => Rep[K])(implicit ctx: SourceContext) = darray_buffer_groupBy(d,key)
    def groupByReduce[K:Manifest,V:Manifest](key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]) = darray_buffer_groupByReduce(d,key,value,reduce)
    def flatMap[B:Manifest](func: Rep[A] => Rep[DeliteArrayBuffer[B]])(implicit ctx: SourceContext) = darray_buffer_flatmap(d,func)
  }

  def darray_buffer_new[A:Manifest](initSize: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]
  def darray_buffer_new_imm[A:Manifest](data: Rep[DeliteArray[A]], length: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]
  def darray_buffer_from_function[A:Manifest](size: Rep[Int], func: Rep[Int] => Rep[A])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]
  def darray_buffer_apply[A:Manifest](d: Rep[DeliteArrayBuffer[A]], idx: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def darray_buffer_update[A:Manifest](d: Rep[DeliteArrayBuffer[A]], idx: Rep[Int], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_length[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[Int]
  def darray_buffer_append[A:Manifest](d: Rep[DeliteArrayBuffer[A]], elem: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_appendAll[A:Manifest](d: Rep[DeliteArrayBuffer[A]], elems: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_insert[A:Manifest](d: Rep[DeliteArrayBuffer[A]], pos: Rep[Int], elem: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_result[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def darray_buffer_mutable[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]
  def darray_buffer_immutable[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]

  def darray_buffer_map[A:Manifest,B:Manifest](d: Rep[DeliteArrayBuffer[A]], func: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[B]]
  def darray_buffer_filter[A:Manifest](d: Rep[DeliteArrayBuffer[A]], pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[A]]
  def darray_buffer_zip[A:Manifest,B:Manifest,R:Manifest](d: Rep[DeliteArrayBuffer[A]], that: Rep[DeliteArrayBuffer[B]], func: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[R]]
  def darray_buffer_reduce[A:Manifest](d: Rep[DeliteArrayBuffer[A]], func: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def darray_buffer_foreach[A:Manifest](d: Rep[DeliteArrayBuffer[A]], func: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_forIndices[A:Manifest](d: Rep[DeliteArrayBuffer[A]], func: Rep[Int] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def darray_buffer_groupBy[A:Manifest,K:Manifest](d: Rep[DeliteArrayBuffer[A]], key: Rep[A] => Rep[K])(implicit ctx: SourceContext): Rep[DeliteMap[K,DeliteArrayBuffer[A]]]
  def darray_buffer_groupByReduce[A:Manifest,K:Manifest,V:Manifest](d: Rep[DeliteArrayBuffer[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMap[K,V]]
  def darray_buffer_flatmap[A:Manifest,B:Manifest](d: Rep[DeliteArrayBuffer[A]], func: Rep[A] => Rep[DeliteArrayBuffer[B]])(implicit ctx: SourceContext): Rep[DeliteArrayBuffer[B]]
}

trait DeliteArrayBufferCompilerOps extends DeliteArrayBufferOps { 
  def darray_buffer_unsafe_result[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
}

trait DeliteArrayBufferOpsExp extends DeliteArrayBufferOps with DeliteCollectionOpsExp with StructExp with PrimitiveOpsExp with EqualExp with VariablesExp with DeliteStructsExp {
  this: DeliteArrayOpsExpOpt with DeliteOpsExp with DeliteMapOpsExp =>

  /////////////////////////////////
  // sequential mutable buffer ops

  case class DeliteArrayBufferNew[A:Manifest](initSize: Exp[Int], logicalSize: Exp[Int])(implicit ctx: SourceContext) extends DeliteStruct[DeliteArrayBuffer[A]] {
    val elems = copyTransformedElems(Seq("data" -> var_new(DeliteArray[A](initSize)).e, "length" -> var_new(logicalSize).e))
    val mA = manifest[A]
  }

  case class DeliteArrayBufferNewImm[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int]) extends DeliteStruct[DeliteArrayBuffer[A]] {
    val elems = copyTransformedElems(Seq("data" -> data, "length" -> length))
    val mA = manifest[A]
  }

  def darray_buffer_new[A:Manifest](initSize: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = darray_buffer_new(initSize, unit(0))

  def darray_buffer_new[A:Manifest](initSize: Exp[Int], logicalSize: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = reflectMutable(DeliteArrayBufferNew(initSize, logicalSize))

  def darray_buffer_new_imm[A:Manifest](data: Exp[DeliteArray[A]], length: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArrayBuffer[A]] = reflectPure(DeliteArrayBufferNewImm(data, length))

  def darray_buffer_apply[A:Manifest](d: Exp[DeliteArrayBuffer[A]], idx: Exp[Int])(implicit ctx: SourceContext): Exp[A] = darray_buffer_raw_data(d).apply(idx)

  def darray_buffer_update[A:Manifest](d: Exp[DeliteArrayBuffer[A]], idx: Exp[Int], x: Exp[A])(implicit ctx: SourceContext) = darray_buffer_raw_data(d).update(idx,x)

  def darray_buffer_append[A:Manifest](d: Exp[DeliteArrayBuffer[A]], elem: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    //darray_buffer_insert(d, d.length, elem)
    darray_buffer_ensureextra(d,unit(1))
    darray_buffer_update(d,d.length,elem)
    darray_buffer_set_length(d, d.length+unit(1))
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
    darray_buffer_raw_data(d)
  }

  protected def darray_buffer_raw_data[A:Manifest](d: Exp[DeliteArrayBuffer[A]])(implicit ctx: SourceContext): Exp[DeliteArray[A]] = field[DeliteArray[A]](d, "data")
  protected def darray_buffer_set_raw_data[A:Manifest](d: Exp[DeliteArrayBuffer[A]], data: Exp[DeliteArray[A]]) = field_update[DeliteArray[A]](d, "data", data)

  def darray_buffer_length[A:Manifest](d: Exp[DeliteArrayBuffer[A]])(implicit ctx: SourceContext) = field[Int](d, "length")
  protected def darray_buffer_set_length[A:Manifest](d: Exp[DeliteArrayBuffer[A]], len: Exp[Int]) = field_update[Int](d, "length", len)

  def darray_buffer_insert[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], x: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = {
    darray_buffer_insertspace(d,pos,unit(1))
    darray_buffer_update(d,pos,x)
  }

  protected def darray_buffer_insertAll[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], xs: Exp[DeliteArray[A]])(implicit ctx: SourceContext): Exp[Unit] = {
    darray_buffer_insertspace(d,pos,xs.length)
    darray_buffer_copyfrom(d, pos, xs)
  }

  protected def darray_buffer_copyfrom[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], xs: Exp[DeliteArray[A]]): Exp[Unit] = {
    val data = darray_buffer_raw_data(d)
    darray_copy(xs, unit(0), data, pos, xs.length)
  }

  protected def darray_buffer_insertspace[A:Manifest](d: Exp[DeliteArrayBuffer[A]], pos: Exp[Int], len: Exp[Int]): Exp[Unit] = {
    darray_buffer_ensureextra(d,len)
    val data = darray_buffer_raw_data(d)
    darray_copy(data, pos, data, pos + len, d.length - pos)
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
    darray_copy(oldData, unit(0), newData, unit(0), d.length)
    darray_buffer_set_raw_data(d, newData.unsafeImmutable)
  }

  /////////////////////
  // parallel bulk ops

  case class DeliteArrayBufferMap[A:Manifest,B:Manifest](in: Exp[DeliteArrayBuffer[A]], func: Exp[A] => Exp[B])(implicit ctx: SourceContext)
    extends DeliteOpMap[A,B,DeliteArrayBuffer[B]] {

    val size = copyTransformedOrElse(_.size)(in.length)
    override def alloc(len: Exp[Int]) = darray_buffer_new[B](len,len) //flat
  }

  case class DeliteArrayBufferMapIndices[A:Manifest](size: Exp[Int], func: Exp[Int] => Exp[A])
    extends DeliteOpMapIndices[A,DeliteArrayBuffer[A]] {

    override def alloc(len: Exp[Int]) = darray_buffer_new[A](len,len) //flat
  }

  case class DeliteArrayBufferZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteArrayBuffer[A]], inB: Exp[DeliteArrayBuffer[B]], func: (Exp[A], Exp[B]) => Exp[R])
    extends DeliteOpZipWith[A,B,R,DeliteArrayBuffer[R]] {

    override def alloc(len: Exp[Int]) = darray_buffer_new[R](len,len) //flat
    val size = copyTransformedOrElse(_.size)(inA.length)
  }

  case class DeliteArrayBufferFilter[A:Manifest](in: Exp[DeliteArrayBuffer[A]], cond: Exp[A] => Exp[Boolean])
    extends DeliteOpFilter[A,A,DeliteArrayBuffer[A]] {

    override def alloc(len: Exp[Int]) = darray_buffer_new[A](len) //buffer
    val size = copyTransformedOrElse(_.size)(in.length)
    def func = e => e
  }

  case class DeliteArrayBufferFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArrayBuffer[A]], func: Exp[A] => Exp[DeliteArrayBuffer[B]])
    extends DeliteOpFlatMap[A,B,DeliteArrayBuffer[B]] {

    override def alloc(len: Exp[Int]) = darray_buffer_new[B](len) //buffer
    val size = copyTransformedOrElse(_.size)(in.length)
  }
  
  case class DeliteArrayBufferReduce[A:Manifest](in: Exp[DeliteArrayBuffer[A]], func: (Exp[A], Exp[A]) => Exp[A], zero: Exp[A])
    extends DeliteOpReduce[A] {
    
    val size = copyTransformedOrElse(_.size)(in.length)    
  }

  case class DeliteArrayBufferGroupBy[A:Manifest,K:Manifest](in: Exp[DeliteArrayBuffer[A]], keyFunc: Exp[A] => Exp[K]) 
    extends DeliteOpGroupBy[K,A,DeliteArrayBuffer[A],DeliteArray[DeliteArrayBuffer[A]]] {

    def alloc(len: Exp[Int]) = DeliteArray[DeliteArrayBuffer[A]](len)
    def allocI(len: Exp[Int]) = darray_buffer_new[A](len) //buffer
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class DeliteArrayBufferForeach[A:Manifest](in: Exp[DeliteArrayBuffer[A]], func: Rep[A] => Rep[Unit]) extends DeliteOpForeach[A] {
    def sync = null //unused
    val size = copyTransformedOrElse(_.size)(in.length)
    val mA = manifest[A]
  }

  case class DeliteArrayBufferForIndices[A:Manifest](in: Exp[DeliteArrayBuffer[A]], func: Rep[Int] => Rep[Unit])(implicit ctx: SourceContext) extends DeliteOpIndexedLoop {
    val size = copyTransformedOrElse(_.size)(in.length)
    val mA = manifest[A]
  }

  def darray_buffer_mutable[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext) = reflectMutable(DeliteArrayBufferMap(d,(e:Rep[A])=>e))
  def darray_buffer_immutable[A:Manifest](d: Rep[DeliteArrayBuffer[A]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBufferMap(d,(e:Rep[A])=>e))

  def darray_buffer_from_function[A:Manifest](size: Exp[Int], func: Exp[Int] => Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBufferMapIndices(size,func))
  def darray_buffer_map[A:Manifest,B:Manifest](d: Rep[DeliteArrayBuffer[A]], func: Rep[A] => Rep[B])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBufferMap(d,func))
  def darray_buffer_filter[A:Manifest](d: Rep[DeliteArrayBuffer[A]], pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBufferFilter(d,pred))
  def darray_buffer_zip[A:Manifest,B:Manifest,R:Manifest](d: Rep[DeliteArrayBuffer[A]], that: Rep[DeliteArrayBuffer[B]], func: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBufferZipWith(d,that,func))
  def darray_buffer_flatmap[A:Manifest,B:Manifest](d: Exp[DeliteArrayBuffer[A]], func: Rep[A] => Rep[DeliteArrayBuffer[B]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBufferFlatMap(d,func))
  def darray_buffer_reduce[A:Manifest](d: Rep[DeliteArrayBuffer[A]], func: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext) = reflectPure(DeliteArrayBufferReduce(d,func,zero))
  def darray_buffer_foreach[A:Manifest](d: Rep[DeliteArrayBuffer[A]], func: Rep[A] => Rep[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteArrayBufferForeach(d,func)
    reflectEffect(df, summarizeEffects(df.body.asInstanceOf[DeliteForeachElem[A]].func).star andAlso Simple())
  }
  def darray_buffer_forIndices[A:Manifest](d: Rep[DeliteArrayBuffer[A]], func: Rep[Int] => Rep[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteArrayBufferForIndices(d,func)
    reflectEffect(df, summarizeEffects(df.body.asInstanceOf[DeliteForeachElem[A]].func).star andAlso Simple())
  }
  def darray_buffer_groupBy[A:Manifest,K:Manifest](d: Exp[DeliteArrayBuffer[A]], key: Rep[A] => Rep[K])(implicit ctx: SourceContext): Exp[DeliteMap[K,DeliteArrayBuffer[A]]] = DeliteMap(d, key, reflectPure(DeliteArrayBufferGroupBy(d,key)))
  def darray_buffer_groupByReduce[A:Manifest,K:Manifest,V:Manifest](d: Rep[DeliteArrayBuffer[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMap[K,V]] = DeliteMap(d, key, value, reduce)


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
    if (isDeliteArrayBuffer(x)) {
      val buf = asDeliteArrayBuffer(x)
      darray_buffer_set_length(buf, y)
    }
    else super.dc_set_logical_size(x,y)        
  }
  
  override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (isDeliteArrayBuffer(x)) {
      if (hasConditions) ParSimpleBuffer else ParFlat
    }
    else super.dc_parallelization(x, hasConditions)
  }

  override def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArrayBuffer(x)) { unit(true) }
    else super.dc_appendable(x,i,y)
  }

  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArrayBuffer(x)) { asDeliteArrayBuffer(x) += y; }
    else super.dc_append(x,i,y)
  }
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isDeliteArrayBuffer(x)) darray_buffer_new[A](size,size).asInstanceOf[Exp[CA]] //flat alloc
    else super.dc_alloc[A,CA](x,size)
  } 
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isDeliteArrayBuffer(src) && isDeliteArrayBuffer(dst)) {
      darray_copy(darray_buffer_raw_data(asDeliteArrayBuffer(src)), srcPos, darray_buffer_raw_data(asDeliteArrayBuffer(dst)), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }

  override def dc_data_field[A:Manifest](x: Exp[DeliteCollection[A]]) = {
    if (isDeliteArrayBuffer(x)) "data"
    else super.dc_data_field(x)
  }

  override def dc_size_field[A:Manifest](x: Exp[DeliteCollection[A]]) = {
    if (isDeliteArrayBuffer(x)) "length"
    else super.dc_size_field(x)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteArrayBufferNewImm(d,l) => reflectPure(new {override val original = Some(f,e) } with DeliteArrayBufferNewImm(f(d),f(l))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteArrayBufferNewImm(d,l), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferNewImm(f(d),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferNew(p,l), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferNew(f(p),f(l))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case e@DeliteArrayBufferMap(in,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayBufferMap(f(in),f(g))(e.dmA,e.dmB,ctx))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteArrayBufferMapIndices(s,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayBufferMapIndices(f(s),f(g))(e.dmA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteArrayBufferFilter(in,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayBufferFilter(f(in),f(g))(e.dmA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteArrayBufferZipWith(a,b,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayBufferZipWith(f(a),f(b),f(g))(e.dmA,e.dmB,e.dmR))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteArrayBufferReduce(in,g,z) => e.asInstanceOf[DeliteArrayBufferReduce[A]] match { //scalac typer bug
      case e@DeliteArrayBufferReduce(in,g,z) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayBufferReduce(f(in),f(g),f(z))(e.dmA))(mtype(manifest[A]),implicitly[SourceContext])
    }    
    case e@DeliteArrayBufferGroupBy(in,k) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayBufferGroupBy(f(in),f(k))(mtype(e.dmV),mtype(e.dmK)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteArrayBufferFlatMap(in,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayBufferFlatMap(f(in),f(g))(mtype(e.dmA),mtype(e.dmB)))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@DeliteArrayBufferMap(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferMap(f(in),f(g))(e.dmA,e.dmB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferMapIndices(s,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferMapIndices(f(s),f(g))(e.dmA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferFilter(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferFilter(f(in),f(g))(e.dmA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferZipWith(a,b,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferZipWith(f(a),f(b),f(g))(e.dmA,e.dmB,e.dmR), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferReduce(in,g,z), u, es) => e.asInstanceOf[DeliteArrayBufferReduce[A]] match { //scalac typer bug
      case e@DeliteArrayBufferReduce(in,g,z) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferReduce(f(in),f(g),f(z))(e.dmA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    }    
    case Reflect(e@DeliteArrayBufferGroupBy(in,k), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferGroupBy(f(in),f(k))(mtype(e.dmV),mtype(e.dmK)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferFlatMap(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferFlatMap(f(in),f(g))(mtype(e.dmA),mtype(e.dmB)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferForeach(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferForeach(f(in),f(g))(mtype(e.mA)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteArrayBufferForIndices(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayBufferForIndices(f(in),f(g))(mtype(e.mA),ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if t.erasure == classOf[DeliteArrayBuffer[_]] => Some((classTag(t), List("data","length") zip List(darrayManifest(t.typeArguments(0)), manifest[Int]))) 
    case _ => super.unapplyStructType
  }
}

trait ScalaGenDeliteArrayBufferOps extends ScalaGenEffect {
  val IR: DeliteArrayBufferOpsExp
}

trait CudaGenDeliteArrayBufferOps extends CudaGenEffect {
  val IR: DeliteArrayBufferOpsExp
}
