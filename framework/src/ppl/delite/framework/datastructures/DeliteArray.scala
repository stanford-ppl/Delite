package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.delite.framework.ops._
import ppl.delite.framework.Util._
import ppl.delite.framework.Config
import scala.collection.mutable.HashSet


trait DeliteArray[T] extends DeliteCollection[T] 

trait DeliteArrayOps extends StringOps {
  
  object DeliteArray {
    def apply[T:Manifest](length: Rep[Int])(implicit ctx: SourceContext) = darray_new(length)
    def imm[T:Manifest](length: Rep[Int])(implicit ctx: SourceContext) = darray_new_immutable(length)
    def fromFunction[T:Manifest](length: Rep[Int])(func: Rep[Int] => Rep[T])(implicit ctx: SourceContext) = darray_fromfunction(length, func)
    def sortIndices(length: Rep[Int])(comparator: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext) = darray_sortIndices(length, comparator)
  }

  implicit def repDArrayToDArrayOps[T:Manifest](da: Rep[DeliteArray[T]])(implicit ctx: SourceContext) = new DeliteArrayOpsCls(da)

  class DeliteArrayOpsCls[T:Manifest](da: Rep[DeliteArray[T]])(implicit ctx: SourceContext) {
    def length: Rep[Int] = darray_length(da)
    def apply(i: Rep[Int]): Rep[T] = darray_apply(da,i)
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = darray_update(da,i,x)
    def mutable = darray_mutable(da)
    def map[B:Manifest](f: Rep[T] => Rep[B]) = darray_map(da,f)
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteArray[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteArray[R]] = darray_zipwith(da,y,f)
    def reduce(f: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] = darray_reduce(da,f,zero)
    def foreach(f: Rep[T] => Rep[Unit]) = darray_foreach(da,f)
    def filter(f: Rep[T] => Rep[Boolean]) = darray_filter(da,f)
    def flatMap[B:Manifest](func: Rep[T] => Rep[DeliteArray[B]])(implicit ctx: SourceContext) = darray_flatmap(da,func)
    def groupByReduce[K:Manifest,V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]) = darray_groupByReduce(da,key,value,reduce)
    def mkString(del: Rep[String]) = darray_mkstring(da,del)
    def union(rhs: Rep[DeliteArray[T]]) = darray_union(da,rhs)
    def intersect(rhs: Rep[DeliteArray[T]]) = darray_intersect(da,rhs)
    def take(n: Rep[Int]) = darray_take(da,n)
    def sort = darray_sort(da)
    def toSeq = darray_toseq(da)
  }
    
  implicit def darrayToString[A:Manifest](x: Rep[DeliteArray[A]]): Rep[String] = "[ " + repDArrayToDArrayOps(x).mkString(unit(" ")) + " ]"
  def infix_+[A:Manifest](lhs: String, rhs: Rep[DeliteArray[A]]) = string_plus(unit(lhs), darrayToString[A](rhs))
  
  def darray_new[T:Manifest](length: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray[T]]
  def darray_new_immutable[T:Manifest](length: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray[T]]
  def darray_length[T:Manifest](da: Rep[DeliteArray[T]])(implicit ctx: SourceContext): Rep[Int]
  def darray_apply[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int])(implicit ctx: SourceContext): Rep[T]
  def darray_update[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int], x: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def darray_mutable[T:Manifest](d: Rep[DeliteArray[T]])(implicit ctx: SourceContext): Rep[DeliteArray[T]]
  def darray_copy[T:Manifest](src: Rep[DeliteArray[T]], srcPos: Rep[Int], dest: Rep[DeliteArray[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def darray_map[A:Manifest,B:Manifest](a: Rep[DeliteArray[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[DeliteArray[B]]    
  def darray_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[DeliteArray[A]], y: Rep[DeliteArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[DeliteArray[R]]
  def darray_reduce[A:Manifest](x: Rep[DeliteArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def darray_foreach[A:Manifest](d: Rep[DeliteArray[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def darray_filter[A:Manifest](x: Rep[DeliteArray[A]], f: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def darray_groupByReduce[A:Manifest,K:Manifest,V:Manifest](da: Rep[DeliteArray[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMap[K,V]]
  def darray_flatmap[A:Manifest,B:Manifest](da: Rep[DeliteArray[A]], func: Rep[A] => Rep[DeliteArray[B]])(implicit ctx: SourceContext): Rep[DeliteArray[B]]
  def darray_mkstring[A:Manifest](a: Rep[DeliteArray[A]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def darray_union[A:Manifest](lhs: Rep[DeliteArray[A]], rhs: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def darray_intersect[A:Manifest](lhs: Rep[DeliteArray[A]], rhs: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def darray_take[A:Manifest](lhs: Rep[DeliteArray[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def darray_sort[A:Manifest](lhs: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def darray_sortIndices(length: Rep[Int], comparator: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray[Int]]
  def darray_range(st: Rep[Int], en: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray[Int]]
  def darray_toseq[A:Manifest](a: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Seq[A]]
  def darray_fromfunction[T:Manifest](length: Rep[Int], func: Rep[Int] => Rep[T])(implicit ctx: SourceContext): Rep[DeliteArray[T]]

  def darray_set_act_buf[A:Manifest](da: Rep[DeliteArray[A]]): Rep[Unit]
}

trait DeliteArrayCompilerOps extends DeliteArrayOps {
  def darray_unsafe_update[T:Manifest](x: Rep[DeliteArray[T]], n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def darray_unsafe_copy[T:Manifest](src: Rep[DeliteArray[T]], srcPos: Rep[Int], dest: Rep[DeliteArray[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

trait DeliteArrayOpsExp extends DeliteArrayCompilerOps with DeliteArrayStructTags with DeliteCollectionOpsExp with DeliteStructsExp with EffectExp with PrimitiveOpsExp {
  this: DeliteOpsExp with DeliteMapOpsExp =>
  
  //////////////////
  // codegen ops

  case class DeliteArrayNew[T](length: Exp[Int], m:Manifest[T]) extends Def[DeliteArray[T]] //pass in manifest explicitly so it becomes part of equality (cse) check
  case class DeliteArrayLength[T:Manifest](da: Exp[DeliteArray[T]]) extends DefWithManifest[T,Int]
  case class DeliteArrayApply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) extends DefWithManifest[T,T]
  case class DeliteArrayUpdate[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) extends DefWithManifest[T,Unit]
  case class DeliteArrayCopy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int]) extends DefWithManifest[T,Unit]
  
  //TODO: ideally this group of ops should be implemented in the IR using the 'core' ops above
  case class DeliteArrayMkString[T:Manifest](da: Exp[DeliteArray[T]], del: Exp[String]) extends DefWithManifest[T,String]
  case class DeliteArrayUnion[T:Manifest](lhs: Exp[DeliteArray[T]], rhs: Exp[DeliteArray[T]]) extends DefWithManifest[T,DeliteArray[T]]
  case class DeliteArrayIntersect[T:Manifest](lhs: Exp[DeliteArray[T]], rhs: Exp[DeliteArray[T]]) extends DefWithManifest[T,DeliteArray[T]]
  case class DeliteArrayTake[T:Manifest](lhs: Exp[DeliteArray[T]], n: Exp[Int]) extends DefWithManifest[T,DeliteArray[T]]
  case class DeliteArraySort[T:Manifest](da: Exp[DeliteArray[T]]) extends DefWithManifest[T,DeliteArray[T]]
  case class DeliteArrayToSeq[A:Manifest](x: Exp[DeliteArray[A]]) extends Def[Seq[A]]

  //this is a hack to make writes to DeliteArray within a Struct an atomic operation in order to avoid creating mutable aliases
  //fortunately due to our limited data structure design this trick isn't required all over the place
  case class StructUpdate[T:Manifest](struct: Exp[Any], fields: List[String], i: List[Exp[Int]], x: Exp[T]) extends DefWithManifest[T,Unit]
  case class VarUpdate[T:Manifest](v: Var[DeliteArray[T]], i: Exp[Int], x: Exp[T]) extends DefWithManifest[T,Unit]
  case class StructCopy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], destStruct: Exp[Any], fields: List[String], destPos: List[Exp[Int]], len: Exp[Int]) extends DefWithManifest[T,Unit]
  case class VarCopy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Var[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int]) extends DefWithManifest[T,Unit]

  //this is a hack to make DeliteArray implement the buffer interface within Delite Ops without having to wrap the DeliteArray in a DeliteArrayBuffer
  case class DeliteArraySetActBuffer[T:Manifest](da: Exp[DeliteArray[T]]) extends DefWithManifest[T,Unit]
  // switched because of a NoSuchMethodError problem when matching on the case object in other traits..
  // case object DeliteArrayGetActSize extends Def[Int]
  case class DeliteArrayGetActSize() extends Def[Int]

  //////////////////
  // delite ops
  
  case class DeliteArrayMap[A:Manifest,B:Manifest](in: Exp[DeliteArray[A]], func: Exp[A] => Exp[B])(implicit ctx: SourceContext)
    extends DeliteOpMap[A,B,DeliteArray[B]] {

    val size = copyTransformedOrElse(_.size)(in.length)
    override def alloc(len: Exp[Int]) = DeliteArray[B](len)
  }
  
  case class DeliteArrayZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteArray[A]], inB: Exp[DeliteArray[B]],
                                                                  func: (Exp[A], Exp[B]) => Exp[R])
    extends DeliteOpZipWith[A,B,R,DeliteArray[R]] {

    override def alloc(len: Exp[Int]) = DeliteArray[R](len)
    val size = copyTransformedOrElse(_.size)(inA.length)
  }
  
  case class DeliteArrayReduce[A:Manifest](in: Exp[DeliteArray[A]], func: (Exp[A], Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext)
    extends DeliteOpReduce[A] {
    
    val size = copyTransformedOrElse(_.size)(in.length)    
  }  
  
  case class DeliteArrayForeach[A:Manifest](in: Exp[DeliteArray[A]], func: Rep[A] => Rep[Unit]) extends DeliteOpForeach[A] {
    def sync = null //unused
    val size = copyTransformedOrElse(_.size)(in.length)
    val mA = manifest[A]
  }
  case class DeliteArrayMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])
    extends DeliteOpFilter[A,B,DeliteArray[B]] {

    override def alloc(len: Exp[Int]) = DeliteArray[B](len)
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class DeliteArrayFromFunction[A:Manifest](length: Rep[Int], func: Exp[Int] => Exp[A]) extends DeliteOpMapIndices[A,DeliteArray[A]] {
    val size = copyTransformedOrElse(_.size)(length)
    override def alloc(len: Exp[Int]) = DeliteArray[A](len)
  }

  case class DeliteArrayFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray[A]], func: Exp[A] => Exp[DeliteArray[B]])
    extends DeliteOpFlatMap[A,B,DeliteArray[B]] {

    override def alloc(len: Exp[Int]) = DeliteArray[B](len)
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class DeliteArraySortIndices[A:Manifest](length: Rep[Int], sV: (Sym[Int],Sym[Int]), comparator: Block[Int]) extends DefWithManifest[A,DeliteArray[Int]]
  
   
  /////////////////////
  // delite collection
    
  def isDeliteArray[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[DeliteArray[A]])  
  def asDeliteArray[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DeliteArray[A]]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDeliteArray(x)) asDeliteArray(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) asDeliteArray(x).apply(n)
    else super.dc_apply(x,n)
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) asDeliteArray(x).update(n,y)
    else super.dc_update(x,n,y)        
  }

  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) {
      val arr = asDeliteArray(x)
      if (arr.length > y) { //trim
        val newArr = DeliteArray[A](y)
        darray_unsafe_copy(arr, unit(0), newArr, unit(0), y)
        darray_unsafe_set_act_buf(newArr)
      }
      unit(())
    }
    else super.dc_set_logical_size(x,y)        
  }
  
  override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) {
      if (hasConditions) ParSimpleBuffer else ParFlat
    }
    else super.dc_parallelization(x, hasConditions)
  }
  override def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) { unit(true) }
    else super.dc_appendable(x,i,y)
  }

  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) {
      val arr = asDeliteArray(x)
      val size = darray_unsafe_get_act_size
      val length = arr.length
      if (size >= length) {
        val n = if (length < unit(16)) unit(16) else length*unit(2)
        val newArr = DeliteArray[A](n)
        darray_copy(arr, unit(0), newArr, unit(0), length)
        newArr(size) = y
        darray_unsafe_set_act_buf(newArr)
      }
      else {
        arr(size) = y
      }
    }
    else super.dc_append(x,i,y)
  }
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isDeliteArray(x)) DeliteArray[A](size).asInstanceOf[Exp[CA]]
    else super.dc_alloc[A,CA](x,size)
  } 
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isDeliteArray(src) && isDeliteArray(dst)) {
      darray_copy(asDeliteArray(src), srcPos, asDeliteArray(dst), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }


  //////////////////
  // public methods
    
  def darray_new[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(DeliteArrayNew(length,manifest[T]))
  def darray_new_immutable[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteArrayNew(length,manifest[T]))
  def darray_length[T:Manifest](da: Exp[DeliteArray[T]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayLength[T](da))
  def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteArrayApply[T](da,i))
  
  /* 
   * rewrites to make update operations atomic when the array is nested within another object (Variable, Struct)
   * these allow DSL authors to create data structures such as Var(Array), access them normally, and still work with the effects system
   * by preventing mutable aliases, i.e. preventing the compiler from ever sharing a reference to anything but the outermost object   
   */
  def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T])(implicit ctx: SourceContext) = da match {
    case Def(Field(struct,name)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructUpdate[T](s,f,List(i),x))) //Struct(Array)
    case Def(Reflect(Field(struct,name),u,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructUpdate[T](s,f,List(i),x))) //Struct(Array) 
    case Def(Reflect(ReadVar(Variable(Def(Reflect(Field(struct,name),_,_)))),_,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(struct)(StructUpdate[T](s,f,List(i),x))) //Struct(Var(Array))
    case Def(Reflect(ReadVar(v),_,_)) => reflectWrite(v.e)(VarUpdate[T](v,i,x)) //Var(Array)
    case Def(DeliteArrayApply(arr,j)) => recurseFields(arr, Nil, (s,f) => reflectWrite(s)(StructUpdate[T](s,f,List(j,i),x))) //Struct(Array(Array)) //TODO: generalize to arbitrary array nests?
    case _ => reflectWrite(da)(DeliteArrayUpdate[T](da,i,x))
  }

  private def recurseFields[T:Manifest](struct: Exp[Any], fields: List[String], result: (Exp[Any],List[String]) => Exp[Unit]): Exp[Unit] = struct match {
    case Def(Field(s,name)) => recurseFields(s, name :: fields, result)
    case Def(Reflect(Field(s,name),_,_)) => recurseFields(s, name :: fields, result)
    case _ => result(struct, fields)
  }
  
  //should ideally express all update-like operations in terms of darray_update in order to avoid this duplication, but we want to special-case darray_copy codegen
  def darray_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = dest match {
    case Def(Field(struct,name)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructCopy[T](src,srcPos,s,f,List(destPos),len))) //Struct(Array)
    case Def(Reflect(Field(struct,name),_,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructCopy[T](src,srcPos,s,f,List(destPos),len))) //Struct(Array)
    case Def(Reflect(ReadVar(Variable(Def(Reflect(Field(struct,name),_,_)))),_,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructCopy[T](src,srcPos,s,f,List(destPos),len))) //Struct(Var(Array))
    case Def(Reflect(ReadVar(v),_,_)) => reflectWrite(v.e)(VarCopy[T](src, srcPos, v, destPos, len)) //Var(Array)
    case Def(DeliteArrayApply(arr,j)) => recurseFields(arr, Nil, (s,f) => reflectWrite(s)(StructCopy[T](src,srcPos,s,f,List(j,destPos),len))) //Struct(Array(Array))
    case _ => reflectWrite(dest)(DeliteArrayCopy(src,srcPos,dest,destPos,len))  
  }

  def darray_map[A:Manifest,B:Manifest](a: Exp[DeliteArray[A]], f: Exp[A] => Exp[B])(implicit ctx: SourceContext) = reflectPure(DeliteArrayMap(a,f))   
  def darray_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[DeliteArray[A]], y: Rep[DeliteArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext) = reflectPure(DeliteArrayZipWith(x,y,f))
  def darray_reduce[A:Manifest](x: Exp[DeliteArray[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteArrayReduce(x,f,zero))
  def darray_foreach[A:Manifest](x: Rep[DeliteArray[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteArrayForeach(x,f)
    reflectEffect(df, summarizeEffects(df.body.asInstanceOf[DeliteForeachElem[A]].func).star andAlso Simple())
  }
  def darray_filter[A:Manifest](x: Exp[DeliteArray[A]], f: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = darray_mapfilter(x, (e:Exp[A]) => e, f)
  def darray_groupByReduce[A:Manifest,K:Manifest,V:Manifest](da: Rep[DeliteArray[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext) = DeliteMap(da, key, value, reduce)
  def darray_flatmap[A:Manifest,B:Manifest](da: Rep[DeliteArray[A]], func: Rep[A] => Rep[DeliteArray[B]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayFlatMap(da,func))
  def darray_mkstring[A:Manifest](a: Exp[DeliteArray[A]], del: Exp[String])(implicit ctx: SourceContext) = reflectPure(DeliteArrayMkString(a,del))
  def darray_union[A:Manifest](lhs: Exp[DeliteArray[A]], rhs: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayUnion(lhs,rhs))
  def darray_intersect[A:Manifest](lhs: Exp[DeliteArray[A]], rhs: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayIntersect(lhs,rhs))
  def darray_take[A:Manifest](lhs: Exp[DeliteArray[A]], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteArrayTake(lhs,n))
  def darray_sort[A:Manifest](lhs: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteArraySort(lhs))
  def darray_range(st: Exp[Int], en: Exp[Int])(implicit ctx: SourceContext) = darray_fromfunction(en-st, i => i+st)
  def darray_mapfilter[A:Manifest,B:Manifest](lhs: Exp[DeliteArray[A]], map: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DeliteArrayMapFilter(lhs,map,cond))
  def darray_toseq[A:Manifest](a: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = DeliteArrayToSeq(a)
  def darray_fromfunction[T:Manifest](length: Rep[Int], func: Rep[Int] => Rep[T])(implicit ctx: SourceContext) = reflectPure(DeliteArrayFromFunction(length,func))
  
  def darray_mutable[T:Manifest](da: Rep[DeliteArray[T]])(implicit ctx: SourceContext) = reflectMutable(DeliteArrayMap(da,(e:Rep[T])=>e))
  def darray_sortIndices(length: Exp[Int], comparator: (Exp[Int], Exp[Int]) => Exp[Int])(implicit ctx: SourceContext) = {
    val sV = (fresh[Int],fresh[Int])
    reflectPure(DeliteArraySortIndices(length, sV, reifyEffects(comparator(sV._1,sV._2))))
  }

  /////////////
  // internal
  
  def darray_unsafe_update[T:Manifest](x: Exp[DeliteArray[T]], n: Exp[Int], y: Exp[T])(implicit ctx: SourceContext) = DeliteArrayUpdate(x,n,y)
  def darray_unsafe_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = DeliteArrayCopy(src,srcPos,dest,destPos,len)  
  
  def darray_set_act_buf[A:Manifest](da: Exp[DeliteArray[A]]) = reflectEffect(DeliteArraySetActBuffer(da), Write(List(da.asInstanceOf[Sym[Any]])) andAlso Simple())
  def darray_unsafe_set_act_buf[A:Manifest](da: Exp[DeliteArray[A]]) = reflectEffect(DeliteArraySetActBuffer(da))
  def darray_unsafe_get_act_size(): Exp[Int] = reflectEffect(DeliteArrayGetActSize())


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case SimpleStruct(SoaTag(tag, length), elems) => struct(SoaTag(tag, f(length)), elems map { case (k,v) => (k, f(v)) })      
      case e@DeliteArrayNew(l,m) => darray_new_immutable(f(l))(m,pos)
      case DeliteArrayLength(a) => darray_length(f(a))
      case e@DeliteArrayApply(a,x) => darray_apply(f(a),f(x))(e.mA,pos)
      case e@DeliteArrayTake(a,x) => darray_take(f(a),f(x))(e.mA,pos)
      case e@DeliteArrayMkString(a,s) => darray_mkstring(f(a),f(s))(e.mA,pos)
      case e@DeliteArraySort(x) => darray_sort(f(x))(e.mA,pos)
      case e@DeliteArrayUpdate(l,i,r) => darray_unsafe_update(f(l),f(i),f(r))
      case e@DeliteArrayCopy(a,ap,d,dp,l) => toAtom(DeliteArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.mA))(mtype(manifest[A]),pos)
      case e@DeliteArrayMap(in,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayMap(f(in),f(g))(e.dmA,e.dmB,pos))(mtype(manifest[A]),pos)      
      case e@DeliteArrayFlatMap(in,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayFlatMap(f(in),f(g))(e.dmA,e.dmB))(mtype(manifest[A]),pos)      
      case e@DeliteArrayZipWith(inA,inB,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayZipWith(f(inA),f(inB),f(g))(e.dmA,e.dmB,e.dmR))(mtype(manifest[A]),pos)
      case e@DeliteArrayReduce(in,g,z) => 
        e.asInstanceOf[DeliteArrayReduce[A]] match { //scalac typer bug
          case e@DeliteArrayReduce(in,g,z) => 
            reflectPure(new { override val original = Some(f,e) } with DeliteArrayReduce[A](f(in),f(g),f(z))(mtype(e.dmA),pos))(mtype(e.dmA),pos)
        }
      case e@DeliteArrayMapFilter(in,g,c) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayMapFilter(f(in),f(g),f(c))(e.dmA,e.dmB))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DeliteArrayFromFunction(l,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayFromFunction(f(l),f(g))(e.dmA))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DeliteArraySortIndices(l,s,c) => reflectPure(DeliteArraySortIndices(f(l), (f(s._1).asInstanceOf[Sym[Int]],f(s._2).asInstanceOf[Sym[Int]]), f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
      case Reflect(SimpleStruct(SoaTag(tag, length), elems), u, es) => reflectMirrored(Reflect(SimpleStruct(SoaTag(tag, f(length)), elems map { case (k,v) => (k, f(v)) }), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayNew(l,m), u, es) => reflectMirrored(Reflect(DeliteArrayNew(f(l),m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayLength(a), u, es) => reflectMirrored(Reflect(DeliteArrayLength(f(a))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayApply(l,r), u, es) => reflectMirrored(Reflect(DeliteArrayApply(f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(DeliteArrayUpdate(f(l),f(i),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)   
      case Reflect(e@StructUpdate(s,n,i,x), u, es) => reflectMirrored(Reflect(StructUpdate(f(s),n,f(i),f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)   
      case Reflect(e@VarUpdate(Variable(a),i,x), u, es) => reflectMirrored(Reflect(VarUpdate(Variable(f(a)),f(i),f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)  
      case Reflect(e@StructCopy(s,sp,st,n,dp,l), u, es) => reflectMirrored(Reflect(StructCopy(f(s),f(sp),f(st),n,f(dp),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)   
      case Reflect(e@VarCopy(s,sp,Variable(a),dp,l), u, es) => reflectMirrored(Reflect(VarCopy(f(s),f(sp),Variable(f(a)),f(dp),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)  
      case Reflect(e@DeliteArrayTake(a,x), u, es) => reflectMirrored(Reflect(DeliteArrayTake(f(a),f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayMkString(a,s), u, es) => reflectMirrored(Reflect(DeliteArrayMkString(f(a),f(s))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArraySort(x), u, es) => reflectMirrored(Reflect(DeliteArraySort(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)     
      case Reflect(e@DeliteArrayCopy(a,ap,d,dp,l), u, es) => reflectMirrored(Reflect(DeliteArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)     
      case Reflect(e@DeliteArrayMap(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayMap(f(in),f(g))(e.dmA,e.dmB,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayFlatMap(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayFlatMap(f(in),f(g))(e.dmA,e.dmB), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayZipWith(inA,inB,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayZipWith(f(inA),f(inB),f(g))(e.dmA,e.dmB,e.dmR), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayReduce(in,g,z), u, es) => 
        e.asInstanceOf[DeliteArrayReduce[A]] match { //scalac typer bug
          case e@DeliteArrayReduce(in,g,z) => 
            reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayReduce(f(in),f(g),f(z))(e.dmA,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
        }
      case Reflect(e@DeliteArrayMapFilter(in,g,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayMapFilter(f(in),f(g),f(c))(e.dmA,e.dmB), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayFromFunction(l,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayFromFunction(f(l),f(g))(e.dmA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayGetActSize(), u, es) => reflectMirrored(Reflect(DeliteArrayGetActSize(), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArraySetActBuffer(da), u, es) => reflectMirrored(Reflect(DeliteArraySetActBuffer(f(da))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(e@DeliteArrayForeach(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayForeach(f(in),f(g))(mtype(e.mA)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Def(SimpleStruct(SoaTag(tag, length), elems)) => syms(length) ++ super.syms(e)
    case _ => super.syms(e)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Def(SimpleStruct(SoaTag(tag, length), elems)) => symsFreq(length) ++ super.symsFreq(e)
    case _ => super.symsFreq(e)
  }  
  
  /////////////////////
  // aliases and sharing
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteArrayCopy(s,sp,d,dp,l) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(Def(Reflect(DeliteArrayNew(_,_),_,_))) => Nil  //ignore nested mutability for Var(Array): this is only safe because we rewrite mutations on Var(Array) to atomic operations
    case DeliteArrayCopy(s,sp,d,dp,l) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteArrayCopy(s,sp,d,dp,l) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DeliteArrayCopy(s,sp,d,dp,l) => Nil // ??
    case _ => super.copySyms(e)
  }    

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteArraySortIndices(len, v, comp) => syms(v) ++ effectSyms(comp)
    case _ => super.boundSyms(e)
  }

  
}

trait DeliteArrayStructTags extends Base with StructTags {
  case class SoaTag[T,DA <: DeliteArray[T]](base: StructTag[T], length: Rep[Int]) extends StructTag[DA]
}

trait DeliteArrayOpsExpOpt extends DeliteArrayOpsExp with DeliteArrayStructTags with StructExpOptCommon with DeliteStructsExp {
  this: DeliteOpsExp with DeliteMapOpsExp =>

  object Loop {
    def unapply[A](d: Def[A]): Option[(Exp[Int], Sym[Int], Def[A])] = d match {
      case Reflect(l:AbstractLoop[_], u, es) if u == Control() => Some((l.size, l.v, l.body))          
      case l:AbstractLoop[_] => Some((l.size, l.v, l.body))          
      case _ => None
    }
  }

  object StructIR {
    def unapply[A](e: Exp[DeliteArray[A]]): Option[(StructTag[A], Exp[Int], Seq[(String,Exp[DeliteArray[Any]])])] = e match {
      case Def(Struct(SoaTag(tag: StructTag[A],len),elems:Seq[(String,Exp[DeliteArray[Any]])])) => Some((tag,len,elems))
      case Def(Reflect(Struct(SoaTag(tag: StructTag[A],len), elems:Seq[(String,Exp[DeliteArray[Any]])]), u, es)) => Some((tag,len,elems))
      case _ => None
    }
  }
  

  //choosing the length of the first array creates an unnecessary dependency (all arrays must have same length), so we store length in the tag
  override def darray_length[T:Manifest](da: Exp[DeliteArray[T]])(implicit ctx: SourceContext) = da match {
    case Def(Loop(size,_,b:DeliteCollectElem[_,_,_])) if b.cond == Nil && b.par == ParFlat => size
    case StructIR(tag, len, elems) => 
      printlog("**** extracted array length: " + len.toString)
      len
    case StructType(tag, fields) if Config.soaEnabled =>
      val z = dlength(field(da,fields(0)._1)(mtype(darrayManifest(fields(0)._2)),ctx))(mtype(fields(0)._2),ctx)
      printlog("**** fallback array length: " + z.toString + " of " + da.toString)
      z
    case _ => super.darray_length(da)
  }

  override def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int])(implicit ctx: SourceContext) = da match {
    case StructIR(tag, len, elems) =>
      struct[T](tag, elems.map(p=>(p._1, darray_apply(p._2,i)(argManifest(p._2.tp),ctx))))
    case StructType(tag, fields) if Config.soaEnabled =>
      struct[T](tag, fields.map(p=>(p._1, darray_apply(field(da,p._1)(mtype(darrayManifest(p._2)),ctx),i)(mtype(p._2),ctx))))
    case _ => super.darray_apply(da, i)
  }

  //x more likely to match as a Struct than da?
  override def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T])(implicit ctx: SourceContext) = da match {
    case StructIR(tag, len, elems) =>
      elems.foreach(p=>darray_update(p._2,i,field(x,p._1)(argManifest(p._2.tp),ctx))(argManifest(p._2.tp),ctx))
    case StructType(tag, fields) if Config.soaEnabled =>
      fields.foreach(p=>darray_update(field(da,p._1)(mtype(darrayManifest(p._2)),ctx), i, field(x,p._1)(mtype(p._2),ctx))(mtype(p._2),ctx))
    case _ => super.darray_update(da, i, x)
  }

  override def darray_unsafe_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T])(implicit ctx: SourceContext) = da match {
    case StructIR(tag, len, elems) =>
      elems.foreach(p=>darray_unsafe_update(p._2,i,field(x,p._1)(argManifest(p._2.tp),ctx))(argManifest(p._2.tp),ctx))
    case StructType(tag, fields) if Config.soaEnabled =>
      fields.foreach(p=>darray_unsafe_update(field(da,p._1)(mtype(darrayManifest(p._2)),ctx), i, field(x,p._1)(mtype(p._2),ctx))(mtype(p._2),ctx))
    case _ => super.darray_unsafe_update(da, i, x)
  }

  override def darray_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], length: Exp[Int])(implicit ctx: SourceContext) = dest match {
    case StructIR(tag, _, elems) =>
      elems.foreach{ case (k,v) => darray_copy(field(src,k)(v.tp,ctx), srcPos, v, destPos, length)(argManifest(v.tp),ctx) }
    case StructType(tag, fields) if Config.soaEnabled =>
      fields.foreach{ case (k,tp) => darray_copy(field(src,k)(mtype(darrayManifest(tp)),ctx), srcPos, field(dest,k)(mtype(darrayManifest(tp)),ctx), destPos, length)(mtype(tp),ctx) }
    case _ => super.darray_copy(src, srcPos, dest, destPos, length)
  }

  override def darray_unsafe_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], length: Exp[Int])(implicit ctx: SourceContext) = dest match {
    case StructIR(tag, _, elems) =>
      elems.foreach{ case (k,v) => darray_unsafe_copy(field(src,k)(v.tp,ctx), srcPos, v, destPos, length)(argManifest(v.tp),ctx) }
    case StructType(tag, fields) if Config.soaEnabled =>
      fields.foreach{ case (k,tp) => darray_unsafe_copy(field(src,k)(mtype(darrayManifest(tp)),ctx), srcPos, field(dest,k)(mtype(darrayManifest(tp)),ctx), destPos, length)(mtype(tp),ctx) }
    case _ => super.darray_unsafe_copy(src, srcPos, dest, destPos, length)
  }

  //TODO: implement in the IR to avoid this
  override def darray_take[T:Manifest](da: Exp[DeliteArray[T]], n: Rep[Int])(implicit ctx: SourceContext) = da match {
    case StructIR(tag, _, elems) =>
      struct[DeliteArray[T]](SoaTag(tag, n), elems.map(p=>(p._1, darray_take(p._2,n)(argManifest(p._2.tp),ctx))))
    case StructType(tag, fields) if Config.soaEnabled =>
      struct[DeliteArray[T]](SoaTag(tag, n), fields.map(p=>(p._1, darray_take(field(da,p._1)(mtype(darrayManifest(p._2)),ctx),n)(mtype(p._2),ctx))))
    case _ => super.darray_take(da, n)
  }


  //when we perform a field update, check if that field actually came from some outer DeliteArray
  override def field_update[T:Manifest](struct: Exp[Any], index: String, rhs: Exp[T]) = struct match {
    case Def(Struct(_,elems)) => elems.find(_._1 == index).get._2 match {
      case Def(DeliteArrayApply(arr:Exp[DeliteArray[T]],j)) => darray_update(arr,j,rhs) //primitive apply node
      case Def(Struct(_,es)) => //struct of apply nodes
        var sArr: Exp[DeliteArray[T]] = null
        var sj: Exp[Int] = null
        def ifEqual(a: Exp[Any], j: Exp[Any]) {
          if (sArr eq null) { sArr = a; sj = j }
          else if (sArr != a || sj != j) super.field_update(struct, index, rhs) //fields not consistent
        }
        for ((idx,sym) <- es) { sym match {
          case Def(DeliteArrayApply(pa,j)) => pa match { //outer physical array
            case Def(FieldApply(arr:Exp[DeliteArray[T]], field)) if field == idx => ifEqual(arr,j) //outer logical array
            case _ => super.field_update(struct, index, rhs)
          }
          case _ => super.field_update(struct, index, rhs)
        } }
        darray_update(sArr,sj,rhs)
      case _ => super.field_update(struct, index, rhs)
    }
    case _ => super.field_update(struct, index, rhs)
  }


  private def argManifest[A,B](m: Manifest[A]): Manifest[B] = m.typeArguments(0).asInstanceOf[Manifest[B]]

  //forwarder to appease type-checker
  private def dnew[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArray[T]] = darray_new(length)
  private def dnewi[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext): Exp[DeliteArray[T]] = darray_new_immutable(length)
  private def dlength[T:Manifest](da: Exp[DeliteArray[T]])(implicit ctx: SourceContext): Exp[Int] = darray_length(da)

  //TODO: if T <: Record, but no RefinedManifest -- how do we map the fields? currently using unapplyStructType as a substitute
  override def darray_new[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext) = manifest[T] match {
    case StructType(tag,fields) if Config.soaEnabled => 
      struct[DeliteArray[T]](SoaTag(tag,length), fields.map(p=>(p._1,dnew(length)(p._2,ctx))))
    case _ => super.darray_new(length)
  }

  override def darray_new_immutable[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext) = manifest[T] match {
    case StructType(tag,fields) if Config.soaEnabled => 
      struct[DeliteArray[T]](SoaTag(tag,length), fields.map(p=>(p._1,dnewi(length)(p._2,ctx))))
    case _ => super.darray_new_immutable(length)
  }

  def darrayManifest(typeArg: Manifest[_]) = makeManifest(classOf[DeliteArray[_]], List(typeArg))

  def deliteArrayPure[T:Manifest](da: Exp[DeliteArray[T]], elems: RefinedManifest[T])(implicit ctx: SourceContext): Exp[DeliteArray[T]] = {
    if (Config.soaEnabled)
      struct[DeliteArray[T]](SoaTag(AnonTag(elems),da.length), elems.fields.map(e=>(e._1, field[DeliteArray[_]](da,e._1)(darrayManifest(e._2),ctx))))
    else
      da
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case NewVar(Def(Reflect(Struct(tag,_),_,_))) if tag.isInstanceOf[SoaTag[_,_]] => Nil //as above for SoA array
    case _ => super.containSyms(e)
  }

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case d if d.erasure == classOf[DeliteArray[_]] && Config.soaEnabled =>
      val elems = unapplyStructType(d.typeArguments(0))
      elems.map { case (tag: StructTag[T],fields) => (tag, fields.map(e => (e._1, darrayManifest(e._2)))) }
    case _ => super.unapplyStructType
  }
}

trait DeliteArrayFatExp extends DeliteArrayOpsExpOpt with StructFatExpOptCommon {
  this: DeliteOpsExp with DeliteMapOpsExp =>
}

trait BaseGenDeliteArrayOps extends GenericFatCodegen {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = e match {
    case DeliteArrayApply(da, idx) => Some((da,idx))
    case _ => super.unapplySimpleIndex(e)
  }

  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    //case DeliteArrayLength(da) => Some(da)
    case DeliteArrayLength(a @ Def(Loop(_,_,_:DeliteCollectElem[_,_,_]))) => Some(a) // exclude hash elems
    case _ => super.unapplySimpleDomain(e)
  }

}

trait ScalaGenDeliteArrayOps extends BaseGenDeliteArrayOps with ScalaGenDeliteStruct with ScalaGenDeliteOps {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  def emitLogOfArrayAllocation(symId: Int, arrayLength: Exp[Int], elemType: String): Unit = {
    stream.println("ppl.delite.runtime.profiler.MemoryProfiler.logArrayAllocation(\"x" + symId + "\", " + quote(arrayLength) + ", \"" + elemType + "\")")    
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n,m) if Config.generateSerializable && isPrimitiveType(m) => 
      emitValDef(sym, "new ppl.delite.runtime.data.LocalDeliteArray" + remap(m) + "(" + quote(n) + ")")
      if (Config.enableProfiler) emitLogOfArrayAllocation(sym.id, n, m.erasure.getSimpleName)
    case a@DeliteArrayNew(n,m) if Config.generateSerializable =>
      emitValDef(sym, "new ppl.delite.runtime.data.LocalDeliteArrayObject[" + remap(m) + "](" + quote(n) + ")")
      if (Config.enableProfiler) emitLogOfArrayAllocation(sym.id, n, m.erasure.getSimpleName)
    case a@DeliteArrayNew(n,m) =>
      emitValDef(sym, "new Array[" + remap(m) + "](" + quote(n) + ")")
      if (Config.enableProfiler) emitLogOfArrayAllocation(sym.id, n, m.erasure.getSimpleName)
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + ".length //" + quotePos(sym))
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ") = " + quote(x))
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) if Config.generateSerializable =>
      emitValDef(sym, quote(src) + ".copy(" + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) =>
      emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    case DeliteArrayTake(lhs,n) =>
      emitValDef(sym, quote(lhs) + ".take(" + quote(n) + ")")
    case DeliteArrayMkString(da,x) if !Config.generateSerializable =>
      emitValDef(sym, quote(da) + ".mkString(" + quote(x) + ")")
    case DeliteArrayUnion(lhs,rhs) if !Config.generateSerializable =>
      emitValDef(sym, quote(lhs) + " union " + quote(rhs))
    case DeliteArrayIntersect(lhs,rhs) if !Config.generateSerializable =>
      emitValDef(sym, quote(lhs) + " intersect " + quote(rhs))  
    case a@DeliteArraySort(x) if !Config.generateSerializable => 
      stream.println("val " + quote(sym) + " = {")
      stream.println("val d = new Array[" + remap(a.mA) + "](" + quote(x) + ".length" + ")")
      stream.println("System.arraycopy(" + quote(x) + ", 0, d, 0, " + quote(x) + ".length)")
      stream.println("generated.scala.container.SortingImpl.sort(d)")
      stream.println("d")
      stream.println("}")    
    case a@DeliteArraySortIndices(len,sV,comp) if !Config.generateSerializable =>
      stream.println("val " + quote(sym) + " = {")
      stream.println("val len = " + quote(len))
      stream.println("val comp = new generated.scala.container.Comparator {")
      stream.println("def compare(o1: Int, o2: Int): Int = {")
      emitValDef(sV._1, "o1")
      emitValDef(sV._2, "o2")
      emitBlock(comp)
      stream.println(quote(getBlockResult(comp)))
      stream.println("} }")
      stream.println("val d = new Array[Int](len)")
      stream.println("var i = 0; while(i < len) { d(i) = i; i += 1 }")
      stream.println("generated.scala.container.SortingImpl.sort(d,comp)")
      stream.println("d")
      stream.println("}")
    case DeliteArrayToSeq(a) if !Config.generateSerializable => 
      emitValDef(sym, quote(a) + ".toSeq")
    case StructUpdate(struct, fields, idx, x) =>
      emitValDef(sym, quote(struct) + "." + fields.mkString(".") + idx.map(e=>"("+quote(e)+")").mkString("") + " = " + quote(x))
    case VarUpdate(Variable(a), idx, x) =>
      val readVar = if (deliteInputs contains a) ".get" else ""
      emitValDef(sym, quote(a) + readVar + "(" + quote(idx) + ") = " + quote(x))
    case StructCopy(src,srcPos,struct,fields,destPos,len) if Config.generateSerializable =>
      val dest = quote(struct) + "." + fields.mkString(".") + destPos.take(destPos.length-1).map(e=>"("+quote(e)+")").mkString("")
      emitValDef(sym, quote(src) + ".copy(" + quote(srcPos) + "," + dest + "," + quote(destPos(destPos.length-1)) + "," + quote(len) + ")")
    case StructCopy(src,srcPos,struct,fields,destPos,len) =>
      val dest = quote(struct) + "." + fields.mkString(".") + destPos.take(destPos.length-1).map(e=>"("+quote(e)+")").mkString("")
      emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + dest + "," + quote(destPos(destPos.length-1)) + "," + quote(len) + ")")
    case VarCopy(src,srcPos,Variable(a),destPos,len) if Config.generateSerializable =>
      val dest = quote(a) + (if (deliteInputs contains a) ".get" else "")
      emitValDef(sym, quote(src) + ".copy(" + quote(srcPos) + "," + dest + "," + quote(destPos) + "," + quote(len) + ")")
    case VarCopy(src,srcPos,Variable(a),destPos,len) =>
      val dest = quote(a) + (if (deliteInputs contains a) ".get" else "")
      emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + dest + "," + quote(destPos) + "," + quote(len) + ")")
    case DeliteArrayGetActSize() =>
      emitValDef(sym, getActSize)
    case DeliteArraySetActBuffer(da) =>
      emitValDef(sym, getActBuffer.head + " = " + quote(da))
      getActBuffer.tail.foreach(buf => stream.println(buf + " = " + quote(da)))
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => m.typeArguments(0) match {
      case StructType(_,_) if Config.soaEnabled => super.remap(m)
      case s if s <:< manifest[Record] && Config.soaEnabled => super.remap(m) // occurs due to restaging
      case arg if isPrimitiveType(arg) && Config.generateSerializable => "ppl.delite.runtime.data.DeliteArray" + remap(arg)
      case arg if Config.generateSerializable => "ppl.delite.runtime.data.DeliteArrayObject[" + remap(arg) + "]"
      case arg => "Array[" + remap(arg) + "]"
    }
    case _ => super.remap(m)
  }

}

trait CLikeGenDeliteArrayOps extends BaseGenDeliteArrayOps with CLikeGenDeliteStruct {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def remap[A](m: Manifest[A]): String = {
    if (isArrayType(m)) {
      m.typeArguments.head match {
        case StructType(_,_) if Config.soaEnabled => super.remap(m)
        case s if s <:< manifest[Record] && Config.soaEnabled => super.remap(m) // occurs due to restaging
        case arg if (cppMemMgr == "refcnt") => wrapSharedPtr(deviceTarget + "DeliteArray" + unwrapSharedPtr(remap(arg)))
        case arg => deviceTarget + "DeliteArray" + remap(arg)
      }
    }
    else
      super.remap(m)
  }

  override def remapHost[A](m: Manifest[A]): String = {
    if(isArrayType(m)) {
      m.typeArguments.head match {
        case StructType(_,_) if Config.soaEnabled => super.remapHost(m)
        case s if s <:< manifest[Record] && Config.soaEnabled => super.remapHost(m)
        case arg if (cppMemMgr == "refcnt") => wrapSharedPtr(hostTarget + "DeliteArray" + unwrapSharedPtr(remapHost(arg)))
        case arg => hostTarget + "DeliteArray" + remapHost(arg)
      }
    }
    else
      super.remapHost(m)
  }

  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
    val stream = new PrintWriter(path + deviceTarget + "DeliteArrays.h")
    stream.println("#include \"" + deviceTarget + "DeliteStructs.h\"")
    stream.println("#include \"" + deviceTarget + "HashMap.h\"")
    for((tp,name) <- dsTypesList if(isArrayType(tp))) {
      emitDeliteArray(tp, path, stream)
    }
    stream.close()
  }

  private val generatedDeliteArray = HashSet[String]()

  protected val deliteArrayString: String

  private def emitDeliteArray(m: Manifest[_], path: String, header: PrintWriter) {
    try {
      val mArg = m.typeArguments(0)
      val mString = if (cppMemMgr == "refcnt") unwrapSharedPtr(remap(m)) else remap(m)
      val mArgString = if (cppMemMgr == "refcnt") unwrapSharedPtr(remap(mArg)) else remap(mArg)
      val shouldGenerate = mArg match {
        case StructType(_,_) if Config.soaEnabled => false
        case s if s <:< manifest[Record] && Config.soaEnabled => false
        case _ => true
      }
      if(!generatedDeliteArray.contains(mString) && shouldGenerate) {
        val stream = new PrintWriter(path + mString + ".h")
        stream.println("#ifndef __" + mString + "__")
        stream.println("#define __" + mString + "__")
        if(!isPrimitiveType(mArg)) stream.println("#include \"" + mArgString + ".h\"")
        stream.println(deliteArrayString.replaceAll("__T__",mString).replaceAll("__TARG__",remapWithRef(mArg)))
        stream.println("#endif")
        stream.close()
        header.println("#include \"" + mString + ".h\"")
        generatedDeliteArray.add(mString)
      }
    }
    catch {
      case e: GenerationFailedException => //
      case e: Exception => throw(e)
    }
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"" + deviceTarget + "DeliteArrays.h\"\n")
    if (isAcceleratorTarget) out.append("#include \"" + hostTarget + "DeliteArrays.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }
}

trait CudaGenDeliteArrayOps extends CLikeGenDeliteArrayOps with CudaGenFat with CudaGenDeliteStruct {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n,m) =>
      // If isNestedNode, each thread allocates its own DeliteArray (TODO: Check the allocation does not escape the kernel)
      if(isNestedNode) {
        // If size is known before launching the kernel (same size for all the threads), allocate outside the kernel
        //TODO: automatically figure out which access pattern is the best
        if(deliteInputs.contains(n)) { 
          val allocSym = registerTempAlloc(sym,m,n)
          stream.println(remap(sym.tp) + " " + quote(sym) + "(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",max(2*blockDim.x*gridDim.x,blockDim.x*(1+" + quote(outerLoopSize) + "/blockDim.x)));")
          //stream.println("DeliteArray< " + remap(m) + " > " + quote(sym) + " = DeliteArray< " + remap(m) + " >(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",blockDim.x*gridDim.x);")
        }
        else if (boundMap.contains(n) && deliteInputs.contains(boundMap(n))) {
          val allocSym = registerTempAlloc(sym,m,boundMap(n))
          stream.println(remap(sym.tp) + " " + quote(sym) + "(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",max(2*blockDim.x*gridDim.x,blockDim.x*(1+" + quote(outerLoopSize) + "/blockDim.x)));")
          //stream.println("DeliteArray< " + remap(m) + " > " + quote(sym) + " = DeliteArray< " + remap(m) + " >(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",blockDim.x*gridDim.x);")
        }
        // If size is not known before launching the kernel, use temporary memory
        // TODO: Figure out the size is the same for all the threads
        else {
          stream.println("if (tempMemSize < tempMemUsage[" + quote(outerLoopSym) + "] + sizeof(" + remap(m) + ")*" + quote(n) + ") {")
          stream.println("assert(false);")
          stream.println("}")
          stream.println(remap(m) + " *" + quote(sym) + "Ptr = (" + remap(m) + "*)(tempMemPtr + tempMemUsage[" + quote(outerLoopSym) + "]*" + quote(outerLoopSize) + ");") 
          stream.println("tempMemUsage[" + quote(outerLoopSym) + "] = tempMemUsage[" + quote(outerLoopSym) + "] + sizeof(" + remap(m) + ")*" + quote(n) + ";")
          stream.println(remap(sym.tp) + " " + quote(sym) + "(" + quote(n) + "," + quote(sym) + "Ptr," + quote(outerLoopSym) + "*" + quote(n) + ",1);")
        }
      }
      // Allocated only once for the entire kernel by helper function
      else {
        stream.println(remap(sym.tp) + " *" + quote(sym) + "_ptr = new " + remap(sym.tp) + "(" + quote(n) + ");")
        emitValDef(sym, "*" + quote(sym) + "_ptr;")
      }
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + ".length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + ".apply(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      if(multiDimMapping && (currentLoopLevel < maxLoopLevel))
        stream.println(getInnerLoopGuard + "{" + quote(da) + ".update(" + quote(idx) + "," + quote(x) + "); }")
      else
        stream.println(quote(da) + ".update(" + quote(idx) + "," + quote(x) + ");")
    case StructUpdate(struct, fields, idx, x) =>
      if(idx.size > 1)
        throw new GenerationFailedException("CudaCodegen: Does not support nested array update inside struct.\n")
      if(multiDimMapping && (currentLoopLevel < maxLoopLevel))
        stream.println(getInnerLoopGuard + "{" + quote(struct) + "." + fields.reduceLeft(_ + "." + _) + ".update(" + quote(idx.head) + "," + quote(x) + "); }")
      else
        stream.println(quote(struct) + "." + fields.reduceLeft(_ + "." + _) + ".update(" + quote(idx.head) + "," + quote(x) + ");")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) =>
      stream.println("for(int i=0; i<"+quote(len)+"; i++) {")
      stream.println(quote(dest) + ".update(" + quote(destPos) + "+i," + quote(src) + ".apply(" + quote(srcPos) + "+i));")
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }

  protected val deliteArrayString = """
#include "DeliteCuda.h"

class __T__ {
public:
    __TARG__ *data;
    int length;
    int offset;
    int stride;
    int flag;

    // Constructors
    __host__ __device__ __T__(void) {
      length = 0;
      data = NULL;
    }

    __host__ __T__(int _length) {
        length = _length;
        offset = 0;
        stride = 1;
        flag = 1;
        DeliteCudaMalloc((void**)&data,length*sizeof(__TARG__));
    }

    __host__ __device__ __T__(int _length, __TARG__ *_data, int _offset) {
        length = _length;
        data = _data;
        offset = _offset *_length;
        stride = 1;
        flag = 1;
    }

    __host__ __device__ __T__(int _length, __TARG__ *_data, int _offset, int _stride) {
        length = _length;
        data = _data;
        offset = _offset;
        stride = _stride;
        flag = 1;
    }

    __host__ __device__ __TARG__ apply(int idx) {
      if(flag!=1)
        return data[offset + (idx % flag) * stride + idx / flag];
      else
        return data[offset + idx * stride];
    }

    __host__ __device__ void update(int idx, __TARG__ value) {
      if(flag!=1)
        data[offset + (idx % flag) * stride + idx / flag] = value;
      else
        data[offset + idx * stride] = value;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ __TARG__ dc_apply(int idx) {
        return apply(idx);
    }

    __host__ __device__ void dc_update(int idx, __TARG__ value) {
        update(idx,value);
    }

    __host__ __device__ void dc_copy(__T__ from) {
      for(int i=0; i<length; i++)
        update(i,from.apply(i));
    }

    __host__ __T__ *dc_alloc(void) {
      return new __T__(length);
    }

    __host__ __T__ *dc_alloc(int size) {
      return new __T__(size);
    }
};
"""

}

trait OpenCLGenDeliteArrayOps extends CLikeGenDeliteArrayOps with OpenCLGenFat with OpenCLGenDeliteStruct {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n,m) =>
      emitValDef(sym, "new Array[" + remap(m) + "](" + quote(n) + ")")
    case DeliteArrayLength(da) =>
      emitValDef(sym, remap(da.tp) + "_size(" + quote(da) + ")")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, remap(da.tp) + "_apply(" + quote(da) + "," + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      stream.println(remap(da.tp) + "_update(" + quote(da) + "," + quote(idx) + "," + quote(x) + ");")
    case _ => super.emitNode(sym, rhs)
  }

  protected val deliteArrayString = "//TODO: fill in"

}

trait CGenDeliteArrayOps extends CLikeGenDeliteArrayOps with CGenDeliteStruct with CGenDeliteOps {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n,m) => 
      // NOTE: DSL operations should not rely on the fact that JVM initializes arrays with 0
      if (cppMemMgr == "refcnt")  
        stream.println(remap(sym.tp) + " " + quote(sym) + "(new " + unwrapSharedPtr(remap(sym.tp)) + "(" + quote(n) + "), " + unwrapSharedPtr(remap(sym.tp)) + "D());")
      else
        emitValDef(sym, "new (" + resourceInfoSym + ".thread_id) " + remap(sym.tp) + "(" + quote(n) + ", " + resourceInfoSym + ".thread_id)")
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + "->length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + "->apply(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      stream.println(quote(da) + "->update(" + quote(idx) + ", " + quote(x) + ");")
    case StructUpdate(struct, fields, idx, x) =>
      val nestedApply = if (idx.length > 1) idx.take(idx.length-1).map(i=>"apply("+quote(i)+")").mkString("","->","->") else ""
      stream.println(quote(struct) + fields.mkString("->","->","->") + nestedApply + "update(" + quote(idx(idx.length-1)) + "," + quote(x) + ");")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) =>
      stream.println("if((" + quote(src) + "->data==" + quote(dest) + "->data) && (" + quote(srcPos) + "<" + quote(destPos) + "))")
      stream.println("std::copy_backward(" + quote(src) + "->data+" + quote(srcPos) + "," + quote(src) + "->data+" + quote(srcPos) + "+" + quote(len) + "," + quote(dest) + "->data+" + quote(destPos) + "+" + quote(len) + ");")
      stream.println("else ")
      stream.println("std::copy(" + quote(src) + "->data+" + quote(srcPos) + "," + quote(src) + "->data+" + quote(srcPos) + "+" + quote(len) + "," + quote(dest) + "->data+" + quote(destPos) + ");")
    case sc@StructCopy(src,srcPos,struct,fields,destPos,len) =>
      val nestedApply = if (destPos.length > 1) destPos.take(destPos.length-1).map(i=>"apply("+quote(i)+")").mkString("","->","->") else ""
      val dest = quote(struct) + fields.mkString("->","->","->") + nestedApply
      stream.println("if((" + quote(src) + "->data==" + dest + "data) && (" + quote(srcPos) + "<" + quote(destPos(destPos.length-1)) + "))")
      stream.println("std::copy_backward(" + quote(src) + "->data+" + quote(srcPos) + "," + quote(src) + "->data+" + quote(srcPos) + "+" + quote(len) + "," + dest + "data+" + quote(destPos(destPos.length-1)) + "+" + quote(len) + ");")
      stream.println("else")
      stream.println("std::copy(" + quote(src) + "->data+" + quote(srcPos) + "," + quote(src) + "->data+" + quote(srcPos) + "+" + quote(len) + "," + dest + "data+" + quote(destPos(destPos.length-1)) + ");")
    case DeliteArrayUnion(lhs,rhs) =>
      emitValDef(sym, quote(lhs) + "->arrayunion(" + quote(rhs) + ")")
    case DeliteArrayIntersect(lhs,rhs) =>
      emitValDef(sym, quote(lhs) + "->intersect(" + quote(rhs) + ")")
    case DeliteArrayTake(lhs,n) =>
      emitValDef(sym, quote(lhs) + "->take(" + quote(n) + ")")
    case DeliteArrayGetActSize() =>
      emitValDef(sym, getActSize)
    case DeliteArraySetActBuffer(da) =>
      stream.println(getActBuffer.head + " = " + quote(da) + ";")
      getActBuffer.tail.foreach(buf => stream.println(buf + " = " + quote(da) + ";"))
    case _ => super.emitNode(sym, rhs)
  }

  protected val deliteArrayString = """
#include "DeliteNamespaces.h"
#include "DeliteMemory.h"
class __T__ : public DeliteMemory {
public:
  __TARG__ *data;
  int length;

  __T__(int _length, int heapIdx): data((__TARG__ *)(DeliteHeapAlloc(sizeof(__TARG__)*_length,heapIdx))), length(_length) { }

  __T__(int _length): data((__TARG__ *)(new __TARG__[_length])), length(_length) { }

  __T__(__TARG__ *_data, int _length) {
    data = _data;
    length = _length;
  }

  __TARG__ apply(int idx) {
    return data[idx];
  }

  void update(int idx, __TARG__ val) {
    data[idx] = val;
  }

  void print(void) {
    printf("length is %d\n", length);
  }

  bool equals(__T__ *to) {
    return this == this;
  }

  uint32_t hashcode(void) {
    return (uintptr_t)this;
  }
};

struct __T__D {
  void operator()(__T__ *p) {
    //printf("__T__: deleting %p\n",p);
    delete[] p->data;
  }
};

"""
}
