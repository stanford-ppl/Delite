package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import scala.virtualization.lms.common._
import ppl.delite.framework.ops._
import ppl.delite.framework.Util._
import ppl.delite.framework.Config


trait DeliteArray[T] extends DeliteCollection[T] 

trait DeliteArrayOps extends StringOps {
  
  object DeliteArray {
    def apply[T:Manifest](length: Rep[Int])(implicit ctx: SourceContext) = darray_new(length)
    def imm[T:Manifest](length: Rep[Int])(implicit ctx: SourceContext) = darray_new_immutable(length)
  }

  implicit def repDArrayToDArrayOps[T:Manifest](da: Rep[DeliteArray[T]]) = new DeliteArrayOpsCls(da)

  class DeliteArrayOpsCls[T:Manifest](da: Rep[DeliteArray[T]]) {
    def length: Rep[Int] = darray_length(da)
    def apply(i: Rep[Int]): Rep[T] = darray_apply(da,i)
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = darray_update(da,i,x)
    def map[B:Manifest](f: Rep[T] => Rep[B]) = darray_map(da,f)
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteArray[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteArray[R]] = darray_zipwith(da,y,f)
    def reduce(f: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] = darray_reduce(da,f,zero)
    def filter(f: Rep[T] => Rep[Boolean]) = darray_filter(da,f)
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
  def darray_copy[T:Manifest](src: Rep[DeliteArray[T]], srcPos: Rep[Int], dest: Rep[DeliteArray[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def darray_map[A:Manifest,B:Manifest](a: Rep[DeliteArray[A]], f: Rep[A] => Rep[B]): Rep[DeliteArray[B]]    
  def darray_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[DeliteArray[A]], y: Rep[DeliteArray[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[DeliteArray[R]]
  def darray_reduce[A:Manifest](x: Rep[DeliteArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A]): Rep[A]
  def darray_filter[A:Manifest](x: Rep[DeliteArray[A]], f: Rep[A] => Rep[Boolean]): Rep[DeliteArray[A]]
  def darray_mkstring[A:Manifest](a: Rep[DeliteArray[A]], del: Rep[String]): Rep[String]
  def darray_union[A:Manifest](lhs: Rep[DeliteArray[A]], rhs: Rep[DeliteArray[A]]): Rep[DeliteArray[A]]
  def darray_intersect[A:Manifest](lhs: Rep[DeliteArray[A]], rhs: Rep[DeliteArray[A]]): Rep[DeliteArray[A]]
  def darray_take[A:Manifest](lhs: Rep[DeliteArray[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def darray_sort[A:Manifest](lhs: Rep[DeliteArray[A]]): Rep[DeliteArray[A]]
  def darray_range(st: Rep[Int], en: Rep[Int]): Rep[DeliteArray[Int]]
  def darray_toseq[A:Manifest](a: Rep[DeliteArray[A]]): Rep[Seq[A]]

  def darray_set_act_buf[A:Manifest](da: Rep[DeliteArray[A]]): Rep[Unit]
  def darray_set_act_final[A:Manifest](da: Rep[DeliteArray[A]]): Rep[Unit]
}

trait DeliteArrayCompilerOps extends DeliteArrayOps {
  def darray_unsafe_update[T:Manifest](x: Rep[DeliteArray[T]], n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def darray_unsafe_copy[T:Manifest](src: Rep[DeliteArray[T]], srcPos: Rep[Int], dest: Rep[DeliteArray[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

trait DeliteArrayOpsExp extends DeliteArrayCompilerOps with DeliteArrayStructTags with DeliteCollectionOpsExp with DeliteStructsExp with EffectExp with PrimitiveOpsExp {
  this: DeliteOpsExp =>
  
  case class DeliteArrayNew[T:Manifest](length: Exp[Int]) extends DefWithManifest[T,DeliteArray[T]] 
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
  case class DeliteArrayRange(st: Exp[Int], en: Exp[Int]) extends Def[DeliteArray[Int]]
  case class DeliteArrayToSeq[A:Manifest](x: Exp[DeliteArray[A]]) extends Def[Seq[A]]

  //this is a hack to make writes to DeliteArray within a Struct an atomic operation in order to avoid creating mutable aliases
  //fortunately due to our limited data structure design this trick isn't required all over the place
  case class StructUpdate[T:Manifest](struct: Exp[Any], fields: List[String], i: Exp[Int], x: Exp[T]) extends DefWithManifest[T,Unit]
  case class VarUpdate[T:Manifest](v: Var[DeliteArray[T]], i: Exp[Int], x: Exp[T]) extends DefWithManifest[T,Unit]
  case class StructCopy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], destStruct: Exp[Any], fields: List[String], destPos: Exp[Int], len: Exp[Int]) extends DefWithManifest[T,Unit]
  case class VarCopy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Var[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int]) extends DefWithManifest[T,Unit]

  //this is a hack to make DeliteArray implement the buffer interface within Delite Ops without having to wrap the DeliteArray in a DeliteArrayBuffer
  case class DeliteArraySetActBuffer[T:Manifest](da: Exp[DeliteArray[T]]) extends DefWithManifest[T,Unit]
  case class DeliteArraySetActFinal[T:Manifest](da: Exp[DeliteArray[T]]) extends DefWithManifest[T,Unit]
  // switched because of a NoSuchMethodError problem when matching on the case object in other traits..
  // case object DeliteArrayGetActSize extends Def[Int]
  case class DeliteArrayGetActSize() extends Def[Int]

  //////////////////
  // delite ops
  
  case class DeliteArrayMap[A:Manifest,B:Manifest](in: Exp[DeliteArray[A]], func: Exp[A] => Exp[B])
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
  
  case class DeliteArrayReduce[A:Manifest](in: Exp[DeliteArray[A]], func: (Exp[A], Exp[A]) => Exp[A], zero: Exp[A])
    extends DeliteOpReduce[A] {
    
    val size = copyTransformedOrElse(_.size)(in.length)    
  }  
  
  case class DeliteArrayMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])
    extends DeliteOpFilter[A,B,DeliteArray[B]] {

    override def alloc(len: Exp[Int]) = DeliteArray[B](len)
    val size = copyTransformedOrElse(_.size)(in.length)
  }
  
   
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
        darray_unsafe_set_act_final(newArr)
      }
      unit(())
    }
    else super.dc_set_logical_size(x,y)        
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
      darray_unsafe_copy(asDeliteArray(src), srcPos, asDeliteArray(dst), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }

  def darray_set_act_buf[A:Manifest](da: Exp[DeliteArray[A]]) = reflectEffect(DeliteArraySetActBuffer(da), Write(List(da.asInstanceOf[Sym[Any]])) andAlso Simple())
  def darray_set_act_final[A:Manifest](da: Exp[DeliteArray[A]]) = reflectEffect(DeliteArraySetActFinal(da), Write(List(da.asInstanceOf[Sym[Any]])) andAlso Simple())
  def darray_unsafe_set_act_buf[A:Manifest](da: Exp[DeliteArray[A]]) = reflectEffect(DeliteArraySetActBuffer(da))
  def darray_unsafe_set_act_final[A:Manifest](da: Exp[DeliteArray[A]]) = reflectEffect(DeliteArraySetActFinal(da))
  def darray_unsafe_get_act_size(): Exp[Int] = reflectEffect(DeliteArrayGetActSize())

  /* override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) {
      if (hasConditions == true) throw new UnsupportedOperationException("DeliteArray: cannot have conditional Delite ops with a DeliteArray as input")
      ParFlat
    }
    else super.dc_parallelization(x, hasConditions)
  } */
  
    
  def darray_new[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(DeliteArrayNew[T](length))
  def darray_new_immutable[T:Manifest](length: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteArrayNew[T](length))
  def darray_length[T:Manifest](da: Exp[DeliteArray[T]])(implicit ctx: SourceContext) = reflectPure(DeliteArrayLength[T](da))
  def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteArrayApply[T](da,i))
  
  /* 
   * rewrites to make update operations atomic when the array is nested within another object (Variable, Struct)
   * these allow DSL authors to create data structures such as Var(Array), access them normally, and still work with the effects system
   * by preventing mutable aliases, i.e. preventing the compiler from ever sharing a reference to anything but the outermost object   
   */
  def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T])(implicit ctx: SourceContext) = da match {
    case Def(Field(struct,name)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructUpdate[T](s,f,i,x))) //Struct(Array)
    case Def(Reflect(Field(struct,name),_,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(struct)(StructUpdate[T](s,f,i,x))) //Struct(Array)
    case Def(Reflect(ReadVar(Variable(Def(Reflect(Field(struct,name),_,_)))),_,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(struct)(StructUpdate[T](s,f,i,x))) //Struct(Var(Array))
    case Def(Reflect(ReadVar(v),_,_)) => reflectWrite(v.e)(VarUpdate[T](v,i,x)) //Var(Array)
    case _ => reflectWrite(da)(DeliteArrayUpdate[T](da,i,x))
  }

  private def recurseFields[T:Manifest](struct: Exp[Any], fields: List[String], result: (Exp[Any],List[String]) => Exp[Unit]): Exp[Unit] = struct match {
    case Def(Field(s,name)) => recurseFields(s, name :: fields, result)
    case Def(Reflect(Field(s,name),_,_)) => recurseFields(s, name :: fields, result)
    case _ => result(struct, fields)
  }
  
  //should ideally express all update-like operations in terms of darray_update in order to avoid this duplication, but we want to special-case darray_copy codegen
  def darray_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = dest match {
    case Def(Field(struct,name)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructCopy[T](src,srcPos,s,f,destPos,len))) //Struct(Array)
    case Def(Reflect(Field(struct,name),_,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructCopy[T](src,srcPos,s,f,destPos,len))) //Struct(Array)
    case Def(Reflect(ReadVar(Variable(Def(Reflect(Field(struct,name),_,_)))),_,_)) => recurseFields(struct, List(name), (s,f) => reflectWrite(s)(StructCopy[T](src,srcPos,s,f,destPos,len))) //Struct(Var(Array))
    case Def(Reflect(ReadVar(v),_,_)) => reflectWrite(v.e)(VarCopy[T](src, srcPos, v, destPos, len)) //Var(Array)
    case _ => reflectWrite(dest)(DeliteArrayCopy(src,srcPos,dest,destPos,len))  
  }

  def darray_map[A:Manifest,B:Manifest](a: Exp[DeliteArray[A]], f: Exp[A] => Exp[B]) = reflectPure(DeliteArrayMap(a,f))   
  def darray_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[DeliteArray[A]], y: Rep[DeliteArray[B]], f: (Rep[A],Rep[B]) => Rep[R]) = reflectPure(DeliteArrayZipWith(x,y,f))
  def darray_reduce[A:Manifest](x: Exp[DeliteArray[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A]) = reflectPure(DeliteArrayReduce(x,f,zero))
  def darray_filter[A:Manifest](x: Exp[DeliteArray[A]], f: Exp[A] => Exp[Boolean]) = darray_mapfilter(x, (e:Exp[A]) => e, f)
  def darray_mkstring[A:Manifest](a: Exp[DeliteArray[A]], del: Exp[String]) = reflectPure(DeliteArrayMkString(a,del))
  def darray_union[A:Manifest](lhs: Exp[DeliteArray[A]], rhs: Exp[DeliteArray[A]]) = reflectPure(DeliteArrayUnion(lhs,rhs))
  def darray_intersect[A:Manifest](lhs: Exp[DeliteArray[A]], rhs: Exp[DeliteArray[A]]) = reflectPure(DeliteArrayIntersect(lhs,rhs))
  def darray_take[A:Manifest](lhs: Exp[DeliteArray[A]], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteArrayTake(lhs,n))
  def darray_sort[A:Manifest](lhs: Exp[DeliteArray[A]]) = reflectPure(DeliteArraySort(lhs))
  def darray_range(st: Exp[Int], en: Exp[Int]) = reflectPure(DeliteArrayRange(st,en))
  def darray_mapfilter[A:Manifest,B:Manifest](lhs: Exp[DeliteArray[A]], map: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean]) = reflectPure(DeliteArrayMapFilter(lhs,map,cond))
  def darray_toseq[A:Manifest](a: Exp[DeliteArray[A]]) = DeliteArrayToSeq(a)
  
  /////////////
  // internal
  
  def darray_unsafe_update[T:Manifest](x: Exp[DeliteArray[T]], n: Exp[Int], y: Exp[T])(implicit ctx: SourceContext) = DeliteArrayUpdate(x,n,y)
  def darray_unsafe_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = DeliteArrayCopy(src,srcPos,dest,destPos,len)  


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
    (e match {
      case SimpleStruct(SoaTag(tag, length), elems) => struct(SoaTag(tag, f(length)), elems map { case (k,v) => (k, f(v)) })      
      case e@DeliteArrayNew(l) => darray_new_immutable(f(l))(e.mA,pos)
      case DeliteArrayLength(a) => darray_length(f(a))
      //LANCET
      /*
      case DeliteArrayApply(a,x) => darray_apply(f(a),f(x))
      case DeliteArrayRange(s,e) => darray_range(f(s),f(e))
      case e@DeliteArrayNew(l) => darray_new_immutable(f(l))(e.mA,ctx)
      case e@DeliteArrayTake(a,x) => darray_take(f(a),f(x))(e.mA,ctx)
      case e@DeliteArraySort(x) => darray_sort(f(x))(e.mA)
      case e@DeliteArrayUpdate(l,i,r) => darray_unsafe_update(f(l),f(i),f(r))
      case e@DeliteArrayCopy(a,ap,d,dp,l) => toAtom(DeliteArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DeliteArrayMap(in,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayMap(f(in),f(g))(e.dmA,e.dmB))(mtype(manifest[A]),implicitly[SourceContext])      
      case e@DeliteArrayReduce(in,g,z) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayReduce(f(in),f(g),f(z))(e.dmA))(mtype(manifest[A]),implicitly[SourceContext])      
      case Reflect(e@DeliteArrayNew(l), u, es) => reflectMirrored(Reflect(DeliteArrayNew(f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(DeliteArrayApply(l,r), u, es) => reflectMirrored(Reflect(DeliteArrayApply(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(DeliteArrayLength(a), u, es) => reflectMirrored(Reflect(DeliteArrayLength(f(a)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(DeliteArrayRange(s,e), u, es) => reflectMirrored(Reflect(DeliteArrayRange(f(s),f(e)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(DeliteArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(DeliteArrayUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))   
      case Reflect(DeliteArrayTake(a,x), u, es) => reflectMirrored(Reflect(DeliteArrayTake(f(a),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(StructUpdate(s,n,i,x), u, es) => reflectMirrored(Reflect(StructUpdate(f(s),n,f(i),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))   
      case Reflect(VarUpdate(Variable(a),i,x), u, es) => reflectMirrored(Reflect(VarUpdate(Variable(f(a)),f(i),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))*/  
      case e@DeliteArrayApply(a,x) => darray_apply(f(a),f(x))(e.mA,pos)
      case e@DeliteArrayTake(a,x) => darray_take(f(a),f(x))(e.mA,pos)
      case e@DeliteArraySort(x) => darray_sort(f(x))(e.mA)
      case e@DeliteArrayUpdate(l,i,r) => darray_unsafe_update(f(l),f(i),f(r))
      case e@DeliteArrayCopy(a,ap,d,dp,l) => toAtom(DeliteArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.mA))(mtype(manifest[A]),pos)
      case e@DeliteArrayMap(in,g) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayMap(f(in),f(g))(e.dmA,e.dmB))(mtype(manifest[A]),pos)      
      case e@DeliteArrayReduce(in,g,z) => 
        e.asInstanceOf[DeliteArrayReduce[A]] match { //scalac typer bug
          case e@DeliteArrayReduce(in,g,z) => 
            reflectPure(new { override val original = Some(f,e) } with DeliteArrayReduce[A](f(in),f(g),f(z))(mtype(e.dmA)))(mtype(e.dmA),pos)
        }
      case e@DeliteArrayMapFilter(in,g,c) => reflectPure(new { override val original = Some(f,e) } with DeliteArrayMapFilter(f(in),f(g),f(c))(e.dmA,e.dmB))(mtype(manifest[A]),implicitly[SourceContext])
      case Reflect(SimpleStruct(SoaTag(tag, length), elems), u, es) => reflectMirrored(Reflect(SimpleStruct(SoaTag(tag, f(length)), elems map { case (k,v) => (k, f(v)) }), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArrayNew(l), u, es) => reflectMirrored(Reflect(DeliteArrayNew(f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArrayLength(a), u, es) => reflectMirrored(Reflect(DeliteArrayLength(f(a))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArrayApply(l,r), u, es) => reflectMirrored(Reflect(DeliteArrayApply(f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArrayRange(s,e1), u, es) => reflectMirrored(Reflect(DeliteArrayRange(f(s),f(e1)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(DeliteArrayUpdate(f(l),f(i),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
      case Reflect(e@DeliteArrayTake(a,x), u, es) => reflectMirrored(Reflect(DeliteArrayTake(f(a),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@StructUpdate(s,n,i,x), u, es) => reflectMirrored(Reflect(StructUpdate(f(s),n,f(i),f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
      case Reflect(e@VarUpdate(Variable(a),i,x), u, es) => reflectMirrored(Reflect(VarUpdate(Variable(f(a)),f(i),f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
      case Reflect(e@StructCopy(s,sp,st,n,dp,l), u, es) => reflectMirrored(Reflect(StructCopy(f(s),f(sp),f(st),n,f(dp),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
      case Reflect(e@VarCopy(s,sp,Variable(a),dp,l), u, es) => reflectMirrored(Reflect(VarCopy(f(s),f(sp),Variable(f(a)),f(dp),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
      case Reflect(e@DeliteArrayTake(a,x), u, es) => reflectMirrored(Reflect(DeliteArrayTake(f(a),f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArraySort(x), u, es) => reflectMirrored(Reflect(DeliteArraySort(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))     
      case Reflect(e@DeliteArrayCopy(a,ap,d,dp,l), u, es) => reflectMirrored(Reflect(DeliteArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))     
      case Reflect(e@DeliteArrayMap(in,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayMap(f(in),f(g))(e.dmA,e.dmB), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArrayReduce(in,g,z), u, es) => 
        e.asInstanceOf[DeliteArrayReduce[A]] match { //scalac typer bug
          case e@DeliteArrayReduce(in,g,z) => 
            reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayReduce(f(in),f(g),f(z))(e.dmA), mapOver(f,u), f(es)))(mtype(manifest[A]))
        }
      case Reflect(e@DeliteArrayMapFilter(in,g,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteArrayMapFilter(f(in),f(g),f(c))(e.dmA,e.dmB), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArrayGetActSize(), u, es) => reflectMirrored(Reflect(DeliteArrayGetActSize(), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArraySetActBuffer(da), u, es) => reflectMirrored(Reflect(DeliteArraySetActBuffer(f(da))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DeliteArraySetActFinal(da), u, es) => reflectMirrored(Reflect(DeliteArraySetActFinal(f(da))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
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
    case NewVar(Def(Reflect(DeliteArrayNew(len),_,_))) => Nil  //ignore nested mutability for Var(Array): this is only safe because we rewrite mutations on Var(Array) to atomic operations
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
  
}

trait DeliteArrayStructTags extends Base with StructTags {
  case class SoaTag[T,DA <: DeliteArray[T]](base: StructTag[T], length: Rep[Int]) extends StructTag[DA]
}


/**
 * Functionality required for running the generic benchmark acceleration experiment in Lancet (NameScore)
 */
trait DeliteArrayOpsLancet extends DeliteArrayOpsExp with TupleOpsExp {
  this: DeliteOpsExp =>
  
  // case class SourceFromFile(path: Exp[String]) extends Def[io.BufferedSource]
  // def source_fromfile(path: Exp[String]) = reflectPure(SourceFromFile(path))
  // 
  // case class SourceMkString(src: Exp[io.BufferedSource]) extends Def[String]
  // def source_mkstring(src: Exp[io.BufferedSource]) = reflectPure(SourceMkString(src))

  case class SourceStringFromFile(path: Exp[String]) extends Def[String]
  def source_stringfromfile(path: Exp[String]) = reflectPure(SourceStringFromFile(path)) //source_mkstring(source_fromfile(path))
  
  case class DeliteArrayFromSplit(s: Exp[String], del: Exp[String]) extends Def[DeliteArray[String]]
  def darray_fromsplit(s: Exp[String], del: Exp[String]) = reflectPure(DeliteArrayFromSplit(s,del))
    
  def darray_zipwithindex[T:Manifest](x: Exp[DeliteArray[T]]): Exp[DeliteArray[(T,Int)]] = x.zip(darray_range(unit(0),x.length)) { (a,b) => make_tuple2(a,b) }
    
  // should be moved to StringOps  
  case class StringSlice(x: Exp[String], start: Exp[Int], end: Exp[Int]) extends Def[String]
  case class StringLength(x: Exp[String]) extends Def[Int]  
  case class StringMap[A:Manifest](x: Exp[String], c: Sym[Char], body: Block[A]) extends DefWithManifest[A,DeliteArray[A]]  
  def string_slice(x: Exp[String], start: Exp[Int], end: Exp[Int]) = reflectPure(StringSlice(x,start,end))  
  def string_length(x: Exp[String]) = reflectPure(StringLength(x))
  def string_map[A:Manifest](x: Exp[String], f: Exp[Char] => Exp[A]) = {
    val c = fresh[Char]
    val b = reifyEffects(f(c))
    reflectEffect(StringMap(x, c, b), summarizeEffects(b))    
  }
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case StringMap(s, c, body) => syms(s):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case StringMap(s, c, body) => c :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case StringMap(s, c, body) => freqNormal(s):::freqHot(body)
    case _ => super.symsFreq(e)
  }  
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    (e match {
      // case SourceFromFile(p) => source_fromfile(f(p))
      // case SourceMkString(s) => source_mkstring(f(s))
      case SourceStringFromFile(p) => source_stringfromfile(f(p))
      case DeliteArrayFromSplit(s,d) => darray_fromsplit(f(s),f(d))
      case StringSlice(s,st,en) => string_slice(f(s),f(st),f(en))
      case StringLength(x) => string_length(f(x))
      case e@StringMap(x,c,b) => reflectPure(StringMap(f(x),f(c).asInstanceOf[Sym[Char]],f(b))(e.mA))
      case Reflect(e@StringMap(x,c,b), u, es) => reflectMirrored(Reflect(StringMap(f(x),f(c).asInstanceOf[Sym[Char]],f(b))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??  
  }
}


trait DeliteArrayOpsExpOpt extends DeliteArrayOpsLancet /*change for lancet*/ with DeliteArrayStructTags with StructExpOptCommon with DeliteStructsExp {
  this: DeliteOpsExp =>

  object StructIR {
    def unapply[A](e: Exp[DeliteArray[A]]): Option[(StructTag[A], Exp[Int], Seq[(String,Exp[DeliteArray[Any]])])] = e match {
      case Def(Struct(SoaTag(tag: StructTag[A],len),elems:Seq[(String,Exp[DeliteArray[Any]])])) => Some((tag,len,elems))
      case Def(Reflect(Struct(SoaTag(tag: StructTag[A],len), elems:Seq[(String,Exp[DeliteArray[Any]])]), u, es)) => Some((tag,len,elems))
      case _ => None
    }
  }

  //choosing the length of the first array creates an unnecessary dependency (all arrays must have same length), so we store length in the tag
  override def darray_length[T:Manifest](da: Exp[DeliteArray[T]])(implicit ctx: SourceContext) = da match {
    case Def(Loop(size,_,b:DeliteCollectElem[_,_,_])) if b.cond == Nil => size
    case StructIR(tag, len, elems) => 
      printlog("**** extracted array length: " + len.toString)
      len
    case StructType(tag, fields) if Config.soaEnabled =>
      val z = dlength(field(da,fields(0)._1)(mtype(darrayManifest(fields(0)._2)),ctx))(mtype(fields(0)._2),ctx)
      printlog("**** fallback array length: " + z + " of " + da.toString)
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

  def darrayManifest(arg: Manifest[_]) = new Manifest[DeliteArray[_]] {
    override val erasure = classOf[DeliteArray[_]]    
    override val typeArguments = List(arg)
    val runtimeClass = classOf[DeliteArray[_]]
  }

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
  this: DeliteOpsExp =>
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
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n) if Config.generateSerializable && isPrimitiveType(a.mA) => 
      emitValDef(sym, "new ppl.delite.runtime.data.LocalDeliteArray" + remap(a.mA) + "(" + quote(n) + ")")
    case a@DeliteArrayNew(n) if Config.generateSerializable =>
      emitValDef(sym, "new ppl.delite.runtime.data.LocalDeliteArrayObject[" + remap(a.mA) + "](" + quote(n) + ")")
    case a@DeliteArrayNew(n) =>
      emitValDef(sym, "new Array[" + remap(a.mA) + "](" + quote(n) + ")")
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
      stream.println("scala.util.Sorting.quickSort(d)")
      stream.println("d")
      stream.println("}")    
    case DeliteArrayRange(st,en) if !Config.generateSerializable =>
      emitValDef(sym, "Array.range(" + quote(st) + "," + quote(en) + ")")
    case DeliteArrayToSeq(a) if !Config.generateSerializable => 
      emitValDef(sym, quote(a) + ".toSeq")
    case StructUpdate(struct, fields, idx, x) =>
      emitValDef(sym, quote(struct) + "." + fields.reduceLeft(_ + "." + _) + "(" + quote(idx) + ") = " + quote(x))
    case VarUpdate(Variable(a), idx, x) =>
      val readVar = if (deliteInputs contains a) ".get" else ""
      emitValDef(sym, quote(a) + readVar + "(" + quote(idx) + ") = " + quote(x))
      
    // lancet benchmark
    // case SourceFromFile(p) => emitValDef(sym, "scala.io.Source.fromFile(" + quote(p) + ")")
    // case SourceMkString(s) => emitValDef(sym, quote(s) + ".mkString")
    case SourceStringFromFile(p) => emitValDef(sym, "scala.io.Source.fromFile(" + quote(p) + ").mkString")
    case DeliteArrayFromSplit(s,d) => emitValDef(sym, quote(s) + ".split(" + quote(d) + ")")
    case StringSlice(s,st,en) => emitValDef(sym, quote(s) + ".slice(" + quote(st) + ", " + quote(en) + ")")
    case StringLength(s) => emitValDef(sym, quote(s) + ".length")
    case StringMap(s,c,b) => {
      stream.println("val " + quote(sym) + " = {")
      stream.println(quote(s) + ".map(" + quote(c) + " => {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("})")
      stream.println("}")
    }
    case StructCopy(src,srcPos,struct,fields,destPos,len) if Config.generateSerializable =>
      val dest = quote(struct) + "." + fields.reduceLeft(_ + "." + _)
      emitValDef(sym, quote(src) + ".copy(" + quote(srcPos) + "," + dest + "," + quote(destPos) + "," + quote(len) + ")")
    case StructCopy(src,srcPos,struct,fields,destPos,len) =>
      val dest = quote(struct) + "." + fields.reduceLeft(_ + "." + _)
      emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + dest + "," + quote(destPos) + "," + quote(len) + ")")
    case VarCopy(src,srcPos,Variable(a),destPos,len) if Config.generateSerializable =>
      val dest = quote(a) + (if (deliteInputs contains a) ".get" else "")
      emitValDef(sym, quote(src) + ".copy(" + quote(srcPos) + "," + dest + "," + quote(destPos) + "," + quote(len) + ")")
    case VarCopy(src,srcPos,Variable(a),destPos,len) =>
      val dest = quote(a) + (if (deliteInputs contains a) ".get" else "")
      emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + dest + "," + quote(destPos) + "," + quote(len) + ")")
    case DeliteArrayGetActSize() =>
      emitValDef(sym, getActSize)
    case DeliteArraySetActBuffer(da) =>
      emitValDef(sym, getActBuffer + " = " + quote(da))
    case DeliteArraySetActFinal(da) =>
      emitValDef(sym, getActFinal + " = " + quote(da))
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


trait CudaGenDeliteArrayOps extends BaseGenDeliteArrayOps with CudaGenFat with CudaGenDeliteStruct {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n) =>
      // If isNestedNode, each thread allocates its own DeliteArray (TODO: Check the allocation does not escape the kernel)
      if(isNestedNode) {
        // If size is known before launching the kernel (same size for all the threads), allocate outside the kernel
        //TODO: automatically figure out which access pattern is the best
        if(deliteInputs.contains(n)) { 
          val allocSym = registerTempAlloc(sym,a.mA,n)
          stream.println("cudaDeliteArray< " + remap(a.mA) + " > " + quote(sym) + " = cudaDeliteArray< " + remap(a.mA) + " >(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",max(2*blockDim.x*gridDim.x,blockDim.x*(1+" + quote(outerLoopSize) + "/blockDim.x)));")
          //stream.println("DeliteArray< " + remap(a.mA) + " > " + quote(sym) + " = DeliteArray< " + remap(a.mA) + " >(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",blockDim.x*gridDim.x);")
        }
        else if (boundMap.contains(n) && deliteInputs.contains(boundMap(n))) {
          val allocSym = registerTempAlloc(sym,a.mA,boundMap(n))
          stream.println("cudaDeliteArray< " + remap(a.mA) + " > " + quote(sym) + " = cudaDeliteArray< " + remap(a.mA) + " >(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",max(2*blockDim.x*gridDim.x,blockDim.x*(1+" + quote(outerLoopSize) + "/blockDim.x)));")
          //stream.println("DeliteArray< " + remap(a.mA) + " > " + quote(sym) + " = DeliteArray< " + remap(a.mA) + " >(" + quote(n) + "," + allocSym + "," + quote(outerLoopSym) + ",blockDim.x*gridDim.x);")
        }
        // If size is not known before launching the kernel, use temporary memory
        // TODO: Figure out the size is the same for all the threads
        else {
          stream.println("if (tempMemSize < tempMemUsage[" + quote(outerLoopSym) + "] + sizeof(" + remap(a.mA) + ")*" + quote(n) + ") {")
          stream.println("assert(false);")
          stream.println("}")
          stream.println(remap(a.mA) + " *" + quote(sym) + "Ptr = (" + remap(a.mA) + "*)(tempMemPtr + tempMemUsage[" + quote(outerLoopSym) + "]*" + quote(outerLoopSize) + ");") 
          stream.println("tempMemUsage[" + quote(outerLoopSym) + "] = tempMemUsage[" + quote(outerLoopSym) + "] + sizeof(" + remap(a.mA) + ")*" + quote(n) + ";")
          stream.println("cudaDeliteArray< " + remap(a.mA) + " > " + quote(sym) + " = cudaDeliteArray< " + remap(a.mA) + " >(" + quote(n) + "," + quote(sym) + "Ptr," + quote(outerLoopSym) + "*" + quote(n) + ",1);")
        }
      }
      // Allocated only once for the entire kernel by helper function
      else {
        stream.println("cudaDeliteArray< " + remap(a.mA) + " > *" + quote(sym) + "_ptr = new cudaDeliteArray< " + remap(a.mA) + " >(" + quote(n) + ");")
        stream.println("cudaDeliteArray< " + remap(a.mA) + " > " + quote(sym) + " = *" + quote(sym) + "_ptr;")
      }
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + ".length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + ".apply(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      stream.println(quote(da) + ".update(" + quote(idx) + "," + quote(x) + ");")
    case StructUpdate(struct, fields, idx, x) =>
      stream.println(quote(struct) + "." + fields.reduceLeft(_ + "." + _) + ".update(" + quote(idx) + "," + quote(x) + ");\n")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) =>
      stream.println("for(int i=0; i<"+quote(len)+"; i++) {")
      stream.println(quote(dest) + ".update(" + quote(destPos) + "+i," + quote(src) + ".apply(" + quote(srcPos) + "+i));")
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }

  override def remapHost[A](m: Manifest[A]): String = {
    if(isArrayType(baseType(m))) {
      m.typeArguments(0) match {
        case StructType(_,_) if Config.soaEnabled => super.remapHost(m)
        case s if s <:< manifest[Record] && Config.soaEnabled => super.remapHost(m)  
        case arg => hostTarget + "DeliteArray< " + remapHost(arg) + addRef(arg) + " >"
      }
    }
    else 
      super.remapHost(m)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => m.typeArguments(0) match {
      //case p if !isPrimitiveType(p) => throw new GenerationFailedException("DeliteArray of type object is not supported on this branch.")
      case StructType(_,_) if Config.soaEnabled => super.remap(m)
      case s if s <:< manifest[Record] && Config.soaEnabled => super.remap(m) // occurs due to restaging
      case arg => deviceTarget + "DeliteArray< " + remap(arg) + " >"
    }
    case _ => super.remap(m)
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"" + deviceTarget + "DeliteArray.h\"\n")
    out.append("#include \"Host" + deviceTarget + "DeliteArray.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }
}

trait OpenCLGenDeliteArrayOps extends BaseGenDeliteArrayOps with OpenCLGenFat with OpenCLGenDeliteStruct {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n) =>
      emitValDef(sym, "new Array[" + remap(a.mA) + "](" + quote(n) + ")")
    case DeliteArrayLength(da) =>
      emitValDef(sym, remap(da.tp) + "_size(" + quote(da) + ")")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, remap(da.tp) + "_apply(" + quote(da) + "," + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      emitValDef(sym, remap(da.tp) + "_update(" + quote(da) + "," + quote(idx) + "," + quote(x) + ")")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => m.typeArguments(0) match {
      case StructType(_,_) if Config.soaEnabled => super.remap(m)
      case s if s <:< manifest[Record] && Config.soaEnabled => super.remap(m) // occurs due to restaging
      case arg => "OpenCLDeliteArray_" + remap(arg) + addRef(arg)
    }
    case _ => super.remap(m)
  }
}

trait CGenDeliteArrayOps extends BaseGenDeliteArrayOps with CGenDeliteStruct with CGenDeliteOps {
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n) => 
      emitValDef(sym, "(" + remap(sym.tp) + " *) malloc(sizeof(" + remap(sym.tp) + "));")
      stream.println(quote(sym) + "->data = (" + remap(a.mA) + addRef(a.mA) + " *)malloc(" + quote(n) + "*sizeof(" + remap(a.mA) + addRef(a.mA) + "));")
      //TODO: remove below memory initialization. 
      // This memset is currently needed for C target because JVM initializes arrays with 0
      // and some DSL operations assume that. That should not be the case and moved into IR if init is needed.
      stream.println("memset(" + quote(sym) + "->data,0," + quote(n) + "*sizeof(" + remap(a.mA) + addRef(a.mA) + "));")
      stream.println(quote(sym) + "->length = " + quote(n) + ";")
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + "->length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + "->apply(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      stream.println(quote(da) + "->update(" + quote(idx) + ", " + quote(x) + ");")
    case StructUpdate(struct, fields, idx, x) =>
      stream.println(quote(struct) + "->" + fields.reduceLeft(_ + "->" + _) + "->update(" + quote(idx) + "," + quote(x) + ");")
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) =>
      stream.println(quote(src) + "->copy(" + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ");")
    case sc@StructCopy(src,srcPos,struct,fields,destPos,len) =>
      val dest = quote(struct) + "->" + fields.reduceLeft(_ + "->" + _)
      val elemM = src.tp.typeArguments(0)
      val elemType = if(isPrimitiveType(elemM)) remap(elemM) else (remap(elemM)+"*")
      stream.println("memcpy((" + dest + "->data)+" + quote(destPos) + ",(" + quote(src) + "->data)+" + quote(srcPos) + "," + quote(len) + "*sizeof(" + elemType + "));")
    //case DeliteArrayMkString(da,x) =>
    //  emitValDef(sym, quote(da) + ".mkString(" + quote(x) + ")")
    case DeliteArrayUnion(lhs,rhs) =>
      emitValDef(sym, quote(lhs) + "->arrayunion(" + quote(rhs) + ")")
    case DeliteArrayIntersect(lhs,rhs) =>
      emitValDef(sym, quote(lhs) + "->intersect(" + quote(rhs) + ")")
    case DeliteArrayTake(lhs,n) =>
      emitValDef(sym, quote(lhs) + "->take(" + quote(n) + ")")
    //case a@DeliteArraySort(x) =>
    //  stream.println("val " + quote(sym) + " = {")
    //  stream.println("val d = new Array[" + remap(a.mA) + "](" + quote(x) + ".length" + ")")
    //  stream.println("System.arraycopy(" + quote(x) + ", 0, d, 0, " + quote(x) + ".length)")
    //  stream.println("scala.util.Sorting.quickSort(d)")
    //  stream.println("d")
    //  stream.println("}")
    case DeliteArrayRange(st,en) =>
      emitValDef(sym, "new cppDeliteArray< int >(" + quote(st) + "," + quote(en) + ")")
    //case DeliteArrayToSeq(a) => emitValDef(sym, quote(a) + ".toSeq")
    case _ => super.emitNode(sym, rhs)
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => m.typeArguments(0) match {
      case StructType(_,_) if Config.soaEnabled => super.remap(m)
      case s if s <:< manifest[Record] && Config.soaEnabled => super.remap(m) // occurs due to restaging
      case arg => "cppDeliteArray< " + remap(arg) + addRef(arg) + " >"
    }
    case _ => super.remap(m)
  }

  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
    val stream = new PrintWriter(path + deviceTarget + "DeliteArrayRelease.h")
    stream.println("#include \"" + deviceTarget + "DeliteStructs.h\"")
    for(tp <- dsTypesList if(isArrayType(tp) && remap(tp)!="string")) {
      try {
        tp.typeArguments(0) match {
          case StructType(_,_) if Config.soaEnabled => 
          case s if s <:< manifest[Record] && Config.soaEnabled => 
          case _ => stream.println("template void " + remap(tp) + "::release(void);\n")    
        }
      }
      catch {
        case e: GenerationFailedException => //
        case e: Exception => throw(e)
      }
    }
    stream.close()
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"" + deviceTarget + "DeliteArray.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }
}
