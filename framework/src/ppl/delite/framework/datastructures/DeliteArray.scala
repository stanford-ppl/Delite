package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.Util._

trait DeliteArray[T] extends DeliteCollection[T] 
// TODO: DeliteArrayBuffer[T] extends DeliteCollection[T] ? // Struct{Int,DeliteArray[T]}

trait DeliteArrayOps extends StringOps {
  
  object DeliteArray {
    def apply[T:Manifest](length: Rep[Int]) = darray_new(length)
    //def apply[T:Manifest](length: Rep[Int])(f: Rep[Int] => Rep[T]) = darray_fromFunction(length, f)
  }
  
  implicit def repDArrayToDArrayOps[T:Manifest](da: Rep[DeliteArray[T]]) = new DeliteArrayOpsCls(da)
  
  class DeliteArrayOpsCls[T:Manifest](da: Rep[DeliteArray[T]]) {
    def length: Rep[Int] = darray_length(da)
    def apply(i: Rep[Int]): Rep[T] = darray_apply(da,i)
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = darray_update(da,i,x)
    def map[B:Manifest](f: Rep[T] => Rep[B]) = darray_map(da,f)
    def mkString(del: Rep[String]) = darray_mkstring(da,del)
  }
    
  implicit def darrayToString[A:Manifest](x: Rep[DeliteArray[A]]): Rep[String] = "[ " + repDArrayToDArrayOps(x).mkString(unit(" ")) + " ]"
  def infix_+[A:Manifest](lhs: String, rhs: Rep[DeliteArray[A]]) = string_plus(unit(lhs), darrayToString[A](rhs))
  
  def darray_new[T:Manifest](length: Rep[Int]): Rep[DeliteArray[T]]
  //def darray_fromFunction[T:Manifest](length: Rep[Int], f: Rep[Int] => Rep[T]): Rep[DeliteArray[T]]
  def darray_length[T:Manifest](da: Rep[DeliteArray[T]]): Rep[Int]
  def darray_apply[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int]): Rep[T]
  def darray_update[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
  def darray_copy[T:Manifest](src: Rep[DeliteArray[T]], srcPos: Rep[Int], dest: Rep[DeliteArray[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def darray_map[A:Manifest,B:Manifest](a: Rep[DeliteArray[A]], f: Rep[A] => Rep[B]): Rep[DeliteArray[B]]    
  def darray_mkstring[A:Manifest](a: Rep[DeliteArray[A]], del: Rep[String]): Rep[String]
}

trait DeliteArrayCompilerOps extends DeliteArrayOps {
  def darray_unsafe_update[T:Manifest](x: Rep[DeliteArray[T]], n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext): Rep[Unit]
  def darray_unsafe_copy[T:Manifest](src: Rep[DeliteArray[T]], srcPos: Rep[Int], dest: Rep[DeliteArray[T]], destPos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

trait DeliteArrayOpsExp extends DeliteArrayCompilerOps with DeliteCollectionOpsExp with StructExp with EffectExp {
  this: DeliteOpsExp =>
  
  case class DeliteArrayNew[T:Manifest](length: Exp[Int]) extends DefWithManifest[T,DeliteArray[T]] 
  
  /* TODO: re-enable
  case class DeliteArrayFromFunction[T:Manifest](length: Exp[Int], f: Exp[Int] => Exp[T]) extends DeliteOpLoop[DeliteArray[T]] {
    val size = copyTransformedOrElse(_.size)(length)
    lazy val body = copyBodyOrElse(DeliteCollectElem[T, DeliteArray[T]](
      alloc = reifyEffects(DeliteArray(length)),
      func = reifyEffects(f(v))
    ))
  }
  */
  
  case class DeliteArrayLength[T:Manifest](da: Exp[DeliteArray[T]]) extends Def[Int]
  case class DeliteArrayApply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) extends DefWithManifest[T,T]
  case class DeliteArrayUpdate[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) extends DefWithManifest[T,Unit]
  case class DeliteArrayCopy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int]) extends DefWithManifest[T,Unit]
  case class DeliteArrayMkString[T:Manifest](da: Exp[DeliteArray[T]], del: Exp[String]) extends DefWithManifest[T,String]
  
  //////////////////
  // delite ops
  
  case class DeliteArrayMap[A:Manifest,B:Manifest](in: Exp[DeliteArray[A]], func: Exp[A] => Exp[B])
    extends DeliteOpMap[A,B,DeliteArray[B]] {

    val size = copyTransformedOrElse(_.size)(in.length)
    override def alloc(len: Exp[Int]) = DeliteArray[B](len)

    val mA = manifest[A]
    val mB = manifest[B]
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
    else {
      super.dc_apply(x,n)    
    }
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) asDeliteArray(x).update(n,y)
    else super.dc_update(x,n,y)        
  }
  
  override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (isDeliteArray(x)) {
      if (hasConditions == true) throw new UnsupportedOperationException("DeliteArray: cannot have conditional Delite ops with a DeliteArray as input")
      ParFlat
    }
    else super.dc_parallelization(x, hasConditions)
  }
    
  def darray_new[T:Manifest](length: Exp[Int]) = reflectMutable(DeliteArrayNew[T](length))
  //def darray_fromFunction[T:Manifest](length: Exp[Int], f: Exp[Int] => Exp[T]) = reflectPure(DeliteArrayFromFunction(length,f))
  //def darray_create[T:Manifest](length: Exp[Int], elem: Exp[T]): Exp[DeliteArray[T]] = DeliteArray(length)  //TODO: fix & then fromFunction should call this
  def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = reflectPure(DeliteArrayLength[T](da))
  def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = reflectPure(DeliteArrayApply[T](da,i))
  def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) = reflectWrite(da)(DeliteArrayUpdate[T](da,i,x))  
  def darray_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(dest)(DeliteArrayCopy(src,srcPos,dest,destPos,len))  
  def darray_map[A:Manifest,B:Manifest](a: Exp[DeliteArray[A]], f: Exp[A] => Exp[B]) = reflectPure(DeliteArrayMap(a,f))   
  def darray_mkstring[A:Manifest](a: Exp[DeliteArray[A]], del: Exp[String]) = reflectPure(DeliteArrayMkString(a,del))
  
  /////////////
  // internal
  
  def darray_unsafe_update[T:Manifest](x: Exp[DeliteArray[T]], n: Exp[Int], y: Exp[T])(implicit ctx: SourceContext) = DeliteArrayUpdate(x,n,y)
  def darray_unsafe_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = DeliteArrayCopy(src,srcPos,dest,destPos,len)
    
  //def reflectPure[T](x: Def[T])(implicit ctx: SourceContext) = x
  
  //def array_length[T:Manifest](da: Exp[Array[T]]) = darray_length(da.asInstanceOf[Exp[DeliteArray[T]]]) // FIXME
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    (e match {
      case DeliteArrayLength(a) => darray_length(f(a))
      case DeliteArrayApply(a,x) => darray_apply(f(a),f(x))
      case e@DeliteArrayCopy(a,ap,d,dp,l) => toAtom(DeliteArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
      case Reflect(DeliteArrayApply(l,r), u, es) => reflectMirrored(Reflect(DeliteArrayApply(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(DeliteArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(DeliteArrayUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))   
      case Reflect(e@DeliteArrayCopy(a,ap,d,dp,l), u, es) => reflectMirrored(Reflect(DeliteArrayCopy(f(a),f(ap),f(d),f(dp),f(l))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))     
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  /////////////////////
  // aliases and sharing
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteArrayCopy(s,sp,d,dp,l) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
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

trait DeliteArrayOpsExpOpt extends DeliteArrayOpsExp with StructExpOptCommon {
  this: DeliteOpsExp =>
  
  override def field[T:Manifest](struct: Rep[Any], index: String)(implicit ctx: SourceContext): Rep[T] = struct match {
    //case Def(m: DeliteOpMapLike[_,_]) =>
    //  val alloc = m.body.asInstanceOf[DeliteCollectElem[_,_]].alloc
    //  field(alloc, index)
    case _ => super.field[T](struct, index)
  }
  
  /* override def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = da match {
    case Def(l: DeliteOpLoop[_]) => l.size
    case Def(Struct(prefix::tag, elems:Map[String,Exp[DeliteArray[T]]])) =>
      assert(prefix == "DeliteArray")
      val ll = elems.map(p=>darray_length(p._2)) // all arrays must have same length!
      ll reduceLeft { (a1,a2) => assert(a1 == a2); a1 }      
    case _ => super.darray_length(da)
  }
  
  override def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = da match {
    case Def(Struct(prefix::tag, elems:Map[String,Exp[DeliteArray[T]]])) =>
      assert(prefix == "DeliteArray")
      struct[T](tag, elems.map(p=>(p._1, darray_apply(p._2,i))))
    case _ => super.darray_apply(da,i)
  }
  
  //TODO: ?? override def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) =
  
  /* override def darray_create[T:Manifest](length: Exp[Int], elem: Exp[T]) = elem match {
    case Def(Struct(tag, elems)) =>
      struct[DeliteArray[T]]("DeliteArray"::tag, elems.map(p=>(p._1,darray_create(length, p._2))))
    case Def(DeliteArrayApply(da,i)) if (da.length == length) => da.asInstanceOf[Exp[DeliteArray[T]]] //eta-reduce!
    case _ => super.darray_create(length, elem)
  } */
  */
}

trait DeliteArrayFatExp extends DeliteArrayOpsExpOpt with StructFatExpOptCommon {
  this: DeliteOpsExp =>
}

trait ScalaGenDeliteArrayOps extends ScalaGenEffect {
  val IR: DeliteArrayOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@DeliteArrayNew(n) => emitValDef(sym, "new Array[" + remap(a.mA) + "](" + quote(n) + ")")
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + ".length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ") = " + quote(x))
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) =>
      emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    case DeliteArrayMkString(da,x) =>
      emitValDef(sym, quote(da) + ".mkString(" + quote(x) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => "Array[" + remap(m.typeArguments(0)) + "]"
    case _ => super.remap(m)
  }
}
