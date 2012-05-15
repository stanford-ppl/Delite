package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{Base, EffectExp, StructExp, StructExpOptCommon, StructFatExpOptCommon, ScalaGenEffect}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastruct.scala.DeliteCollection

trait DeliteArray[T] extends DeliteCollection[T] 

/**
 * A DeliteArray is a struct containing a data array and a length scalar variable that acts
 * like an ArrayBuffer.
 * 
 * It can be used by DSLs as a generic, growable data structure that implements the DeliteCollection
 * interface.
 * 
 * TODO: DeliteArray should be converted to using a programmatic struct before enabling the buffering
 * functionality and integrating with other DSLs.
 */
trait DeliteArrayOps extends Base {
    
  object DeliteArray {
    def apply[T:Manifest](length: Rep[Int]) = darray_new(length)
    //def apply[T:Manifest](length: Rep[Int])(f: Rep[Int] => Rep[T]) = darray_fromFunction(length, f)
  }
  
  implicit def repDArrayToDArrayOps[T:Manifest](da: Rep[DeliteArray[T]]) = new DeliteArrayOpsCls(da)
  
  class DeliteArrayOpsCls[T:Manifest](da: Rep[DeliteArray[T]]) {
    def length: Rep[Int] = darray_length(da)
    def apply(i: Rep[Int]): Rep[T] = darray_apply(da,i)
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = darray_update(da,i,x)
  }
    
  def darray_new[T:Manifest](length: Rep[Int]): Rep[DeliteArray[T]]
  //def darray_fromFunction[T:Manifest](length: Rep[Int], f: Rep[Int] => Rep[T]): Rep[DeliteArray[T]]
  def darray_length[T:Manifest](da: Rep[DeliteArray[T]]): Rep[Int]
  def darray_apply[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int]): Rep[T]
  def darray_update[T:Manifest](da: Rep[DeliteArray[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
    
}

trait DeliteArrayOpsExp extends DeliteArrayOps with StructExp with EffectExp {
  this: DeliteArrayImplOps with DeliteOpsExp =>
  
  case class DeliteArrayNew[T:Manifest](length: Exp[Int]) extends Def[DeliteArray[T]]
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
  case class DeliteArrayApply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) extends Def[T] {
    val mT = manifest[T]
  }
  case class DeliteArrayUpdate[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) extends Def[Unit] {
    val mT = manifest[T]
  }
  // case class DeliteArrayCopyFrom[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], y: Exp[DeliteArray[A]])
  //   extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(delitearray_copyfrom_impl(x,pos,y)))    
  // case class DeliteArrayInsert[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], y: Exp[A]) 
  //   extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(delitearray_insert_impl(x,pos,y)))    
  // case class DeliteArrayInsertAll[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], y: Exp[DeliteArray[A]])
  //   extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(delitearray_insertall_impl(x,pos,y)))    
  // case class DeliteArrayRemoveAll[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], len: Exp[Int])
  //   extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(delitearray_removeall_impl(x,pos,len)))
  case class DeliteArrayCopy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int]) extends Def[Unit]
  
  def darray_new[T:Manifest](length: Exp[Int]) = reflectMutable(DeliteArrayNew[T](length))
  //def darray_fromFunction[T:Manifest](length: Exp[Int], f: Exp[Int] => Exp[T]) = reflectPure(DeliteArrayFromFunction(length,f))
  //def darray_create[T:Manifest](length: Exp[Int], elem: Exp[T]): Exp[DeliteArray[T]] = DeliteArray(length)  //TODO: fix & then fromFunction should call this
  def darray_length[T:Manifest](da: Exp[DeliteArray[T]]) = reflectPure(DeliteArrayLength[T](da))
  def darray_apply[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int]) = reflectPure(DeliteArrayApply[T](da,i))
  def darray_update[T:Manifest](da: Exp[DeliteArray[T]], i: Exp[Int], x: Exp[T]) = reflectWrite(da)(DeliteArrayUpdate[T](da,i,x))
  
  // for these operations, the representation of DeliteArray needs to be a struct (hopefully unwrapped) with data and length fields
  // def darray_copyfrom[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], y: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DeliteArrayCopyFrom(x, pos, y))
  // def darray_insert[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(DeliteArrayInsert(x, pos, y))
  // def darray_insertall[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], y: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DeliteArrayInsertAll(x, pos, y))
  // def darray_removeall[A:Manifest](x: Exp[DeliteArray[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DeliteArrayRemoveAll(x, pos, len))  
  def darray_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(dest)(DeliteArrayCopy(src,srcPos,dest,destPos,len))  

  /////////////
  // internal
  
  def darray_unsafe_update[T:Manifest](x: Rep[DeliteArray[T]], n: Rep[Int], y: Rep[T])(implicit ctx: SourceContext) = DeliteArrayUpdate(x,n,y)
  def darray_unsafe_copy[T:Manifest](src: Exp[DeliteArray[T]], srcPos: Exp[Int], dest: Exp[DeliteArray[T]], destPos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = DeliteArrayCopy(src,srcPos,dest,destPos,len)

    
  //def reflectPure[T](x: Def[T])(implicit ctx: SourceContext) = x
  
  //def array_length[T:Manifest](da: Exp[Array[T]]) = darray_length(da.asInstanceOf[Exp[DeliteArray[T]]]) // FIXME
  
}

trait DeliteArrayOpsExpOpt extends DeliteArrayOpsExp with StructExpOptCommon {
  this: DeliteArrayImplOps with DeliteOpsExp =>
  
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
  this: DeliteArrayImplOps with DeliteOpsExp =>
}

trait ScalaGenDeliteArrayOps extends ScalaGenEffect {
  val IR: DeliteArrayOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DeliteArrayNew(length) =>
      emitValDef(sym, "new Array[" + sym.Type.typeArguments(0).toString + "](" + quote(length) + ")")
    case DeliteArrayLength(da) =>
      emitValDef(sym, quote(da) + ".length")
    case DeliteArrayApply(da, idx) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ")")
    case DeliteArrayUpdate(da, idx, x) =>
      emitValDef(sym, quote(da) + "(" + quote(idx) + ") = " + quote(x))
    case DeliteArrayCopy(src,srcPos,dest,destPos,len) =>
      emitValDef(sym, "System.arraycopy(" + quote(src) + "," + quote(srcPos) + "," + quote(dest) + "," + quote(destPos) + "," + quote(len) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DeliteArray" => "Array[" + remap(m.typeArguments(0)) + "]"
    case _ => super.remap(m)
  }
}
