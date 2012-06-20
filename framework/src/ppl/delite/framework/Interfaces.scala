package ppl.delite.framework

import scala.virtualization.lms.common.{Base, BaseExp}
import scala.virtualization.lms.internal.{Blocks, Effects, AbstractTransformer, Transforming}
import scala.virtualization.lms.util.OverloadHack

trait Interfaces extends Base {
  
  trait InterfaceOps[+T] {
    type Self
    val elem: Rep[Self]   
    def wrap(x: Rep[Self]): Interface[T]
  }
  
  trait Interface[+T] { // Interface[Vector[T]]
    val ops: InterfaceOps[T]
  }
  
}

trait InterfacesExp extends Interfaces with BaseExp with Effects with Blocks with OverloadHack {
  
  object Interface {
    def unapply[T](intf: Interface[T]): Option[Exp[Any]] = Some(intf.ops.elem)
  }

  ////////////////
  // transformers
  
  implicit def transformerToInterfaceTransformer(a: AbstractTransformer{val IR: InterfacesExp.this.type}) = new {
    def apply[A](x: Interface[A]): Interface[A] = x.ops.wrap(a.apply[x.ops.Self](x.ops.elem))
    def apply[X,A](f: X=>Interface[A])(implicit o: Overloaded1): X=>Interface[A] = (z:X) => apply(f(z))    
  }
  
  ////////////////
  // dependencies

  override def syms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => syms(i.ops.elem)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => boundSyms(i.ops.elem)
    case _ => super.boundSyms(e)
  }

  override def effectSyms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => effectSyms(i.ops.elem)
    case _ => super.effectSyms(e)
  }

  override def softSyms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => softSyms(i.ops.elem)
    case _ => super.softSyms(e)
  }

  override def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case i: Interface[Any] => rsyms(i.ops.elem)(f)
    case _ => super.rsyms(e)(f)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case i: Interface[Any] => symsFreq(i.ops.elem)
    case _ => super.symsFreq(e)
  }


  ////////////
  // aliases
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => aliasSyms(i.ops.elem)
    case _ => super.aliasSyms(e)
  }  

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => containSyms(i.ops.elem)
    case _ => super.containSyms(e)    
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => extractSyms(i.ops.elem)
    case _ => super.extractSyms(e)   
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case i: Interface[Any] => copySyms(i.ops.elem)
    case _ => super.copySyms(e)
  }
}