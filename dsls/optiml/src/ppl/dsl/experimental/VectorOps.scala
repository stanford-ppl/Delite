package ppl.dsl.experimental

import ppl.dsl.optiml.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

trait VectorOps extends Variables {
  this: Sandbox =>

  object Vector {
    // default to dense
    def dense[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) = dense_vector_obj_new(len, isRow)
    def sparse[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) = sparse_vector_obj_new(len, isRow)
  }
  
  /**
   * Experimenting with using implicit ops to handle abstract operations. 
   *  
   */

  /*
  trait BinaryOp[A,B,R] {
    def apply(a: Rep[A], b: Rep[B]): Rep[R]
  }
  
  trait CanAdd[A,B,R] extends BinaryOp[A,B,R]
      
  // works, but requires a CanAdd instance to be supplied along with the IsVector instance (less desirable)
  // def infix_+[A,B,R](x: Rep[A], y: Rep[B])(implicit ca: CanAdd[A,B,R]) = ca(x,y)
  //   
  //   abstract class IsVector[V[X],T:Manifest](x: Rep[V[T]]) {
  //     def length: Rep[Int]
  //   }
  
  implicit def isVectorToVectorOps[V[X],T:Manifest](lhs: Rep[V[T]])(implicit v: IsVector[V,T]) = new IsVectorOps[V,T](lhs)
  
  class IsVectorOps[V[X],T:Manifest](lhs: Rep[V[T]])(implicit v: IsVector[V,T]) {
    def length = v.length(lhs)
    //def +[B,R](rhs: Rep[B])(implicit ca: CanAdd[V[T],B,R]) = v.plus(lhs, rhs)    
    //def +(rhs: Rep[V[T]])(implicit a: Arith[T]) = v.plus(lhs,rhs)
    def +[Y[X]](rhs: Rep[Y[T]])(implicit a: Arith[T], yv: IsVector[Y,T]) = v.plus(lhs, rhs)
  }
  
  trait IsVector[V[X],T] {
    def length(lhs: Rep[V[T]]): Rep[Int]    
    
    // doesn't work unless we supply the CanAdd explicitly at the call site (can't construct it generically)
    //def plus[B,R](lhs: Rep[V[T]], rhs: Rep[B])(implicit ca: CanAdd[V[T],B,R]) = ca(lhs,rhs)     
    
    // works, but not expressive
    //def plus(lhs: Rep[V[T]], rhs: Rep[V[T]])(implicit a: Arith[T]): Rep[V[T]]
    
    // works, but underlying return type is not wrapped like in Interface, so must be fixed for each operation
    def plus[Y[X]](lhs: Rep[V[T]], rhs: Rep[Y[T]])(implicit a: Arith[T], v: IsVector[Y,T]): Rep[V[T]]
  }
  */

  /**
   * The following is a scheme meant to implement a version of "call-site dispatch" in lieu of dynamic dispatch for ops. 
   * The idea is that if a client does not care about whether a particular type T is T1, T2, .., it can use Interface[T] instead of
   * reproducing identical code for each of T1, T2, .. . The main result is providing a sort of static abstract method capability.  
   */
  
  // Delite needs to be able to pre-allocate builders for parallel ops. What's the right way to do this?
  abstract class VectorBuilder[-Elem, +To] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]): Rep[To]
  }  
  def denseVectorBuilder[A:Manifest] = new VectorBuilder[A,DenseVector[A]] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = Vector.dense[A](length, isRow)
  }  
  def sparseVectorBuilder[A:Manifest] = new VectorBuilder[A,SparseVector[A]] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = Vector.sparse[A](length, isRow)
  }
    
  abstract class VecOpsCls[A:Manifest] extends DCInterfaceOps[Vector[A],A] {
    type V[X] // <: DeliteCollection[X] // necessary?
    implicit def toIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]]    
    implicit def builder[B:Manifest]: VectorBuilder[B,V[B]]    
    implicit def mVB[B:Manifest]: Manifest[V[B]] 
    
    val x: Rep[V[A]]
    
    def mutable: Rep[V[A]]
    
    // delite collection
    def dcSize: Rep[Int] 
    def dcApply(n: Rep[Int]): Rep[A] 
    def dcUpdate(n: Rep[Int], y: Rep[A]): Rep[Unit]
    
    // accessors
    def length: Rep[Int] 
    def apply(n: Rep[Int]) = dcApply(n)
    def update(n: Rep[Int], y: Rep[A]) = dcUpdate(n,y)

    def +(y: Rep[V[A]])(implicit a: Arith[A]): Rep[V[A]] // needed for Arith    

    // we only need to go through this gymnastic hack when we have different return values for different ops;
    // usually this would just return Rep[V[A]] (i.e. the same as the lhs)
    type VPLUSR
    def vplusToIntf(x: Rep[VPLUSR]): Interface[Vector[A]]
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]): Rep[VPLUSR]
    //def +(y: Interface[Vector[A]])(implicit a: Arith[A]): Rep[V[A]]
    
    def map[B:Manifest](f: Rep[A] => Rep[B]): Rep[V[B]] = vector_map[A,B,V[B]](x,f)
  }
  
  // x: Rep[DenseVector[T]]
  // we implicitly convert that to an Interface[Vector[T]]: (this would be better if we didn't need Interface, but then Rep can't simply be an abstract type)  
  implicit def denseToInterface[A:Manifest](lhs: Rep[DenseVector[A]]) = VInterface[A](new DenseVecOpsCls[A](lhs))
  implicit def sparseToInterface[A:Manifest](lhs: Rep[SparseVector[A]]) = VInterface[A](new SparseVecOpsCls[A](lhs))
  
  // clients that can handle multiple kinds of vector must accept an Interface[Vector[T]],  not a Rep[Vector[T]]
  case class VInterface[A:Manifest](ops: VecOpsCls[A]) extends DCInterface[Vector[A],A] // clients use Interface[Vector]

  // then we convert from a Interface[Vector[T]] to an interfaceVecToOpsCls, providing all of the original vector methods  
  implicit def interfaceToVecOps[A:Manifest](intf: Interface[Vector[A]]): InterfaceVecOpsCls[A] = new InterfaceVecOpsCls(intf.asInstanceOf[VInterface[A]]) // should only be one instance of Interface[Vector], but can we enforce this?
  
  class InterfaceVecOpsCls[A:Manifest](val intf: VInterface[A]) {
    def mutable: Interface[Vector[A]] = intf.ops.toIntf[A](intf.ops.mutable)
    
    def length = intf.ops.length
    def apply(n: Rep[Int]) = intf.ops.apply(n)
    def update(n: Rep[Int], y: Rep[A]) = intf.ops.update(n,y)
    
    //def +(y: Rep[intf.ops.V[A]])(implicit a: Arith[A]) = intf.ops.toIntf(intf.ops.+(y)) // doesn't work, would need dynamic type of ops
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.vplusToIntf(intf.ops.+(y))
    def map[B:Manifest](f: Rep[A] => Rep[B]): Interface[Vector[B]] = intf.ops.toIntf(intf.ops.map(f))
    
    // would be nice - could short-circuit the operation if known statically!
    // toSparse
    // toDense
  }
  
  // object defs
  def dense_vector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[A]]
  def sparse_vector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[SparseVector[A]]
  
  def vector_map[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Rep[A] => Rep[B])(implicit b: VectorBuilder[B,VB]): Rep[VB]
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp {

  this: SandboxExp =>


  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class DenseVectorNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[A]] {
    val mA = manifest[A]
  }

  case class SparseVectorNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) extends Def[SparseVector[A]] {
    val mA = manifest[A]
  }

  case class VectorMap[A:Manifest,B:Manifest,VB:Manifest](in: Interface[Vector[A]], func: Exp[A] => Exp[B])(implicit b: VectorBuilder[B,VB])
    extends DeliteOpMap[A,B,VB] {

    val size = copyTransformedOrElse(_.size)(in.length)
    def alloc = b.alloc(in.length, unit(true))
    
    val mA = manifest[A]
    val mB = manifest[B]
  }
  
  def dense_vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(DenseVectorNew[A](len, isRow)) //XXX
  def sparse_vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(SparseVectorNew[A](len, isRow)) //XXX
  
  def vector_map[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Exp[A] => Exp[B])(implicit b: VectorBuilder[B,VB])
    = reflectPure(VectorMap[A,B,VB](x, f)) // TODO: effect if func effectful!  
}

trait BaseGenVectorOps extends GenericFatCodegen {
  val IR: VectorOpsExp
  import IR._

}

trait ScalaGenVectorOps extends BaseGenVectorOps with ScalaGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new generated.scala.DenseVector[" + remap(v.mA) + "](" + quote(length) + "," + quote(isRow) + ")")
    case v@SparseVectorNew(length, isRow) => emitValDef(sym, "new generated.scala.SparseVector[" + remap(v.mA) + "](" + quote(length) + "," + quote(isRow) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
