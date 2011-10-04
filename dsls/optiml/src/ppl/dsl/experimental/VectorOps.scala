package ppl.dsl.experimental

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

trait VectorOps extends DSLType with Variables {
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
    
    // works, but return type is fixed like in Interface - would need to use same (ugly) type alias trick
    def plus[Y[X]](lhs: Rep[V[T]], rhs: Rep[Y[T]])(implicit a: Arith[T], v: IsVector[Y,T]): Rep[V[T]]
  }
  */

  /**
   * The following is a scheme meant to implement a version of "call-site dispatch" in lieu of dynamic dispatch for ops. 
   * The idea is that if a client does not care about whether a particular type T is T1, T2, .., it can use Interface[T] instead of
   * reproducing identical code for each of T1, T2, .. . The main result is providing a sort of static abstract method capability.  
   *  
   * One major limitation with this scheme is that return types must be inferred, since they are also call-site dependent.
   * A function that accepts an Interface[Vector[A]] could return either a DenseVector[A] or a SparseVector[A], depending on which was
   * supplied at the call-site. It is statically definable from the call-graph, but not client-independent. 
   *  
   * A similar problem arises with multiple arguments: e.g. foo(a: Interface[Vector[A]], b: Interface[Vector[A]]).
   * The interface needs to be to perform an operation on its desired argument type as well as that argument
   * type wrapped inside an Interface. There is a idea of an ugly workaround now (see Arith.scala), that unfortunately doesn't quite work
   * (but looks like maybe it should).
   */
  abstract class VecOpsCls[A:Manifest] extends DCInterfaceOps[A] {
    type V[X]
    val x: Rep[V[A]]
    
    // delite collection
    def dcSize: Rep[Int] 
    def dcApply(n: Rep[Int]): Rep[A] 
    def dcUpdate(n: Rep[Int], y: Rep[A]): Rep[Unit]
    
    // accessors
    def length: Rep[Int] 
        
    type VPLUSR[X]
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]): Rep[VPLUSR[A]]
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
    def length = intf.ops.length
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.+(y)
  }
  
  // object defs
  def dense_vector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[A]]
  def sparse_vector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[SparseVector[A]]
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

  def dense_vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(DenseVectorNew[A](len, isRow)) //XXX
  def sparse_vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(SparseVectorNew[A](len, isRow)) //XXX
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
    case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new generated.scala.VectorImpl[" + remap(v.mA) + "](" + quote(length) + "," + quote(isRow) + ")")
    case v@SparseVectorNew(length, isRow) => throw new UnsupportedOperationException("sparse vector impl not yet implemented")
    case _ => super.emitNode(sym, rhs)
  }
}
