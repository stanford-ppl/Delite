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
    def dense[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) = vector_obj_new(len, isRow)
  }
  
  /**
   * Experimenting with using implicit ops to handle abstract operations. 
   *  
   * The main issue with this is that:  
   *    1) you have to enumerate the combinations of various sub-types.
   *       e.g. [DenseVector,RangeVector], [DenseVector,IndexVector], ...
   *       there is no common interface to exploit
   *    2) have to use unchecked variance to get the inheritance to work out
   */
  /*
  implicit def repVecToVecOps[A:Manifest](x: Rep[Vector[A]]) = new VecOpsCls1(x)
  implicit def varToVecOps[A:Manifest](x: Var[Vector[A]]) = new VecOpsCls1(readVar(x))

  class CanSum[-A,+R] {
    def apply(a: Rep[A])(implicit ar: Arith[A]): Rep[R]
  }
  class CanSumVector[A] {
    def apply(a: Rep[Vector[A]])(implicit ar: Arith[A]): Rep[A]
  }
  class CanSumDenseVector[A:Manifest] extends CanSumVector[A] {
    def apply(a: Rep[DenseVector[A]])
  }
  // class CanSumDenseVector[A:Manifest] extends CanSum[DenseVector[A],A] {
  //     def apply(a: Rep[DenseVector[A]])(implicit ar: Arith[A]) = a.sum
  //   }  
  implicit def canSumDenseVector[A:Manifest] = new CanSumDenseVector()
  
  // we need CanSumDenseVector[A] <:< CanSum[Vector[A],A], but the function argument needs to be contravariant...  
  class VecOpsCls1[A:Manifest](x: Rep[Vector[A]]) {
    def sum(implicit a: Arith[A], cs: CanSum[Vector[A], A]) = cs(x)
    // but how do you add ? to Dense/Sparse ?
    // sum(Rep[Dense])
    // sum(Rep[Sparse])
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
  // TODO: one type variable is not enough.. e.g. Interface[Vector[A]] (from a DenseVector) + SparseVector, or Interface[Vector[A]] (from a SparseVector) + DenseVector
  // TODO: switch to infix, so that any sub-type can invoke super-type implementations if they exist
  abstract class VecOpsCls[A:Manifest] extends InterfaceOps {
    type V[X]
    val x: Rep[V[A]]
    
    // accessors
    def length: Rep[Int] 
    def +(y: Rep[V[A]])(implicit a: Arith[A]): Rep[V[A]]
    
    // how can we add to another arbitrary vector of a different type (that also implements the vector/delitecollection interface)?
    // seems like it may be doable if we relax delite ops...
  }
  
  // x: Rep[DenseVector[T]]
  // we implicitly convert that to an Interface[Vector[T]]: (this would be better if we didn't need Interface, but then Rep can't simply be an abstract type)  
  implicit def denseToInterfaceVecOps[A:Manifest](lhs: Rep[DenseVector[A]]) = VInterface[A](new DenseVecOpsCls[A](lhs))
  // implicit def sparseToInterfaceVecOps[A:Manifest](lhs: Rep[SparseVector[A]]) = VInterface[A](new SparseVecOpsCls[A](lhs))
  
  // clients that can handle multiple kinds of vector must accept an Interface[Vector[T]],  not a Rep[Vector[T]]
  case class VInterface[A:Manifest](ops: VecOpsCls[A]) extends Interface[Vector[A]] // clients use Interface[Vector]

  // class DenseVecOpsCls[A:Manifest](x: Rep[DenseVector[A]]) extends VecOpsCls[A] {
  //   type V[X] = DenseVector[X]  
  // }
  
  // then we convert from a Interface[Vector[T]] to an interfaceVecToOpsCls, providing all of the original vector methods  
  implicit def interfaceToVecOps[A:Manifest](intf: Interface[Vector[A]]) = new InterfaceVecOpsCls(intf.asInstanceOf[VInterface[A]]) // should only be one instance of Interface[Vector], but can we enforce this?
  
  class InterfaceVecOpsCls[A:Manifest](val intf: VInterface[A]) {
    def length = intf.ops.length
    def +(y: Rep[intf.ops.V[A]])(implicit a: Arith[A]) = intf.ops.+(y)
  }
  
  // object defs
  def vector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[A]]
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp {

  this: SandboxExp =>


  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class VectorNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[A]] {
    val mA = manifest[A]
  }


  def vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(VectorNew[A](len, isRow)) //XXX
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
    case v@VectorNew(length, isRow) => emitValDef(sym, "new generated.scala.VectorImpl[" + remap(v.mA) + "](" + quote(length) + "," + quote(isRow) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
