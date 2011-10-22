package ppl.dsl.experimental

import ppl.dsl.optiml.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.experimental._

/**
 * This file defines all Sparse and Sparse-Sparse vector operations. 
 */

trait SparseVectorOps extends Variables {
  this: Sandbox =>

  implicit def repVecToSparseVecOps[A:Manifest](x: Rep[SparseVector[A]]) = new SparseVecOpsCls(x)
  implicit def varToSparseVecOps[A:Manifest](x: Var[SparseVector[A]]) = new SparseVecOpsCls(readVar(x))

  class SparseVecOpsCls[A:Manifest](val elem: Rep[SparseVector[A]]) extends VecOpsCls[A] {
    type V[X] = SparseVector[X]
    type Self = SparseVector[A]
    def wrap(x: Rep[SparseVector[A]]) = sparseToInterface(x)    
    implicit def toIntf[B:Manifest](x: Rep[SparseVector[B]]): Interface[Vector[B]] = sparseToInterface(x)
    implicit def builder[B:Manifest]: VectorBuilder[B,V[B]] = sparseVectorBuilder[B]
    implicit def mVB[B:Manifest] = manifest[SparseVector[B]]
    val x = elem
    
    def mutable = throw new UnsupportedOperationException("not implemented yet")
    
    def dcSize = sparsevector_length(x)
    def dcApply(n: Rep[Int]): Rep[A] = sparsevector_apply(x,n)
    def dcUpdate(n: Rep[Int], y: Rep[A]): Rep[Unit] = sparsevector_update(x,n,y)
  
    // accessors
    def length = sparsevector_length(x)
    
    type VPLUSR = DenseVector[A]
    def vplusToIntf(x: Rep[VPLUSR]) = denseToInterface(x)
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = sparsevector_plus_generic(x,y)    
    // unfortunately, this doesn't work unless we pass around complicated structural types
    //def +(y: Interface[Vector[A]]{ val ops: SparseVecOpsCls[A] })(implicit a: Arith[A], o: Overloaded1) = sparsevector_plus_sparse(x,y.ops.x)
    def +(y: Rep[SparseVector[A]])(implicit a: Arith[A]) = sparsevector_plus_sparse(x,y)
  }

  // class defs
  def sparsevector_length[A:Manifest](x: Rep[SparseVector[A]]): Rep[Int]
  def sparsevector_apply[A:Manifest](x: Rep[SparseVector[A]], n: Rep[Int]): Rep[A]
  def sparsevector_update[A:Manifest](x: Rep[SparseVector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]    
  def sparsevector_plus_sparse[A:Manifest:Arith](x: Rep[SparseVector[A]], y: Rep[SparseVector[A]]): Rep[SparseVector[A]]
  def sparsevector_plus_generic[A:Manifest:Arith](x: Rep[SparseVector[A]], y: Interface[Vector[A]]): Rep[DenseVector[A]]
}

trait SparseVectorOpsExp extends SparseVectorOps with VariablesExp with BaseFatExp {

  this: SandboxExp =>

  case class SparseVectorLength[A:Manifest](x: Exp[SparseVector[A]]) extends Def[Int]
  case class SparseVectorApply[A:Manifest](x: Exp[SparseVector[A]], n: Exp[Int]) extends Def[A]
  case class SparseVectorUpdate[A:Manifest](x: Exp[SparseVector[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  
  abstract class SparseVectorArithmeticZipWith[A:Manifest:Arith,RA <: DeliteCollection[A]:Manifest](inA: Interface[Vector[A]], inB: Interface[Vector[A]]) extends DeliteOpZipWith[A,A,A,RA] {
    val size = copyTransformedOrElse(_.size)(inA.length)
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  abstract class SparseVectorPlus[A:Manifest:Arith,RA <: DeliteCollection[A]:Manifest](inA: Interface[Vector[A]], inB: Interface[Vector[A]]) extends SparseVectorArithmeticZipWith[A,RA](inA,inB) {
    def func = (a,b) => a + b
  }

  case class SparseVectorPlusSparse[A:Manifest:Arith](inA: Interface[Vector[A]], inB: Interface[Vector[A]]) extends SparseVectorPlus[A,SparseVector[A]](inA,inB) {
    def alloc = Vector.sparse[A](inA.length, unit(true))    
  }
  
  case class SparseVectorPlusGeneric[A:Manifest:Arith](inA: Interface[Vector[A]], inB: Interface[Vector[A]]) extends SparseVectorPlus[A,DenseVector[A]](inA,inB) {
    def alloc = Vector.dense[A](inA.length, unit(true))
  }
 
  // class interface
  def sparsevector_length[A:Manifest](x: Exp[SparseVector[A]]) = reflectPure(SparseVectorLength(x))
  def sparsevector_apply[A:Manifest](x: Exp[SparseVector[A]], n: Exp[Int]) = reflectPure(SparseVectorApply(x,n))
  def sparsevector_update[A:Manifest](x: Exp[SparseVector[A]], n: Exp[Int], y: Exp[A]) = reflectWrite(x)(SparseVectorUpdate(x,n,y))      
  def sparsevector_plus_sparse[A:Manifest:Arith](x: Exp[SparseVector[A]], y: Exp[SparseVector[A]]) = reflectPure(SparseVectorPlusSparse(sparseToInterface(x),sparseToInterface(y)))
  def sparsevector_plus_generic[A:Manifest:Arith](x: Rep[SparseVector[A]], y: Interface[Vector[A]]) = reflectPure(SparseVectorPlusGeneric(x,y))
}

trait BaseGenSparseVectorOps extends GenericFatCodegen {
  val IR: SparseVectorOpsExp
  import IR._
}

trait ScalaGenSparseVectorOps extends BaseGenSparseVectorOps with ScalaGenFat {
  val IR: SparseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case SparseVectorLength(x)    => emitValDef(sym, quote(x) + ".length")
    case SparseVectorApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case SparseVectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))    
    case _ => super.emitNode(sym, rhs)
  }
}
