package ppl.dsl.experimental

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.experimental._

/**
 * This file defines all Sparse and Sparse-Sparse vector operations. 
 */

trait SparseVectorOps extends DSLType with Variables {
  this: Sandbox =>

  implicit def repVecToSparseVecOps[A:Manifest](x: Rep[SparseVector[A]]) = new SparseVecOpsCls(x)
  implicit def varToSparseVecOps[A:Manifest](x: Var[SparseVector[A]]) = new SparseVecOpsCls(readVar(x))

  class SparseVecOpsCls[A:Manifest](val x: Rep[SparseVector[A]]) extends VecOpsCls[A] {
    type V[X] = SparseVector[X]
    
    // accessors
    def length = sparsevector_length(x)
    
    // SparseVector + Generic = DenseVector[X]
    override type VPLUSR[X] = DenseVector[X]
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = sparsevector_plus_generic(x,y)
    def +(y: Rep[SparseVector[A]])(implicit a: Arith[A]) = sparsevector_plus_sparse(x,y)
    
  }

  // class defs
  def sparsevector_length[A:Manifest](x: Rep[SparseVector[A]]): Rep[Int]
  def sparsevector_plus_sparse[A:Manifest:Arith](x: Rep[SparseVector[A]], y: Rep[SparseVector[A]]): Rep[SparseVector[A]]
  def sparsevector_plus_generic[A:Manifest:Arith](x: Rep[SparseVector[A]], y: Interface[Vector[A]]): Rep[DenseVector[A]]
}

trait SparseVectorOpsExp extends SparseVectorOps with VariablesExp with BaseFatExp {

  this: SandboxExp =>

  case class SparseVectorLength[A:Manifest](x: Exp[SparseVector[A]]) extends Def[Int]
  
  abstract class SparseVectorArithmeticZipWith[A:Manifest:Arith](inA: Exp[SparseVector[A]], inB: Exp[SparseVector[A]]) extends DeliteOpZipWith[A,A,A,SparseVector[A]] {
    def alloc = Vector.sparse[A](inA.length, unit(true))
    val size = copyTransformedOrElse(_.size)(inA.length)
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class SparseVectorPlusSparse[A:Manifest:Arith](inA: Exp[SparseVector[A]], inB: Exp[SparseVector[A]])
    extends SparseVectorArithmeticZipWith[A](inA, inB) {

    def func = (a,b) => a + b
  }

  case class SparseVectorPlusGeneric[A:Manifest:Arith](inA: Exp[SparseVector[A]], inB: Interface[Vector[A]]) extends Def[DenseVector[A]]
 
  // class interface
  def sparsevector_length[A:Manifest](x: Exp[SparseVector[A]]) = reflectPure(SparseVectorLength(x))
  def sparsevector_plus_sparse[A:Manifest:Arith](x: Exp[SparseVector[A]], y: Exp[SparseVector[A]]) = reflectPure(SparseVectorPlusSparse(x,y))
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
    case _ => super.emitNode(sym, rhs)
  }
}
