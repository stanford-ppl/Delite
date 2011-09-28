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
 * This file defines all Dense and Dense-Dense vector operations. 
 */

trait DenseVectorOps extends DSLType with Variables {
  this: Sandbox =>

  implicit def repVecToDenseVecOps[A:Manifest](x: Rep[DenseVector[A]]) = new DenseVecOpsCls(x)
  implicit def varToDenseVecOps[A:Manifest](x: Var[DenseVector[A]]) = new DenseVecOpsCls(readVar(x))

  // TODO: switch this to infix, so that we don't have a chained implicit problem when doing e.g. dense + dense
  // if we change to infix, then we don't have the ops inheritance...
  
  /**
   * Infix methods handle any computation where an implicit conversion is required on the rhs, but the lhs is known.
   * Normally these would not work because of the initial conversion required on the lhs (to DenseVecOps).
   *  
   * The duplication is unfortunate though.. is there a better way of doing this?
   */
  //   def infix_length[A:Manifest](x: Rep[DenseVector[A]]) = vector_length(x)
  //   def infix_isRow[A:Manifest](x: Rep[DenseVector[A]]) = vector_isRow(x)
  //   def infix_slice[A:Manifest](x: Rep[DenseVector[A]], start: Rep[Int], end: Rep[Int]) = vector_slice(x, start, end)
  //   def infix_contains[A:Manifest](x: Rep[DenseVector[A]], y: Rep[A]) = vector_contains(x,y)
  //   def infix_distinct[A:Manifest](x: Rep[DenseVector[A]]) = vector_distinct(x)
  // ...
    
  class DenseVecOpsCls[A:Manifest](val x: Rep[DenseVector[A]]) extends VecOpsCls[A] {
    type V[X] = DenseVector[X]
    
    // accessors
    def length = densevector_length(x)
    
    // because both arguments in a DeliteOpZipWith must be DeliteCollections,
    // this will not work unless Interface itself implements a DeliteCollection...
    // even if it does, we would have to re-think delite op parameters
    def +(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = densevector_plus_dense(x,y)
    //def +(y: Rep[SparseVector[A]])(implicit a: Arith[A]) = densevector_plus_sparse(x,y)
    
    // how should this work? previously it was accepted as an argument because it was a Vector[Int],
    // but now we do not accept arbitrary Vectors
    //def +(y: Rep[RangeVector])
  }

  // class defs
  def densevector_length[A:Manifest](x: Rep[DenseVector[A]]): Rep[Int]
  def densevector_plus_dense[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[DenseVector[A]]
}

trait DenseVectorOpsExp extends DenseVectorOps with VariablesExp with BaseFatExp {

  this: SandboxExp =>

  case class DenseVectorLength[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Int]
  
  abstract class DenseVectorArithmeticZipWith[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]]) extends DeliteOpZipWith[A,A,A,DenseVector[A]] {
    def alloc = Vector.dense[A](inA.length, unit(true))
    val size = copyTransformedOrElse(_.size)(inA.length)
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class DenseVectorPlusDense[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DenseVectorArithmeticZipWith[A](inA, inB) {

    def func = (a,b) => a + b
  }
  
 
  // class interface
  def densevector_length[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorLength(x))
  def densevector_plus_dense[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorPlusDense(x,y))
}

trait BaseGenDenseVectorOps extends GenericFatCodegen {
  val IR: DenseVectorOpsExp
  import IR._
}

trait ScalaGenDenseVectorOps extends BaseGenDenseVectorOps with ScalaGenFat {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case DenseVectorLength(x)    => emitValDef(sym, quote(x) + ".length")
    case _ => super.emitNode(sym, rhs)
  }
}
