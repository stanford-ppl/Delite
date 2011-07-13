package ppl.dsl.deliszt.vec

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/26/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class VecPrivateOps extends DSLType with Variables {
  this: DeLiszt =>

  object Vec {
  }

  implicit def repVecToVecPrivateOps[N <: IntM, VT : Manifest](x: Rep[Vec[N, VT]]) = new vecOpsCls(x)
  implicit def varToVecPrivateOps[N <: IntM, VT : Manifest](x: Var[Vec[N, VT]]) = new vecOpsCls(readVar(x))

  /**
   * This class defines the private interface for the Vec[T] class.
   */
  class vecOpsCls[N <: IntM, VT : Manifest](x: Rep[Vec[N, VT]]) {
    type Self = Vec[N,VT]

		def map
  }
}

trait VecPrivateOpsExp extends VecPrivateOps with VariablesExp with BaseFatExp with CleanRoom {
  this: VecImplOps with DeLisztExp =>

  def reflectPure[VT:Manifest](x: Def[VT]): Exp[VT] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  ////////////////////////////////
  // implemented via delite ops

  abstract class DeliteOpVecLoop[VT] extends DeliteOpLoop[Vec[VT]] {
    val size: Exp[Int] //inherited
  }

  //////////////
  // mirroring

  override def mirror[VT:Manifest](e: Def[VT], f: Transformer): Exp[VT] = (e match {
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[VT]] // why??


  /////////////////////
  // object interface

  /////////////////////
  // class interface
  case class VectorZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[Vector[A]], inB: Exp[Vector[B]],
                                                             v: (Sym[A],Sym[B]), func: Exp[R])
    extends DeliteOpZipWith[A,B,R,Vector] {

    val alloc = reifyEffects(Vector[R](inA.length, inA.isRow))
  }
}

trait VecPrivateOpsExpOpt extends VecPrivateOpsExp {
  this: VecPrivateImplOps with DeLisztExp =>
}

trait ScalaGenVecPrivateOps extends ScalaGenBase {
  val IR: VecPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVecPrivateOps extends CudaGenBase with CudaGenDataStruct {
  val IR: VecPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVecPrivateOps extends CGenBase {
  val IR: VecPrivateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
