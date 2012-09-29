package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait FunctionOps extends Base {
  
  case class VexityAnnot(val v: Signum)
  case class MonotonicityAnnot(val m: Signum)

  val affine = VexityAnnot(Vexity.affine)
  val convex = VexityAnnot(Vexity.convex)
  val concave = VexityAnnot(Vexity.concave)
  val novexity = VexityAnnot(Vexity.none)

  val constant = MonotonicityAnnot(Monotonicity.constant)
  val nondecreasing = MonotonicityAnnot(Monotonicity.nondecreasing)
  val nonincreasing = MonotonicityAnnot(Monotonicity.nonincreasing)
  val nomonotonicity = MonotonicityAnnot(Monotonicity.none)

  val increasing = nondecreasing
  val decreasing = nonincreasing

  case class CvxFunWithVexity(va: Signum) {
    def arguments(ma0: MonotonicityAnnot) =
      CvxFunWithArguments1(va,ma0.m)
    def arguments(ma0: MonotonicityAnnot,ma1: MonotonicityAnnot) =
      CvxFunWithArguments2(va,ma0.m,ma1.m)
  }

  def cvxfun(va: VexityAnnot) =
    CvxFunWithVexity(va.v)

  case class CvxFunWithArguments1(va: Signum, ma0: Signum) {
    def body(fx: (Rep[Expr]) => Rep[Expr])
      = CvxFun1(va,ma0,fx)
  }
  case class CvxFunWithArguments2(va: Signum, ma0: Signum, ma1: Signum) {
    def body(fx: (Rep[Expr],Rep[Expr]) => Rep[Expr])
      = CvxFun2(va,ma0,ma1,fx)
  }
  
  case class CvxFun1(va: Signum, ma0: Signum, fx: (Rep[Expr]) => Rep[Expr]) {
    def apply(a0: Rep[Expr]): Rep[Expr]
      = cvxfun1apply(va,ma0,fx,a0)
  }
  case class CvxFun2(va: Signum, ma0: Signum, ma1: Signum, fx: (Rep[Expr],Rep[Expr]) => Rep[Expr]) {
    def apply(a0: Rep[Expr], a1: Rep[Expr]): Rep[Expr]
      = cvxfun2apply(va,ma0,ma1,fx,a0,a1)
  }

  def cvxfun1apply(va: Signum,
                   ma0: Signum,
                   fx: (Rep[Expr])=>Rep[Expr], 
                   a0: Rep[Expr])
                   : Rep[Expr]

  def cvxfun2apply(va: Signum, 
                   ma0: Signum, ma1: Signum, 
                   fx: (Rep[Expr],Rep[Expr])=>Rep[Expr], 
                   a0: Rep[Expr], a1: Rep[Expr])
                   : Rep[Expr]

}

trait FunctionOpsExp extends FunctionOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprShapeOpsExp with OptVarOpsExp with VectorOpsExp with ExprOpsExp =>

  case class CvxFunAppExp(rv: Exp[Expr], vx: Signum) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = canonicalize(rv).get_Ax(x)
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = canonicalize(rv).get_ATy(y, sz)
    def get_b(): Exp[CVXVector] = canonicalize(rv).get_b()
    
    def vexity(): Signum = vx
    def shape(): Exp[ExprShape] = canonicalize(rv).shape()
    
    def vars(): Set[OptVarTr] = canonicalize(rv).vars()

    def resolve(): Exp[CVXVector] = canonicalize(rv).resolve()

    override def toString(): String = canonicalize(rv).toString()

    def asExp: Exp[Expr] = this
  }

  def cvxfun1apply(va: Signum,
                   ma0: Signum,
                   fx: (Exp[Expr])=>Exp[Expr], 
                   a0: Exp[Expr])
                   : Exp[Expr] = {
    val vf = va + ma0*canonicalize(a0).vexity()
    if(vf == Vexity.none) {
      throw new Exception("DCP Error in function: Could not conclude anything about result vexity.")
    }
    CvxFunAppExp(fx(a0),vf)
  }

  def cvxfun2apply(va: Signum, 
                   ma0: Signum, ma1: Signum, 
                   fx: (Exp[Expr],Exp[Expr])=>Exp[Expr], 
                   a0: Exp[Expr], a1: Exp[Expr])
                   : Exp[Expr] = {
    val vf = va + ma0*canonicalize(a0).vexity() + ma1*canonicalize(a1).vexity()
    if(vf == Vexity.none) {
      throw new Exception("DCP Error in function: Could not conclude anything about result vexity.")
    }
    CvxFunAppExp(fx(a0, a1),vf)
  }

}

trait ScalaGenFunctionOps extends ScalaGenBase {
  val IR: FunctionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}