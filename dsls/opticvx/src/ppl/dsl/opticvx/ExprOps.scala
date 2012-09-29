package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait ExprOps extends Base {

  def reshape(x: Rep[Expr], sh: Rep[ExprShape]): Rep[Expr]

  def sum(x: Rep[Expr], y: Rep[Expr]): Rep[Expr]
  def neg(x: Rep[Expr]): Rep[Expr]
  def shapeof(x: Rep[Expr]): Rep[ExprShape]

  def prod(x: Rep[Expr], a: Rep[Double], sa: Signum): Rep[Expr]

  def infix_+(x: Rep[Expr], y: Rep[Expr]): Rep[Expr] = sum(x,y)
  def infix_-(x: Rep[Expr], y: Rep[Expr]): Rep[Expr] = sum(x,neg(y))

  case class MultiplicativeScalar(val value: Rep[Double], val sign: Signum)

  def infix_*(x: Rep[Expr], y: MultiplicativeScalar): Rep[Expr] = prod(x, y.value, y.sign)
  def infix_*(x: MultiplicativeScalar, y: Rep[Expr]): Rep[Expr] = prod(y, x.value, x.sign)

  //these lines are needed to fix some sort of conflict with OptiLA
  def infix_*(x: Double, y: Rep[Expr]): Rep[Expr] = prod(y, unit(x), Signum.sgn(x))
  def infix_*(y: Rep[Expr], x: Double): Rep[Expr] = prod(y, unit(x), Signum.sgn(x))

  implicit def doubleToMultScalar(x: Double): MultiplicativeScalar
    = MultiplicativeScalar(unit(x), Signum.sgn(x))

  implicit def floatToMultScalar(x: Float): MultiplicativeScalar
    = doubleToMultScalar(x.toDouble)

  implicit def intToMultScalar(x: Int): MultiplicativeScalar
    = doubleToMultScalar(x.toDouble)

  implicit def repdoubleToMultScalar(x: Rep[Double]): MultiplicativeScalar
    = MultiplicativeScalar(x, Signum.All)

  def positive(x: Rep[Double]): MultiplicativeScalar

  def negative(x: Rep[Double]): MultiplicativeScalar


  case class ExprOpsImplicitHack(x: Rep[Expr]) {
    def unary_-(): Rep[Expr] = neg(x)
    def shape = shapeof(x)
    def apply(i: Rep[Int]) = indexat(x,i)
    def apply(i: Rep[Int], j: Rep[Int]) = indexat(x,i,j)
  }
  implicit def expropsimplicithack(x: Rep[Expr])
    = ExprOpsImplicitHack(x)

  def indexat(x: Rep[Expr], i: Rep[Int]): Rep[Expr]
  def indexat(x: Rep[Expr], i: Rep[Int], j: Rep[Int]): Rep[Expr]

  def resolve(x: Rep[Expr]): Rep[Double]

  def introspect(x: Rep[Expr]): Rep[Unit]
  def introspect(x: Rep[Expr], s: String): Rep[Unit]
}

trait ExprOpsExp extends ExprOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with DeliteOpsExp with EffectExp {
  self: ExprShapeOpsExp with OptVarOpsExp with VectorOpsExp with IfThenElseExp =>

  def introspect(x: Rep[Expr]): Rep[Unit] = {
    println("[" + Console.BLUE + "introspect" + Console.RESET + "] Unnamed expression has vexity " + Vexity.format(canonicalize(x).vexity()) + ".")
  }

  def introspect(x: Rep[Expr], s: String): Rep[Unit] = {
    println("[" + Console.BLUE + "introspect" + Console.RESET + "] Expression \"" + s + "\" has vexity " + Vexity.format(canonicalize(x).vexity()) + ".")
  }

  def positive(x: Exp[Double]): MultiplicativeScalar = {
    if(x < unit(0.0)) {
      println("Warning: Scalar expression constrained to be positive is negative.")
    }
    MultiplicativeScalar(x, Signum.Positive)
  }

  def negative(x: Exp[Double]): MultiplicativeScalar = {
    if(x > unit(0.0)) {
      println("Warning: Scalar expression constrained to be negative is positive.")
    }
    MultiplicativeScalar(x, Signum.Negative)
  }

  trait ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector]
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector]
    def get_b(): Exp[CVXVector]
    
    def vexity(): Signum
    def shape(): Exp[ExprShape]

    def vars(): Set[OptVarTr]

    def resolve(): Exp[CVXVector]

    def size: Exp[Int] = canonicalize(shape()).size

    def asExp: Exp[Expr]
  }

  def canonicalize(x: Exp[Expr]): ExprTr = {
    x match {
      case e: ExprTr =>
        return e
      case s: Sym[Expr] =>
        findDefinition(s) match {
          case None =>
            throw new Exception("Couldn't canonicalize node " + x)
          case Some(TP(sym,rhs)) =>
            rhs match {
              case e: ExprTr =>
                return e
              case _ =>
                throw new Exception("Couldn't canonicalize node " + x)
            }
        }
      case _ =>
        throw new Exception("Couldn't canonicalize node " + x)
    }
  }

  case class ExprReshapeExp(a: Exp[Expr], sh: Exp[ExprShape]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = canonicalize(a).get_Ax(x)
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = canonicalize(a).get_ATy(y,sz)
    def get_b(): Exp[CVXVector] = canonicalize(a).get_b()

    def resolve(): Exp[CVXVector] = canonicalize(a).resolve()

    def vexity(): Signum = Vexity.affine

    def shape(): Exp[ExprShape] = sh

    def vars(): Set[OptVarTr] = canonicalize(a).vars()

    override def toString(): String = canonicalize(a).toString()

    def asExp: Exp[Expr] = this
  }
  def reshape(x: Rep[Expr], sh: Rep[ExprShape]): Rep[Expr] = {
    //if(!(canonicalize(x).vexity() <= Vexity.affine)) {
    //  throw new Exception("Error: Can't reshape a non-affine expression.")
    //}
    if(canonicalize(x).size != canonicalize(sh).size) {
      println(unit("Error in reshape: size mismatch."))
    }
    ExprReshapeExp(x,sh)
  }

  case class ExprSumExp(a: Exp[Expr], b: Exp[Expr]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      val ax = canonicalize(a).get_Ax(x)
      val bx = canonicalize(b).get_Ax(x)
      vector_sum(ax,bx)
    }
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      val ay = canonicalize(a).get_ATy(y,sz)
      val by = canonicalize(b).get_ATy(y,sz)
      vector_sum(ay,by)
    }
    def get_b(): Exp[CVXVector] = {
      val ab = canonicalize(a).get_b()
      val bb = canonicalize(b).get_b()
      vector_sum(ab,bb)
    }

    def resolve(): Exp[CVXVector] = {
      vector_sum(canonicalize(a).resolve(), canonicalize(b).resolve())
    }

    def vexity(): Signum
      = canonicalize(a).vexity() + canonicalize(b).vexity()

    def shape(): Exp[ExprShape]
      = canonicalize(a).shape()

    def vars(): Set[OptVarTr]
      = canonicalize(a).vars() ++ canonicalize(b).vars()

    override def toString(): String
      = "(" + canonicalize(a).toString() + " + " + canonicalize(b).toString() + ")"
  
    def asExp: Exp[Expr] = this
  }
  def sum(x: Exp[Expr], y: Exp[Expr]): Exp[Expr] = 
    ExprSumExp(x,y)
  

  case class ExprNegExp(a: Exp[Expr]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      val ax = canonicalize(a).get_Ax(x)
      vector_neg(ax)
    }
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      val ay = canonicalize(a).get_ATy(y,sz)
      vector_neg(ay)
    }
    def get_b(): Exp[CVXVector] = {
      val ab = canonicalize(a).get_b()
      vector_neg(ab)
    }

    def resolve(): Exp[CVXVector] = {
      vector_neg(canonicalize(a).resolve())
    }

    def vexity(): Signum
      = -(canonicalize(a).vexity())

    def shape(): Exp[ExprShape]
      = canonicalize(a).shape()

    def vars(): Set[OptVarTr]
      = canonicalize(a).vars()

    override def toString(): String
      = "(-" + canonicalize(a).toString() + ")"

    def asExp: Exp[Expr] = this
  }
  def neg(x: Exp[Expr]): Exp[Expr] = 
    ExprNegExp(x)

  case class ExprProdExp(a: Exp[Expr], c: Exp[Double], sc: Signum) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      val ax = canonicalize(a).get_Ax(x)
      vector_scale(ax,c)
    }

    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      val ay = canonicalize(a).get_ATy(y,sz)
      vector_scale(ay,c)
    }

    def get_b(): Exp[CVXVector] = {
      val ab = canonicalize(a).get_b()
      vector_scale(ab,c)
    }
    
    def vexity(): Signum
      = canonicalize(a).vexity() * sc
    def shape(): Exp[ExprShape]
      = canonicalize(a).shape()

    def vars(): Set[OptVarTr]
      = canonicalize(a).vars()

    def resolve(): Exp[CVXVector]
      = vector_scale(canonicalize(a).resolve(),c)

    override def toString(): String
      = "(" + canonicalize(a).toString() + "*" + c.toString() + ")"

    def asExp: Exp[Expr] = this
  }
  def prod(x: Exp[Expr], a: Exp[Double], sa: Signum): Exp[Expr]
    = ExprProdExp(x,a,sa)

  def shapeof(x: Exp[Expr]): Exp[ExprShape] =
    canonicalize(x).shape()
  
  case class ExprIndexExp(a: Exp[Expr], i: Exp[Int]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_select(canonicalize(a).get_Ax(x), i, Const(1))
    }
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      canonicalize(a).get_ATy(vector_cat(vector_cat(vector_zeros(i),y),vector_zeros(canonicalize(a).size-i-Const(1))),sz)
    }
    def get_b(): Exp[CVXVector] = {
      vector_select(canonicalize(a).get_b(), i, Const(1))
    }

    def resolve(): Exp[CVXVector] = {
      vector_select(canonicalize(a).resolve(), i, Const(1))
    }
    
    def vexity(): Signum = {
      if(canonicalize(a).vexity() <= Vexity.affine) Vexity.affine
      else Vexity.none
    }
    def shape(): Exp[ExprShape] = scalar()

    def vars(): Set[OptVarTr]
      = canonicalize(a).vars()

    override def toString(): String
      = "(" + canonicalize(a).toString() + "[...])"

    def asExp: Exp[Expr] = this
  }
  
  def indexat(x: Exp[Expr], i: Exp[Int]): Exp[Expr] = {
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeVectorExp => ExprIndexExp(x,i)
      case _ => throw new Exception("Could not index non-vector expression as a vector.")
    }
  }

  def indexat(x: Exp[Expr], i: Exp[Int], j: Exp[Int]): Exp[Expr] = {
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeSMatrixExp => ExprIndexExp(x,sh.indexof(i,j))
      case _ => throw new Exception("Could not index non-matrix expression as a matrix.")
    }
  }

  def resolve(x: Exp[Expr]): Exp[Double] = {
    canonicalize(canonicalize(x).shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not resolve non-scalar expression as double.")
    }
    vector_at(canonicalize(x).resolve(),Const(0))
  }

}

trait ScalaGenExprOps extends ScalaGenBase {
  val IR: ExprOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}