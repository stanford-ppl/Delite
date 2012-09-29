package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, FunctionsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait AbstractMatrixOps extends Base {

}

trait AbstractMatrixOpsExp extends AbstractMatrixOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: StringOpsExp with WhileExp with MiscOpsExp with VectorOpsExp with FunctionsExp with IfThenElseExp 
    with VariablesExp with MathOpsExp =>

  trait AbstractMatrix {
    def m(): Exp[Int]
    def n(): Exp[Int]
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector]
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector]

    def ::(A: AbstractMatrix): AbstractMatrix
      = amatrix_hcat(A,this)
    def :::(A: AbstractMatrix): AbstractMatrix
      = amatrix_vcat(A,this)
  }

  //matrix sum
  class AbstractMatrixSum(A: AbstractMatrix,B: AbstractMatrix) extends AbstractMatrix {
    def m(): Exp[Int] = A.m()
    def n(): Exp[Int] = A.n()
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = A.get_Ax(x) + B.get_Ax(x)
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = A.get_ATy(y) + B.get_ATy(y)
  }
  def amatrix_sum(A: AbstractMatrix,B: AbstractMatrix): AbstractMatrix
    = new AbstractMatrixSum(A,B)

  //matrix identity
  class AbstractMatrixIdentity(sn: Exp[Int]) extends AbstractMatrix {
    def m(): Exp[Int] = sn
    def n(): Exp[Int] = sn
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = x
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = y
  }
  def amatrix_identity(sn: Exp[Int]): AbstractMatrix
    = new AbstractMatrixIdentity(sn)

  //concatenation for block matrices
  class AbstractMatrixHCat(A: AbstractMatrix, B: AbstractMatrix) extends AbstractMatrix {
    def m(): Exp[Int] = A.m()
    def n(): Exp[Int] = A.n() + B.n()
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_sum(A.get_Ax(vector_select(x,unit(0),A.n())),B.get_Ax(vector_select(x,A.n(),B.n())))
    }
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = {
      vector_cat(A.get_ATy(y),B.get_ATy(y))
    }
  }
  def amatrix_hcat(A: AbstractMatrix,B: AbstractMatrix): AbstractMatrix = {
    //println(unit("Blocking matrices with sizes: ") + string_valueof(A.m()) + " and " + string_valueof(B.m()))
    new AbstractMatrixHCat(A,B)
  }
  def amatrix_vcat(A: AbstractMatrix,B: AbstractMatrix): AbstractMatrix
    = amatrix_transp(amatrix_hcat(amatrix_transp(A),amatrix_transp(B)))

  //zero matrices
  class AbstractMatrixZero(sm: Exp[Int], sn: Exp[Int]) extends AbstractMatrix {
    def m(): Exp[Int] = sm
    def n(): Exp[Int] = sn
    def get_Ax(x: Exp[CVXVector]) = vector_zeros(m)
    def get_ATy(x: Exp[CVXVector]) = vector_zeros(n)
  }
  def amatrix_zeros(m: Exp[Int], n: Exp[Int]): AbstractMatrix
    = new AbstractMatrixZero(m,n)

  //a matrix from a vector
  class AbstractMatrixVector(c: Exp[CVXVector]) extends AbstractMatrix {
    def m(): Exp[Int] = vector_len(c)
    def n(): Exp[Int] = unit(1)
    def get_Ax(x: Exp[CVXVector]) = vector_scale(c,vector_at(x,unit(0)))
    def get_ATy(y: Exp[CVXVector]) = vector1(vector_dot(c,y))
  }
  def amatrix_fromvector(c: Exp[CVXVector]): AbstractMatrix
    = new AbstractMatrixVector(c)

  class AbstractMatrixLambda(fAx: Rep[CVXVector=>CVXVector], fATy: Rep[CVXVector=>CVXVector], sm: Exp[Int], sn: Exp[Int]) extends AbstractMatrix {
    def m() = sm
    def n() = sn
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector]
      = doApply(fAx,x)
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector]
      = doApply(fATy,y)
  }

  def amatrix_lambda(A: AbstractMatrix): AbstractMatrix = {
    new AbstractMatrixLambda(
      doLambda((x: Exp[CVXVector]) => A.get_Ax(x)),
      doLambda((y: Exp[CVXVector]) => A.get_ATy(y)),
      A.m(),
      A.n())
  }

  //matrix product
  class AbstractMatrixProd(A: AbstractMatrix, B: AbstractMatrix) extends AbstractMatrix {
    def m(): Exp[Int] = A.m()
    def n(): Exp[Int] = B.n()
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector]
      = A.get_Ax(B.get_Ax(x))
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector]
      = B.get_ATy(A.get_ATy(y))
  }
  def amatrix_prod(A: AbstractMatrix, B: AbstractMatrix): AbstractMatrix
    = new AbstractMatrixProd(A,B)

  //matrix transpose
  class AbstractMatrixTransp(A: AbstractMatrix) extends AbstractMatrix {
    def m(): Exp[Int] = A.n()
    def n(): Exp[Int] = A.m()
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = A.get_ATy(x)
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = A.get_Ax(y)
  }
  def amatrix_transp(A: AbstractMatrix): AbstractMatrix
    = new AbstractMatrixTransp(A)

  //matrix inverse using lsqr, with caching results as iteration 
  class AbstractMatrixInvLSQR(A: AbstractMatrix, eps: Exp[Double], maxiter: Exp[Int]) extends AbstractMatrix {
    val x0 = var_new[CVXVector](vector_zeros(A.n()))
    val y0 = var_new[CVXVector](vector_zeros(A.m()))
    def m(): Exp[Int] = A.n()
    def n(): Exp[Int] = A.m()
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      val rv = vector_sum(lsqr(A,vector_sum(x,vector_neg(A.get_Ax(x0))),eps,maxiter),x0)
      //val rv = lsqr(A,x,eps,maxiter)
      var_assign(x0, rv)
      //println(Const("         x = ") + vector_to_string_matlab(x))
      //println(Const("    Ainv*x = ") + vector_to_string_matlab(rv))
      //println(Const("A*(Ainv*x) = ") + vector_to_string_matlab(A.get_Ax(rv)))
      //println(Const(""))
      rv
    }
    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = {
      val rv = vector_sum(lsqr(amatrix_transp(A),vector_sum(y,vector_neg(A.get_ATy(y0))),eps,maxiter),y0)
      //val rv = lsqr(amatrix_transp(A),y,eps,maxiter)
      var_assign(y0, rv)
      rv
    }
  }
  def amatrix_inv_lsqr(A: AbstractMatrix, eps: Exp[Double], maxiter: Exp[Int]): AbstractMatrix
    = new AbstractMatrixInvLSQR(A,eps,maxiter)

  //solves the equation Ax=b for x using the LSQR method and returns x
  //TODO: Add stopping criteria based on the eps error parameter
  def lsqr(A: AbstractMatrix, b: Exp[CVXVector], eps: Exp[Double], itermax: Exp[Int]): Exp[CVXVector] = {
    eps = Const(1e-40)
    //initialization code
    val beta_init = math_sqrt(vector_dot(b,b))
    val u_init = vector_scale(b,Const(1.0)/(beta_init+eps))
    val ATu_init = A.get_ATy(u_init)
    val alpha_init = math_sqrt(vector_dot(ATu_init,ATu_init))
    val v_init = vector_scale(ATu_init,Const(1.0)/(alpha_init+eps))
    val w_init = v_init
    val x_init = vector_zeros(A.n())//x0
    val phi_init = beta_init
    val rho_init = alpha_init
    //define the variables
    val beta = var_new[Double](beta_init)
    val u = var_new[CVXVector](u_init)
    val alpha = var_new[Double](alpha_init)
    val v = var_new[CVXVector](v_init)
    val w = var_new[CVXVector](w_init)
    val x = var_new[CVXVector](x_init)
    val phi = var_new[Double](phi_init)
    val rho = var_new[Double](rho_init)
    val iterct = var_new[Int](Const(0))
    //iterate
    __whileDo((iterct < itermax), {
      //bidiagonization step
      val Avau = vector_sum(A.get_Ax(readVar(v)),vector_scale(readVar(u),Const(-1.0)*readVar(alpha)))
      val beta_next = math_sqrt(vector_dot(Avau,Avau))
      val u_next = vector_scale(Avau,Const(1.0)/(beta_next+eps))
      val ATubv = vector_sum(A.get_ATy(u_next),vector_scale(readVar(v),Const(-1.0)*beta_next))
      val alpha_next = math_sqrt(vector_dot(ATubv,ATubv))
      val v_next = vector_scale(ATubv,Const(1.0)/(alpha_next+eps))
      //orthogonal transformation step
      val rho_nobar = math_sqrt(readVar(rho)*readVar(rho)+beta_next*beta_next)
      val c = readVar(rho)/(rho_nobar+eps)
      val s = beta_next/(rho_nobar+eps)
      val theta = s*alpha_next
      val rho_next = Const(-1.0)*c*alpha_next
      val phi_nobar = c*readVar(phi)
      val phi_next = s*readVar(phi)
      //update x,w step
      val x_next = vector_sum(readVar(x), vector_scale(readVar(w), phi_nobar/(rho_nobar+eps)))
      val w_next = vector_sum(v_next, vector_scale(readVar(w),Const(-1.0)*theta/(rho_nobar+eps)))
      //set the variables
      var_assign(beta, beta_next)
      var_assign(u, u_next)
      var_assign(alpha, alpha_next)
      var_assign(v, v_next)
      var_assign(w, w_next)
      var_assign(phi, phi_next)
      var_assign(rho, rho_next)
      var_assign(iterct, readVar(iterct) + Const(1))
      var_assign(x, x_next)
      //println(Const("beta = ") + string_valueof(beta_next))
    })
    //println(Const(""))
    //return the value converged to
    return readVar(x)
  }
}

trait ScalaGenAbstractMatrixOps extends ScalaGenBase {
  val IR: AbstractMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}