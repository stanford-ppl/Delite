package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, PrimitiveOps, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait SolverOps extends Base {

}

trait SolverOpsExp extends SolverOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with OptVarOpsExp with ExprShapeOpsExp with StringOpsExp with WhileExp
    with MiscOpsExp with ConstraintOpsExp with VectorOpsExp with IfThenElseExp 
    with VariablesExp with MathOpsExp with AbstractMatrixOpsExp with PrimitiveOps
    with ConeOpsExp =>
  
  //minimize c'*x subject to Ax*x + bx = 0 and Az*x + bz \in K
  def solve(Ax: AbstractMatrix, Az: AbstractMatrix, bx: Exp[CVXVector], bz: Exp[CVXVector], c: Exp[CVXVector], K: Cone): Exp[CVXVector] = {
    new ADMMSolver(Ax,Az,bx,bz,c,K,unit(1.0)).solve()
  }
    
  trait Solver {
    def solve(): Exp[CVXVector]
  }
  
  class ADMMSolver(Ax: AbstractMatrix, Az: AbstractMatrix, bx: Exp[CVXVector], bz: Exp[CVXVector], c: Exp[CVXVector], K: Cone, rho: Exp[Double]) extends Solver {
    
    var PA: AbstractMatrix = null
    
    def setup() {
      Ax = amatrix_lambda(Ax)
      Az = amatrix_lambda(Az)
      val ATA = amatrix_sum(amatrix_prod(amatrix_transp(Ax),Ax),amatrix_prod(amatrix_transp(Az),Az))
      PA = amatrix_inv_lsqr(ATA,Const(0.001),Const(10))
      PA = amatrix_lambda(PA)
    }
    
    def solve(): Exp[CVXVector] = {
      //println(unit("sizes = (") + string_valueof(Az.m()) + unit(", ") + string_valueof(K.size()) + unit(")"))
      setup()
      //set up ADMM variables
      val x = var_new[CVXVector](vector_zeros(Ax.n()))
      val z = var_new[CVXVector](vector_zeros(Az.m()))
      val ux = var_new[CVXVector](vector_zeros(Ax.m()))
      val uz = var_new[CVXVector](vector_zeros(Az.m()))
      //iterate
      val niters = var_new[Int](unit(0))
      val loopdone = var_new[Boolean](unit(false))
      __whileDo((niters <= unit(1000))&&(!readVar(loopdone)), {
        //println(unit("x = ") + vector_to_string_matlab(x))
        //update x
        var_assign(x, update_x(readVar(z),readVar(ux),readVar(uz)))
        //update z
        var_assign(z, update_z(readVar(x),readVar(uz)))
        //update u
        var_assign(ux, readVar(ux) + Ax.get_Ax(readVar(x)) + bx)
        var_assign(uz, readVar(uz) + Az.get_Ax(readVar(x)) + readVar(z) + bz)
        //evaluate stopping conditions
        val Ex = Ax.get_Ax(readVar(x)) + bx
        val Ez = Az.get_Ax(readVar(x)) + readVar(z) + bz
        val Err = math_sqrt(vector_dot(Ex,Ex) + vector_dot(Ez,Ez))
        //println(unit("Err = ") + string_valueof(Err))
        var_assign(loopdone, (Err <= unit(1e-5))&&(niters >= unit(5)))
        //increment loop variable
        var_assign(niters, readVar(niters)+Const(1))
      })
      println(unit("Converged in ") + string_valueof(niters) + unit(" steps."))
      x
    }
    
    def update_x(z: Exp[CVXVector], ux: Exp[CVXVector], uz: Exp[CVXVector]): Exp[CVXVector] = {
      vector_neg(PA.get_Ax(vector_scale(c,unit(1.0)/rho) + Az.get_ATy(z + bz + uz) + Ax.get_ATy(bx + ux)))
    }
    
    def update_z(x: Exp[CVXVector], uz: Exp[CVXVector]): Exp[CVXVector] = {
      vector_neg(K.project(bz + Az.get_Ax(x) + uz))
    }
    
  }
  
  /*
  //minimize c'*x subject to A*x = b and x \in K
  def solve(A: AbstractMatrix, b: Exp[CVXVector], c: Exp[CVXVector], K: SymmetricCone): Exp[CVXVector] = {
    println(Const("Matrix A is ") + string_valueof(A.m()) + Const(" by ") + string_valueof(A.n()))
    new PrimalDualHomogenousSolver(A,b,c,K).solve()
  }

  
  class PrimalDualHomogenousSolver(A: AbstractMatrix, b: Exp[CVXVector], c: Exp[CVXVector], K: SymmetricCone) extends Solver {
    
    var M: AbstractMatrix = null
    var PM: AbstractMatrix = null
    
    def setup() {
      A = amatrix_lambda(A)
      //initialize homogenous embedding matrix
      M = A :: amatrix_zeros(A.m(),A.m()+A.n()-K.unconstrained_sz) :: amatrix_fromvector(vector_neg(b)) :: amatrix_zeros(A.m(),unit(1))
      //println(unit("M is now ") + string_valueof(M.m()) + "x" + string_valueof(M.n()))
      M :::= amatrix_zeros(A.n(),A.n()) :: amatrix_transp(A) :: (amatrix_zeros(K.unconstrained_sz,A.n()-K.unconstrained_sz) ::: amatrix_identity(A.n()-K.unconstrained_sz)) :: amatrix_fromvector(vector_neg(c)) :: amatrix_zeros(A.n(),unit(1))
      //println(unit("M is now ") + string_valueof(M.m()) + "x" + string_valueof(M.n()))
      M :::= amatrix_transp(amatrix_fromvector(c)) :: amatrix_transp(amatrix_fromvector(vector_neg(b))) :: amatrix_zeros(unit(1),A.n()+unit(1)-K.unconstrained_sz) :: amatrix_identity(unit(1))
      //println(unit("M is now ") + string_valueof(M.m()) + "x" + string_valueof(M.n()))
      //initialize projection matrix
      val MMTinv = amatrix_inv_lsqr(amatrix_prod(M,amatrix_transp(M)),Const(0.001),Const(10))
      PM = amatrix_prod(amatrix_prod(amatrix_transp(M),MMTinv),M)
      PM = amatrix_lambda(PM)
    }
    
    def select_x(vv: Exp[CVXVector]) = vector_select(vv,unit(0),A.n())
    def select_y(vv: Exp[CVXVector]) = vector_select(vv,A.n(),A.m())
    def select_lambda(vv: Exp[CVXVector]) = vector_select(vv,A.n()+A.m(),A.n()-K.unconstrained_sz)
    def select_tau(vv: Exp[CVXVector]) = vector_at(vv,A.n()+A.m()+A.n()-K.unconstrained_sz)
    def select_rho(vv: Exp[CVXVector]) = vector_at(vv,A.n()+A.m()+A.n()-K.unconstrained_sz+unit(1))
    
    def project_onto_KK(vv: Exp[CVXVector]): Exp[CVXVector] = {
      //println(unit("selecting x..."))
      val x = select_x(vv)
      //println(unit("selecting y..."))
      val y = select_y(vv)
      //println(unit("selecting lambda..."))
      val lambda = select_lambda(vv)
      //println(unit("selecting tau..."))
      val tau = select_tau(vv)
      //println(unit("selecting rho..."))
      val rho = select_rho(vv)
      
      //println(unit("projecting x..."))
      val x_proj = K.project(x)
      //println(unit("projecting lambda..."))
      val lambda_proj = vector_select(K.project(vector_cat(vector_zeros(K.unconstrained_sz),lambda)),K.unconstrained_sz,A.n()-K.unconstrained_sz)
      val tau_proj = math_max(unit(1.0),tau)
      val rho_proj = math_max(unit(0.0),rho)
      
      //println(unit("assembling result..."))
      val rv = vector_cat(vector_cat(vector_cat(vector_cat(x_proj, y),lambda_proj),vector1(tau_proj)),vector1(rho_proj))
//       println(unit("len(x) = ") + string_valueof(vector_len(x)))
//       println(unit("len(PK(x)) = ") + string_valueof(vector_len(x_proj)))
//       println(unit("len(lambda) = ") + string_valueof(vector_len(lambda)))
//       println(unit("len(PK(lambda)) = ") + string_valueof(vector_len(lambda_proj)))
//       println(unit("len(vv) = ") + string_valueof(vector_len(vv)))
//       println(unit("len(rv) = ") + string_valueof(vector_len(rv)))
      rv
    }
    
    def in_KK(vv: Exp[CVXVector]): Exp[Boolean] = {
      val x = select_x(vv)
      val lambda = select_lambda(vv)
      K.contains(x) && K.contains(vector_cat(vector_zeros(K.unconstrained_sz),lambda))
    }
    
    def project_onto_Mx(vv: Exp[CVXVector]): Exp[CVXVector] = {
      val rv = vector_sum(vv,vector_neg(PM.get_Ax(vv)))
      //println(unit("vv = ") + vector_to_string_matlab(vv))
      //println(unit("P(vv) = ") + vector_to_string_matlab(rv))
      //println(unit("MP(vv) = ") + vector_to_string_matlab(M.get_Ax(rv)))
      //println(unit(""))
      val erv = M.get_Ax(rv)
      //println(unit("err = ") + string_valueof(math_sqrt(vector_dot(erv,erv))))
      rv
    }
    
    def overproject_onto_KK(vv: Exp[CVXVector]): Exp[CVXVector] = {
      val rv = project_onto_KK(vv)
      val dv = vector_sum(rv,vector_neg(vv))
      vector_sum(vv,vector_scale(dv,unit(1.5)))
    }
    
    def overproject_onto_Mx(vv: Exp[CVXVector]): Exp[CVXVector] = {
      val rv = project_onto_Mx(vv)
      val dv = vector_sum(rv,vector_neg(vv))
      vector_sum(vv,vector_scale(dv,unit(1.5)))
    }
    
    def solve(): Exp[CVXVector] = {
      setup()
      println(Const("Solving..."))
      val vv = var_new[CVXVector](vector_cat(vector_cat(vector_zeros(M.n()-unit(2)),vector1(unit(1.0))),vector1(unit(0.0))))
      val niters = var_new[Int](Const(0))
      __whileDo(niters <= Const(10000), {
        //println(unit("Projecting onto affine constraint..."))
        var_assign(vv, overproject_onto_Mx(readVar(vv)))
        //println(unit("Projecting onto conic constraint..."))
        var_assign(vv, overproject_onto_KK(readVar(vv)))
        //might want to add a strictly unnecessary renormalization step
        //var_assign(vv, vector_scale(vv,unit(1.0)/math_sqrt(vector_dot(vv,vv))))
        var_assign(niters, readVar(niters)+Const(1))
      })
//       println(unit("Solved!"))
//       println(unit("b = ") + vector_to_string_matlab(b))
//       println(unit("c = ") + vector_to_string_matlab(c))
//       println(unit("x = ") + vector_to_string_matlab(select_x(vv)))
//       println(unit("y = ") + vector_to_string_matlab(select_y(vv)))
//       val cx = vector_dot(c,select_x(vv))
//       println(unit("cx = ") + string_valueof(cx))
//       val by = vector_dot(b,select_y(vv))
//       println(unit("by = ") + string_valueof(by))
//       println(unit("L = ") + vector_to_string_matlab(select_lambda(vv)))
//       println(unit("T = ") + string_valueof(select_tau(vv)))
//       println(unit("R = ") + string_valueof(select_rho(vv)))
//       val MSTL = amatrix_transp(amatrix_fromvector(c)) :: amatrix_transp(amatrix_fromvector(vector_neg(b))) :: amatrix_zeros(unit(1),A.n()+unit(1)) :: amatrix_identity(unit(1))
//       println(unit("cx-by+T = ") + string_valueof(vector_at(MSTL.get_Ax(vv),unit(0))))
//       println(unit("MSTL = ") + vector_to_string_matlab(MSTL.get_ATy(vector1(unit(1.0)))))
//       val EE = M.get_Ax(vv)
//       val nE = math_sqrt(vector_dot(EE,EE))
//       println(unit("EV = ") + vector_to_string_matlab(EE))
//       println(unit("E = ") + string_valueof(nE))
//       println(unit(""))
      vector_scale(select_x(vv),unit(1.0)/select_tau(vv))
    }
    
  }
  
  class ProjectedGradientSolver(A: AbstractMatrix, b: Exp[CVXVector], c: Exp[CVXVector], K: SymmetricCone) extends Solver {
    var Ainv_b: Exp[CVXVector] = null
    var PA: AbstractMatrix = null
    var c_hat: Exp[CVXVector] = null
    
    
    def solve(): Exp[CVXVector] = {
      A = amatrix_lambda(A)
      //println(Const("Setting up solver..."))
      println(Const("Solving..."))
      setup()
      val x = var_new[CVXVector](vector_zeros(A.n()))
      val niters = var_new[Int](Const(0))
      __whileDo(niters <= Const(10000), {
        //print(Const("Iteration ") + string_valueof(readVar(niters)) + ": " + vector_to_string_matlab(readVar(x)))
        var_assign(x, vector_sum(readVar(x),vector_scale(c_hat,Const(-1.0)*step_size(niters))))
        //print(Const(" -> ") + vector_to_string_matlab(readVar(x)))
        var_assign(x, project_onto_K(readVar(x)))
        //println(Const(" -> ") + vector_to_string_matlab(readVar(x)))
        var_assign(x, project_onto_Axb(readVar(x)))
        var_assign(niters, readVar(niters)+Const(1))
      })
      x
    }
    
    def setup() {
      //compute A^-1*b
      //println(Const("A = ") + vector_to_string_matlab(A.get_ATy(vector1(Const(1.0)))))
      //println(Const("b = ") + vector_to_string_matlab(b))
      //println(Const("c = ") + vector_to_string_matlab(c))
      //println(Const("K.sz_unc = ") + string_valueof(K.unconstrained_sz))
      //println(Const("K.sz_pos = ") + string_valueof(K.psimplex_sz))
      //println(Const(""))
      val Ainv = amatrix_inv_lsqr(A,Const(0.001),Const(20))
      Ainv_b = Ainv.get_Ax(b)
      //println(Const("    Ainv*b = ") + vector_to_string_matlab(Ainv_b))
      //println(Const("A*(Ainv*b) = ") + vector_to_string_matlab(A.get_Ax(Ainv_b)))
      //setup the projection matrix
      val AATinv = amatrix_inv_lsqr(amatrix_prod(A,amatrix_transp(A)),Const(0.001),Const(10))
      PA = amatrix_prod(amatrix_prod(amatrix_transp(A),AATinv),A)
      PA = amatrix_lambda(PA)
      //normalize the objective
      c_hat = vector_scale(c, Const(1.0)/math_sqrt(vector_dot(c,c)))
    }
    
    def step_size(iter: Exp[Int]): Exp[Double] = {
      Const(1.0)/math_sqrt(repIntToRepDouble(iter+Const(1)))
    }
    
    def project_onto_Axb(x: Exp[CVXVector]): Exp[CVXVector] = {
      //xm = x - Ainv*b
      //println(Const("Projection call size: ") + string_valueof(vector_len(x)))
      //println(Const("Matrix PA is ") + string_valueof(PA.m()) + Const(" by ") + string_valueof(PA.n()))
      val xm = vector_sum(x,vector_scale(Ainv_b,Const(-1.0)))
      //
      val xmp = vector_sum(xm,vector_scale(PA.get_Ax(xm),Const(-1.0)))
      //
      val xrv = vector_sum(xmp,Ainv_b)
      //println(Const("Projection return size: ") + string_valueof(vector_len(xrv)))
      //return the computed value
      //println(Const("in = ") + vector_to_string_matlab(x))
      //println(Const(" b = ") + vector_to_string_matlab(b))
      //println(Const("rv = ") + vector_to_string_matlab(xrv))
      //println(Const("Am = ") + vector_to_string_matlab(A.get_Ax(xmp)))
      //println(Const("Ar = ") + vector_to_string_matlab(A.get_Ax(xrv)))
      //println(Const(""))
      return xrv
    }
    
    def project_onto_K(x: Exp[CVXVector]): Exp[CVXVector] = {
      K.project(x)
    }
  }
  
  
  class PrimalProjectionSolver(A: AbstractMatrix, b: Exp[CVXVector], c: Exp[CVXVector], K: SymmetricCone) extends Solver {
    var Ainv_b: Exp[CVXVector] = null
    var PA: AbstractMatrix = null
    var c_hat: Exp[CVXVector] = null
    
    
    def solve(): Exp[CVXVector] = {
      A = amatrix_lambda(A)
      //println(Const("Setting up solver..."))
      println(Const("Solving..."))
      setup()
      val x = var_new[CVXVector](vector_zeros(A.n()))
      val iterend = var_new[Boolean](unit(false))
      val niters = var_new[Int](Const(0))
      __whileDo((niters <= Const(100))&&(!readVar(iterend)), {
        //print(Const("Iteration ") + string_valueof(readVar(niters)) + ": " + vector_to_string_matlab(readVar(x)))
        //no objective step
        //var_assign(x, vector_sum(readVar(x),vector_scale(c_hat,Const(-1.0)*step_size(niters))))
        //print(Const(" -> ") + vector_to_string_matlab(readVar(x)))
        var_assign(x, project_onto_K(readVar(x)))
        //println(Const(" -> ") + vector_to_string_matlab(readVar(x)))
        var_assign(x, project_onto_Axb(readVar(x)))
        var_assign(iterend, in_K(readVar(x)))
        var_assign(niters, readVar(niters)+Const(1))
      })
      println(unit("Feasibile point finder converged in ") + string_valueof(readVar(niters)) + unit(" steps"))
      println(unit("x = ") + vector_to_string_matlab(readVar(x)))
      //get objective search direction
      var objd = var_new[CVXVector](project_onto_Ax(vector_neg(c)))
      //reset iteration count
      var_assign(niters,unit(0))
      var_assign(iterend,unit(false))
      __whileDo((niters <= unit(100))&&(!readVar(iterend)), {
        val xnext = vector_sum(x, readVar(objd))
        if(in_K(xnext)) {
          var_assign(x, xnext)
          var_assign(objd, vector_scale(readVar(objd),unit(2.0)))
        }
        else {
          var_assign(objd, vector_scale(readVar(objd),unit(0.5)))
        }
        var_assign(niters, readVar(niters)+Const(1))
        var_assign(iterend, vector_dot(readVar(objd),readVar(objd)) <= unit(1e-10))
      })
      println(unit("Objective stepper converged in ") + string_valueof(readVar(niters)) + unit(" steps"))
      println(unit("x = ") + vector_to_string_matlab(readVar(x)))      
      readVar(x)
    }
    
    def setup() {
      //compute A^-1*b
      val Ainv = amatrix_inv_lsqr(A,Const(0.001),Const(20))
      Ainv_b = Ainv.get_Ax(b)
      //setup the projection matrix
      val AATinv = amatrix_inv_lsqr(amatrix_prod(A,amatrix_transp(A)),Const(0.001),Const(10))
      PA = amatrix_prod(amatrix_prod(amatrix_transp(A),AATinv),A)
      PA = amatrix_lambda(PA)
      //normalize the objective
      c_hat = vector_scale(c, Const(1.0)/math_sqrt(vector_dot(c,c)))
    }
    
    def project_onto_Axb(x: Exp[CVXVector]): Exp[CVXVector] = {
      val xm = vector_sum(x,vector_scale(Ainv_b,Const(-1.0)))
      val xmp = vector_sum(xm,vector_scale(PA.get_Ax(xm),Const(-1.0)))
      val xrv = vector_sum(xmp,Ainv_b)
      return xrv
    }
    
    def project_onto_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_sum(x,vector_scale(PA.get_Ax(x),Const(-1.0)))
    }
    
    def project_onto_K(x: Exp[CVXVector]): Exp[CVXVector] = {
      val px = K.project(x)
      vector_sum(x,vector_scale(vector_sum(px,vector_neg(x)),unit(1.5)))
    }
    
    def in_K(x: Exp[CVXVector]): Exp[Boolean] = {
      K.contains(x)
    }
  }
  */
}

trait ScalaGenSolverOps extends ScalaGenBase {
  val IR: SolverOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}