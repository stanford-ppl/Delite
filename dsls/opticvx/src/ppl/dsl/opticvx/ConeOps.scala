package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, PrimitiveOps, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait ConeOps extends Base {

}

trait ConeOpsExp extends ConeOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: StringOpsExp with WhileExp with MiscOpsExp with VectorOpsExp with IfThenElseExp 
    with VariablesExp with MathOpsExp with PrimitiveOps =>

  abstract class Cone {
    def size(): Exp[Int]
    def contains(x: Exp[CVXVector]): Exp[Boolean]
    def contains_dual(y: Exp[CVXVector]): Exp[Boolean]
    def project(x: Exp[CVXVector]): Exp[CVXVector]
    def project_dual(y: Exp[CVXVector]): Exp[CVXVector]
  }

  abstract class SelfDualCone extends Cone {
    final def contains_dual(y: Exp[CVXVector]): Exp[Boolean]
      = contains(y)
    final def project_dual(y: Exp[CVXVector]): Exp[CVXVector]
      = project(y)
  }

  case class NullCone extends SelfDualCone {
    def size(): Exp[Int] = unit(0)
    def contains(x: Exp[CVXVector]): Exp[Boolean] = unit(true)
    def project(x: Exp[CVXVector]): Exp[CVXVector] = vector_zeros(unit(0))
  }

  case class CartesianProductCone(C1: Cone, C2: Cone) extends Cone {
    def size(): Exp[Int] = C1.size() + C2.size()
    def select1(x: Exp[CVXVector]): Exp[CVXVector] = vector_select(x,unit(0),C1.size())
    def select2(x: Exp[CVXVector]): Exp[CVXVector] = vector_select(x,C1.size(),C2.size())
    def contains(x: Exp[CVXVector]): Exp[Boolean] = {
      C1.contains(select1(x)) && C2.contains(select2(x))
    }
    def contains_dual(x: Exp[CVXVector]): Exp[Boolean] = {
      C1.contains_dual(select1(x)) && C2.contains_dual(select2(x))
    }
    def project(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_cat(C1.project(select1(x)),C2.project(select2(x)))
    }
    def project_dual(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_cat(C1.project_dual(select1(x)),C2.project_dual(select2(x)))
    }
  }

  def coneproduct(C1: Cone, C2: Cone): Cone = {
    if((C1.isInstanceOf[NullCone])&&(C2.isInstanceOf[NullCone])) {
      NullCone()
    }
    else if(C1.isInstanceOf[NullCone]) {
      C2
    }
    else if(C2.isInstanceOf[NullCone]) {
      C1
    }
    else {
      CartesianProductCone(C1,C2)
    }
  }

  case class DualCone(C: Cone) extends Cone {
    def size(): Exp[Int] = C.size()
    def contains(x: Exp[CVXVector]): Exp[Boolean] = C.contains_dual(x)
    def contains_dual(y: Exp[CVXVector]): Exp[Boolean] = C.contains(y)
    def project(x: Exp[CVXVector]): Exp[CVXVector] = C.project_dual(x)
    def project_dual(y: Exp[CVXVector]): Exp[CVXVector] = C.project(y)
  }
  def conedual(C: Cone): Cone = {
    DualCone(C)
  }

  case class NonNegativeSimplexCone(n: Exp[Int]) extends SelfDualCone {
    def size(): Exp[Int] = n
    def contains(x: Exp[CVXVector]): Exp[Boolean] = {
      val ix = var_new[Int](unit(0))
      val rv = var_new[Boolean](unit(true))
      __whileDo((readVar(ix) < n)&&(readVar(rv)), {
        var_assign(rv, readVar(rv) && (vector_at(x,readVar(ix)) >= unit(0.0)))
        var_assign(ix, readVar(ix) + unit(1))
      })
      readVar(rv)
    }
    def project(x: Exp[CVXVector]): Exp[CVXVector] = {
      val ix = var_new[Int](unit(0))
      val rv = var_new[CVXVector](vector_zeros(unit(0)))
      __whileDo(readVar(ix) < n, {
        var_assign(rv, vector_cat(readVar(rv), vector1(math_max(unit(0.0), vector_at(x,readVar(ix))))))
        var_assign(ix, readVar(ix) + unit(1))
      })
      readVar(rv)
    }
  }

  case class SecondOrderCone(n: Exp[Int]) extends SelfDualCone {
    def size(): Exp[Int] = n + unit(1)
    def contains(x: Exp[CVXVector]): Exp[Boolean] = {
      val v = vector_select(x, unit(0), n)
      val z = vector_at(x, n)
      ((vector_dot(v,v)) <= z*z)&&(unit(0.0) <= z)
    }
    def project(x: Exp[CVXVector]): Exp[CVXVector] = {
      val v = vector_select(x, unit(0), n)
      val z = vector_at(x, n)
      val norm2v = vector_dot(v,v)
      if((z*z) >= norm2v) {
        if(z <= Const(0.0)) {
          //projection is onto the zero point
          vector_zeros(n + unit(1))
        }
        else {
          //projection retains the original value
          x
        }
      }
      else {
        //use the projection formula on pg447 of Boyd and Vandenberghe
        val normv: Exp[Double] = math_sqrt(norm2v)
        val cscale: Exp[Double] = unit(0.5)*(unit(1.0) + z/normv)
        vector_cat(vector_scale(v, cscale), vector1(normv * cscale))
      }
    }
  }

  case class SemidefiniteCone(n: Exp[Int]) extends SelfDualCone {
    def size(): Exp[Int] = n*(n+1)/unit(2)
    def contains(x: Exp[CVXVector]): Exp[Boolean] = {
      throw new Exception("Semidefinite cone not implemented.")
    }
    def project(x: Exp[CVXVector]): Exp[CVXVector] = {
      throw new Exception("Semidefinite cone not implemented.")
    }
  }

  /*
  case class SymmetricCone(
    //size of unconstrained variables
    //val unconstrained_sz: Exp[Int], 
    //size of positive simplex variables
    val psimplex_sz: Exp[Int],
    //n values for second-order-cone constraints 
    val soc_ns: Seq[Exp[Int]],
    //n values for definitness constraints
    val definite_ns: Seq[Exp[Int]]
  ) {
    def contains(x: Exp[CVXVector]): Exp[Boolean] = {
      var ind = unit(0) //unconstrained_sz
      val xsimplex = vector_select(x,ind,psimplex_sz);
      ind  = ind + psimplex_sz
      var rv = vector_ispositive(xsimplex)
      for(n <- soc_ns) {
        val cx: Exp[CVXVector] = vector_select(x,ind,n)
        ind = ind + n
        val cz: Exp[Double] = vector_at(x, ind)
        ind = ind + unit(1)
        val norm2cx: Exp[Double] = vector_dot(cx,cx);
        rv = rv && ((cz*cz) >= norm2cx)
      }
      for(n <- definite_ns) {
        //throw an error as this projection is not implemented
        throw new Exception("Definitness constraints not implemented yet.")
      }
      rv
    }
    def project(x: Exp[CVXVector]): Exp[CVXVector] = {
      val rv = var_new[CVXVector](vector_zeros(Const(0)))
      var ind: Exp[Int] = Const(0)
      //pass the constants through unmodified
      //var_assign(rv, vector_cat(readVar(rv), vector_select(x, ind, unconstrained_sz)))
      //ind = ind + unconstrained_sz
      //make the positive simplex nodes positive
      var_assign(rv, vector_cat(readVar(rv), vector_positive_part(vector_select(x, ind, psimplex_sz))))
      ind = ind + psimplex_sz
      //project onto second-order cones
      for(n <- soc_ns) {
        val cx: Exp[CVXVector] = vector_select(x, ind, n)
        ind = ind + n
        val cz: Exp[Double] = vector_at(x, ind)
        ind = ind + Const(1)
        val norm2cx: Exp[Double] = vector_dot(cx,cx);
        if((cz*cz) >= norm2cx) {
          if(cz <= Const(0.0)) {
            //projection is onto the zero point
            var_assign(rv, vector_cat(readVar(rv), vector_zeros(n + Const(1))))
          }
          else {
            //projection retains the original value
            var_assign(rv, vector_cat(readVar(rv), cx))
            var_assign(rv, vector_cat(readVar(rv), vector1(cz)))
          }
        }
        else {
          //use the projection formula on pg447 of Boyd and Vandenberghe
          val normcx: Exp[Double] = math_sqrt(norm2cx)
          val cscale: Exp[Double] = Const(0.5)*(Const(1.0) + cz/normcx)
          var_assign(rv, vector_cat(readVar(rv), vector_scale(cx, cscale)))
          var_assign(rv, vector_cat(readVar(rv), vector1(normcx * cscale)))
        }
      }
      //project onto semidefinite cone
      for(n <- definite_ns) {
        //throw an error as this projection is not implemented
        throw new Exception("Definitness constraints not implemented yet.")
      }
      //return the accumulated vector
      return readVar(rv)    
    }
    //def project_dual(x: Exp[CVXVector]): Exp[CVXVector] = {
    //  val proj = project(x)
    //  vector_cat(vector_zeros(unconstrained_sz),vector_select(proj,unconstrained_sz,vector_len(proj)-unconstrained_sz))
    //}
  }
  */
}

trait ScalaGenConeOps extends ScalaGenBase {
  val IR: ConeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}