package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait ObjectiveOps extends Base {
  self: ExprOps =>

  case class MinimizeObjStatement(x: Rep[Expr]) {
    def over(vs: Rep[OptVar]*) = minimize_over(x,vs)
  }
  def minimize(x: Rep[Expr]) = MinimizeObjStatement(x)

  def maximize(x: Rep[Expr]) = minimize(-x)

  def minimize_over(x: Rep[Expr], vs: Seq[Rep[OptVar]]): Rep[Unit]
}

trait ObjectiveOpsExp extends ObjectiveOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with ExprShapeOpsExp with ConstraintOpsExp with MatlabCVXOpsExp
    with OptVarOpsExp with SolverOpsExp with VectorOpsExp with AbstractMatrixOpsExp
    with ConeOpsExp =>

  def minimize_over(x: Exp[Expr], vs: Seq[Exp[OptVar]]): Exp[Unit] = {
    val cx = canonicalize(x)
    //check that the expression to minimize is scalar and convex
    if(!(cx.vexity() <= Vexity.convex)) {
      throw new Exception("Could not minimize non-convex expression.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not minimize non-scalar expression.")
    }
    //bind all the variables to optimize over
    for(v <- vs) {
      val cv = canonicalize(v)
      if(cv.bound == true) {
        throw new Exception("Variable in optimize-set was already bound to another optimization statement.")
      }
      cv.bound = true
    }
    //accumulate the set of all variables that are connected to this objective by constraints
    var convars: Set[OptVarTr] = cx.vars()
    for(v <- vs) {
      val cv = canonicalize(v)
      convars += cv
      convars ++= cv.vars()
    }
    //iterate, gathering more vars until stable
    {
      var next_convars: Set[OptVarTr] = convars
      do {
        convars = next_convars
        for(vv <- convars) {
          for(constraint <- vv.constraints) {
            next_convars ++= constraint.vars()
          }
        }
      } while(next_convars != convars)
    }
    //verify that all the connected vars are bound
    for(v <- convars) {
      if(v.bound == false) {
        println("Found partial optimization statement over " + vs.length + " variables; not solving yet.")
        return
      }
    }
    println("Encountered optimization statement over " + vs.length + " variables (reduced to " + convars.size + "); proceeding to transform.")
    //collect all the constraints
    var constraints: Set[Constraint] = Set()
    for(v <- convars) {
      constraints ++= v.constraints
    }
    //DEBUG display the variables and constraints
    println("Partially-transformed problem: ")
    println("  variables ->")
    var strout = "    "
    for(v <- convars) {
      strout += v + " " //"(" + v.size + ") "
    }
    println(strout)
    println("  constraints ->")
    for(c <- constraints) {
      println("    " + c)
    }
    //we now assign limits to the variables
    var problem_size: Exp[Int] = Const(0)
    for(v <- convars) {
      if(v.solved == false) {
        v.lookup_offset = problem_size
        problem_size = problem_size + v.size
      }
    }
    //output the problem in matlab
    //matlab_print_problem(cx,constraints,problem_size)
    //sort the constraints
    println("Transforming " + constraints.size + " constraints.")
    //val unconstrained_sz = problem_size
    //var psimplex_sz: Exp[Int] = Const(0)
    //var soc_ns: Seq[Exp[Int]] = Seq()
    //var definite_ns: Seq[Exp[Int]] = Seq()
    var zero_exps: Seq[ExprTr] = Seq()
    var cone_exps: Seq[ExprTr] = Seq()
    var cone: Cone = NullCone()
    for(c <- constraints) {
      c match {
        case cc: ConstrainZero =>
          val x = cc.x
          println("Processing equality constraint...")
          zero_exps :+= x
        case _ =>
          //println("Deferring non-equality constraint of type " + c.getClass())
      }
    }
    for(c <- constraints) {
      c match {
        case cc: ConstrainNonnegative =>
          val x = cc.x
          println("Processing nonnegative constraint...")
          cone = coneproduct(cone,NonNegativeSimplexCone(unit(1)))
          cone_exps :+= x
        case cc: ConstrainSecondOrderCone =>
          val x = cc.x
          val z = cc.z
          println("Processing second-order cone constraint...")
          canonicalize(x.shape()) match {
            case ExprShapeVectorExp(n) =>
              cone = coneproduct(cone,SecondOrderCone(n))
            case _ =>
              throw new Exception("Internal Error: Invalid shape on SOC constraint.")
          }
          cone_exps :+= x
          cone_exps :+= z
        case cc: ConstrainRotatedCone =>
          val x = cc.x
          val y = cc.y
          val z = cc.z
          println("Processing rotated cone constraint...")
          canonicalize(x.shape()) match {
            case ExprShapeVectorExp(n) =>
              cone = coneproduct(cone, SecondOrderCone(n+unit(1)))
            case _ =>
              throw new Exception("Internal Error: Invalid shape on rotated cone constraint.")
          }
          cone_exps :+= x
          cone_exps :+= canonicalize((y.asExp - z.asExp)*0.5)
          cone_exps :+= canonicalize((y.asExp + z.asExp)*0.5)
        case cc: ConstrainSemidefinite =>
          val x = cc.x
          println("Processing semidefinite constraint...")
          canonicalize(x.shape()) match {
            case ExprShapeSMatrixExp(n) =>
              cone = coneproduct(cone,SemidefiniteCone(n))
            case _ =>
              throw new Exception("Internal Error: Invalid shape on definiteness constraint.")
          }
          cone_exps :+= x
        case _ =>
          //println("Deferring non-nonnegative constraint of type " + c.getClass())
      }
    }
    println("Transformed to " + zero_exps.length + " total equality and " + cone_exps.length + " total inequality constraints.")
    //convert into standard form
    val stdAx = new ExprSeqMatrix(zero_exps, problem_size)
    val stdAz = new ExprSeqMatrix(cone_exps, problem_size)
    var stdBx = vector_zeros(Const(0))
    for(x <- zero_exps) {
      stdBx = vector_cat(stdBx, x.get_b())
    }
    var stdBz = vector_zeros(Const(0))
    for(x <- cone_exps) {
      stdBz = vector_cat(stdBz, x.get_b())
    }
    //stdB = vector_neg(stdB)
    val stdC = cx.get_ATy(vector1(Const(1.0)), problem_size)
    val stdK = cone
    //invoke the solver
    val solution = solve(stdAx, stdAz, stdBx, stdBz, stdC, stdK)
    //distribute the solution
    for(v <- convars) {
      if(v.solved == false) {
        v.value = vector_sum(v.get_Ax(solution),v.get_b())
        v.lookup_offset = null
        v.solved = true
      }
    }
  }

  class ExprSeqMatrix(es: Seq[ExprTr], sz: Exp[Int]) extends AbstractMatrix {
    def m(): Exp[Int] = {
      var rv: Exp[Int] = Const(0)
      for(e <- es) {
        rv = (rv + e.size)
      }
      rv
    }

    def n(): Exp[Int] = sz

    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      var rv: Exp[CVXVector] = vector_zeros(Const(0))
      for(e <- es) {
        rv = vector_cat(rv, e.get_Ax(x))
      }
      rv
    }

    def get_ATy(y: Exp[CVXVector]): Exp[CVXVector] = {
      var rv: Exp[CVXVector] = vector_zeros(sz)
      var ind: Exp[Int] = Const(0)
      for(e <- es) {
        rv = vector_sum(rv, e.get_ATy(vector_select(y, ind, e.size), sz))
        ind = ind + e.size
      }
      rv
    }
  }
}

trait ScalaGenObjectiveOps extends ScalaGenBase {
  val IR: ObjectiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}