package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, WhileExp, StringOpsExp, BooleanOpsExp, MiscOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, VariablesExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait MatlabCVXOps extends Base {

}

trait MatlabCVXOpsExp extends MatlabCVXOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with OptVarOpsExp with ExprShapeOpsExp with StringOpsExp with WhileExp
    with MiscOpsExp with ConstraintOpsExp with VectorOpsExp with IfThenElseExp with VariablesExp =>

  case class SolverExp(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]) extends Def[Unit]

  def make_line(v: Var[String], s: Exp[String]): Exp[Unit] = {
    var_assign(v, readVar(v) + s + Const("\\n"))
  }
  
  def matlab_make_expression(vrv: Var[String], x: ExprTr, sz: Exp[Int]): Exp[Unit] = {
    make_line(vrv, Const("AA = [];"))
    val vari = var_new[Int](Const(0))
    __whileDo(readVar(vari) < x.size, {
      val tt = vector_cat(vector_cat(vector_zeros(readVar(vari)),vector1(Const(1))),vector_zeros(x.size-readVar(vari)-Const(1)))
      val at = x.get_ATy(tt, sz)
      make_line(vrv, Const("AA = vertcat(AA,") + vector_to_string_matlab(at) + Const(");"))
      var_assign(vari, readVar(vari) + Const(1))
    })
    val bt = x.get_b()
    make_line(vrv, Const("bb = ") + vector_to_string_matlab(bt) + Const(";"))
  }
  
  def matlab_make_problem(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]): Exp[String] = {
    val vrv = var_new[String](Const(""))
    val vrmmct = var_new[Int](Const(0))
    make_line(vrv, Const("cvx_begin"))
    make_line(vrv, Const("variable x(") + string_valueof(sz) + Const(")"))
    for(c <- cs) {
      make_line(vrv, Const("% " + c.toString()))
      c match {
        case ConstrainZero(x: ExprTr) =>
          matlab_make_expression(vrv,x,sz)
          make_line(vrv, Const("uu = AA*x + transpose(bb);")) 
          make_line(vrv, Const("0 == uu;"))

        case ConstrainNonnegative(x: ExprTr) =>
          matlab_make_expression(vrv,x,sz)
          make_line(vrv,Const("uu = AA*x + transpose(bb);")) 
          make_line(vrv,Const("0 <= uu;"))
          //val at = x.get_ATy(vector1(Const(1)), sz)
          //val bt = x.get_b()
          //println(Const("0 <= ") + vector_to_string_matlab(at) + Const("*x + ") + vector_to_string_matlab(bt))

        case ConstrainSecondOrderCone(x: ExprTr, z: ExprTr) =>
          matlab_make_expression(vrv,x,sz)
          make_line(vrv,Const("uu = AA*x + transpose(bb);"))
          matlab_make_expression(vrv,z,sz)
          make_line(vrv,Const("zz = AA*x + transpose(bb);"))
          make_line(vrv,Const("norm(uu) <= zz;"))

        case ConstrainSemidefinite(x: ExprTr) =>
          matlab_make_expression(vrv,x,sz)
          make_line(vrv,Const("uu = AA*x + transpose(bb);"))
          val msz = canonicalize(x.shape()).asInstanceOf[ExprShapeSMatrixExp]
          make_line(vrv,Const("expression mm") + string_valueof(readVar(vrmmct)) + Const("(") + string_valueof(msz.n) + Const(");"))
          make_line(vrv,Const("for ii = 0:") + string_valueof(msz.n-Const(1)))
          make_line(vrv,Const("for jj = 0:") + string_valueof(msz.n-Const(1)))
          make_line(vrv,Const("kk = max(ii,jj)*(max(ii,jj)+1)/2 + min(ii,jj);"))
          make_line(vrv,Const("mm") + string_valueof(readVar(vrmmct)) + Const("(ii+1,jj+1) = uu(kk+1);"))
          make_line(vrv,Const("end"))
          make_line(vrv,Const("end"))
          make_line(vrv,"mm" + string_valueof(readVar(vrmmct)) + " == semidefinite(" + msz.n + ");")
          var_assign(vrmmct, readVar(vrmmct) + Const(1))

        case _ =>
          throw new Exception("Error: On solver emission, invalid constraint.")
      }
    }
    val atobj = obj.get_ATy(vector1(Const(1)), sz)
    val btobj = obj.get_b()
    make_line(vrv,Const("minimize ") + vector_to_string_matlab(atobj) + Const("*x + ") + vector_to_string_matlab(btobj))
    make_line(vrv,Const("cvx_end"))
    make_line(vrv,Const("fprintf(\'$$$OUTPUT BEGIN$$$\\\\n\');"))
    make_line(vrv,Const("fprintf(\'%g\\\\n\',x);"))
    make_line(vrv,Const("fprintf(\'$$$OUTPUT END$$$\\\\n\');"))
    make_line(vrv,Const("fprintf(\'$$$OUTPUT END$$$\\\\n\');"))
    readVar(vrv)
  }

  def matlab_print_problem(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]): Exp[Unit] = {
    print(matlab_make_problem(obj,cs,sz))
  }
  /*
  def terribleSolver(obj: ExprTr, cs: Set[Constraint], sz: Exp[Int]): Exp[CVXVector] = {
    val eps = Const(0.01)
    val objvect_uns = obj.get_ATy(vector1(Const(1)),sz)
    val objvect = vector_scale(objvect_uns, Const(1.0)/Math.sqrt(vector_dot(objvect_uns, objvect_uns)))
    val ovscale = var_new[Double](Const(10.0))
    val xx = var_new[CVXVector](vector_zeros(sz))
    val bfeas = var_new[Boolean](Const(false))
    __whileDo((!bfeas)||(readVar(ovscale) >= eps), {
      //println(Const("iterating: "))
      var_assign(bfeas, Const(true))
      var xxi: Exp[CVXVector] = readVar(xx)
      for(c <- cs) {
        val cvalid = c.valid(xxi,eps)
        var_assign(bfeas, readVar(bfeas) && cvalid)
        xxi = c.project(xxi)
        //println(Const("  (") + string_valueof(cvalid) + Const(") ") + vector_to_string_matlab(xxi)) 
        if(!c.valid(xxi,eps)) {
          println(Const("Warning: Constraint projection did not result in valid constraint."))
        }
      }
      if(bfeas) {
        //println(Const("Performing objective step..."))
        var_assign(xx, vector_sum(xxi, vector_scale(objvect, ovscale*Const(-1.0))))
        var_assign(ovscale, readVar(ovscale) * Const(0.95))
      }
      else {
        var_assign(xx, xxi)
      }
    })
    println(Const("converged!"))
    xx
  }
  */
  
  //case class MatlabSolveExp(mpstr: Exp[String], sz: Exp[Int]) extends Def[CVXVector]
  
}

trait ScalaGenMatlabCVXOps extends ScalaGenBase {
  val IR: MatlabCVXOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      /*
      case MatlabSolveExp(mpstr, sz) =>
        println("Emitting solver code...")
        stream.println("val " + quote(sym) + " = new Array[Double](" + quote(sz) + ")")
        stream.println("val rt = java.lang.Runtime.getRuntime()")
        stream.println("val p = rt.exec(\"matlab\")")
        stream.println("println(\"write: \" + " + quote(mpstr) + ")")
        stream.println("p.getOutputStream().write(" + quote(mpstr) + ".getBytes())")
        stream.println("p.getOutputStream().write(\"exit;\\n\".getBytes())")
        stream.println("p.getOutputStream().close()")
        stream.println("val ins = new java.io.BufferedReader(new java.io.InputStreamReader(p.getInputStream()))")
        stream.println("var rlrst: String = \"\"")
        stream.println("while((rlrst = ins.readLine()) != \"$$$OUTPUT BEGIN$$$\") {")
        stream.println("if(rlrst == null) throw new Exception(\"MATLAB read returned null.\")")
        stream.println("println(\"read: \" + rlrst)")
        stream.println("}")
        stream.println("for(ii <- 0 until " + quote(sz) + ") {")
        stream.println(quote(sym) + "(ii) = ins.readLine().toDouble")
        stream.println("}")
        stream.println("if(ins.readLine() != \"$$$OUTPUT END$$$\") {")
        stream.println("throw new Exception(\"Output length error in MATLAB interface.\")")
        stream.println("}")
        stream.println("p.waitFor()")
      */
      /*
      case SolverExp(obj,cs,sz) =>
        println("Emitting solver code...")
        stream.println("println(\"cvx_begin\")")
        stream.println("println(\"variable x(\" + " + quote(sz) + " + \")\")")
        stream.println("println(\"cvx_end\")")
      */

      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}