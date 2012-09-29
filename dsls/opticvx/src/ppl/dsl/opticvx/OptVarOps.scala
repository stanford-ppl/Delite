package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.{Set}

import java.io.PrintWriter


trait OptVarOps extends Base {

  def variable(sh: Rep[ExprShape]): Rep[OptVar]
  def variable(): Rep[OptVar]
}

trait OptVarOpsExp extends OptVarOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprShapeOpsExp with ExprOpsExp with VectorOpsExp with ConstraintOpsExp with ObjectiveOpsExp =>

  object OptVarTr {
    private var nextID = 0
    def getNextID(): Int = {
      nextID += 1
      nextID
    }
  }

  trait OptVarTr extends ExprTr {
    //for pretty-printing the expressions
    val ID: Int = OptVarTr.getNextID()

    var constraints: Seq[Constraint] = Seq()
    var bound: Boolean = false
    var solved: Boolean = false

    var lookup_offset: Exp[Int] = null

    var value: Exp[CVXVector] = null

    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      if(solved) {
        vector_zeros(size)
      }
      else {
        vector_select(x, lookup_offset, size)
      }
    }
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      if(solved) {
        vector_zeros(sz)
      }
      else {
        vector_cat(vector_cat(vector_zeros(lookup_offset), y), vector_zeros(sz - (lookup_offset + size)))
      }
    }
    def get_b(): Exp[CVXVector] = {
      if(solved) {
        value
      }
      else {
        vector_zeros(size)
      }
    }

    def vexity(): Signum
      = Vexity.affine

    def resolve(): Exp[CVXVector] = {
      if(!solved) {
        throw new Exception("Tried to resolve non-solved variable.")
      }
      value
    }

    def vars(): Set[OptVarTr] = Set[OptVarTr](this)

    override def toString(): String = {
      canonicalize(shape()) match {
        case sh: ExprShapeScalarExp => "x" + ID
        case sh: ExprShapeVectorExp => "v" + ID
        case sh: ExprShapeSMatrixExp => "m" + ID
        case _ => throw new Exception("Error: Invalid expression shape.")
      }
    }
  }

  def canonicalize(x: Exp[OptVar]): OptVarTr = {
    x match {
      case e: OptVarTr =>
        return e
      case s: Sym[OptVar] =>
        findDefinition(s) match {
          case None =>
            throw new Exception("Couldn't canonicalize node " + x)
          case Some(TP(sym,rhs)) =>
            rhs match {
              case e: OptVarTr =>
                return e
              case _ =>
                throw new Exception("Couldn't canonicalize node " + x)
            }
        }
      case _ =>
        throw new Exception("Couldn't canonicalize node " + x)
    }
  }

  class OptVarExp(val sh: Exp[ExprShape]) extends Def[OptVar] with OptVarTr {
    def shape(): Exp[ExprShape]
      = sh

    def asExp: Exp[Expr] = this
  }

  def variable(sh: Exp[ExprShape]): Exp[OptVar]
    = new OptVarExp(sh)

  def variable(): Rep[OptVar] = variable(scalar())
  
}

trait ScalaGenOptVarOps extends ScalaGenBase {
  val IR: OptVarOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}