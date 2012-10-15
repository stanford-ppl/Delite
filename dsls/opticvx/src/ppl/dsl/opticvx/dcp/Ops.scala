package ppl.dsl.opticvx.dcp

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{Base, BaseExp, ArrayOpsExp}

trait DCPOps extends Base {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr =>

  def cvxvar(shape: Shape = ShapeScalar()): OptVar
  def cvxinput(input: Rep[Float], sign: Signum = Signum.All): Expr
  def cvxinputvector(input: Rep[Array[Float]], sign: Signum = Signum.All): Expr
  def cvxintparam(param: Rep[Int]): IntParam
  
}

trait DCPOpsExp extends DCPOps with BaseExp with ArrayOpsExp {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr =>

  def cvxvar(shape: Shape = ShapeScalar()): OptVar
    = new OptVar(shape)
  
  def cvxinput(input: Exp[Float], sign: Signum = Signum.All): Expr
    = new ExprInputScalar(input, sign)
  
  def cvxinputvector(input: Exp[Array[Float]], sign: Signum = Signum.All): Expr
    = new ExprInputVector(cvxintparam(input.length), input, sign)
  
  def cvxintparam(param: Exp[Int]): IntParam
    = new IntParamInput(param)
}
