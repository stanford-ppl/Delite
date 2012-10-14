package ppl.dsl.opticvx.dcp

trait DCPOps {
  self: DCPShape with DCPShapeNames with DCPSize with DCPExpr =>

  def cvxvar(shape: Shape = ShapeScalar()): OptVar
  def cvxinput(): Expr
  def cvxintparam(): IntParam
  
}
