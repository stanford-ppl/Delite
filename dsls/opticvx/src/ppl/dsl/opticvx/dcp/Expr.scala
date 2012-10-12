package ppl.dsl.opticvx.dcp

trait DCPExpr {
  self: DCPShape with DCPShapeNames =>


  trait Expr {
    def shape: VShape
  }
  
  class ExprVar extends Expr {
    def shape: VShape = affine
  }
  
  class ExprConstant extends Expr {
    def shape: VShape = affine
  }
  
}