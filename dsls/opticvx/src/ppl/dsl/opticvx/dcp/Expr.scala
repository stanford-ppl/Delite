package ppl.dsl.opticvx.dcp

trait DCPExpr {
  self: DCPShape =>
  
  abstract class Expr {
    def desc: VexityDesc
  }
  
  class OptVar(shape: Shape) extends Expr {
    def desc: VexityDesc = affineVexityDesc(shape)
  }
  
  def affineVexityDesc(shape: Shape): VexityDesc = {
    shape match {
      case ShapeScalar() => VexityDescScalar(Vexity.affine, Signum.All)
      case ShapeFor(size, body) => VexityDescFor(size, affineVexityDesc(body))
      case ShapeStruct(body) => VexityDescStruct(body map affineVexityDesc)
    }
  }
  
}