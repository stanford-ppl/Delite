package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{Base, BaseExp, ArrayOpsExp, RangeOpsExp, NumericOps, NumericOpsExp}

trait DCPOps extends Base with NumericOps
  with dcp.DCPOps
  with dcp.DCPShape
  with dcp.DCPShapeNames
  with dcp.DCPAlmap
  with dcp.DCPExpr
  with dcp.DCPCone
  with dcp.DCPConstraint
  with dcp.DCPInput {
  
  type RepInt = Rep[Int]
  type RepDouble = Rep[Double]
  
  val repint0: Rep[Int] = unit(0)
}

trait DCPOpsExp extends DCPOps with NumericOpsExp with ArrayOpsExp with RangeOpsExp
  with dcp.DCPOpsExp
  with dcp.DCPProblem {
  
  
}

