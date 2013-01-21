package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsFunctionPrimitives extends DCPOpsFunction {

  val in_positive_cone = Function.fromcone(ConeNonNegative(0))
  val in_secondorder_cone = Function.fromcone(ConeSecondOrder(IRPoly.param(0, 1)))


}