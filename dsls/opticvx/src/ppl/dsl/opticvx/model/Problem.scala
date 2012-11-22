package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class Problem(
  val objective: Almap,
  val affineAlmap: Almap,
  val affineOffset: Almap,
  val conicAlmap: Almap,
  val conicOffset: Almap,
  val conicCone: Cone) extends HasArity[Problem]
{
  val arity: Int = affineAlmap.arity
  val varSize: IRPoly = affineAlmap.domain
  val inputSize: IRPoly = affineAlmap.input
  val coneSize: IRPoly = conicAlmap.codomain
  val affineCstrtSize: IRPoly = affineAlmap.codomain
  
  //Verify that all expressions have the same arity
  if (affineOffset.arity != arity) throw new IRValidationException()
  if (conicAlmap.arity != arity) throw new IRValidationException()
  if (conicOffset.arity != arity) throw new IRValidationException()
  if (conicCone.arity != arity) throw new IRValidationException()

  //Verify that all expressions have the same variable size
  if (conicAlmap.domain != varSize) throw new IRValidationException()
  if (objective.domain != varSize) throw new IRValidationException()

  //Verify that all expressions have the same input size
  if (affineOffset.input != inputSize) throw new IRValidationException()
  if (conicAlmap.input != inputSize) throw new IRValidationException()
  if (conicOffset.input != inputSize) throw new IRValidationException()

  //Verify that codomains agree
  if (affineAlmap.codomain != affineOffset.codomain) throw new IRValidationException()
  if (conicAlmap.codomain != conicOffset.codomain) throw new IRValidationException()
  if (conicAlmap.codomain != conicCone.size) throw new IRValidationException()

  //Verify that offsets have the proper domain
  if (affineOffset.domain != IRPoly.const(1, arity)) throw new IRValidationException()
  if (conicOffset.domain != IRPoly.const(1, arity)) throw new IRValidationException()
  if (objective.codomain != IRPoly.const(1, arity)) throw new IRValidationException()

  def arityOp(op: ArityOp): Problem = Problem(
    objective.arityOp(op),
    affineAlmap.arityOp(op),
    affineOffset.arityOp(op),
    conicAlmap.arityOp(op),
    conicOffset.arityOp(op),
    conicCone.arityOp(op))
}
