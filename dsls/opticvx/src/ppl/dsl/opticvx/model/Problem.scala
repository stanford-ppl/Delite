package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class Problem(
  val inputSize: IRPoly,
  val objective: AVector,
  val affineAlmap: Almap,
  val affineOffset: AVector,
  val conicAlmap: Almap,
  val conicOffset: AVector,
  val conicCone: Cone) extends HasArity[Problem]
{
  val arity: Int = affineAlmap.arity
  val varSize: IRPoly = affineAlmap.domain
  val coneSize: IRPoly = conicAlmap.codomain
  val affineCstrtSize: IRPoly = affineAlmap.codomain
  
  //Verify that all expressions have the same arity
  if (affineOffset.arity != arity) throw new IRValidationException()
  if (conicAlmap.arity != arity) throw new IRValidationException()
  if (conicOffset.arity != arity) throw new IRValidationException()
  if (conicCone.arity != arity) throw new IRValidationException()

  //Verify that all expressions have the same variable size
  if (conicAlmap.domain != varSize) throw new IRValidationException()
  if (objective.size != varSize) throw new IRValidationException()

  //Verify that codomains agree
  if (affineAlmap.codomain != affineOffset.size) throw new IRValidationException()
  if (conicAlmap.codomain != conicOffset.size) throw new IRValidationException()
  if (conicAlmap.codomain != conicCone.size) throw new IRValidationException()

  def arityOp(op: ArityOp): Problem = Problem(
    inputSize.arityOp(op),
    objective.arityOp(op),
    affineAlmap.arityOp(op),
    affineOffset.arityOp(op),
    conicAlmap.arityOp(op),
    conicOffset.arityOp(op),
    conicCone.arityOp(op))
}
