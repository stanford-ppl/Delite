package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class Problem(
  val input: InputDesc,
  val objective: AVector,
  val affineAlmap: Almap,
  val affineOffset: AVector,
  val conicAlmap: Almap,
  val conicOffset: AVector,
  val conicCone: Cone) extends HasInput[Problem]
{
  val arity: Int = input.arity
  val varSize: IRPoly = affineAlmap.domain
  val coneSize: IRPoly = conicAlmap.codomain
  val affineCstrtSize: IRPoly = affineAlmap.codomain
  
  //Verify that all expressions have the same arity
  if (affineAlmap.arity != arity) throw new IRValidationException()
  if (affineOffset.arity != arity) throw new IRValidationException()
  if (conicAlmap.arity != arity) throw new IRValidationException()
  if (conicOffset.arity != arity) throw new IRValidationException()
  if (conicCone.arity != arity) throw new IRValidationException()
  
  //Verify that all expressions have the same input
  if (affineAlmap.input != input) throw new IRValidationException()
  if (affineOffset.input != input) throw new IRValidationException()
  if (conicAlmap.input != input) throw new IRValidationException()
  if (conicOffset.input != input) throw new IRValidationException()

  //Verify that all expressions have the same variable size
  if (conicAlmap.domain != varSize) throw new IRValidationException()
  if (objective.size != varSize) throw new IRValidationException()

  //Verify that codomains agree
  if (affineAlmap.codomain != affineOffset.size) throw new IRValidationException()
  if (conicAlmap.codomain != conicOffset.size) throw new IRValidationException()
  if (conicAlmap.codomain != conicCone.size) throw new IRValidationException()

  def arityOp(op: ArityOp): Problem = Problem(
    input.arityOp(op),
    objective.arityOp(op),
    affineAlmap.arityOp(op),
    affineOffset.arityOp(op),
    conicAlmap.arityOp(op),
    conicOffset.arityOp(op),
    conicCone.arityOp(op))

  def inputOp(op: InputOp): Problem = Problem(
    op.input,
    objective.inputOp(op),
    affineAlmap.inputOp(op),
    affineOffset.inputOp(op),
    conicAlmap.inputOp(op),
    conicOffset.inputOp(op),
    conicCone)

  def display() {
    println("")
    println("objective: " + objective.toString)
    println("affineAlmap: " + affineAlmap.toString)
    println("affineOffset: " + affineOffset.toString)
    println("conicAlmap: " + conicAlmap.toString)
    println("conicOffset: " + conicOffset.toString)
    println("conicCone: " + conicCone.toString)
    println("")
  }
}
