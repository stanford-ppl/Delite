package ppl.dsl.optiml.datastruct.scala

/* This file should be auto-generated from a preprocessing compilation stage of application code.
 * It represents the interface of any user-defined data structure, for which the user must provide
 * concrete implementations.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 1/17/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait DenoiseVertexData extends MessageData {
  def potential: Vector[Double]
  def belief: Vector[Double]
  def setBelief(b: Vector[Double])
  def id: Int
}

trait DenoiseEdgeData extends MessageData {
  def message: Vector[Double]
  def setMessage(m: Vector[Double])
  def oldMessage: Vector[Double]
  def setOldMessage(oM: Vector[Double])
  def cloneL : DenoiseEdgeData
}