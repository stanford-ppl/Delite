package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

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

// TODO: we auto-generate everything else, but can't handle getters and setters yet

trait DenoiseVertexData {
  def potential: DenseVector[Double]
  def belief: DenseVector[Double]
  def setBelief(b: DenseVector[Double])
  def id: Int
}

trait DenoiseEdgeData {
  def message: DenseVector[Double]
  def setMessage(m: DenseVector[Double])
  def oldMessage: DenseVector[Double]
  def setOldMessage(oM: DenseVector[Double])
  def Clone : DenoiseEdgeData
}

class DenoiseVertexDataImpl(val _id : Int, var _belief : DenseVector[Double], var _potential : DenseVector[Double]) extends DenoiseVertexData {
  def id = _id

  def belief = _belief
  def setBelief(b: DenseVector[Double]) = {
    _belief = b
  }
  def belief_=(b: DenseVector[Double]) = {
    _belief = b
  }

  def potential = _potential
}

class DenoiseEdgeDataImpl(var _msg : DenseVector[Double], var _oldMsg : DenseVector[Double]) extends DenoiseEdgeData {
  def message = _msg
  def setMessage(msg: DenseVector[Double]) = {
    _msg = msg
  }
  def message_=(msg: DenseVector[Double]) = {
    _msg = msg
  }

  def oldMessage = _oldMsg
  def setOldMessage(msg: DenseVector[Double]) = {
    _oldMsg = msg
  }
  def oldMessage_=(msg: DenseVector[Double]) = {
    _oldMsg = msg
  }

  def Clone = {
    new DenoiseEdgeDataImpl(_msg.Clone, _oldMsg.Clone)
  }
}
