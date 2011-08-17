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

// TODO: we auto-generate everything else, but can't handle getters and setters yet

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

class DenoiseVertexDataImpl(val _id : Int, var _belief : Vector[Double], var _potential : Vector[Double]) extends DenoiseVertexData {
  def id = _id

  def belief = _belief
  def setBelief(b: Vector[Double]) = {
    _belief = b
  }
  def belief_=(b: Vector[Double]) = {
    _belief = b
  }

  def potential = _potential
}

class DenoiseEdgeDataImpl(var _msg : Vector[Double], var _oldMsg : Vector[Double]) extends DenoiseEdgeData {
  def message = _msg
  def setMessage(msg: Vector[Double]) = {
    _msg = msg
  }
  def message_=(msg: Vector[Double]) = {
    _msg = msg
  }

  def oldMessage = _oldMsg
  def setOldMessage(msg: Vector[Double]) = {
    _oldMsg = msg
  }
  def oldMessage_=(msg: Vector[Double]) = {
    _oldMsg = msg
  }

  def cloneL = {
    new DenoiseEdgeDataImpl(_msg.cloneL, _oldMsg.cloneL)
  }
}
