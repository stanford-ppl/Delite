package ppl.dsl.optiml.datastruct.scala

/**
 * Created by IntelliJ IDEA.
 * User: mmwu
 * Date: Jan 23, 2011
 * Time: 5:24:55 PM
 * To change this template use File | Settings | File Templates.
 */


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