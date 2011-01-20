package ppl.apps.ml.lbpdenoise
import ppl.dsl.optiml.datastruct.scala.Vector

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class DenoiseVertex extends VertexData {
  var belief : Vector[Double]
  var potential : Vector[Double]
}

class DenoiseEdge extends MessageEdgeData {
  
}

class LBPDenoise