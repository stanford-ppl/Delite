package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MeshLoader {
  @native
  def loadMesh(file : String) : Mesh = {new Mesh}
}