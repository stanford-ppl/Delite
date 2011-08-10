package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/05/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Liszt {
  def init(args: Array[String]) {
    val cfgFile = if(args.length > 0) args(0) else "liszt.cfg"
    
    MeshLoader.init(cfgFile)
  }
}