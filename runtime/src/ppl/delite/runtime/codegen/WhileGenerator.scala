package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops.OP_While

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 4:09 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object WhileGenerator {

  def makeExecutable(whileLoop: OP_While) {
    //print predicate
    //print while
    //output while body
    //print update predicate
    //print end
  }

  def writeMethodHeader(out: StringBuilder) {
    out.append("def apply (")
    //TODO: write input list
    out.append(") {\n")
  }
}
