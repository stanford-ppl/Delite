package ppl.delite.walktime.graph

import ops.DeliteOP
import collection.mutable.ArrayBuffer

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:06:03 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class DeliteTaskGraph {


  val ops = new ArrayBuffer[DeliteOP]
  var _result:DeliteOP = _

  def addOP(kernelId: String, opType: DeliteOP)(deps: DeliteOP*) = {
    
  }

  
  def result : DeliteOP = _result

}