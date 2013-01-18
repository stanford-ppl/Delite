package ppl.apps.interop

import Types._
/** 
 * A Property object is used to associate data with graph nodes or edges
 * When the Property object is first created, the data value of each node/edge is set 
 * to the default of the data type (e.g. Boolean -> False, Int -> 0, String -> null)  
 * Note: properties can be associated only with immutable graph instances
 */

class Property[@specialized T: ClassManifest](val g: Graph, val size: Int) {
  
  /* Stores the property value for each graph node/edge */
  var data: Array[T] = new Array[T](size)
  
  def update(n: Node, x: T): Unit = dcUpdate(n, x)
  def apply(n: Node): T = dcApply(n)

  // DeliteCollection ops
  /* Returns the property value of the node/edge with the given id */
  def dcApply(idx: Int) : T = data(idx)
  /* Updates the property value of the node/edge with the given id */
  def dcUpdate(idx: Int, x: T) : Unit = { data(idx) = x }
  /* Returns the length of the property data array (which is the number of nodes or edges in the graph)*/
  def dcSize : Int = data.length
}
