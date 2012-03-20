package ppl.dsl.optigraph.datastruct.scala

/** 
 * A Property object is used to associate data with graph nodes or edges
 * When the Property object is first created, the data value of each node/edge is set 
 * to the default of the data type (e.g. Boolean -> False, Int -> 0, String -> null)  
 * Note: properties can be associated only with immutable graph instances
 */

class Property[@specialized T: ClassManifest](val g: Graph, val size: Int) {
  
  if(!g.isImmutable) { throw new RuntimeException("Cannot create properties for mutable graphs")}
  
  /* Stores the property value for each graph node/edge */
  private var data: Array[T] = new Array[T](size)
  /* Stores the deferred property value for each graph node/edge */
  private var deferred_data: Array[T] = new Array[T](size)
  /* Indicates if there is a deferred value for a given node/edge */
  private var deferred_data_bitmap: Array[Boolean] = new Array[Boolean](size)
  
  def defer(idx: Int, x: T): Unit = { 
    deferred_data(idx) = x 
    deferred_data_bitmap(idx) = true
  }
  
  def getDeferredValue(idx: Int) = deferred_data(idx)
  def hasDeferredValue(idx: Int) = deferred_data_bitmap(idx)
  def clearDeferredValue(idx: Int) = { deferred_data_bitmap(idx) = false }
  
  // DeliteCollection ops
  /* Returns the property value of the node/edge with the given id */
  def dcApply(idx: Int) : T = data(idx)
  /* Updates the property value of the node/edge with the given id */
  def dcUpdate(idx: Int, x: T) : Unit = { data(idx) = x }
  /* Returns the length of the property data array (which is the number of nodes or edges in the graph)*/
  def dcSize : Int = data.length
}