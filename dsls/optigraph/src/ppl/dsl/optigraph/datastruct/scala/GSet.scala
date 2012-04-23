package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{HashSet, HashMap}

class GSet[@specialized T: ClassManifest] {
  
  private val data = HashSet[T]()
  private var dataArray: Array[T] = null
  private def resetDataArray = {
    dataArray = null
  }
  private def refreshDataArray = {
    if(dataArray == null) {
    	dataArray = data.toArray
    }
  }
  def dataAsArray:Array[T] = {
    refreshDataArray
    dataArray
  }
  
  def cloneL = { val s = new GSet[T](); s.addSet(this); s }
  
  /** Items in the set */
  def items: GIterable[T] = { new GIterable[T](dataAsArray) }
  
  /** Returns true if the set contains the node */
  def contains(n: T): Boolean = { data.contains(n) }
 
  /** Returns the number of items in the set */
  def size = { data.size }
  
  /** Adds the node to the set */
  def add(n: T) = { 
    data.add(n) 
    resetDataArray
  }
  
  /** Adds all the nodes in the input set to the current set */
  def addSet(s: GSet[T]) = { 
    data ++= s.data 
    resetDataArray
  }
  
  /** Removes the node from the set */
  def remove(n: T) = { 
    data.remove(n) 
    resetDataArray
  }
  
  /** Removes all the nodes in the input set from the current set */
  def removeSet(s: GSet[T]) = { 
    data --= s.data 
    resetDataArray
  }
  
  /** Remove all the nodes from the set */
  def clear = { 
    data.clear 
    resetDataArray
  }
  
  // DeliteCollection ops
  def dcSize : Int = { dataAsArray.size }
  def dcApply(idx: Int) : T = { dataAsArray(idx) }
  def dcUpdate(idx: Int, x: T) : Unit = { 
    //TODO: update does not make sense for a set
    throw new RuntimeException("Set dcUpdate should not be called")
  }
}