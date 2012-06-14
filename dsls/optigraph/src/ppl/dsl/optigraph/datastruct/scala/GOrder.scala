package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{HashMap, Map, MutableList}

class GOrder[@specialized T: ClassManifest] {
  
  private var data = new MutableList[T]()
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
  
  def items: GIterable[T] = { new GIterable[T](dataAsArray) }
  def contains(n: T): Boolean = { data.contains(n) }
  def size = { data.size }
  def front() = { data(0) }
  def back() = { data(data.size-1) }
  
  def pushBack(n: T): Unit = { 
    if(!data.contains(n)) {
      data.+=(n)
      resetDataArray
    }
  }

  def pushFront(n: T): Unit = {
    if(!data.contains(n)) {
      data.+=:(n)
      resetDataArray
    }
    
  }

  def pushFrontOrd(s: GOrder[T]): Unit = { 
    var i = s.size - 1
    while (i >= 0) {
      pushFront(s.data(i))
      i += 1
    }
  }
  
  def pushBackOrd(s: GOrder[T]): Unit = { 
    var i = 0
    while (i < s.size) {
      pushBack(s.data(i)) 
      i += 1
    }
  }

  def popFront(): T = { 
    val f = front()
    data = data.tail
    resetDataArray
    f
  }

  def popBack(): T = { 
    val l = back()
    data = data.take(data.size - 2) 
    resetDataArray
    l
  }
  
  def apply(idx: Int): T = {
    if(idx > data.size) throw new RuntimeException("Index is out of bounds")
    data(idx)
  }
}