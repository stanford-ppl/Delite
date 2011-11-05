package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/25/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
object DoubleVecImpl {
  def ofSize(size: Int) = {
    new DoubleVecImpl(size)
  }
  
  def withData(data : Array[Double]) = {
    new DoubleVecImpl(data)
  }
  
  def apply(xs: Double*) = {
    new DoubleVecImpl(xs.toArray)
  }
}

class DoubleVecImpl(val data : Array[Double]) extends Vec[Double] with Copyable {
  def this(size : Int) = this(new Array[Double](size))
  
  override def size = data.length

  def apply(n : Int) = data(n)
  def update(n : Int, v : Double) = {
    data(n) = v
  }
  
  def cloneL = {
    new DoubleVecImpl(data.clone)
  }
  
  def copy() = {
    val v = new DoubleVecImpl(size)
  
    if(classManifest[Double] <:< classManifest[Copyable]) {
      for(i <- 0 until size) {
        v(i) = data(i).asInstanceOf[Copyable].copy.asInstanceOf[Double]
      }
    }
    else {
      for(i <- 0 until size) {
        v(i) = data(i)
      }
    }
    
    v
  }
  
  override def toString() = {
    "Vec[" + size + "](" + data.map(_.toString).reduceLeft(_ + "," + _) + ")"
  }
}
