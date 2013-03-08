import ppl.dsl.optiml._
import ppl.delite.framework.datastructures.DeliteArray

object StencilExperimentsRunner extends OptiMLApplicationRunner with StencilExperiments 
trait StencilExperiments extends OptiMLApplication { 
  

  /**
   * The simplest partitionable program: a single loop over a vector
   */
  def exp1() = {
    // type inference gets confused here..
    val a: Rep[DeliteArray[Double]] = DeliteFileReader.readLines("foo.dat"){ l => l.toDouble }
    val v = Vector[Double](a, true)
    val z = (0::v.length) { i => v(i)*v(i)-v(i) }
    println(z(0))
  }
    
  /**
   * A simple partitionable program: two loops (one to allocate the vector and one to map it)
   * 
   * How do we tag the allocation so that we can make it distributed?
   */
  def exp2() = {
    val v = Vector.rand(100000)
    val z = exp(v)
    println(z(0))
  }
  
  /**
   * One loop over two vectors
   */ 
  def exp3() = {
    val v1 = Vector.rand(100000)
    val v2 = Vector.rand(100000)
    
    val z = v1.zip(v2) { (a,b) => a+b }
    println(z(0))
  }
  
  /**
   * One loop over two vectors of different sizes (only first should be partitioned)
   */ 
  def exp4() = {
    val vl = Vector.rand(100000)
    val vs = Vector.rand(10)
    
    val z = vl.map(e => e+(vs+e).sum)
    println(z(0))
  }
  
  /**
   * Two loops over a matrix, one via rows, one via elements
   */
  def exp5() = {
    val m = Matrix.rand(100,100)
    val z1 = exp(m)
    // should be: (m.numCols,1,m.numCols)
    // got: (v*1,m.numCols,m.numCols)
    val z2 = m.mapRowsToVector(row => row.sum)
    println(z1(0))
    println(z2(0))
  }
  
  /**
   * Reductions
   */
  def exp6() = {
    val v = Vector.rand(100000)
    val a = v.sum
    println(a)
  }
  
  /**
   * Vertical Fusion
   */
  def exp7() = {
    val v1 = Vector.rand(10000)
    val a = v1.map(e => e+1)
    val b = a.map(e => e+2)
    println(b(0))
  }

  /**
   * Horizontal Fusion (currently test is broken)
   */
  def exp8() = {
    val v1 = Vector[Double](100,true)
    val v2 = Vector[Double](100,true)
    
    val a = v1.map(e => e+1)
    val b = v2.map(e => e+2)    
    println(a(0))
    println(b(0))
  }
  
  def main() = {
    exp1()
    exp2()
    exp3()
    exp4()
    exp5()
    exp6()
    exp7()
    // exp8()
  }
}
