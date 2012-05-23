import ppl.dsl.optiml._
object HelloWorldRunner extends OptiMLApplicationRunner with HelloWorld 
trait HelloWorld extends OptiMLApplication { 
  def main() = {
    // hello world has been co-opted into a faster compiling, temporary scratch space
    // for tests that haven't been made into scalatests yet
    
    //println("hello world")
    
    // sparse matrix testing
    val mb = Matrix.sparse[Double](1000,1000)
    mb(10,100) = 5
    mb(9,100) = 1
    mb(9,722) = 722
    mb(9,331) = 331
    mb(11,101) = 2
    mb(573,71) = 15
    mb(200,17) = 3
    
    val m = mb.finish
    println("m numRows: " + m.numRows)
    println("m numCols: " + m.numCols)
    println("m nnz: " + m.nnz)
    println("m(10,10) = (should be 0): " + m(10,10))
    println("m(0,100) = (should be 0): " + m(0,100))
    println("m(9,100) = (should be 1): " + m(9,100))
    println("m(9,722) = (should be 722): " + m(9,722))
    println("m(9,331) = (should be 331): " + m(9,331))
    println("m(9,500) = (should be 0): " + m(9,500))
    println("m(10,772) = (should be 0): " + m(10,772))
    println("m(10,100) = (should be 5): " + m(10,100))
    println("m(11,101) = (should be 2): " + m(11,101))
    println("m(200,17) = (should be 3): " + m(200,17))
    println("m(573,71) = (should be 15): " + m(573,71))
    println("m(500,500) = (should be 0): " + m(500,500))
    println("m(10,101) = (should be 0): " + m(10,101))
    println("m(200,71) = (should be 0): " + m(200,71))    
    
    val t1 = m map { e => if (e != 0.0) 99. else 0.0 }
    println("t1.numRows: " + t1.numRows)
    println("t1.numCols: " + t1.numCols)
    println("t1 nnz: " + t1.nnz)    

    // TODO: test sparse matrix nested operations (inline kernel gen)
  
  
  
  
  
  
    /*
    // sparse vector testing
    val d = Vector.ones(100) //DenseVector[Double](100,true)
    val v = SparseVector[Double](100,true)
    v(5) = 10
    v(75) = 20
    //v.pprint
    
    // val t1 = v map { e => if (e != 0.0) 32. else 0.0 }
    // println("t1 nnz: " + t1.nnz)
    // println("t1.length: " + t1.length)
    // t1.pprint    
    
    // testing sparse ops (currently only nested implemented)
    // for (i <- 0::1) {
      // val t1 = v map { e => if (e != 0.0) 32. else 0.0 }
      // t1.pprint
      val t1: Rep[SparseVector[Double]] = d*v
      val t2: Rep[SparseVector[Double]] = v*d
      
      println("t1 length: " + t1.length)
      println("t1 nnz: " + t1.nnz)
      println("t1: ")
      t1.pprint
      println("t2 nnz: " + t2.nnz)
      println("t2: ")
      t2.pprint
    // }
    
    val t3 = v mapNZ { e => 72. }
    println("t3 length: " + t3.length)
    println("t3 nnz: " + t3.nnz) 
    println("t3: ")
    t3.pprint
    
         
    // for (i <- 0::1) {
    //   val t1 = v map { e => 32. }
    //   t1.pprint
    // }    
    
    // val d = DenseVector[Double](10,true)
    // for (i <- 0::1) {
    //   val t1 = d map { e => 64. }
    //   t1.pprint
    // }
    */
    
    // val vl = log(v)
    // vl.pprint
    // println(vl)
    // println("vl is: " + vl)
    //     
    // val m = Matrix.rand(10,10)
    // m.pprint    
    // val me = exp(m)
    // me.pprint
    // 
    // println("--- testing pattern matching")
    // val a = Vector(1,2,3)
    // val b = Vector(3,4,5)
    // val c = Vector(6,7,8)
    // val pattern = a*b + a*c
    // pattern.pprint
  }
}
