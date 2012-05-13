import ppl.dsl.optiml._
object HelloWorldRunner extends OptiMLApplicationRunner with HelloWorld 
trait HelloWorld extends OptiMLApplication { 
  def main() = {
    // hello world has been co-opted into a faster compiling, temporary scratch space
    // for tests that haven't been made into scalatests yet
    
    //println("hello world")
    
    val d = Vector.ones(100) //DenseVector[Double](100,true)
    val v = SparseVector[Double](100,true)
    v(5) = 10
    v(75) = 20
    //v.pprint
    
    // val t1 = v map { e => if (e != 0.0) 32. else 0.0 }
    // println("t1 number of nnz: " + t1.nnz)
    // println("t1.length: " + t1.length)
    // t1.pprint    
    
    // testing sparse ops (currently only nested implemented)
    // for (i <- 0::1) {
      // val t1 = v map { e => if (e != 0.0) 32. else 0.0 }
      // t1.pprint
      val t1: Rep[DenseVector[Double]] = d*v
      val t2: Rep[SparseVector[Double]] = v*d
      
      println("t1 length: " + t1.length)
      // println("t1 number of nnz: " + t1.nnz)
      println("t1: ")
      t1.pprint
      println("t2 number of nnz: " + t2.nnz)
      println("t2: ")
      t2.pprint
    // }
         
    // for (i <- 0::1) {
    //   val t1 = v map { e => 32. }
    //   t1.pprint
    // }    
    
    // val d = DenseVector[Double](10,true)
    // for (i <- 0::1) {
    //   val t1 = d map { e => 64. }
    //   t1.pprint
    // }
    
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
