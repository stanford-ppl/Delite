import ppl.dsl.optiml._
object HelloWorldRunner extends OptiMLApplicationRunner with HelloWorld 
trait HelloWorld extends OptiMLApplication { 
  def main() = {
    // hello world has been co-opted into a faster compiling, temporary scratch space
    // for tests that haven't been made into scalatests yet
    
    //println("hello world")
    
    val tri1 = utriangle(4)
    tri1.pprint
    
    val tri2 = utriangle(4,false)
    tri2.pprint
    
    // matrix bulk operations with changed views
    /*
    //val m = Matrix.zeros(10,10)
    val m = Matrix.sparse[Double](10,10).finish
    val t1 = m mapRows { row => row + 1 }
    t1.pprint
    
    val v = t1 reduceRows { (a,b) => a*b }
    v.pprint
    
    // foreachRow
    t1 foreachRow { row => println("fr--"); row.pprint }
    
    // filterRows
    val t2 = t1 :+ Vector(5.,5.,5.,5.,5.,5.,5.,5.,5.,5.) 
    println("t2 before filter: ")
    t2.pprint
    val t3 = t2 filterRows { row => row == Vector(5.,5.,5.,5.,5.,5.,5.,5.,5.,5.) } 
    //val t3 = t2 filterRows { row => row(0) == 5 } 
    println("t2 after filter: ")
    t3.pprint
    */
    
    /*
    // sparse matrix testing
    
    // inserting/removing rows/cols
    // val mb = Matrix.sparse[Int](10,10)
    // mb.insertCol(5,DenseVector(1,2,3,4,5,6,7,8,9,10))
    // mb.insertCol(10,DenseVector(0,0,0,0,25,0,0,0,0,0))
    // mb.removeCols(2,2)
    // val m = mb.finish
    // println("m numRows: " + m.numRows)
    // println("m numCols: " + m.numCols)
    // println("m nnz: " + m.nnz)    
    // m.pprint
     
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
    
    val t1 = m map { e => if (e != 0.0) 99. else 0.0 }
    println("t1.numRows: " + t1.numRows)
    println("t1.numCols: " + t1.numCols)
    println("t1 nnz: " + t1.nnz)    

    println("t1(9,722) = (should be 99): " + t1(9,722))
    println("t1(573,71) = (should be 99): " + t1(573,71))
    println("t1(500,500) = (should be 0): " + t1(500,500))    
    
    // inline kernel gen
    for (i <- 0::1) {
      val t2 = m map { e => if (e != 0.0) 101. else 0.0 }
      println("t2.numRows: " + t2.numRows)
      println("t2.numCols: " + t2.numCols)
      println("t2 nnz: " + t2.nnz)    
      
      println("t2(9,722) = (should be 101): " + t2(9,722))
      println("t2(573,71) = (should be 101): " + t2(573,71))
      println("t2(500,500) = (should be 0): " + t2(500,500))          
    }      
    */
    
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
    
         
    for (i <- 0::1) {
      val t1 = v map { e => 32. }
      t1.pprint
    }    
    
    // val d = DenseVector[Double](10,true)
    for (i <- 0::1) {
      val t1 = d map { e => 64. }
      t1.pprint
    }
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
