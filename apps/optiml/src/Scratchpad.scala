import ppl.dsl.optiml._

object ScratchpadRunner extends OptiMLApplicationRunner with Scratchpad
trait Scratchpad extends OptiMLApplication { 
  def main() = {
    
    // can be representation transparent using new Struct inheritance support?
    // ratings: Rep[Matrix[Int]], sims: Rep[Matrix[Double]], check tags on dispatch?
    // abstract interface (Ops) forwards to a dispatch method in OpsExp?
    def preferences(user: Rep[Int], ratings: Rep[DenseMatrix[Int]], sims: Rep[SparseMatrix[Double]]) = {
      sims.mapRowsToVector { testProfile => // each row is a unique profile
        val num = sum(0, ratings.numRows) { i => testProfile(ratings(i,1))*ratings(i,2) }
        val den = sum(0, ratings.numRows) { i => abs(testProfile(ratings(i,1))) }
        num/(den+1)
      }
    }
    
    
    val simsB = SparseMatrix[Double](10000,10000)
    /*
    simsB(100,100) = 75
    simsB(500,500) = 150
    val sims = simsB.finish
    */
    
    var i = 0
    while (i < simsB.numRows) {
      if (random[Double] < .05) {
        var j = 0
        while (j < simsB.numCols) {
          if (random[Double] < .05) {
            simsB(i,j) = random[Double]*1000
          }
          j += 1
        }
     }
     i += 1
    }
    val sims = simsB.finish   
    println("sims nnz: " + sims.nnz) 
    
    val ratingsB = DenseMatrix[Int](0,0)
    ratingsB.insertCol(0, 0::10000) // user ids
    ratingsB.insertCol(1, Vector.rand(10000).map(e=>(e*1000).AsInstanceOf[Int])) // rating user id
    ratingsB.insertCol(2, Vector.rand(10000).map(e=>(e*10).AsInstanceOf[Int])) // rating, 0 to 10
    val ratings = ratingsB.unsafeImmutable
    
    tic()
    val p = preferences(0, ratings, sims)
    toc(p)
    println("p nnz: " + p.nnz)
    
    // multiloop unwrapping 
    
    // -- test collect
    /*
    val x = (0::1000) { i =>
      val y = (0::100) { _*2 }
      y.sum
    }
    println(x(0))
    */
    
    /*
    val x = (0::10) { i =>
      val y = (0::2) { x => 
        println("blah") // should preclude GPU gen
        x*i         
      }
      y(1)
    }
    println(x(2))
    */
    
    // -- test foreach
    /*
    for (i <- 0::10) {
      val y = (0::5){ x => /*println("blah");*/ x*i }
      println(y.sum)
    }
    */
        
    // -- test reduce
    /*
    val v = DenseVector[Double](1,2,3,4,5)
    v.reduce( (a,b) = {
      val y = (0::10){ _*a+b }
      println(y.sum)
      y.sum
    })
    */
    
    /*
    // sparse matrix specialization testing
    val m1b = SparseMatrix[Double](10000,10000)
    m1b(1000,1000) = 500
    m1b(5000,5000) = 1000
    val m1 = m1b.finish
  
    // -- specialized maps
  
    tic("mapSpec")
    val a1 = m1 map { e => e } // should be specialized
    toc("mapSpec", a1)
    println("a1(1000,1000): " + a1(1000,1000))
  
    tic("mapSpec2")
    val a2 = m1 map { e => 0. } // should be specialized
    toc("mapSpec2", a2)
    println("a2(1000,1000): " + a2(1000,1000))
  
    // fast because most of the time we are adding zeros (e is usually 0)
    tic("mapNoSpec")
    val a3 = m1 map { e => if (random[Double] < 0.99999) e else 0.0 } // should NOT be specialized
    toc("mapNoSpec", a3)
    println("a3(1000,1000): " + a3(1000,1000))
    println("a3.nnz: " + a3.nnz)
  
    // very slow (~30s) because we are densifying a sparse representation - this should never be done
    // however, this shows that we scale incredibly poorly with nnz - need to investigate performance on structures that are ~50% sparse?
    // is most of the time in the parallel op, or in the COOtoCSR conversion?  --> guess is COOtoCSR conversion, should be optimized
    // tic("mapNoSpec2")
    // val a4 = m1 map { e => 1. } // should NOT be specialized
    // toc("mapNoSpec2", a4)
    // println("a4(1000,1000): " + a4(1000,1000))
    // println("a4.nnz: " + a4.nnz)
  
  
    // -- specialized zips
  
    val m2b = SparseMatrix[Double](10000,10000)        
    m2b(1000,1000) = 50
    m2b(7000,7000) = 2
    val m2 = m2b.finish
      
    tic("matZipLeftSpec")
    val mz1 = m1.zip(m2) { (a,b) => a } // should be specialized 
    toc("matZipLeftSpec",mz1)
    println("mz1(1000,1000): " + mz1(1000,1000))
    println("mz1(5000,5000): " + mz1(5000,5000))
    println("mz1(7000,7000): " + mz1(7000,7000))    
  
    tic("matZipAddNoSpec")
    val mz2 = m1.zip(m2) { (a,b) => if (random[Double] > .5) a else b } // should NOT be specialized 
    toc("matZipAddNoSpec",mz2)
    println("mz2(1000,1000): " + mz2(1000,1000))
    println("mz2(5000,1000): " + mz2(5000,5000))    
  
    tic("matZipAddSpec")
    val mz3 = m1+m2 // should be specialized 
    toc("matZipAddSpec",mz3)
    println("mz3(1000): " + mz3(1000,1000))
    println("mz3(5000): " + mz3(5000,5000)) 
    println("mz3(7000): " + mz3(7000,7000))       
    println("mz3 nnz: " + mz3.nnz) 
  
    tic("matZipMult")
    val mz4 = m1*:*m2 // should be specialized 
    toc("matZipMult",mz4)
    println("mz4(1000): " + mz4(1000,1000))
    println("mz4(5000): " + mz4(5000,5000)) 
    println("mz4(7000): " + mz4(7000,7000))            
    println("mz4 nnz: " + mz4.nnz)
  
    // -- specialized reduces
    tic("matRedSpec")
    val r1 = m1.sum // should be specialized
    toc("matRedSpec", r1)
    println("r1: " + r1)
  
    tic("matRedNoSpec")
    val r2 = m1.min  // should NOT be specialized (how should we actually implement this?)
    toc("matRedNoSpec",r2)
    println("r2: " + r2)    
    */
    
    // sparse vector specialization testing
    /*
    val v = SparseVector[Double](1000000,true)
    v(1000) = 1
    v(5000) = 2
      
    // --- specialized maps
  
    tic("v2")
    val v2 = v.map(e => e) // should be specialized
    toc("v2",v2)
    println("v2(1000): " + v2(1000))
    println("v2(1001): " + v2(1001))
    // v2.pprint
  
    tic("v3")
    val v3 = v.map(e => 0.) // should be specialized
    toc("v3",v3)
    // v3.pprint
  
    tic("v4")
    val v4 = v.map(e => 1.) // should NOT be specialized 
    toc("v4",v4)
  
    tic("v5")
    val v5 = v.mapNZ(e => 1.) 
    toc("v5",v5)
  
    tic("scalarMult")
    val v6 = v*5 // should be specialized
    toc("scalarMult", v6)
    println("v6(1000): " + v6(1000))
    println("v6(1001): " + v6(1001))
    // v5.pprint
  
    tic("scalarAdd")
    val v7 = v+5 // should NOT be specialized
    toc("scalarAdd", v7)
    println("v7(1000): " + v7(1000))
    println("v7(1001): " + v7(1001))    
    // v6.pprint
  
    val free = Vector.ones(10)
  
    tic("scalarMultFreeVar")
    val v8 = v*free(2)// should be specialized
    toc("scalarMultFreeVar", v8)
    println("v8(1000): " + v8(1000))
    println("v8(1001): " + v8(1001))
  
    // --- specialized zips
  
    val y = SparseVector[Double](1000000,true)
    y(1000) = 99
    y(9000) = -99
  
    tic("zipMult")
    val z1 = v*y // should be specialized 
    toc("zipMult",z1)
    println("z1(1000): " + z1(1000))
    println("z1(5000): " + z1(5000))    
  
    tic("zipAddSpec")
    val z2 = v+y // should be specialized 
    toc("zipAddSpec",z2)
    println("z2(1000): " + z2(1000))
    println("z2(5000): " + z2(5000))    
  
    tic("zipAddNoSpec")
    val z3 = v.zip(y) { (a,b) => a + b + 1 } // should NOT be specialized 
    toc("zipAddNoSpec",z3)
    println("z3(1000): " + z3(1000))
    println("z3(5000): " + z3(5000))    
  
    tic("zipLeftSpec")
    val z4 = v.zip(y) { (a,b) => a } // should be specialized 
    toc("zipLeftSpec",z4)
    println("z4(1000): " + z4(1000))
    println("z4(5000): " + z4(5000))    
   
   
    // --- specialized reduces
    tic("sum")
    val a = v.sum // should be specialized
    toc("sum",a)
    println("a: " + a)
  
    tic("redNoSpec")
    val a2 = v.reduce((a,b) => a+b+1)  // should NOT be specialized
    toc("redNoSpec",a2)
    println("a2: " + a2)
    */
  
    /*
    val tri1 = utriangle(4)
    tri1.pprint
  
    val tri2 = utriangle(4,false)
    tri2.pprint
    */
  
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
