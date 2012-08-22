/**
 * This file contains the examples published on the OptiML github pages
 * http://stanford-ppl.github.com/Delite/optiml/examples.html 
 * 
 * Each example can be executed using the delitec/delite scripts located in <DELITE_HOME>/bin, e.g.
 *   delitec Example1Runner
 *   delite Example1Runner
 */

import ppl.dsl.optiml._

// DenseVector, DenseMatrix
object Example1Runner extends OptiMLApplicationRunner with Example1
trait Example1 extends OptiMLApplication { 
  def main() = {  
    // 10000x1 DenseVector
    val v1 = Vector.rand(10000)
    // 1000x1000 DenseMatrix
    val m = Matrix.rand(1000,1000)
    
    // perform some simple infix operations
    val v2 = (v1+10)*2-5
    
    // take the pointwise natural log of v2 and sum results
    val logv2Sum = log(v2).sum
    
    // slice elems 1000-2000 of v2, transpose and multiply by m
    val v3 = v2(1000::2000).t*m // 1x1000 DenseVector result
     
    // print the first 10 elements to the screen
    v3(0::10).pprint   
  }
}

// Vector, Matrix construction
object Example2Runner extends OptiMLApplicationRunner with Example2
trait Example2 extends OptiMLApplication { 
  def main() = {
    /* various ways of constructing a DenseVector */
    val v0 = Vector[Int](100) // 100x1 all zeros, ints
    val v1 = Vector[Int](100,false) // 1x100 all zeros, ints
    val v3 = Vector.rand(100) // 100x1 random doubles
    val v4 = Vector.zeros(100) // 100x1 all zeros, doubles    
    val v5 = Vector(1,2,3,4,5) // [1,2,3,4,5]
    val v6 = Vector(1.,2.,3.,4.,5.) // [1.0,2.0,3.0,4.0,5.0]
    val v7 = (0::100) { e => random[Int] } // 100x1, random ints
    
    /* various ways of constructing a DenseMatrix */
    val m0 = Matrix[Int](100,50) // 100x50 zeros, ints
    val m1 = Matrix.rand(100,50) // 100x50 random doubles
    val m2 = Matrix.zeros(100,50) // 100x50 zeros, doubles
    val m3 = Matrix(Vector(1,2,3),Vector(4,5,6)) // [1,2,3]
                                                 // [4,5,6]
    val m4 = (0::2, *) { i => Vector(2,3,4) }  // [2,3,4]
                                               // [2,3,4]
    val m5 = (0::2, 0::2) { (i,j) => i*j }  // [0,0]
                                            // [0,1]                               
                   
    // print first row                          
    m5(0).pprint
  }
}

// using functional operators (map, zip, filter)
object Example3Runner extends OptiMLApplicationRunner with Example3
trait Example3 extends OptiMLApplication { 
  def main() = {
    /* chaining functional operators */
    val v = Vector.rand(1000)

    // filter selects all the elements matching a predicate
    // map constructs a new vector by applying a function to each element    
    val v2 = (v*1000).filter(e => e < 500).map(e=>e*e*random[Double])
    println(v2.length)
    
    // reduce produces a scalar by applying successively applying a function to pairs of elems
    val logmin = v2.reduce((a,b) => if (log(a) < log(b)) a else b)
    println(logmin)        
    
    // partition splits the vector into two based on a predicate
    val (v2small, v2large) = v2.partition(e => e < 1000)
    println("v2small size: " + v2small.length)
    println("v2large size: " + v2large.length)    
  }
}

// using types (and method declarations)

// file i/o

// untilconverged

// sum, aggregate, etc.

// user-defined type

// SparseVector, SparseMatrix

// Graph

 



 
 

                                               
                                                
    
    
    


 