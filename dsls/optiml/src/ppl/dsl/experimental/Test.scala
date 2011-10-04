package ppl.dsl.experimental

import ppl.delite.framework.DeliteApplication

object TestRunner extends SandboxApplicationRunner with Test

trait Test extends SandboxApplication {
  def main() = {
    val dv1 = Vector.dense[Int](10, true)
    val dv2 = Vector.dense[Int](10, true)
    //val x = dv.length // dispatched on DenseVector[Int]
    
    dv1(0) = 5
    dv2(0) = -2
    val t: Rep[DenseVector[Int]] = dv1 + dv2
    
    println(t(0))
    
    val t2: Rep[DenseVector[Int]] = t map { e => e + 1 }
    
    println(t2(0))
        
    //val y = foo(dv1, dv2) // Interface[Vector[Int]]
    
    //val y = foo(dv1, dv2)(manifest[Int], implicitly[Arith[Int]], denseIsVector(dv1), canAddDense) 
    //val y = foo(dv1, dv2)(manifest[Int], implicitly[Arith[Int]], denseIsVector) 
    
    val sv1 = Vector.sparse[Int](10, true)
    
    val t3: Rep[SparseVector[Double]] = sv1 map { e => 5.0 }
    println(t3(0))
    
    //val sv2 = Vector.sparse[Int](10, true)
    //     
    //     // this is weird.. Sparse+Sparse if wrapped in interfaces returns a dense, but Sparse+Sparse if unwrapped returns sparse?
    //     // wrapped and unwrapped behavior should be the same, except in terms of returning a wrapped (or unwrapped) result
    //     // unfortunately this conflicts with Arith, which expects A+A to return A.
    //     val y2 = foo(sv1, sv2) // Interface+Interface, but actually Sparse+Sparse, returns Interface that is actually dense
    //     
    //     y2(0) = 1
    //     
    //     val y3 = y2 + y // Interface+Interface, but actually Dense+Dense (returns Interface that is actually dense)
    //     val y4 = (sv1 + sv2) + y // Sparse+Interface, but actually Sparse+Dense (returns Interface that is actually dense)
    //     
    //     println(y4(0))
  }

  /**
   * Interface
   */
  
  // works
  // single foo can accept any vector that can be converted to an interface
  //def foo(x: Interface[Vector[Int]]) = x.length  
  def foo(x1: Interface[Vector[Int]], x2: Interface[Vector[Int]]) = x1 + x2
  //def foo(x1: Interface[Vector[Int]], x2: Rep[DenseVector[Int]]) = x1 + x2      
  
  //def foo[A:Manifest:Arith](x1: Interface[Vector[A]], x2: Interface[Vector[A]]) = x1 + x2      

  //----------------------------------------------------------------------------//

    
  /**
   * Type classes 
   */
  
  /*
   // works   
   //def foo[X: Arith, A[X], B[X]](x1: Rep[A[X]], x2: Rep[B[X]])(implicit ev1: IsVector[A[X]], ev2: IsVector[B[X]]) = x1 + x2

   // this works, but requires CanAdd to be passed explicitly in addition to IsVector
   // def foo[V[X],A:Manifest:Arith](x1: Rep[V[A]], x2: Rep[V[A]])(implicit ev1: IsVector[V,A], ca: CanAdd[V[A],V[A],V[A]]) 
   //   = x1 + x2
     //= infix_+[V[A],V[A],V[A]](x1, x2)(canAddGeneric(manifest[A], implicitly[Arith[A]], ev1)) // x1 + x2

   // how can we add with only Arith & IsVector being passed? (i.e., infer the CanAdd from those two)
   // given IsVector[DenseVector,A], Arith[A], IsVector needs to encode plus(rhs: Arith)
   
   // this works 
   def foo[V[X],A:Manifest:Arith](x1: Rep[V[A]], x2: Rep[V[A]])(implicit ev1: IsVector[V,A]) 
     = x1 + x2
       
   // doesn't work  
   //def foo[A[X]:IsVector,B[X]:IsVector](x1: Rep[A[Int]], x2: Rep[B[Int]]) = x1 + x2 // new compiler support could fix
  */
  
  //----------------------------------------------------------------------------//

  
  /**
   * Other options
   */

  // haven't tried
  //def foo[A %> Vector](x: Rep[A]) = x.length
}
