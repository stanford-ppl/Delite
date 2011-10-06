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
    
    //val t3: Rep[SparseVector[Double]] = sv1 map { e => 5.0 }
    //println(t3(0))
    
    // -- testing Sparse+{Dense,Sparse} expressiveness
    val sv2 = Vector.sparse[Int](10, true)
    val st1: Interface[Vector[Int]] = sv1
    val st2: Interface[Vector[Int]] = sv2
    val dt1: Interface[Vector[Int]] = dv1
    
    // these all have logical return values      
    val s1: Rep[SparseVector[Int]] = sv1 + sv2
    val d1: Rep[DenseVector[Int]] = sv1 + dv1
    val d2: Rep[DenseVector[Int]] = sv1 + dt1
    val dint1: Interface[Vector[Int]] = st1 + dt1  // wrapped DenseVector
    assert(dint1.ops.isInstanceOf[DenseVecOpsCls[_]])

    // this is weird.. Sparse+Sparse if wrapped in interfaces returns a dense, but Sparse+Sparse if unwrapped returns sparse.
    // wrapped and unwrapped behavior should be the same, except in terms of returning a wrapped (or unwrapped) result
    // unfortunately this conflicts with Arith, which expects A+A to return A.    

    // these all should probably return a wrapped SparseVector, but they all return a wrapped DenseVector, due to being dispatched on an Interface[Vector]
    val sintbroken1: Interface[Vector[Int]] = st1 + st2 // (Interface[Sparse] + Interface[Sparse] => Interface[Dense]). 
    val sintbroken2: Interface[Vector[Int]] = st1 + sv2 // (Interface[Sparse] + Sparse => Interface[Sparse] + Interface[Sparse] => Interface[Dense])
    val sintbroken3: Interface[Vector[Int]] = st1 + st2 // (Interface[Sparse] + Interface[Sparse] => Interface[Dense])                                                  
    val sbroken2: Rep[DenseVector[Int]] = sv1 + st1 // Sparse + Interface[Sparse] => Dense. ideally, would => Sparse!    
    // -- end testing Sparse+{Dense,Sparse} expressiveness
                    
    //val y2 = foo(sv1, sv2) // Interface+Interface, but actually Sparse+Sparse, returns Interface that is actually dense
    
    //y2(0) = 1 // mutation error
    //val y3 = y2.mutable
    //y3(0) = 1 // ok
    
    //println(y2(0)) 
     
    //     val y3 = y2 + y // Interface+Interface, but actually Dense+Dense (returns Interface that is actually dense)
    //     val y4 = (sv1 + sv2) + y // Sparse+Interface, but actually Sparse+Dense (returns Interface that is actually dense)
    //     
    //     println(y4(0))
    
    // nested op (Vector[Vector[Int]])
    
    // this is a bit weird.. Vector[Int] is not a very meaningful type anymore (only used as a parameter to Interface)
    val nested1: Rep[DenseVector[Vector[Int]]] = Vector.dense[Vector[Int]](10, true)
    //val useless = nested1(0)
    //val useless1 = useless map { e => 5.0 } // error, nothing defined on Vector[Int] 
    
    val nested2: Rep[DenseVector[SparseVector[Int]]] = nested1 map { v => Vector.sparse[Int](5, true) }
    val nested3: Rep[DenseVector[DenseVector[Int]]] = nested1 map { v => Vector.dense[Int](5, true) }
    
    // these operations are not dimension-safe anyways; should we really support them with Arith?
    // maybe now is a good time to remove Vector and Matrix from the Arith type class and see which apps break
    
    //val nested4: Rep[DenseVector[DenseVector[Int]]] = nested2 + nested3 // not part of Arith
    val nested5: Rep[DenseVector[DenseVector[Int]]] = nested3 + nested3
    val nested6: Rep[DenseVector[SparseVector[Int]]] = nested2 + nested2
    
    // TODO: primitive conversions (e.g. V[Int] + V[Double] => V[Double], V[Double] + V[Int] => V[Double], etc.)?
    // we're just as bad as we were before... can we combine this approach with type classes to make this better?
  }

  
  /**
   * Interface
   */
  
  // works
  // single foo can accept any vector that can be converted to an interface  
  //def foo(x: Interface[Vector[Int]]) = x.length  
  def foo(x1: Interface[Vector[Int]], x2: Interface[Vector[Int]]) = {
  //def foo(x1: Rep[SparseVector[Int]], x2: Interface[Vector[Int]]) = {  
    x1(0) = 5 // not an error
    //val t1: Interface[Vector[Int]] = x1 map { e => 10 }
    x1 + x2
  }
  
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
