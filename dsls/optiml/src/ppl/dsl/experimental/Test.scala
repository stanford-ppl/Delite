package ppl.dsl.experimental

import ppl.delite.framework.DeliteApplication

object TestRunner extends SandboxApplicationRunner with Test

trait Test extends SandboxApplication {
  def main() = {
    val v = Vector.dense[Int](10, true)
    val x = v.length // dispatched on DenseVector[Int]
    
    val y = foo(v) // dispatched on Interface[Vector[Int]]
    
    // val v2 = Vector.sparse[Int](10, true)
    // val y2 = foo(v2)
  }
  
  // single foo can accept any vector that can be converted to an interface
  def foo(x: Interface[Vector[Int]]) = x.length
}
