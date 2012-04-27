import ppl.dsl.optiml._
object HelloWorldRunner extends OptiMLApplicationRunner with HelloWorld 
trait HelloWorld extends OptiMLApplication { 
  def main() = {
    // hello world has been co-opted into a faster compiling, temporary scratch space
    // for tests that haven't been made into scalatests yet
    
    //println("hello world")
    
    val v = SparseVector[Double](1000,true)
    v.pprint
    
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
