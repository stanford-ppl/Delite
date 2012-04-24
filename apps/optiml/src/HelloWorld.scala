import ppl.dsl.optiml._
object HelloWorldRunner extends OptiMLApplicationRunner with HelloWorld 
trait HelloWorld extends OptiMLApplication { 
  def main() = {
    //println("hello world")
    val v = Vector(1,2,3,4,5)
    val vl = log(v)
    vl.pprint
    println(vl)
    
    val m = Matrix(v,v)
    val me = exp(m)
    me.pprint
    
    println("median v: " + median(v))
    println("mean v: " + mean(v))
    println("mean m: " + mean(m))
    println("max v: " + max(v))
    println("min m: " + min(m))    
    
    println("mean of (3,6,2,5) is: " + mean(3,6,2,5))
  }
}
