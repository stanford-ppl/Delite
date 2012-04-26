import ppl.dsl.optiml._
object HelloWorldRunner extends OptiMLApplicationRunner with HelloWorld 
trait HelloWorld extends OptiMLApplication { 
  def main() = {
    // hello world has been co-opted into a faster compiling, temporary scratch space
    // for tests that haven't been made into scalatests yet
    
    // TODO: make scalatests out of all the use cases here
    
    //println("hello world")
    val v = Vector(1,2,3,4,5)
    val vl = log(v)
    vl.pprint
    println(vl)
    println("vl is: " + vl)
    
    val m = Matrix.rand(10,10)
    m.pprint
    
    val me = exp(m)
    me.pprint
    
    println("median v: " + median(v))
    println("mean v: " + mean(v))
    println("mean m: " + mean(m))
    println("max v: " + max(v))
    println("min m: " + min(m))    
    
    println("mean of (3,6,2,5) is: " + mean(3,6,2,5))
    
    val vSlice = v(0::2)
    vSlice.pprint    
    val vSlice2 = v(3::5)
    vSlice2.pprint    
    val vSlice3 = v(4,2,0)
    vSlice3.pprint    
    val vSlice4 = vSlice3.sort
    vSlice4.pprint
    
    val mSlice = m(0::2,4::5)
    println("mSlice: ")
    mSlice.pprint
    val mSlice2 = m(3::4)  
    println("mSlice2: ")
    mSlice2.pprint
    val mSlice3 = m(*,0::2)
    println("mSlice3: ")
    mSlice3.pprint
    val mSlice4 = m(1::4,*)  
    println("mSlice4: ")
    mSlice4.pprint
    val mSlice5 = m(IndexVector(3,1,2),IndexVector(4,0,6))
    println("mSlice5: ")
    mSlice5.pprint
    
    println("--- testing pattern matching")
    val a = Vector(1,2,3)
    val b = Vector(3,4,5)
    val c = Vector(6,7,8)
    val pattern = a*b + a*c
    pattern.pprint
  }
}
