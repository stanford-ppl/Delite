package ppl.dsl.optiml.apps.testGPU

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication
object testGPU extends DeliteApplication with OptiMLExp {

  def main() = {

    val a = Const(3)
    val b = Const(5)
    val cc = a + b
    val c = cc + Const(1)

    val dd = doLambda{y:Rep[Int] =>
        if(y == 0) {
        println(cc)
      }
        c * y
    }
    
    val d = doLambda{x:Rep[Int] =>
      val q = x + Const(10)
      val r = c - x
      val s = r * q + dd(r)

      s
    }


    println(d)

    //val a = Vector[Double](100)
    //val b = Vector[Double](100)

    //val b = a(0)
    //val c = a / b
    //val c = a + b
    //val d = a - b
    //println(c)
    //println(d)

    /*
    val myFunc = (x:Rep[Double]) => if(x>0.0) true else false
    //val myFunc = (x:Rep[Double]) => if(x>0.0) true
    val in = a(3)
    val xx = myFunc(in)
    println(xx)

    //val res = a.toBoolean(ele => if(ele>2.3) true else false)
    //val res2 = a + b
    //println(res)
    //println(res2)

    //val vec1 = Vector[Double](100)
    //val vec2 = Vector[Double](100)

    //val vec3 = vec1 / 5.0
    //println(vec3)
    */
    /*

    val a = 3
    val b = 5
    val c = a + b
    println(c)

    val func = (x:Int) => {println(x)}
    println(func(3))
   */
  }
}
