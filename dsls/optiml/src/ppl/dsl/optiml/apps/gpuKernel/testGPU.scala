package ppl.dsl.optiml.apps.testGPU

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication
object testGPU extends DeliteApplication with OptiMLExp {

  def main() = {

    val x = Vector[Double](100)
    val y = Vector[Double](100)
    val z = Matrix[Double](100,100)
    //val sigma = x ** y
    //val sigma = if(x.is_row) x/10 else x~

    /*
    val sigma = sum(0, 10) { i =>
       //z(i) ** z(i+1)
        if(x(i) > 5)
          (((z(i)-x)~)**(z(i)-x))
      else
          (((z(i)-y)~)**(z(i)-y))
    }
    */

    //val z = Vector[Double](100)
    //z += x
    //println(sigma)
    //println(z)

    /*
    val a = Const(3)
    val b = Const(5)
    val cc = a + b
    val c = cc + Const(1)

    //def myFunc(x:Rep[Int], y:Rep[Int]) : Rep[Int] = {x + y}

    val ddd = doLambda{ y: Rep[(Int,Int)] => c }
    
    val dd = doLambda{ y:Rep[Int] =>
        if(cc == 0) {
        println(cc)
      }
        c * c
    }
    
    val d = doLambda{x:Rep[Int] =>
      val q = x + Const(10)
      val r = c - x
      val s = r * q + dd(q) // + myFunc(r,r)
      s
    }


    println(d(3))
    */

    /*
    val a = Vector[Double](100)
    val b = Vector[Double](100)
    val c = Vector[Double](100)
    for(i <- 0 until a.length) {
      for(j <- 1 until b.length) {
        a(j) = b(j) + c(j)
      }
    }
    println(a)
    */

    /*
    val bb = Vector[Double](100)
    val a1 = Vector[Double](bb.length)
    val b1 = Vector[Double](100)
    val c1 = a1 + b1
    println(c1)

    while(a1.is_row) {
      println(c1)
    }
    
    val a2 = Vector[Double](100)
    val b2 = Vector[Double](100)
    val c2 = a2 - b2
    println(c2)

    val a3 = Vector[Double](100)
    val b3 = a3(0)
    val c3 = a3 / b3
    println(c3)

    val a4 = Vector[Double](100)
    val b4 = Vector[Double](100)
    val c4 = a4 outer b4
    println(c4)
    */
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
