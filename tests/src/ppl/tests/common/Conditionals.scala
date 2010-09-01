package ppl.tests.common

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.ScalaCompile
import scala.virtualization.lms.ppl._

trait ConditionalsTests {
  this: ScalaOpsPkg with Equal with IfThenElse =>

  def test1(): Rep[Boolean] = {
    val cond1: Rep[Boolean] = false
    if(cond1 == true) true else false
    val cond2: Rep[Boolean] = false
    if(true == cond2) false else true
  }

  def test2(x: Rep[Double]): Rep[Double] = {

    print("yoyo")

    val z = if (x == x) {
      print("yoyo")
      print("xxx")
      print("yoyo")
      (x+4)
    } else {
      (x+6)
    }

    print("yyy")
    print("yoyo")

    z + (x + 4)
  }
}


object Conditionals {

  def main(args: Array[String]) {
    /* no longer in common embedding
    {
      println("[Conditionals][Test][StringRep]")
      val o = new ConditionalsTests with ScalaOpsRepString with ControlFlowString
      val a = o.test1()
      println(a)
    }
    */
    {
      println("[Conditionals][Test1][StringExpEffect]")
      val o = new ConditionalsTests with ScalaOpsPkgExp with IfThenElseExp with EqualExp with FunctionsExp
      val a = o.test1()
      println(a)
    }

    {
      println("[Conditionals][Test1][CodeGenpEffect]")
      val o = new ConditionalsTests with ScalaOpsPkgExp with ScalaGenScalaOpsPkg with ScalaGenIfThenElse with ScalaGenEqual with ScalaGenFunctions
      import o._
      val arg = (x:Rep[Any]) => o.test1()
      emitScalaSource(arg,"test2", new PrintWriter(System.out))
    }
   {
      println("[Conditionals][Test2][CodeGenpEffect]")
      val o = new ConditionalsTests with ScalaGenScalaOpsPkg with ScalaGenIfThenElse with ScalaGenEqual with ScalaGenFunctions with ScalaCompile
      val t = (x:o.Rep[Double]) => o.test2(x)
      o.emitScalaSource(t,"test2", new PrintWriter(System.out))
      val c = o.compile(t)
      println(c(5))
    }
  }
}
