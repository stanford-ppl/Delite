package ppl.tests.scalatest.firstdsl

import scala.virtualization.lms.common.{ScalaGenEffect, Base, EffectExp}

// this is the abstract interface of our profiling methods
trait ProfileOps extends Base {
  def profile(n: Rep[Int]) = new ProfileOpsCls(n)

  // syntax
  class ProfileOpsCls(n: Rep[Int]) {
    def times(func: => Rep[Any]) = profile_body(n, func)
  }
 
  // implementation
  def profile_body(n: Rep[Int], func: => Rep[Any]): Rep[ProfileArray]
}

trait ProfileOpsExp extends ProfileOps with EffectExp {
  case class Profile(n: Exp[Int], body: Block[Any]) extends Def[ProfileArray]

  def profile_body(n: Exp[Int], func: => Exp[Any]) = {
    reflectEffect(Profile(n, reifyEffects(func)))  // create an IR node
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Profile(n, body) => effectSyms(body)
    case _ => super.boundSyms(e)
  }  
}

trait ScalaGenProfileOps extends ScalaGenEffect {
  val IR: ProfileOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = 
    rhs match {
      // insert instrumentation code around function body
      case Profile(n, body) => 
        stream.println("val " + quote(sym) + " = {")
        stream.println("val out = new ProfileArray(" + quote(n) + ")")
        stream.println("var i = 0")
        stream.println("while (i < " + quote(n) + ") {")
        stream.println("  val start = System.currentTimeMillis()")
        emitBlock(body)
        stream.println("  val end = System.currentTimeMillis()")
        stream.println("  val duration = (end - start)/1000f ")
        stream.println("  out._data(i) = duration")
        stream.println("  i += 1")
        stream.println("}")
        stream.println("out")
        stream.println("}")

      case _ => super.emitNode(sym, rhs)
    }
}