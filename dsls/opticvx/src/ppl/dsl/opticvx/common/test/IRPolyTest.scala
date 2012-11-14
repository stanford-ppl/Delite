package ppl.dsl.opticvx.common.test

import scala.collection.immutable.Seq
import ppl.dsl.opticvx.common._

object IRPolyTestApp extends App {
  implicit val iliimpl = IntLikeInt
  val a = IRPoly.param(0, 3)
  val b = IRPoly.param(1, 3)
  val c = IRPoly.param(2, 3)
  if (a.eval(Seq(0,0,0))!=0) throw new Exception()
  if (a.eval(Seq(1,2,3))!=1) throw new Exception()
  if (c.eval(Seq(1,2,3))!=3) throw new Exception()
  if ((a+c).eval(Seq(1,2,3))!=4) throw new Exception()
  if ((b*c).eval(Seq(1,2,3))!=6) throw new Exception()
  if (((a*(a+IRPoly.const(1,3)))/2).eval(Seq(2,3,4))!=3) throw new Exception()
  a.toString
  b.toString
  c.toString
  (a*a).toString
  (a*b*c).toString
}
