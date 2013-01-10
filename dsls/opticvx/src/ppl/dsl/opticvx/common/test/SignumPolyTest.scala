package ppl.dsl.opticvx.common.test

import scala.collection.immutable.Seq
import ppl.dsl.opticvx.common._

object SignumPolyTestApp extends App {
  import Signum._
  val a = SignumPoly.param(0, 3)
  val b = SignumPoly.param(1, 3)
  val c = SignumPoly.param(2, 3)
  if (a.eval(Seq(Zero,Zero,Zero))!=Zero) throw new Exception()
  if (a.eval(Seq(Positive,Negative,All))!=Positive) throw new Exception()
  if (c.eval(Seq(Positive,Negative,All))!=All) throw new Exception()
  if ((a+c).eval(Seq(Positive,Negative,Zero))!=Positive) throw new Exception()
  if ((b*c).eval(Seq(Zero,Negative,Negative))!=Positive) throw new Exception()
}
