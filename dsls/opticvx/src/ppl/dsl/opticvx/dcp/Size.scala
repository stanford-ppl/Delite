package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Map

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{Base, BaseExp}

class DCPIRValidationException extends Exception

trait DCPSize extends BaseExp {
  self: DCPExpr =>

  abstract class IntParam
  class IntParamInput(val rep: Exp[Int]) extends IntParam
  class IntParamBound extends IntParam
  
  case class Size(val const: Int, val coeffs: Map[IntParam, Int]) {
    //if (const < 0) throw new DCPIRValidationException()
    //for ((ip, c) <- coeffs) {
    //  if (c < 0) throw new DCPIRValidationException()
    //}

    def +(s: Size): Size = {
      var mc: Map[IntParam, Int] = coeffs
      for ((k, v) <- s.coeffs) {
        if (mc contains k) {
          mc += (k -> (mc(k) + v))
        }
        else {
          mc += (k -> v)
        }
      }
      Size(const + s.const, mc)
    }

    def *(a: Int): Size = Size(const*a, coeffs mapValues ((v) => v*a))
  }

  implicit def intparam2size(ip: IntParam): Size = Size(0, Map(ip -> 1))
  implicit def int2size(i: Int): Size = Size(i, Map())
  implicit def expint2size(i: Exp[Int]): Size = Size(0, Map(new IntParamInput(i) -> 1))


  case class SizeIntMpyHack(a: Int) {
    def *(s: Size): Size = s*a
  }

  implicit def sizeintmpyhackimplicit(i: Int) = SizeIntMpyHack(i)
}

