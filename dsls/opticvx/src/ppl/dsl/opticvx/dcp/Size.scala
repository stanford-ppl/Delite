package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Map

class DCPIRValidationException extends Exception

trait DCPSize {

  class IntParam

  case class Size(val const: Int, val coeffs: Map[IntParam, Int]) {
    if (const < 0) throw new DCPIRValidationException()
    for ((ip, c) <- coeffs) {
      if (c < 0) throw new DCPIRValidationException()
    }

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

  case class SizeIntMpyHack(a: Int) {
    def *(s: Size): Size = s*a
  }

  implicit def sizeintmpyhackimplicit(i: Int) = SizeIntMpyHack(i)
}
