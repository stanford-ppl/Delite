package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualSubgradient extends SolverGenBase {
  trait Variables extends SGVariables {

    val x = vector(varSize)
    val v = vector(affineCstrtSize)
    val y = vector(coneSize)
    val Axb = vector(affineCstrtSize)
    val Fxg = vector(coneSize)
    val cTxbTvgTy = scalar
    val ATvFTyc = vector(varSize)
    val pvFxg = vector(coneSize)
    val pvy = vector(coneSize)
    val J = scalar
    val GJx = vector(varSize)
    val GJy = vector(coneSize)
    val GJv = vector(affineCstrtSize)
    val NG = scalar
    val alpha = scalar
  }
  trait Code extends SGCode {
    self: Variables =>

    x := 0
    v := 0
    y := 0
    J := 1
    Axb := A*x + b
    Fxg := F*x + g
    cTxbTvgTy := dot(c,x) + dot(b,v) + dot(g,y)
    ATvFTyc := A.T*v + F.T*y - c
    pvFxg := Fxg - cone.project(Fxg)
    pvy := y - cone.conj.project(y)
    J := norm2(cTxbTvgTy) + norm2(Axb) + norm2(ATvFTyc) + norm2(pvFxg) + norm2(pvy)
    converge(J) {
      GJx := c*cTxbTvgTy + A.T*Axb + F.T*pvFxg
      GJy := g*cTxbTvgTy + F*ATvFTyc + pvy
      GJv := b*cTxbTvgTy + A*ATvFTyc
      NG := norm2(GJx) + norm2(GJy) + norm2(GJv)  
      alpha := J/(NG + NG)
      x := x - GJx * alpha
      y := y - GJy * alpha
      v := v - GJv * alpha
      Axb := A*x + b
      Fxg := F*x + g
      cTxbTvgTy := dot(c,x) + dot(b,v) + dot(g,y)
      ATvFTyc := A.T*v + F.T*y - c
      pvFxg := Fxg - cone.project(Fxg)
      pvy := y - cone.conj.project(y)
      J := norm2(cTxbTvgTy) + norm2(Axb) + norm2(ATvFTyc) + norm2(pvFxg) + norm2(pvy)
    }
  }
  case class Gen(val problem: Problem) extends SGVariables with Variables with SGCode with Code with SGGen
}