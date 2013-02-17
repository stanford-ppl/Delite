package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualSubgradient extends SolverGen {

  def code(A: Almap, b: AVector, F: Almap, g: AVector, c: AVector, cone: Cone) {
    val varSize = A.domain
    val affineCstrtSize = A.codomain
    val coneSize = F.codomain

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

    x := zeros(varSize)
    v := zeros(affineCstrtSize)
    y := zeros(coneSize)
    J := 1.0
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
}