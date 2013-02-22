package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object AlternatingProjections extends SolverGen {


  def code(A: Almap, b: AVector, F: Almap, g: AVector, c: AVector, cone: Cone) {
    val varSize = A.domain
    val affineCstrtSize = A.codomain
    val coneSize = F.codomain

    val bm = v2m(b)
    val cm = v2m(c)
    val gm = v2m(g)

    val x_out = vector(varSize)
    val u = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val x = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val v = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val norm2v = scalar
    val norm2u = scalar
    val udotv = scalar
    val udotx = scalar
    val vdotx = scalar
    val deteq = scalar
    val alpha = scalar
    val beta = scalar
    val J = scalar

    val M = vcat(
      hcat(zeros(A.domain, A.domain), -A.T, -F.T, zeros(F.codomain, A.domain), cm, zeros(1, A.domain)),
      hcat(A, zeros(A.codomain + F.codomain + F.codomain, A.codomain), bm, zeros(1, A.codomain)),
      hcat(F, zeros(A.codomain + F.codomain, F.codomain), -eye(F.codomain), gm, zeros(1, F.codomain)),
      hcat(-cm.T, -bm.T, -gm.T, zeros(F.codomain + 1, 1), -eye(1)))
    val K = cat(freecone(A.domain + A.codomain), cone.conj, cone, ConeNonNegative(cone.arity), ConeNonNegative(cone.arity))
    
    x := cat(zeros(varSize + affineCstrtSize + coneSize + coneSize), ones(2))
    
    v := (M.T * M) * x
    norm2v := norm2(v)
    alpha := dot(v, x)/norm2v
    x := x - v*alpha
    u := M.T*M*x
    udotx := dot(u, x)
    J := ones(1)
    converge(J) {
      norm2u := norm2(u)
      udotv := dot(u, v)
      vdotx := dot(v, x)
      deteq := (norm2u * norm2v) - (udotv * udotv)
      alpha := ((norm2v * udotx) - (udotv * vdotx))/deteq
      beta := ((norm2u * vdotx) - (udotv * udotx))/deteq
      v := u*alpha + v*beta
      x := x - v
      x := K.project(x)
      x := x / sqrt(norm2(x))
      u := M.T*M*x
      udotx := dot(u, x)
      J := udotx
    }

    v := K.conj.project(-x)
    x_out := slice(x, 0, varSize) / slice(x, varSize + affineCstrtSize + coneSize + coneSize, 1)
  }
}

