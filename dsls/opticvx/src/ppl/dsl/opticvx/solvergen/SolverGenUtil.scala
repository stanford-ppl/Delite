package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solverir._
import scala.collection.immutable.Seq


trait SolverGenUtil extends SolverGen {

  class OrthoNullProjectorPartial(val A: Almap) {
    private val ATA: Almap = (A.T * A).simplify
    private val u: SVariable = vector(A.domain)
    private val v: SVariable = vector(A.domain)
    private val norm2u: SVariable = scalar
    private val norm2v: SVariable = scalar
    private val udotx: SVariable = scalar
    private val vdotx: SVariable = scalar
    private val udotv: SVariable = scalar
    private val deteq: SVariable = scalar
    private val alpha: SVariable = scalar
    private val beta: SVariable = scalar
    private var initdone: Boolean = false

    def residual: AVector = {
      if(initdone != true) throw new IRValidationException()
      sqrt(vdotx)
    }

    def proj_init(x: AVector): AVector = {
      if(initdone != false) throw new IRValidationException()
      initdone = true

      u := ATA * x
      norm2u := norm2(u)
      udotx := dot(u, x) //here, we cheat to assign the appropriate residual
      alpha := udotx/norm2u
      x - u*alpha
    }

    def proj(x: AVector): AVector = {
      if(initdone != true) throw new IRValidationException()

      v := ATA * x
      norm2u := norm2(u)
      norm2v := norm2(v)
      udotx := dot(u, x)
      vdotx := dot(v, x)
      udotv := dot(u, v)
      deteq := (norm2u * norm2v) - (udotv * udotv)
      alpha := ((norm2v * udotx) - (udotv * vdotx))/deteq
      beta := ((norm2u * vdotx) - (udotv * udotx))/deteq
      u := u*alpha + v*beta
      x - u
    }
  }

  class LSQR(val A: Almap) {
    private val beta = scalar
    private val betau = vector(A.codomain)
    private val u = betau/beta
    private val alpha = scalar
    private val alphav = vector(A.domain)
    private val v = alphav/alpha
    private val w = vector(A.domain)
    private val x = vector(A.domain)
    private val theta = scalar
    private val phi = scalar
    private val phibar = scalar
    private val rho = scalar
    private val rhobar = scalar
    private val c = scalar
    private val s = scalar

    def solve(b: AVector): AVector = solve(b, -1)

    def solve(b: AVector, itermax: Int): AVector = {

      //initialization phase
      betau := b
      beta := sqrt(norm2(betau))
      alphav := A.T*u
      alpha := sqrt(norm2(alphav))
      w := v
      x := zeros(A.domain)
      phibar := beta
      rhobar := alpha

      converge(phibar) {
        //solution phase
        betau := A*v - u*alpha
        beta := sqrt(norm2(betau))
        alphav := A.T*u - v*beta
        alpha := sqrt(norm2(alphav))

        rho := sqrt(norm2(rhobar) + norm2(beta))
        c := rhobar / rho
        s := beta / rho
        theta := s*alpha
        rhobar := -c*alpha
        phi := c*phibar
        phibar := s*phibar

        x := x + w*(phi/rho)
        w := v - w*(theta/rho)
      }

      x
    }
  }

  class LSQRProject(val A: Almap) {
    private val lsqr = new LSQR(A)

    def proj(x: AVector): AVector = proj(x, -1)

    def proj(x: AVector, itermax: Int): AVector = {
      x - lsqr.solve(A*x, itermax)
    }
  }

}
