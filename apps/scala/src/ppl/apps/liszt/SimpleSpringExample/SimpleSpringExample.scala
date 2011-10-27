package ppl.apps.liszt.SimpleSpringExample

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object SSSRunner extends DeLisztApplicationRunner with SSS

trait SSS extends DeLisztApplication {
  type Float3 = Vec[_3, Float]

  def dampedSpringForce(L : Rep[Float3], W : Rep[Float3]) : Rep[Float3] = {
    val rl = 1
    val Ks = 20.0f
    val Kd = 50.0f
    val l = length(L)

  // Conversions suck
    return -(L/l) * (Ks*(l-rl).floatValueL + Kd*(dot(L,W)/l))
  }

  def main() {
    val Position = FieldWithLabel[Vertex,Float3]("position")
    val Velocity = FieldWithConst[Vertex,Float3](Vec(0.f,0.f,0.f))
    val Force = FieldWithConst[Vertex,Float3](Vec(0.f,0.f,0.f))
  
    var deltat = 0.15f
    var maxforce = 0.0f
    var t = 0.f;
    while (t < 2.0f) {
    for (spring <- edges(mesh)) {
      val v1 = head(spring)
      val v2 = tail(spring)
      val L = Position(v1) - Position(v2)
      val W = Velocity(v1) - Velocity(v2)
      val springForce = dampedSpringForce(L,W)
      Force(v1) += springForce
      Force(v2) -= springForce
      // TODO WHAT IS GOING ON HERE? maxforce = max(maxforce, springForce)
    }
    // TODO CONVERSIONS SUCK
    deltat = 1.0f / maxforce*0.5f 
    for (ptcl <- vertices(mesh)) {
      // TODO IT WAS BACKWARDS BEFORE
      Velocity(ptcl) += Force(ptcl) * deltat
    }
    t += deltat
    for (ptcl <- vertices(mesh)) { Force(ptcl) = Vec(0.f,0.f,0.f) }
    }
    
  }
  //================================================================================
}
