package ppl.apps.liszt.test

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object SimpleLoopRunner extends DeLisztApplicationRunner with SimpleLoop

trait SimpleLoop extends DeLisztApplication {
  var position : Rep[Field[Vertex,Vec[_3,Float]]] = null
  
  //some geometry functions
  def calcFaceCenter(f : Rep[Face]) : Rep[Vec[_3,Float]] = {
    var center = Vec(0.f,0.f,0.f)
    Print("center ", center)
    for(v <- vertices(f)) {
      // TODO
      center = center + position(v)
      Print("center add pos ", center, " pos ", position(v))
    }
    Print("divided", center / size(vertices(f)), " size ", size(vertices(f)))
    center / size(vertices(f))
  }
  
  def calcFaceGeom(f : Rep[Face]) : Rep[Unit] = {
    val approxCenter = calcFaceCenter(f)
    Print("Center ", approxCenter)
    var normal = Vec(0.f,0.f,0.f)
    Print("Normal start ", normal)
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      // TODO
      normal = normal + cross(v1,v0)
      Print("Normal cross ", normal)
    }
    Print("Normal before normalize ", normal)
    normal = normalize(normal)
    Print("Normal normalized ", normal)
  }

  def main() {
    position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")
    val globalVelocity = Vec(1.f,0.f,0.f)

    //initialize geometry fields
    for(f <- faces(mesh)) {
      if(ID(outside(f)) < ID(inside(f))) {
        calcFaceGeom(flip(f))
      } else {
        calcFaceGeom(f)
      }
    }
  }
}
