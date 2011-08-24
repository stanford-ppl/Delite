package ppl.apps.liszt.scalar_convection

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object ScalarConvectionRunner extends DeLisztApplicationRunner with SC

trait SC extends DeLisztApplication {
  var position : Rep[Field[Vertex,Vec[_3,Float]]] = null
  var interior_set : Rep[BoundarySet[Face]] = null
  var inlet_set : Rep[BoundarySet[Face]] = null
  var outlet_set : Rep[BoundarySet[Face]] = null
  var far_field_set : Rep[BoundarySet[Face]] = null
  var float3_zero : Rep[Vec[_3,Float]] = null

  var faceCenter : Rep[Field[Face,Vec[_3,Float]]] = null
  var faceArea : Rep[Field[Face,Float]] = null

  var Phi : Rep[Field[Cell,Float]] = null
  var Flux : Rep[Field[Cell,Float]] = null

  //some geometry fields
  var face_centroid : Rep[Field[Face,Vec[_3,Float]]] = null
  var face_area : Rep[Field[Face,Float]] = null
  var face_normal : Rep[Field[Face,Vec[_3,Float]]] = null
  var face_unit_normal : Rep[Field[Face,Vec[_3,Float]]] = null

  var cell_centroid : Rep[Field[Cell,Vec[_3,Float]]] = null
  var cell_volume : Rep[Field[Cell,Float]] = null
  
  //some geometry functions
  def calcFaceCenter(f : Rep[Face]) : Rep[Vec[_3,Float]] = {
    var center = Vec(0.f,0.f,0.f)
    for(v <- vertices(f)) {
      center += position(v)
    }
    center = center / size(vertices(f))
    return center
  }
  def calcCellCenter(c : Rep[Cell]) : Rep[Vec[_3,Float]] = {
    var center = Vec(0.f,0.f,0.f)
    for(v <- vertices(c)) {
      center += position(v)
    }
    center = center / size(vertices(c))
    return center
  }
  def calcFaceGeom(f : Rep[Face]) : Rep[Unit] = {
    val approxCenter = calcFaceCenter(f)
    var normal = Vec(0.f,0.f,0.f)
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      normal += cross(v1,v0)
    }
    normal = normalize(normal)
    var center = Vec(0.f,0.f,0.f)
    var area = 0.f
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      val tmp_area = dot(normal,cross(v1,v0))
      area += tmp_area
      center += ( approxCenter + position(head(e)) + position(tail(e))) * tmp_area
    }  
    face_centroid(f) = center / (area * 3.f)
    val farea = area / 2.f
    face_area(f) = farea
    face_normal(f) = normal*farea
    face_unit_normal(f) = normal
  }

  def calcCellGeom(c : Rep[Cell]) : Rep[Unit] = {
    val approxCenter = calcCellCenter(c)
    var volume = 0.f
    var center = Vec(0.f,0.f,0.f)
    for(f <- faces(c)) {
      val v0 = face_centroid(f) - approxCenter
      for(e <- edgesCCW(towards(f,c))) {
      val v1 = position(head(e)) - approxCenter
      val v2 = position(tail(e)) - approxCenter
      val tetVol = dot(v0,cross(v1,v2))
      volume += tetVol
      center += ( approxCenter + face_centroid(f) + position(head(e)) + position(tail(e))) * tetVol
      }
    }
    cell_centroid(c) = center / (volume * 4.f)
    cell_volume(c) = volume / 6.f
  }
  def phi_sine_function( t : Rep[Float]) : Rep[Float] = {
    return 10.f * sinf(t*2.f*MATH_PI.asInstanceOfL[Float])
  }
  def normal_pdf(x : Rep[Float]) : Rep[Float] = expf(- x * x / 2.f) / sqrtf(2.f * MATH_PI.asInstanceOfL[Float])
  
  def main() {
    position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")
    interior_set = BoundarySet[Face]("default-interior")
    inlet_set = BoundarySet[Face]("inlet")
    outlet_set = BoundarySet[Face]("outlet")
    far_field_set = BoundarySet[Face]("far_field")
    float3_zero = Vec(0.f,0.f,0.f)

    faceCenter = FieldWithConst[Face,Vec[_3,Float]](float3_zero)
    faceArea = FieldWithConst[Face,Float](0.f)

    Phi = FieldWithConst[Cell,Float](0.f)
    Flux = FieldWithConst[Cell,Float](0.f)

    //some geometry fields
    face_centroid = FieldWithConst[Face,Vec[_3,Float]](float3_zero)
    face_area = FieldWithConst[Face,Float](0.f)
    face_normal = FieldWithConst[Face,Vec[_3,Float]](float3_zero)
    face_unit_normal = FieldWithConst[Face,Vec[_3,Float]](float3_zero)

    cell_centroid = FieldWithConst[Cell,Vec[_3,Float]](float3_zero)
    cell_volume = FieldWithConst[Cell,Float](0.f)
    
    val globalVelocity = Vec(1.f,0.f,0.f)
    //initialize geometry fields
    for(f <- faces(mesh)) {
      if(ID(outside(f)) < ID(inside(f))) {
      calcFaceGeom(flip(f))
      } else {
      calcFaceGeom(f)
      }
    }
    for(f <- faces(mesh)) {
      Print(ID(f),"FaceArea: ",face_area(f),"normal: ",face_unit_normal(f),"face_centroid",face_centroid(f))
    }
    for(c <- cells(mesh)) {
      calcCellGeom(c)
    }
    for(c <- cells(mesh)) {
      Print("c: ",ID(c)," ",cell_volume(c)," ",cell_centroid(c))
    }
    var ll = Vec(MAX_FLOAT,MAX_FLOAT,MAX_FLOAT)
    var ur = Vec(MIN_FLOAT,MIN_FLOAT,MIN_FLOAT)

    for(v <- vertices(mesh)) {
      ll = ll min position(v)
      ur = ur max position(v)
    }
    val mesh_center = (ll + ur) * .5f
    for(c <- cells(mesh)) {
      val center = cell_centroid(c)
      val x = normal_pdf(center.x - mesh_center.x)
      val y = normal_pdf(center.y - mesh_center.y)
      val z = normal_pdf(center.z - mesh_center.z)
      Phi(c) = x * y * z
    }
    val deltat = .015f
    var t = 0.f
    for(c <- cells(mesh)) {
      Print("before cell number: ",ID(c)," -> phi value: ",Phi(c))
    }
    while(t < 2.f) {

      for(f <- interior_set) {
      val normal = face_unit_normal(f)
      val vDotN = dot(globalVelocity,normal)
      val area = face_area(f)
      var flux = 0.f
      val cell = if(vDotN >= 0.f) inside(f) else outside(f)
      
      flux = area * vDotN * Phi(cell)
      
      Flux(inside(f)) -= flux
      Flux(outside(f)) += flux
      }
        for(f <- outlet_set) {
          val normal = face_unit_normal(f)
          if(ID(outside(f)) == 0)
            Flux(inside(f)) -= face_area(f) * dot(normal,globalVelocity) * Phi(inside(f))
          else
            Flux(outside(f)) -= face_area(f) * dot(-normal,globalVelocity) * Phi(outside(f))
        }
      for(f <- inlet_set) {
      val area = face_area(f)
      val vDotN = dot(globalVelocity,face_unit_normal(f))
       if(ID(outside(f)) == 0)
        Flux(inside(f)) += area * vDotN * phi_sine_function(t)
       else
        Flux(outside(f)) += area * vDotN * phi_sine_function(t)		
      }
      for(f <- far_field_set) {
       val normal = face_unit_normal(f)
       if(ID(outside(f)) == 0)
         Flux(inside(f)) -= dot(normal,globalVelocity) * face_area(f) * Phi(inside(f))
       else
         Flux(outside(f)) -= dot(-normal,globalVelocity) * face_area(f) * Phi(outside(f))
      }

      //TODO(zach): some more loops for boundary conditions go here
      for(c <- cells(mesh)) {
      Phi(c) += deltat * Flux(c) / cell_volume(c)
      }
      for(c <- cells(mesh)) {
      Flux(c) = 0.f
      }
      t += deltat
    }
    for(c <- cells(mesh)) {
      Print("cell number: ",ID(c)," -> phi value: ",Phi(c))
    }
  }
}
