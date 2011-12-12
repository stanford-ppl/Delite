package ppl.apps.liszt.scalar_convection

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object ScalarConvectionRunner extends DeLisztApplicationRunner with SC

trait SC extends DeLisztApplication {
  var position : Rep[Field[Vertex,Vec[_3,Double]]] = null
  var interior_set : Rep[BoundarySet[Face]] = null
  var inlet_set : Rep[BoundarySet[Face]] = null
  var outlet_set : Rep[BoundarySet[Face]] = null
  var far_field_set : Rep[BoundarySet[Face]] = null
  var float3_zero : Rep[Vec[_3,Double]] = null

  var faceCenter : Rep[Field[Face,Vec[_3,Double]]] = null
  var faceArea : Rep[Field[Face,Double]] = null

  var Phi : Rep[Field[Cell,Double]] = null
  var Flux : Rep[Field[Cell,Double]] = null

  //some geometry fields
  var face_centroid : Rep[Field[Face,Vec[_3,Double]]] = null
  var face_area : Rep[Field[Face,Double]] = null
  var face_normal : Rep[Field[Face,Vec[_3,Double]]] = null
  var face_unit_normal : Rep[Field[Face,Vec[_3,Double]]] = null

  var cell_centroid : Rep[Field[Cell,Vec[_3,Double]]] = null
  var cell_volume : Rep[Field[Cell,Double]] = null
  
  //some geometry functions
  def calcFaceCenter(f : Rep[Face]) : Rep[Vec[_3,Double]] = {
    var center = Vec(0.0,0.0,0.0)
    for(v <- vertices(f)) {
      // TODO
      center = center + position(v)
    }
    center / size(vertices(f))
  }
  def calcCellCenter(c : Rep[Cell]) : Rep[Vec[_3,Double]] = {
    var center = Vec(0.0,0.0,0.0)
    for(v <- vertices(c)) {
      // TODO
      center = center + position(v)
    }
    center / size(vertices(c))
  }
  def calcFaceGeom(f : Rep[Face]) : Rep[Unit] = {
    val approxCenter = calcFaceCenter(f)
    var normal = Vec(0.0,0.0,0.0)
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      // TODO
      normal = normal + cross(v1,v0)
    }
    normal = normalize(normal)
    var center = Vec(0.0,0.0,0.0)
    var area = 0.0
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      val tmp_area = dot(normal,cross(v1,v0))
      area += tmp_area
      // TODO
      center = center + ( approxCenter + position(head(e)) + position(tail(e))) * tmp_area
    }  
    face_centroid(f) = center / (area * 3.0)
    val farea = area / 2.0
    face_area(f) = farea
    face_normal(f) = normal*farea
    face_unit_normal(f) = normal
  }

  def calcCellGeom(c : Rep[Cell]) : Rep[Unit] = {
    // Print("Calc cell geom ", ID(c))
    var center = Vec(0.0,0.0,0.0)
    val approxCenter = calcCellCenter(c)
    // Print("approx center ", approxCenter)
    var volume = 0.0
    for(f <- faces(c)) {
      // Print("Face ", ID(f))
      val v0 = face_centroid(f) - approxCenter
      // Print("v0", v0)
      for(e <- edgesCCW(towards(f,c))) {
        // Print("edge ccw ", ID(e))
        val v1 = position(head(e)) - approxCenter
        val v2 = position(tail(e)) - approxCenter
        // Print(ID(head(e)), " v1 ", v1)
        // Print(ID(tail(e)), " v2 ", v2)
        val tetVol = dot(v0,cross(v1,v2))
        // Print("tetvol ", tetVol)
        volume += tetVol
        center = center + (approxCenter + face_centroid(f) + position(head(e)) + position(tail(e)))*tetVol
        // Print("center ", center)
      }
    }
    // Print("final center ", center)
    cell_centroid(c) = center / (volume * 4.0)
    cell_volume(c) = volume / 6.0
  }
  def phi_sine_function(t : Rep[Double]) : Rep[Double] = {
    10.0 * sin(t*2.0*MATH_PI)
  }
  def normal_pdf(x : Rep[Double]) : Rep[Double] = exp(- x * x / 2.0) / sqrt(2.0 * MATH_PI)
  
  def main() {
    position = FieldWithLabel[Vertex,Vec[_3,Double]]("position")
    interior_set = BoundarySet[Face]("default-interior")
    inlet_set = BoundarySet[Face]("inlet")
    outlet_set = BoundarySet[Face]("outlet")
    far_field_set = BoundarySet[Face]("far_field")
    float3_zero = Vec(0.0,0.0,0.0)

    faceCenter = FieldWithConst[Face,Vec[_3,Double]](float3_zero)
    faceArea = FieldWithConst[Face,Double](0.0)

    Phi = FieldWithConst[Cell,Double](0.0)
    Flux = FieldWithConst[Cell,Double](0.0)

    //some geometry fields
    face_centroid = FieldWithConst[Face,Vec[_3,Double]](float3_zero)
    face_area = FieldWithConst[Face,Double](0.0)
    face_normal = FieldWithConst[Face,Vec[_3,Double]](float3_zero)
    face_unit_normal = FieldWithConst[Face,Vec[_3,Double]](float3_zero)

    cell_centroid = FieldWithConst[Cell,Vec[_3,Double]](float3_zero)
    cell_volume = FieldWithConst[Cell,Double](0.0)
    
    val globalVelocity = Vec(1.0,0.0,0.0)
    
    //initialize geometry fields
    for(f <- faces(mesh)) {
      if(ID(outside(f)) < ID(inside(f))) {
        calcFaceGeom(flip(f))
      } else {
        calcFaceGeom(f)
      }
    }
    // for(f <- faces(mesh)) {
      // Print(ID(f),"FaceArea: ",face_area(f)," normal: ",face_unit_normal(f)," face_centroid: ",face_centroid(f))
    // }
    for(c <- cells(mesh)) {
      calcCellGeom(c)
    }
    // for(c <- cells(mesh)) {
      // Print("c: ",ID(c)," ",cell_volume(c)," ",cell_centroid(c))
    // }
    
    var ll = Vec(MAX_DOUBLE,MAX_DOUBLE,MAX_DOUBLE)
    var ur = Vec(MIN_DOUBLE,MIN_DOUBLE,MIN_DOUBLE)

    for(v <- vertices(mesh)) {
      ll = ll min position(v)
      ur = ur max position(v)
    }
    val mesh_center = (ll + ur) * .5
    for(c <- cells(mesh)) {
      val center = cell_centroid(c)
      val x = normal_pdf(center.x - mesh_center.x)
      val y = normal_pdf(center.y - mesh_center.y)
      val z = normal_pdf(center.z - mesh_center.z)
      Phi(c) = x * y * z
    }
    val deltat = .015
    var t = 0.0
    // for(c <- cells(mesh)) {
      // Print("before cell number: ",ID(c)," -> phi value: ",Phi(c))
    // }
    
    //var start_time = 0.0
    //var num_iter = 0
    // Print("ZA WHILE LOOP")
     // if(num_iter == 1) {
    val start_time = wall_time()
     // }
    while(t < 2.0) {

      // Print("INTERIOR SET")
      
      /**********
       * Loop 1 *
       **********/
      for(f <- interior_set) {
        val normal = face_unit_normal(f)
        val vDotN = dot(globalVelocity,normal)
        val area = face_area(f)
        var flux = 0.0
        val cell = if(vDotN >= 0.0) inside(f) else outside(f)
        
        flux = area * vDotN * Phi(cell)
        
        Flux(inside(f)) -= flux
        Flux(outside(f)) += flux
      }
      
      /**********
       * Loop 2 *
       **********/
      for(f <- outlet_set) {
      // Print("OUTSET SET")
        // Print(ID(f))
        val normal = face_unit_normal(f)
        if(ID(outside(f)) == 0)
        {
          // Print("outside 0 ", ID(inside(f)))
          Flux(inside(f)) -= face_area(f) * dot(normal,globalVelocity) * Phi(inside(f))
        }
        else {
          // Print("outside okay ", ID(outside(f)))
          Flux(outside(f)) -= face_area(f) * dot(-normal,globalVelocity) * Phi(outside(f))
        }
      }

      /**********
       * Loop 3 *
       **********/
	    //Note: 'phi_sine_function(t)' is manually hoisted from the loop to prevent Ref[Double] type input for a GPU kernel
      val multiplier = phi_sine_function(t)
      for(f <- inlet_set) {
        val area = face_area(f)
        val vDotN = dot(globalVelocity,face_unit_normal(f))
        if(ID(outside(f)) == 0)
          Flux(inside(f)) += area * vDotN * multiplier
        else
          Flux(outside(f)) += area * vDotN * multiplier	
      }

      /**********
       * Loop 4 *
       **********/
      for(f <- far_field_set) {
        val normal = face_unit_normal(f)
        if(ID(outside(f)) == 0)
          Flux(inside(f)) -= dot(normal,globalVelocity) * face_area(f) * Phi(inside(f))
        else
          Flux(outside(f)) -= dot(-normal,globalVelocity) * face_area(f) * Phi(outside(f))
      }

      //TODO(zach): some more loops for boundary conditions go here
      for(c <- cells(mesh)) {
        Phi(c) = Phi(c) + deltat * Flux(c) / cell_volume(c)
      }
      for(c <- cells(mesh)) {
        Flux(c) = 0.0
      }
      
      t += deltat
      //num_iter += 1
    }
    
    Print( "TIME_FOR_LOOP: ", wall_time() - start_time, " num iter: " )
      
     for(c <- cells(mesh)) {
       Print("cell number: ",ID(c)," -> phi value: ",Phi(c))
     }
  }
}

