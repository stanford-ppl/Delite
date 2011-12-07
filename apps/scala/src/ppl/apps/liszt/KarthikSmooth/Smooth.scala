package ppl.apps.liszt.KarthikSmooth

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object MeshSmoothRunner extends DeLisztApplicationRunner with Smooth

trait Smooth extends DeLisztApplication {
  val float3_zero  : Rep[Vec[_3,Double]] = null
  var float33_zero : Rep[Mat[_3,_3,Double]] = null
  var work_array : Rep[Mat[_3,_3,Double]] = null
  var pi : Rep[Double] = null
  var Omega : Rep[Double] = null
  var alpha : Rep[Double] = null

  var Phi        : Rep[Field[Face,Double]] = null
  var PhiOld     : Rep[Field[Face,Double]] = null
  var Phi_Grad   : Rep[Field[Face,Vec[_3,Double]]] = null
  var Flux  	 : Rep[Field[Face,Double]] = null
  var Veloc 	 : Rep[Field[Edge,Vec[_3,Double]]] = null
  var VelocOld	 : Rep[Field[Edge,Vec[_3,Double]]] = null
  var Rhs_Veloc	 : Rep[Field[Edge,Vec[_3,Double]]] = null
  var Velochat_Grad : Rep[Field[Edge,Mat[_3,_3,Double]]] = null
  var Phihat_Grad : Rep[Field[Edge,Vec[_3,Double]]] = null
  var PMat       : Rep[Field[Edge,Mat[_3,_3,Double]]] = null

    //some fields for output
  var Phiv  	 : Rep[Field[Vertex,Double]] = null
  var Phiv_Grad  : Rep[Field[Vertex,Vec[_3,Double]]] = null
  var Velocv     : Rep[Field[Vertex,Vec[_3,Double]]] = null

    //some geometry fields
  var position : Rep[Field[Vertex,Vec[_3,Double]]] = null

  var theta_vertex  : Rep[Field[Vertex,Double]] = null
  var lambda_vertex : Rep[Field[Vertex,Double]] = null

  var theta_face  : Rep[Field[Face,Double]] = null
  var lambda_face : Rep[Field[Face,Double]] = null

  var theta_edge  : Rep[Field[Edge,Double]] = null
  var lambda_edge : Rep[Field[Edge,Double]] = null

  var face_centroid : Rep[Field[Face,Vec[_3,Double]]] = null
  var face_area : Rep[Field[Face,Double]] = null
  var face_normal : Rep[Field[Face,Vec[_3,Double]]] = null

  var edge_center  : Rep[Field[Edge,Vec[_3,Double]]] = null
  var edge_length  : Rep[Field[Edge,Double]] = null
  var edge_normal  : Rep[Field[Edge,Vec[_3,Double]]] = null

  def left(e : Rep[Edge]) : Rep[Face] = {
    val f0 = face(e,0)
    val f = if(ID(outside(f0)) == 0)
      f0
    else
      face(e,1)
    f
  }

  def right(e : Rep[Edge]) : Rep[Face] = {
    val f0 = face(e,1)
    val f = if(ID(inside(f0)) == 0)
      f0
    else
      face(e,0)
    f
  }

  def calcFaceCenter(f : Rep[Face]) : Rep[Vec[_3,Double]] = {
    var center = Vec(0.0,0.0,0.0)
    //val test = Vec(0.0,0.0,0.0)
    for(v <- vertices(f)) {
      center = center + position(v)
    }
    center = center / size(vertices(f))
    return center
  }

  def InterpolateVertex(v : Rep[Vertex]) : Rep[Unit] = {
    var phiv = 0.0
    var phiv_grad = float3_zero

    for(f <- faces(v)) {
      phiv = phiv + Phi(f)
      phiv_grad = phiv_grad + Phi_Grad(f)
    }
    // TODO
    phiv = phiv / size(faces(v))
    
    // TODO
    phiv_grad = phiv_grad / size(faces(v))

    Phiv(v)=phiv
    Phiv_Grad(v)=phiv_grad

    var velv = float3_zero
    for(e <- edges(v)) {
      velv = velv + Veloc(e)
    }
    
    // TODO
    velv = velv / size(edges(v))
    Velocv(v) = velv
  }

  def calcFaceGeom(f : Rep[Face]) : Rep[Unit] = {
    val approxCenter = calcFaceCenter(f)
    var normal = Vec(0.0,0.0,0.0)
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
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
      center = center + ( approxCenter + position(head(e)) + position(tail(e))) * tmp_area
    }
    face_centroid(f) = center / (area * 3.0)
    val farea = area / 2.0
    face_area(f) = farea
    face_normal(f) = normal*farea
  }

  def calcEdgeGeom(e : Rep[Edge]) : Rep[Unit] = {
    var e1 = position(head(e)) 
    var e2 = position(tail(e))
    var e3 = e2-e1
    var center = (e1+e2)*0.5f
    var normal = cross(center,e3)
    var length = sqrt(dot(e3,e3))
    normal = normalize(normal)*length

    edge_center(e) = center
    edge_length(e) = length
    edge_normal(e) = normal
  
    var rad2i = 1.0/dot(center,center)

    work_array = float33_zero.mutable
    work_array(0,0)= 1.0 - center.x*center.x*rad2i
    work_array(0,1)=     - center.x*center.y*rad2i
    work_array(0,2)=     - center.x*center.z*rad2i
    work_array(1,0)=      work_array(0,1) 
    work_array(1,1)= 1.0 - center.y*center.y*rad2i
    work_array(1,2)=     - center.y*center.z*rad2i
    work_array(2,0)=      work_array(0,2) 
    work_array(2,1)=      work_array(1,2)
    work_array(2,2)= 1.0 - center.z*center.z*rad2i
    PMat(e) = work_array.unsafeImmutable
  }

  def SetEdgeVel(e : Rep[Edge]) : Rep[Unit] = {
    val omega = 0.1079
    val K = 0.1079
    val R = 4
    var center = edge_center(e)
    var rad = sqrt(dot(center,center))
    var theta = acos(center.z/rad)
    theta=(pi*0.5).asInstanceOfL[Double]-theta
  
    var lambda = atan2(center.y,center.x)
  
    val ct = cos(theta)
    val st = sin(theta)
    val cr = cos(lambda*R)
    val sr = sin(lambda*R)

    var uhat =  omega*ct+K*pow(ct,(R-1))*(st*st*R-ct*ct)*cr
    var vhat = -K*R*pow(ct,(R-1))*st*sr
    
    var ux = -uhat*sin(lambda)-vhat*sin(theta)*cos(lambda)
    var uy =  uhat*cos(lambda)-vhat*sin(theta)*sin(lambda)
    var uz =  vhat*cos(theta)

    val P  = PMat(e)
    val vel = Vec(ux,uy,uz)
    val vel1 = P*vel

    Veloc(e)=vel1
  }

  def SetIC(f : Rep[Face]) : Rep[Unit] = {
    val omega = 0.1079
    val K = 0.1079
    val R = 4.0
    
    val center = face_centroid(f)
    val theta  = theta_face(f)
    val lambda = lambda_face(f)
    val ct = cos(theta)
    val st = sin(theta)

    val atheta = omega*0.5*(2.0+omega)*ct*ct+0.25*K*K*pow(ct,(2*R))*(ct*ct*(R+1)+(2*R*R-R-2)-(2*R*R)/(ct*ct+1.e-12))
      
    val btheta = 2*(1.0+omega)*K/(R+1)/(R+2)*pow(ct,(R))*((R*R+2*R+2)-ct*ct*(R+1)*(R+1))
    val ctheta = 0.25*K*K*pow(ct,(2*R))*(ct*ct*(R+1)-R-2)

    val phi = 0.3648 + atheta+btheta*cos(lambda*R)+ctheta*cos(lambda*2*R) // TODO

    Phi(f) = phi
  }

  def SetVertexCoords(v : Rep[Vertex]) : Rep[Unit] = {
    var coords = position(v) 
    var rad = sqrt(dot(coords,coords))
    var theta = acos(coords.z/rad)
    theta=(pi*0.5f).asInstanceOfL[Double]-theta
  
    var lambda = atan2(coords.y,coords.x)

    theta_vertex(v)  = theta
    lambda_vertex(v) = lambda
  }

  def SetFaceCoords(f : Rep[Face]) : Rep[Unit] = {
    var coords = face_centroid(f) 
    var rad = sqrt(dot(coords,coords))
    var theta = acos(coords.z/rad)
    theta=pi*0.5f-theta
    var lambda = atan2(coords.y,coords.x)

    theta_face(f)  = theta
    lambda_face(f) = lambda
  }

  def SetEdgeCoords(e : Rep[Edge]) : Rep[Unit] = {
    var coords = edge_center(e) 
    var rad = sqrt(dot(coords,coords))
    var theta = acos(coords.z/rad)
    theta=pi*0.5f-theta
  
    var lambda = atan2(coords.y,coords.x)

    theta_edge(e)  = theta
    lambda_edge(e) = lambda
  }

  def CalcFaceGrad_Phi(f : Rep[Face]) : Rep[Unit] = {
  	var coords  = face_centroid(f) 
    var rad     = sqrt(dot(coords,coords))
    var rad2    = rad*rad
    var rad3    = rad2*rad
    var thetac  = cos(-theta_face(f))
    var thetas  = sin(-theta_face(f))
    var lambdac = cos(-lambda_face(f))
    var lambdas = sin(-lambda_face(f))

    var coords1_x = coords.x*lambdac-coords.y*lambdas
    var coords1_y = coords.x*lambdas+coords.y*lambdac
    var coords1_z = coords.z
      
    var coords2_y = coords1_y
    var coords2_x = coords1_x*thetac-coords1_z*thetas
    var coords2_z = coords1_x*thetas+coords1_z*thetac

    var theta = acos(coords2_z/sqrt(coords2_x*coords2_x+coords2_y*coords2_y+coords2_z*coords2_z))
    theta=pi*0.5f-theta
    var lambda = atan2(coords2_y,coords2_x)
    var Phif = Phi(f)

    var af = 0.0
    var bf = 0.0
    var cf = 0.0
    var df = 0.0
    var ef = 0.0


    for (e <- edges(f)) {
      for (f_c <- faces(e)) {
        if (ID(f_c) != ID(f)) {
          var coordsc = face_centroid(f_c) 
          var coords1c_x = coordsc.x*lambdac-coordsc.y*lambdas
          var coords1c_y = coordsc.x*lambdas+coordsc.y*lambdac
          var coords1c_z = coordsc.z
              
          var coords2c_y = coords1c_y
          var coords2c_x = coords1c_x*thetac-coords1c_z*thetas
          var coords2c_z = coords1c_x*thetas+coords1c_z*thetac
            
              var theta_c  = acos(coords2c_z/sqrt(coords2c_x*coords2c_x+coords2c_y*coords2c_y+coords2c_z*coords2c_z))
          theta_c=(pi*0.5f).asInstanceOfL[Double]-theta_c
              var lambda_c = atan2(coords2c_y,coords2c_x)

          var dlambda = (lambda_c-lambda)
          var dtheta  = (theta_c-theta)
          var dPhi    = Phi(f_c)-Phif
          var weight2 = 1.0/(dlambda*dlambda+dtheta*dtheta)
          af += weight2*dlambda*dlambda
          bf += weight2*dlambda*dtheta
          cf += weight2*dtheta*dtheta
          df += weight2*dPhi*dlambda
          ef += weight2*dPhi*dtheta
        }
      }
    }
    val dPhi_dlam = (df*cf-ef*bf)/(af*cf-bf*bf)
    val dPhi_dthe = (ef*af-df*bf)/(af*cf-bf*bf)
      
    var arg = 1.0/sqrt(1.0-coords.z*coords.z/rad2)
    var dthe_dx = -coords.z*coords.x*arg
    var dthe_dy = -coords.z*coords.y*arg
    var dthe_dz = (-coords.z*coords.z/rad3+1.0/rad)*arg
    var dlam_dx = -coords.y/(sqrt(coords.x*coords.x+coords.y*coords.y))
    var dlam_dy =  coords.x/(sqrt(coords.x*coords.x+coords.y*coords.y))
        
    val dPhi_dx = dPhi_dlam*dlam_dx+dPhi_dthe*dthe_dx
    val dPhi_dy = dPhi_dlam*dlam_dy+dPhi_dthe*dthe_dy
    val dPhi_dz = dPhi_dthe*dthe_dz

    Phi_Grad(f) = Vec(dPhi_dx,dPhi_dy,dPhi_dz)
  }

  def CalcEdgeGrad(e : Rep[Edge]) : Rep[Unit] = {
    var coords  = edge_center(e)
    var rad     = sqrt(dot(coords,coords))
    var rad2    = rad*rad
    var rad3    = rad2*rad
    var thetac  = cos(-theta_edge(e))
    var thetas  = sin(-theta_edge(e))
    var lambdac = cos(-lambda_edge(e))
    var lambdas = sin(-lambda_edge(e))

    var coords1_x = coords.x*lambdac-coords.y*lambdas
    var coords1_y = coords.x*lambdas+coords.y*lambdac
    var coords1_z = coords.z
      
    var coords2_y = coords1_y
    var coords2_x = coords1_x*thetac-coords1_z*thetas
    var coords2_z = coords1_x*thetas+coords1_z*thetac

    var theta = acos(coords2_z/sqrt(coords2_x*coords2_x+coords2_y*coords2_y+coords2_z*coords2_z))
    theta=(pi*0.5f).asInstanceOfL[Double]-theta
    var lambda = atan2(coords2_y,coords2_x)
    var Vele = Veloc(e)

    var af = 0.0
    var bf = 0.0
    var cf = 0.0
    var df0 = 0.0
    var df1 = 0.0
    var df2 = 0.0
    var ef0 = 0.0
    var ef1 = 0.0
    var ef2 = 0.0
    //var df = Vec(0.0,0.0,0.0)
    //var ef = Vec(0.0,0.0,0.0)

    for (f <- faces(e)) {
      for (e_c <- edges(f)) {
        if (ID(e_c) != ID(e)) {
          var coordsc = edge_center(e_c) 
          var coords1c_x = coordsc.x*lambdac-coordsc.y*lambdas
          var coords1c_y = coordsc.x*lambdas+coordsc.y*lambdac
          var coords1c_z = coordsc.z
              
          var coords2c_y = coords1c_y
          var coords2c_x = coords1c_x*thetac-coords1c_z*thetas
          var coords2c_z = coords1c_x*thetas+coords1c_z*thetac
            
              var theta_c  = acos(coords2c_z/sqrt(coords2c_x*coords2c_x+coords2c_y*coords2c_y+coords2c_z*coords2c_z))
          theta_c = (pi*0.5f).asInstanceOfL[Double]-theta_c
              var lambda_c = atan2(coords2c_y,coords2c_x)

          var dVel  = Veloc(e_c)-Vele
          var dlambda = lambda_c-lambda
          var dtheta  = theta_c-theta
          var weight2 = 1.0/(dlambda*dlambda+dtheta*dtheta)
          var dlambdaw2 = dlambda*weight2
          var dthetaw2  = dtheta*weight2

          af += dlambdaw2*dlambda
          bf += dlambdaw2*dtheta
          cf += dthetaw2*dtheta
          df0 += dVel(0)*dlambdaw2
          df1 += dVel(1)*dlambdaw2
          df2 += dVel(2)*dlambdaw2
          ef0 += dVel(0)*dthetaw2
          ef1 += dVel(1)*dthetaw2
          ef2 += dVel(2)*dthetaw2

        }
      }
    }
    val df = Vec(df0,df1,df2)
    val ef = Vec(ef0,ef1,ef2)
        
    val dVel_dlam = (df*cf-ef*bf)/(af*cf-bf*bf)
    val dVel_dthe = (ef*af-df*bf)/(af*cf-bf*bf)
      
    var arg = 1.0/sqrt(1.0-coords.z*coords.z/rad2)
    var dthe_dx =  -coords.z*coords.x*arg
    var dthe_dy =  -coords.z*coords.y*arg
    var dthe_dz = (-coords.z*coords.z/rad3+1.0/rad)*arg
    var dlam_dx = -coords.y/(sqrt(coords.x*coords.x+coords.y*coords.y))
    var dlam_dy =  coords.x/(sqrt(coords.x*coords.x+coords.y*coords.y))

    val dVel_dx = dVel_dlam*dlam_dx+dVel_dthe*dthe_dx
    val dVel_dy = dVel_dlam*dlam_dy+dVel_dthe*dthe_dy
    val dVel_dz = dVel_dthe*dthe_dz
        
    val C  = Mat(dVel_dx,dVel_dy,dVel_dz)
    val P  = PMat(e)
        
    val CP = C*P
        
    Velochat_Grad(e) = CP
             
    var grad = float3_zero
        
    for (f <- faces(e)) 
      grad = grad + Phi_Grad(f)
        
    val grad1 = P*grad
    Phihat_Grad(e)=grad1*0.5f
  }

  def calcGradient() {
   for( f <- faces(mesh) ) 
    Phi_Grad(f) = float3_zero 


  for (e <- edges(mesh)) {
    val f0   = left(e) 
    val f1   = right(e) 
    val Phi0 = Phi(f0) 
    val Phi1 = Phi(f1) 
    
    var nVec = edge_normal(e) 
    val Phi01 = nVec*0.5f*(Phi0 + Phi1)

    Phi_Grad(f0) = Phi_Grad(f0) + Phi01
    Phi_Grad(f1) = Phi_Grad(f1) - Phi01
  }

  for( f <- faces(mesh) ) 
    Phi_Grad(f) /= face_area(f)
  }

  def main() {
    val iterations = 100
    float3_zero  = Vec(0.0,0.0,0.0)
    float33_zero = Mat(float3_zero,float3_zero,float3_zero)
    work_array = float33_zero
    pi = MATH_PI.asInstanceOfL[Double]
    Omega = unit(1.0)
    alpha = 0.0*pi

    Phi        = FieldWithConst[Face,Double](0.0)
    PhiOld     = FieldWithConst[Face,Double](0.0)
    Phi_Grad   = FieldWithConst[Face,Vec[_3,Double]](float3_zero)
    Flux  	 = FieldWithConst[Face,Double](0.0)
    Veloc 	 = FieldWithConst[Edge,Vec[_3,Double]](float3_zero)
    VelocOld	 = FieldWithConst[Edge,Vec[_3,Double]](float3_zero)
    Rhs_Veloc	 = FieldWithConst[Edge,Vec[_3,Double]](float3_zero)
    Velochat_Grad = FieldWithConst[Edge,Mat[_3,_3,Double]](float33_zero)
    Phihat_Grad= FieldWithConst[Edge,Vec[_3,Double]](float3_zero)
    PMat       = FieldWithConst[Edge,Mat[_3,_3,Double]](float33_zero)

    //some fields for output
    Phiv  	 = FieldWithConst[Vertex,Double](0.0)
    Phiv_Grad  = FieldWithConst[Vertex,Vec[_3,Double]](float3_zero)
    Velocv     = FieldWithConst[Vertex,Vec[_3,Double]](float3_zero)

    //some geometry fields
    position = FieldWithLabel[Vertex,Vec[_3,Double]]("position")

    theta_vertex  = FieldWithConst[Vertex,Double](0.0)
    lambda_vertex = FieldWithConst[Vertex,Double](0.0)

    theta_face  = FieldWithConst[Face,Double](0.0)
    lambda_face = FieldWithConst[Face,Double](0.0)

    theta_edge  = FieldWithConst[Edge,Double](0.0)
    lambda_edge = FieldWithConst[Edge,Double](0.0)

    face_centroid = FieldWithConst[Face,Vec[_3,Double]](float3_zero)
    face_area = FieldWithConst[Face,Double](0.0)
    face_normal = FieldWithConst[Face,Vec[_3,Double]](float3_zero)

    edge_center  = FieldWithConst[Edge,Vec[_3,Double]](float3_zero)
    edge_length  = FieldWithConst[Edge,Double](0.0)
    edge_normal  = FieldWithConst[Edge,Vec[_3,Double]](float3_zero)
  
    for(f <- faces(mesh)) 
      calcFaceGeom(f)
    
    /* for(f <- faces(mesh)) {
      Print(ID(f)," FaceArea: ",face_area(f)," normal: ",face_normal(f),"face_centroid",face_centroid(f))
    } */

		for(e <- edges(mesh)) 
			calcEdgeGeom(e)

    /* for(e <- edges(mesh)) {
      Print(ID(e)," PMat: ",PMat(e))
    } */
      
		for(f <- faces(mesh)) 
			SetFaceCoords(f)
    
    /* for(f <- faces(mesh)) {
      Print(ID(f)," theta_face: ",theta_face(f)," lamba_face: ", lambda_face(f))
    } */
   
		for(e <- edges(mesh)) 
			SetEdgeCoords(e)
	  
    /* for(e <- edges(mesh)) {
      Print(ID(e)," theta_edge: ",theta_edge(e)," lamba_edge: ", lambda_edge(e))
    } */
    
		for(e <- edges(mesh))  
			SetEdgeVel(e)
    
    /* for(e <- edges(mesh)) {
      Print(ID(e)," Vel: ",Veloc(e))
    } */
               
    
// ========================== Core ===============================

    for(f <- faces(mesh)) 
      SetIC(f)
      
    /* for(f <- faces(mesh)) {
      Print(ID(f)," Phi: ",Phi(f))
    } */

    var t = 0.0
    var start_time = 0.0;
    var step = 0;
    while(step < iterations) {
      if (step == 1) {
        start_time = wall_time();
      }
      step += 1;
// --------------- time step --------------
      var dt_min  = 100000.0
      
      // Print("STEP ", step)

      // Print("dt_min loop")
     
      for(f <- faces(mesh)) {
        val centroid = face_centroid(f)
        //Print(ID(f), " EDGES")
        for(e <- edges(f)) {
          //Print(ID(e))
          val center= edge_center(e)
          val Vel = Veloc(e)
          val dist = sqrt(dot(center-centroid,center-centroid))
          val Velmag = sqrt(dot(Vel,Vel))
          dt_min = dt_min min (dist/Velmag*0.025f)
        }
      }

      var deltat = dt_min
// --------------- time step --------------

// --------------- Store  --------------

      for(f <- faces(mesh)) {
         val old = Phi(f)
         PhiOld(f) = old
      } 
      
      for(e <- edges(mesh)) {
         val old = Veloc(e)
         VelocOld(e) = old
      } 
// --------------- Store  --------------

      var iter = 0.0
      
      while (iter <= 4) {
        val factor = deltat/(5.0-iter.asInstanceOfL[Double])

    //calcGradient()
   
        for(f <- faces(mesh)) 
          CalcFaceGrad_Phi(f)
   
        for(e <- edges(mesh)) 
          CalcEdgeGrad(e)

    //---------------- Update Phi ---------------------
        // Print("UPDATE FLUX")
    
        for(e <- edges(mesh)) {

          val normal = edge_normal(e)
          val center = edge_center(e)
          val vDotN = dot(Veloc(e),normal)
          
          var flux = 0.0
          val left_face  = left(e)
          val right_face = right(e)
          
          val left_centroid  = face_centroid(left_face)
          val right_centroid = face_centroid(right_face)

          var r0 = center-left_centroid
          var r1 = center-right_centroid

          var Phil = Phi(left_face)+dot(Phi_Grad(left_face),r0)
          var Phir = Phi(right_face)+dot(Phi_Grad(right_face),r1)

          if(vDotN>=0.0) 
            flux = vDotN*Phil
          else
            flux = vDotN*Phir

          Flux(left_face)  = Flux(left_face) - flux
          Flux(right_face) = Flux(right_face) + flux
                             
        }	
        
        for(f <- faces(mesh)) 
          Phi(f) = PhiOld(f) + factor/face_area(f)*Flux(f) 

        // Print("RESET FLUX")
          
        for(f <- faces(mesh)) 
          Flux(f) = 0.0

    //---------------- Update Velocities ---------------------
    
        // Print("UPDATE RHS")
    
        for(e <- edges(mesh)) {

          val VGrad   = Velochat_Grad(e)
          val PhiGrad = Phihat_Grad(e) 
          val V       = Veloc(e) 
          val coords  = edge_center(e)
          val a2inv   = 1.0/dot(coords,coords)
          val fa2inv  = a2inv*2.0*Omega*coords.z
          val p = -1.0*(V.x*VGrad(0,0) + V.y*VGrad(0,1) + V.z*VGrad(0,2) + fa2inv*(coords.y*V.z-coords.z*V.y)+PhiGrad.x)
          val q = -1.0*(V.x*VGrad(1,0) + V.y*VGrad(1,1) + V.z*VGrad(1,2) + fa2inv*(coords.z*V.x-coords.x*V.z)+PhiGrad.y)
          val s = -1.0*(V.x*VGrad(2,0) + V.y*VGrad(2,1) + V.z*VGrad(2,2) + fa2inv*(coords.x*V.y-coords.y*V.x)+PhiGrad.z)
          val mu = (p*coords.x+q*coords.y+s*coords.z)*a2inv
    
          val rhs=Vec(p-mu*coords.x,q-mu*coords.y,s-mu*coords.z)
          Rhs_Veloc(e) = rhs
        }
        
        // Print("UPDATE VELOC")

        for(e <- edges(mesh)) 
          Veloc(e) = VelocOld(e) + Rhs_Veloc(e)*factor
        iter+=1
      }
      t += deltat
      Print("Simulation time = ",t," delta t = ",deltat)
    }	
    Print( "TIME_FOR_LOOP: ", wall_time() - start_time ) ;
// ========================== Core ===============================
         
// ========================== Output ===============================
    var Totalarea = 0.0
    var diff = 0.0
    for(f <- faces(mesh)) {
      Totalarea = Totalarea + face_area(f)
      diff = diff + abs(Phi(f)-PhiOld(f))*face_area(f)
    }
    //Print("Total Area = ",Totalarea)
    Print("|d Phi| = ",diff)
    for(f <- faces(mesh)) 
      SetFaceCoords(f)
    
    for(v <- vertices(mesh)) 
      SetVertexCoords(v)
    
    for(v <- vertices(mesh)) 
      InterpolateVertex(v)
    
    for(v <- vertices(mesh)) {
      var coords = position(v) 
      Print(coords.x," ",coords.y," ",coords.z," ",Phiv(v)," ",theta_vertex(v)," ",lambda_vertex(v))
      Print(coords.x," ",coords.y," ",coords.z," ",Phiv(v)," ",Velocv(v).x," ",Velocv(v).y," ",Velocv(v).z," ",theta_vertex(v)," ",lambda_vertex(v))
    }
  }
}
