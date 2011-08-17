package ppl.apps.liszt.KarthikSmooth

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object MeshSmoothRunner extends DeLisztApplicationRunner with Smooth

trait Smooth extends DeLisztApplication {
  val float3_zero  = Vec(0.f,0.f,0.f)
  lazy val float33_zero = Mat(float3_zero,float3_zero,float3_zero)
  lazy val work_array = float33_zero
  lazy val pi = MATH_PI.asInstanceOfL[Float]
  lazy val Omega = 1.f
  lazy val alpha = 0.f*pi

  lazy val Phi        = FieldWithConst[Face,Float](0.f)
  lazy val PhiOld     = FieldWithConst[Face,Float](0.f)
  lazy val Phi_Grad   = FieldWithConst[Face,Vec[_3,Float]](float3_zero)
  lazy val Flux  	 = FieldWithConst[Face,Float](0.f)
  lazy val Veloc 	 = FieldWithConst[Edge,Vec[_3,Float]](float3_zero)
  lazy val VelocOld	 = FieldWithConst[Edge,Vec[_3,Float]](float3_zero)
  lazy val Rhs_Veloc	 = FieldWithConst[Edge,Vec[_3,Float]](float3_zero)
  lazy val Velochat_Grad = FieldWithConst[Edge,Mat[_3,_3,Float]](float33_zero)
  lazy val Phihat_Grad= FieldWithConst[Edge,Vec[_3,Float]](float3_zero)
  lazy val PMat       = FieldWithConst[Edge,Mat[_3,_3,Float]](float33_zero)

    //some fields for output
  lazy val Phiv  	 = FieldWithConst[Vertex,Float](0.f)
  lazy val Phiv_Grad  = FieldWithConst[Vertex,Vec[_3,Float]](float3_zero)
  lazy val Velocv     = FieldWithConst[Vertex,Vec[_3,Float]](float3_zero)

    //some geometry fields
  lazy val position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")

  lazy val theta_vertex  = FieldWithConst[Vertex,Float](0.f)
  lazy val lambda_vertex = FieldWithConst[Vertex,Float](0.f)

  lazy val theta_face  = FieldWithConst[Face,Float](0.f)
  lazy val lambda_face = FieldWithConst[Face,Float](0.f)

  lazy val theta_edge  = FieldWithConst[Edge,Float](0.f)
  lazy val lambda_edge = FieldWithConst[Edge,Float](0.f)

  lazy val face_centroid = FieldWithConst[Face,Vec[_3,Float]](float3_zero)
  lazy val face_area = FieldWithConst[Face,Float](0.f)
  lazy val face_normal = FieldWithConst[Face,Vec[_3,Float]](float3_zero)

  lazy val edge_center  = FieldWithConst[Edge,Vec[_3,Float]](float3_zero)
  lazy val edge_length  = FieldWithConst[Edge,Float](0.f)
  lazy val edge_normal  = FieldWithConst[Edge,Vec[_3,Float]](float3_zero)

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

  def calcFaceCenter(f : Rep[Face]) : Rep[Vec[_3,Float]] = {
    var center = Vec(0.f,0.f,0.f)
    val test = Vec(0.f,0.f,0.f,0.f)
    for(v <- vertices(f)) {
      center += position(v)
    }
    center = center / size(vertices(f))
    return center
  }

  def InterpolateVertex(v : Rep[Vertex]) : Rep[Unit] = {
    var phiv = 0.f
    var phiv_grad = float3_zero

    for(f <- faces(v)) {
      phiv += Phi(f)
      phiv_grad += Phi_Grad(f)
    }
    // TODO
    phiv = phiv / size(faces(v))
    
    // TODO
    phiv_grad = phiv_grad / size(faces(v))

    Phiv(v)=phiv
    Phiv_Grad(v)=phiv_grad

    var velv = float3_zero
    for(e <- edges(v)) {
      velv += Veloc(e)
    }
    
    // TODO
    velv = velv / size(edges(v))
    Velocv(v) = velv
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
  }

  def calcEdgeGeom(e : Rep[Edge]) : Rep[Unit] = {
    var e1 = position(head(e)) 
    var e2 = position(tail(e))
    var e3 = e2-e1
    var center = (e1+e2)*0.5f
    var normal = cross(center,e3)
    var length = sqrtf(dot(e3,e3))
    normal = normalize(normal)*length

    edge_center(e) = center
    edge_length(e) = length
    edge_normal(e) = normal
  
    var rad2i = 1.f/dot(center,center)

    work_array(0,0)= 1.f - center.x*center.x*rad2i
    work_array(0,1)=     - center.x*center.y*rad2i
    work_array(0,2)=     - center.x*center.z*rad2i
    work_array(1,0)=      work_array(0,1) 
    work_array(1,1)= 1.f - center.y*center.y*rad2i
    work_array(1,2)=     - center.y*center.z*rad2i
    work_array(2,0)=      work_array(0,2) 
    work_array(2,1)=      work_array(1,2)
    work_array(2,2)= 1.f - center.z*center.z*rad2i
    PMat(e) = work_array
  }

  def SetEdgeVel(e : Rep[Edge]) : Rep[Unit] = {
    val omega = 0.1079f
    val K = 0.1079f
    val R = 4
    var center = edge_center(e)
    var rad = sqrtf(dot(center,center))
    var theta = acosf(center.z/rad)
    theta=(pi*0.5f).asInstanceOfL[Float]-theta
  
    var lambda = atan2f(center.y,center.x)
  
    val ct = cosf(theta)
    val st = sinf(theta)
    val cr = cosf(lambda*R)
    val sr = sinf(lambda*R)

    var uhat =  omega*ct+K*powf(ct,(R-1).floatValue)*(st*st*R-ct*ct)*cr
    var vhat = -K*R*powf(ct,(R-1).floatValue)*st*sr
    
    var ux = -uhat*sinf(lambda)-vhat*sinf(theta)*cosf(lambda)
    var uy =  uhat*cosf(lambda)-vhat*sinf(theta)*sinf(lambda)
    var uz =  vhat*cosf(theta)

    val P  = PMat(e)
    val vel = Vec(ux,uy,uz)
    val vel1 = P*vel

    Veloc(e)=vel1
  }

  def SetIC(f : Rep[Face]) : Rep[Unit] = {
    val omega = 0.1079f
    val K = 0.1079f
    val R = 4
    
    val center = face_centroid(f)
    val theta  = theta_face(f)
    val lambda = lambda_face(f)
    val ct = cosf(theta)
    val st = sinf(theta)

    val atheta = omega*0.5f*(2.f+omega)*ct*ct+0.25f*K*K*powf(ct,(2*R).floatValue)*(ct*ct*(R+1)+(2*R*R-R-2)-(2*R*R).floatValue/(ct*ct+1.e-12f))
      
    val btheta = 2*(1.f+omega)*K/(R+1)/(R+2)*powf(ct,(R).floatValue)*((R*R+2*R+2).floatValue-ct*ct*(R+1)*(R+1))
    val ctheta = 0.25f*K*K*powf(ct,(2*R).floatValue)*(ct*ct*(R+1)-R-2)

    val phi = 0.3648f + atheta+btheta*cosf(lambda*R)+ctheta*cosf(lambda*2*R) // TODO

    Phi(f) = phi
  }

  def SetVertexCoords(v : Rep[Vertex]) : Rep[Unit] = {
    var coords = position(v) 
    var rad = sqrtf(dot(coords,coords))
    var theta = acosf(coords.z/rad)
    theta=(pi*0.5f).asInstanceOfL[Float]-theta
  
    var lambda = atan2f(coords.y,coords.x)

    theta_vertex(v)  = theta
    lambda_vertex(v) = lambda
  }

  def SetFaceCoords(f : Rep[Face]) : Rep[Unit] = {
    var coords = face_centroid(f) 
    var rad = sqrtf(dot(coords,coords))
    var theta = acosf(coords.z/rad)
    theta=pi*0.5f-theta
    var lambda = atan2f(coords.y,coords.x)

    theta_face(f)  = theta
    lambda_face(f) = lambda
  }

  def SetEdgeCoords(e : Rep[Edge]) : Rep[Unit] = {
    var coords = edge_center(e) 
    var rad = sqrtf(dot(coords,coords))
    var theta = acosf(coords.z/rad)
    theta=pi*0.5f-theta
  
    var lambda = atan2f(coords.y,coords.x)

    theta_edge(e)  = theta
    lambda_edge(e) = lambda
  }

  def CalcFaceGrad_Phi(f : Rep[Face]) : Rep[Unit] = {
  	var coords  = face_centroid(f) 
    var rad     = sqrtf(dot(coords,coords))
    var rad2    = rad*rad
    var rad3    = rad2*rad
    var thetac  = cosf(-theta_face(f))
    var thetas  = sinf(-theta_face(f))
    var lambdac = cosf(-lambda_face(f))
    var lambdas = sinf(-lambda_face(f))

    var coords1_x = coords.x*lambdac-coords.y*lambdas
    var coords1_y = coords.x*lambdas+coords.y*lambdac
    var coords1_z = coords.z
      
    var coords2_y = coords1_y
    var coords2_x = coords1_x*thetac-coords1_z*thetas
    var coords2_z = coords1_x*thetas+coords1_z*thetac

    var theta = acosf(coords2_z/sqrtf(coords2_x*coords2_x+coords2_y*coords2_y+coords2_z*coords2_z))
    theta=pi*0.5f-theta
    var lambda = atan2f(coords2_y,coords2_x)
    var Phif = Phi(f)

    var af = 0.f
    var bf = 0.f
    var cf = 0.f
    var df = 0.f
    var ef = 0.f


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
            
              var theta_c  = acosf(coords2c_z/sqrtf(coords2c_x*coords2c_x+coords2c_y*coords2c_y+coords2c_z*coords2c_z))
          theta_c=(pi*0.5f).asInstanceOfL[Float]-theta_c
              var lambda_c = atan2f(coords2c_y,coords2c_x)

          var dlambda = (lambda_c-lambda)
          var dtheta  = (theta_c-theta)
          var dPhi    = Phi(f_c)-Phif
          var weight2 = 1.f/(dlambda*dlambda+dtheta*dtheta)
          af += weight2*dlambda*dlambda
          bf += weight2*dlambda*dtheta
          cf += weight2*dtheta*dtheta
          df += weight2*dPhi*dlambda
          ef += weight2*dPhi*dtheta
        }
      }
    }
    var dPhi_dlam = (df*cf-ef*bf)/(af*cf-bf*bf)
    var dPhi_dthe = (ef*af-df*bf)/(af*cf-bf*bf)
      
    var arg = 1.f/sqrtf(1.f-coords.z*coords.z/rad2)
    var dthe_dx = -coords.z*coords.x*arg
    var dthe_dy = -coords.z*coords.y*arg
    var dthe_dz = (-coords.z*coords.z/rad3+1.f/rad)*arg
    var dlam_dx = -coords.y/(sqrtf(coords.x*coords.x+coords.y*coords.y))
    var dlam_dy =  coords.x/(sqrtf(coords.x*coords.x+coords.y*coords.y))
        
    var dPhi_dx = dPhi_dlam*dlam_dx+dPhi_dthe*dthe_dx
    var dPhi_dy = dPhi_dlam*dlam_dy+dPhi_dthe*dthe_dy
    var dPhi_dz = dPhi_dthe*dthe_dz

    Phi_Grad(f) = Vec(dPhi_dx,dPhi_dy,dPhi_dz)
  }

  def CalcEdgeGrad(e : Rep[Edge]) : Rep[Unit] = {
    var coords  = edge_center(e)
    var rad     = sqrtf(dot(coords,coords))
    var rad2    = rad*rad
    var rad3    = rad2*rad
    var thetac  = cosf(-theta_edge(e))
    var thetas  = sinf(-theta_edge(e))
    var lambdac = cosf(-lambda_edge(e))
    var lambdas = sinf(-lambda_edge(e))

    var coords1_x = coords.x*lambdac-coords.y*lambdas
    var coords1_y = coords.x*lambdas+coords.y*lambdac
    var coords1_z = coords.z
      
    var coords2_y = coords1_y
    var coords2_x = coords1_x*thetac-coords1_z*thetas
    var coords2_z = coords1_x*thetas+coords1_z*thetac

    var theta = acosf(coords2_z/sqrtf(coords2_x*coords2_x+coords2_y*coords2_y+coords2_z*coords2_z))
    theta=(pi*0.5f).asInstanceOfL[Float]-theta
    var lambda = atan2f(coords2_y,coords2_x)
    var Vele = Veloc(e)

    var af = 0.f
    var bf = 0.f
    var cf = 0.f
    var df0 = 0.f
    var df1 = 0.f
    var df2 = 0.f
    var ef0 = 0.f
    var ef1 = 0.f
    var ef2 = 0.f
    var df = Vec(0.f,0.f,0.f)
    var ef = Vec(0.f,0.f,0.f)

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
            
              var theta_c  = acosf(coords2c_z/sqrtf(coords2c_x*coords2c_x+coords2c_y*coords2c_y+coords2c_z*coords2c_z))
          theta_c = (pi*0.5f).asInstanceOfL[Float]-theta_c
              var lambda_c = atan2f(coords2c_y,coords2c_x)

          var dVel  = Veloc(e_c)-Vele
          var dlambda = lambda_c-lambda
          var dtheta  = theta_c-theta
          var weight2 = 1.f/(dlambda*dlambda+dtheta*dtheta)
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
    df = Vec(df0,df1,df2)
    ef = Vec(ef0,ef1,ef2)
        
    var dVel_dlam = (df*cf-ef*bf)/(af*cf-bf*bf)
    var dVel_dthe = (ef*af-df*bf)/(af*cf-bf*bf)
      
    var arg = 1.f/sqrtf(1.f-coords.z*coords.z/rad2)
    var dthe_dx =  -coords.z*coords.x*arg
    var dthe_dy =  -coords.z*coords.y*arg
    var dthe_dz = (-coords.z*coords.z/rad3+1.f/rad)*arg
    var dlam_dx = -coords.y/(sqrtf(coords.x*coords.x+coords.y*coords.y))
    var dlam_dy =  coords.x/(sqrtf(coords.x*coords.x+coords.y*coords.y))

    var dVel_dx = dVel_dlam*dlam_dx+dVel_dthe*dthe_dx
    var dVel_dy = dVel_dlam*dlam_dy+dVel_dthe*dthe_dy
    var dVel_dz = dVel_dthe*dthe_dz
        
    val C  = Mat(dVel_dx,dVel_dy,dVel_dz)
    val P  = PMat(e)
        
    val CP = C*P
        
    Velochat_Grad(e) = CP
             
    var grad = float3_zero
        
    for (f <- faces(e)) 
      grad += Phi_Grad(f)
        
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

    Phi_Grad(f0) += Phi01
    Phi_Grad(f1) -= Phi01
  }

  for( f <- faces(mesh) ) 
    Phi_Grad(f) /= face_area(f)
  }

  def main() {
    val iterations = 100
  
    for(f <- faces(mesh)) 
      calcFaceGeom(f)

    for(e <- edges(mesh)) 
      calcEdgeGeom(e)

    for(f <- faces(mesh)) 
      SetFaceCoords(f)
   
    for(e <- edges(mesh)) 
      SetEdgeCoords(e)
         
    for(e <- edges(mesh))  
      SetEdgeVel(e)
               
    
// ========================== Core ===============================

    for(f <- faces(mesh)) 
      SetIC(f)

    var t = 0.f
    var start_time = 0.0;
    var step = 0;
    while(step < iterations) {
      if (step == 1) {
        start_time = wall_time();
      }
      step += 1;
// --------------- time step --------------
      var dt_min  = 100000.f

      for(f <- faces(mesh)) {
        val centroid = face_centroid(f)
        for(e <- edges(f)) {
          val center= edge_center(e)
          val Vel = Veloc(e)
          val dist = sqrtf(dot(center-centroid,center-centroid))
          val Velmag = sqrtf(dot(Vel,Vel))
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

      var iter = 0

      while (iter <= 4) {
        var factor = deltat/(5.f-iter.asInstanceOfL[Float])

    //calcGradient()
   
        for(f <- faces(mesh)) 
          CalcFaceGrad_Phi(f)
   
        for(e <- edges(mesh)) 
          CalcEdgeGrad(e)

    //---------------- Update Phi ---------------------
        for(e <- edges(mesh)) {

          val normal = edge_normal(e)
          val center = edge_center(e)
          val vDotN = dot(Veloc(e),normal)

          var flux = 0.f
          val left_face  = left(e)
          val right_face = right(e)
          val left_centroid  = face_centroid(left_face)
          val right_centroid = face_centroid(right_face)

          var r0 = center-left_centroid
          var r1 = center-right_centroid

          var Phil = Phi(left_face)+dot(Phi_Grad(left_face),r0)
          var Phir = Phi(right_face)+dot(Phi_Grad(right_face),r1)

          if(vDotN>=0.f) 
            flux = vDotN*Phil
          else
            flux = vDotN*Phir

          Flux(left_face)  -= flux
          Flux(right_face) += flux
                             
        }	

        for(f <- faces(mesh)) 
          Phi(f) = PhiOld(f) + factor/face_area(f)*Flux(f) 

        for(f <- faces(mesh)) 
          Flux(f) = 0.f

    //---------------- Update Velocities ---------------------
        for(e <- edges(mesh)) {

          val VGrad   = Velochat_Grad(e)
          val PhiGrad = Phihat_Grad(e) 
          val V       = Veloc(e) 
          val coords  = edge_center(e)
          val a2inv   = 1.f/dot(coords,coords)
          val fa2inv  = a2inv*2.f*Omega*coords.z
          val p = -1.f*(V.x*VGrad(0,0) + V.y*VGrad(0,1) + V.z*VGrad(0,2) + fa2inv*(coords.y*V.z-coords.z*V.y)+PhiGrad.x)
          val q = -1.f*(V.x*VGrad(1,0) + V.y*VGrad(1,1) + V.z*VGrad(1,2) + fa2inv*(coords.z*V.x-coords.x*V.z)+PhiGrad.y)
          val s = -1.f*(V.x*VGrad(2,0) + V.y*VGrad(2,1) + V.z*VGrad(2,2) + fa2inv*(coords.x*V.y-coords.y*V.x)+PhiGrad.z)
          val mu = (p*coords.x+q*coords.y+s*coords.z)*a2inv
    
          val rhs=Vec(p-mu*coords.x,q-mu*coords.y,s-mu*coords.z)
          Rhs_Veloc(e) = rhs
        }

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
    var Totalarea = 0.f
    var diff = 0.f
    for(f <- faces(mesh)) {
      Totalarea = Totalarea + face_area(f)
      diff = diff + fabs(Phi(f)-PhiOld(f))*face_area(f)
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
      //Print(coords.x," ",coords.y," ",coords.z," ",Phiv(v)," ",theta_vertex(v)," ",lambda_vertex(v))
      //Print(coords.x," ",coords.y," ",coords.z," ",Phiv(v)," ",Velocv(v).x," ",Velocv(v).y," ",Velocv(v).z," ",theta_vertex(v)," ",lambda_vertex(v))
    }
  }
}
