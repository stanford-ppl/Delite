package ppl.apps.liszt.KarthikSmooth

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object MeshSmoothRunner extends DeLisztApplicationRunner with Smooth

trait Smooth extends DeLisztApplication {
  /*  
  var double3_zero  : Rep[Vec[_3,Double]] = null
  var double33_zero : Rep[Mat[_3,_3,Double]] = null
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
  */
  
  def main() {
   val double3_zero  = Vec(0.0,0.0,0.0)
   val double33_zero = Mat(double3_zero,double3_zero,double3_zero)
   val work_array = double33_zero
   val pi = MATH_PI
   val Omega = unit(1.0)
   val alpha = 0.0*pi
   val Phi        = FieldWithConst[Face,Double](0.0)
   val PhiOld     = FieldWithConst[Face,Double](0.0)
   val Phi_Grad   = FieldWithConst[Face,Vec[_3,Double]](double3_zero)
   val Flux  	 = FieldWithConst[Face,Double](0.0)
   val Veloc 	 = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
   val VelocOld	 = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
   val Rhs_Veloc	 = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
   val Velochat_Grad = FieldWithConst[Edge,Mat[_3,_3,Double]](double33_zero)
   val Phihat_Grad= FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
   val PMat       = FieldWithConst[Edge,Mat[_3,_3,Double]](double33_zero)

  //some fields for output
   val Phiv  	 = FieldWithConst[Vertex,Double](0.0)
   val Phiv_Grad  = FieldWithConst[Vertex,Vec[_3,Double]](double3_zero)
   val Velocv     = FieldWithConst[Vertex,Vec[_3,Double]](double3_zero)

  //some geometry fields
   val position = FieldWithLabel[Vertex,Vec[_3,Double]]("position")

   val theta_vertex  = FieldWithConst[Vertex,Double](0.0)
   val lambda_vertex = FieldWithConst[Vertex,Double](0.0)

   val theta_face  = FieldWithConst[Face,Double](0.0)
   val lambda_face = FieldWithConst[Face,Double](0.0)

   val theta_edge  = FieldWithConst[Edge,Double](0.0)
   val lambda_edge = FieldWithConst[Edge,Double](0.0)

   val face_centroid = FieldWithConst[Face,Vec[_3,Double]](double3_zero)
   val face_area = FieldWithConst[Face,Double](0.0)
   val face_normal = FieldWithConst[Face,Vec[_3,Double]](double3_zero)

   val edge_center  = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
   val edge_length  = FieldWithConst[Edge,Double](0.0)
   val edge_normal  = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
     
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
    var center = double3_zero //Vec(0.0,0.0,0.0)
    //val test = Vec(0.0,0.0,0.0)
    for(v <- vertices(f)) {
      center = center + position(v)
    }
    center = center / size(vertices(f))
    return center
  }

  def InterpolateVertex(v : Rep[Vertex]) : Rep[Unit] = {
    var phiv = 0.0
    var phiv_grad = double3_zero

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

    var velv = double3_zero
    for(e <- edges(v)) {
      velv = velv + Veloc(e)
    }
    
    // TODO
    velv = velv / size(edges(v))
    Velocv(v) = velv
  }

  def calcFaceGeom(f : Rep[Face]) : Rep[Unit] = {
    val approxCenter = calcFaceCenter(f)
    var normal = double3_zero //Vec(0.0,0.0,0.0)
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      normal = normal + cross(v1,v0)
    }
    normal = normalize(normal)
    var center = double3_zero //Vec(0.0,0.0,0.0)
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
    val e1 = position(head(e)) 
    val e2 = position(tail(e))
    val e3 = e2-e1
    val center = (e1+e2)*0.5
    var normal = cross(center,e3)
    val length = sqrt(dot(e3,e3))
    normal = normalize(normal)*length

    edge_center(e) = center
    edge_length(e) = length
    edge_normal(e) = normal
  
    val rad2i = 1.0/dot(center,center)

    work_array = double33_zero.mutable
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
    val center = edge_center(e)
    val rad = sqrt(dot(center,center))
    var theta = acos(center.z/rad)
    theta=(pi*0.5)-theta
  
    val lambda = atan2(center.y,center.x)
  
    val ct = cos(theta)
    val st = sin(theta)
    val cr = cos(lambda*R)
    val sr = sin(lambda*R)

    val uhat =  omega*ct+K*pow(ct,unit(R-1.0))*(st*st*R-ct*ct)*cr
    val vhat = -K*R*pow(ct,unit(R-1.0))*st*sr
    
    val ux = -uhat*sin(lambda)-vhat*sin(theta)*cos(lambda)
    val uy =  uhat*cos(lambda)-vhat*sin(theta)*sin(lambda)
    val uz =  vhat*cos(theta)

    val P  = PMat(e)
    val vel = Vec(ux,uy,uz)
    val vel1 = P*vel

    Veloc(e)=vel1
  }

  def SetIC(f : Rep[Face]) : Rep[Unit] = {
    val omega = 0.1079
    val K = 0.1079
    val R = 4
    
    val center = face_centroid(f)
    val theta  = theta_face(f)
    val lambda = lambda_face(f)
    val ct = cos(theta)
    val st = sin(theta)

    val atheta = omega*0.5*(2.0+omega)*ct*ct+0.25*K*K*pow(ct,unit(2.0*R))*(ct*ct*(R+1)+(2*R*R-R-2)-unit(2.0*R*R)/(ct*ct+1.0e-12))
      
    val btheta = 2*(1.0+omega)*K/(R+1)/(R+2)*pow(ct,unit(1.0*R))*(unit(R*R+2.0*R+2)-ct*ct*(R+1)*(R+1))
    val ctheta = 0.25*K*K*pow(ct,unit(2.0*R))*(ct*ct*(R+1)-R-2)

    val phi = 0.3648 + atheta+btheta*cos(lambda*R)+ctheta*cos(lambda*2*R) // TODO

    Phi(f) = phi
  }

  def SetVertexCoords(v : Rep[Vertex]) : Rep[Unit] = {
    val coords = position(v) 
    val rad = sqrt(dot(coords,coords))
    var theta = acos(coords.z/rad)
    theta=(pi*0.5)-theta
  
    val lambda = atan2(coords.y,coords.x)

    theta_vertex(v)  = theta
    lambda_vertex(v) = lambda
  }

  def SetFaceCoords(f : Rep[Face]) : Rep[Unit] = {
    val coords = face_centroid(f) 
    val rad = sqrt(dot(coords,coords))
    var theta = acos(coords.z/rad)
    theta=pi*0.5-theta
    val lambda = atan2(coords.y,coords.x)

    theta_face(f)  = theta
    lambda_face(f) = lambda
  }

  def SetEdgeCoords(e : Rep[Edge]) : Rep[Unit] = {
    val coords = edge_center(e) 
    val rad = sqrt(dot(coords,coords))
    var theta = acos(coords.z/rad)
    theta=pi*0.5-theta
  
    var lambda = atan2(coords.y,coords.x)

    theta_edge(e)  = theta
    lambda_edge(e) = lambda
  }

  def CalcFaceGrad_Phi(f : Rep[Face]) : Rep[Unit] = {
  	val coords  = face_centroid(f) 
    val rad     = sqrt(dot(coords,coords))
    val rad2    = rad*rad
    val rad3    = rad2*rad
    val thetac  = cos(-theta_face(f))
    val thetas  = sin(-theta_face(f))
    val lambdac = cos(-lambda_face(f))
    val lambdas = sin(-lambda_face(f))

    val coords1_x = coords.x*lambdac-coords.y*lambdas
    val coords1_y = coords.x*lambdas+coords.y*lambdac
    val coords1_z = coords.z
      
    val coords2_y = coords1_y
    val coords2_x = coords1_x*thetac-coords1_z*thetas
    val coords2_z = coords1_x*thetas+coords1_z*thetac

    var theta = acos(coords2_z/sqrt(coords2_x*coords2_x+coords2_y*coords2_y+coords2_z*coords2_z))
    theta=pi*0.5-theta
    val lambda = atan2(coords2_y,coords2_x)
    val Phif = Phi(f)

    var af = 0.0
    var bf = 0.0
    var cf = 0.0
    var df = 0.0
    var ef = 0.0


    for (e <- edges(f)) {
      for (f_c <- faces(e)) {
        if (ID(f_c) != ID(f)) {
          val coordsc = face_centroid(f_c) 
          val coords1c_x = coordsc.x*lambdac-coordsc.y*lambdas
          val coords1c_y = coordsc.x*lambdas+coordsc.y*lambdac
          val coords1c_z = coordsc.z
              
          val coords2c_y = coords1c_y
          val coords2c_x = coords1c_x*thetac-coords1c_z*thetas
          val coords2c_z = coords1c_x*thetas+coords1c_z*thetac
            
          var theta_c  = acos(coords2c_z/sqrt(coords2c_x*coords2c_x+coords2c_y*coords2c_y+coords2c_z*coords2c_z))
          theta_c=(pi*0.5)-theta_c
          val lambda_c = atan2(coords2c_y,coords2c_x)

          val dlambda = (lambda_c-lambda)
          val dtheta  = (theta_c-theta)
          val dPhi    = Phi(f_c)-Phif
          val weight2 = 1.0/(dlambda*dlambda+dtheta*dtheta)
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
      
    val arg = 1.0/sqrt(1.0-coords.z*coords.z/rad2)
    val dthe_dx = -coords.z*coords.x*arg
    val dthe_dy = -coords.z*coords.y*arg
    val dthe_dz = (-coords.z*coords.z/rad3+1.0/rad)*arg
    val dlam_dx = -coords.y/(sqrt(coords.x*coords.x+coords.y*coords.y))
    val dlam_dy =  coords.x/(sqrt(coords.x*coords.x+coords.y*coords.y))
        
    val dPhi_dx = dPhi_dlam*dlam_dx+dPhi_dthe*dthe_dx
    val dPhi_dy = dPhi_dlam*dlam_dy+dPhi_dthe*dthe_dy
    val dPhi_dz = dPhi_dthe*dthe_dz

    Phi_Grad(f) = Vec(dPhi_dx,dPhi_dy,dPhi_dz)
  }

  def CalcEdgeGrad(e : Rep[Edge]) : Rep[Unit] = {
    val coords  = edge_center(e)
    val rad     = sqrt(dot(coords,coords))
    val rad2    = rad*rad
    val rad3    = rad2*rad
    val thetac  = cos(-theta_edge(e))
    val thetas  = sin(-theta_edge(e))
    val lambdac = cos(-lambda_edge(e))
    val lambdas = sin(-lambda_edge(e))

    val coords1_x = coords.x*lambdac-coords.y*lambdas
    val coords1_y = coords.x*lambdas+coords.y*lambdac
    val coords1_z = coords.z
      
    val coords2_y = coords1_y
    val coords2_x = coords1_x*thetac-coords1_z*thetas
    val coords2_z = coords1_x*thetas+coords1_z*thetac

    var theta = acos(coords2_z/sqrt(coords2_x*coords2_x+coords2_y*coords2_y+coords2_z*coords2_z))
    theta=(pi*0.5)-theta
    val lambda = atan2(coords2_y,coords2_x)
    val Vele = Veloc(e)

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
          val coordsc = edge_center(e_c) 
          val coords1c_x = coordsc.x*lambdac-coordsc.y*lambdas
          val coords1c_y = coordsc.x*lambdas+coordsc.y*lambdac
          val coords1c_z = coordsc.z
              
          val coords2c_y = coords1c_y
          val coords2c_x = coords1c_x*thetac-coords1c_z*thetas
          val coords2c_z = coords1c_x*thetas+coords1c_z*thetac
            
          var theta_c  = acos(coords2c_z/sqrt(coords2c_x*coords2c_x+coords2c_y*coords2c_y+coords2c_z*coords2c_z))
          theta_c = (pi*0.5)-theta_c
          val lambda_c = atan2(coords2c_y,coords2c_x)

          val dVel  = Veloc(e_c)-Vele
          val dlambda = lambda_c-lambda
          val dtheta  = theta_c-theta
          val weight2 = 1.0/(dlambda*dlambda+dtheta*dtheta)
          val dlambdaw2 = dlambda*weight2
          val dthetaw2  = dtheta*weight2

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
      
    val arg = 1.0/sqrt(1.0-coords.z*coords.z/rad2)
    val dthe_dx =  -coords.z*coords.x*arg
    val dthe_dy =  -coords.z*coords.y*arg
    val dthe_dz = (-coords.z*coords.z/rad3+1.0/rad)*arg
    val dlam_dx = -coords.y/(sqrt(coords.x*coords.x+coords.y*coords.y))
    val dlam_dy =  coords.x/(sqrt(coords.x*coords.x+coords.y*coords.y))

    val dVel_dx = dVel_dlam*dlam_dx+dVel_dthe*dthe_dx
    val dVel_dy = dVel_dlam*dlam_dy+dVel_dthe*dthe_dy
    val dVel_dz = dVel_dthe*dthe_dz
        
    val C  = Mat(dVel_dx,dVel_dy,dVel_dz)
    val P  = PMat(e)
        
    val CP = C*P
        
    Velochat_Grad(e) = CP
             
    var grad = double3_zero
        
    for (f <- faces(e)) 
      grad = grad + Phi_Grad(f)
        
    val grad1 = P*grad
    Phihat_Grad(e)=grad1*0.5
  }

  def calcGradient() {
   for( f <- faces(mesh) ) 
    Phi_Grad(f) = double3_zero 


  for (e <- edges(mesh)) {
    val f0   = left(e) 
    val f1   = right(e) 
    val Phi0 = Phi(f0) 
    val Phi1 = Phi(f1) 
    
    val nVec = edge_normal(e) 
    val Phi01 = nVec*0.5f*(Phi0 + Phi1)

    Phi_Grad(f0) = Phi_Grad(f0) + Phi01
    Phi_Grad(f1) = Phi_Grad(f1) - Phi01
  }

  for( f <- faces(mesh) ) 
    Phi_Grad(f) /= face_area(f)
  }

  //def main() {
    //val test = Vec(0.0,0.0,0.0,0.0)
    val iterations = 100
    /*
    double3_zero  = Vec(0.0,0.0,0.0)
    double33_zero = Mat(double3_zero,double3_zero,double3_zero)
    work_array = double33_zero
    pi = MATH_PI
    Omega = unit(1.0)
    alpha = 0.0*pi
    
    Phi        = FieldWithConst[Face,Double](0.0)
    PhiOld     = FieldWithConst[Face,Double](0.0)
    Phi_Grad   = FieldWithConst[Face,Vec[_3,Double]](double3_zero)
    Flux  	 = FieldWithConst[Face,Double](0.0)
    Veloc 	 = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
    VelocOld	 = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
    Rhs_Veloc	 = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
    Velochat_Grad = FieldWithConst[Edge,Mat[_3,_3,Double]](double33_zero)
    Phihat_Grad= FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
    PMat       = FieldWithConst[Edge,Mat[_3,_3,Double]](double33_zero)

    //some fields for output
    Phiv  	 = FieldWithConst[Vertex,Double](0.0)
    Phiv_Grad  = FieldWithConst[Vertex,Vec[_3,Double]](double3_zero)
    Velocv     = FieldWithConst[Vertex,Vec[_3,Double]](double3_zero)

    //some geometry fields
    position = FieldWithLabel[Vertex,Vec[_3,Double]]("position")

    theta_vertex  = FieldWithConst[Vertex,Double](0.0)
    lambda_vertex = FieldWithConst[Vertex,Double](0.0)

    theta_face  = FieldWithConst[Face,Double](0.0)
    lambda_face = FieldWithConst[Face,Double](0.0)

    theta_edge  = FieldWithConst[Edge,Double](0.0)
    lambda_edge = FieldWithConst[Edge,Double](0.0)

    face_centroid = FieldWithConst[Face,Vec[_3,Double]](double3_zero)
    face_area = FieldWithConst[Face,Double](0.0)
    face_normal = FieldWithConst[Face,Vec[_3,Double]](double3_zero)

    edge_center  = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
    edge_length  = FieldWithConst[Edge,Double](0.0)
    edge_normal  = FieldWithConst[Edge,Vec[_3,Double]](double3_zero)
    */
  
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
    var step = 0;
      
    var dt_min  = 100000.0
    for(f <- faces(mesh)) {
        val centroid = face_centroid(f)
        //Print(ID(f), " EDGES")
        for(e <- edges(f)) {
          //Print(ID(e))
          val center= edge_center(e)
          val Vel = Veloc(e)
          val dist = sqrt(dot(center-centroid,center-centroid))
          val Velmag = sqrt(dot(Vel,Vel))
          dt_min = dt_min min (dist/Velmag*0.025)
        }
    }
    val deltat = dt_min
    
    val start_time = wall_time();

    while(step < iterations) {
      step += 1;
// --------------- time step --------------
      
      // Print("STEP ", step)

      // Print("dt_min loop")
     

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
        val factor = deltat/(5.0-readVar(iter))

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

          val r0 = center-left_centroid
          val r1 = center-right_centroid

          val Phil = Phi(left_face)+dot(Phi_Grad(left_face),r0)
          val Phir = Phi(right_face)+dot(Phi_Grad(right_face),r1)

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
     // Print("Simulation time = ",t," delta t = ",deltat)
    }	

    //TODO: Why is the timing function moved up before the while-loop without this condition?
    if(step == iterations)
      Print("TIME_FOR_LOOP: ", wall_time() - start_time ) ;
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
      val coords = position(v) 
      Print(coords.x," ",coords.y," ",coords.z," ",Phiv(v)," ",theta_vertex(v)," ",lambda_vertex(v))
      Print(coords.x," ",coords.y," ",coords.z," ",Phiv(v)," ",Velocv(v).x," ",Velocv(v).y," ",Velocv(v).z," ",theta_vertex(v)," ",lambda_vertex(v))
    }
  }
}
