package ppl.apps.liszt.IncompressibleFlow

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object IncompressibleFlowRunner extends DeLisztApplicationRunner with IncompressibleFlow

object IncompressibleFlow extends DeLisztApplication {

	val interior_set = BoundarySet[Face]("default-interior")
	val inlet_set = BoundarySet[Face]("inlet")
	val outlet_set = BoundarySet[Face]("outlet")
	val far_field_set = BoundarySet[Face]("far_field")
	
	val is_in_interior_set = FieldWithConst[Face,Int](0)
	
	val Velocity = FieldWithConst[Face,Float](0.f)
	val VelocityTemp = FieldWithConst[Face,Float](0.f)
	val VelocityFieldFaces = FieldWithConst[Face,Float3](Vec(0.f,0.f,0.f))
	val VelocityFieldCells = FieldWithConst[Cell,Float3](Vec(0.f,0.f,0.f))
	val VelocityFieldCellsMag = FieldWithConst[Cell,Float](0.f)
	val Pressure = FieldWithConst[Cell,Float](0.f)
	val PressureTemp = FieldWithConst[Cell,Float](0.f)
	
	val Jacobian = FieldWithConst[Cell,Float](0.f)
	
	val TimeHack = FieldWithConst[Cell,Float](0.f)
	
	val i = Vec(1.f,0.f,0.f)
	val j = Vec(0.f,1.f,0.f)
	val k = Vec(0.f,0.f,1.f)
	
	def main() {
		Prelude.calcMeshGeometry()

		for (f <- interior_set) {
			is_in_interior_set(f) = 1
		}
	//initialize geometry fields
		for (f <- inlet_set) {
			Velocity(f) = if (Prelude.face_centroid(f).y > 1.f)  1.0f else 0.0f
			//Velocity(f) = 1.0f
		}
		for (f <- outlet_set) {
			Velocity(f) = if (Prelude.face_centroid(f).y < 2.f)  1.0f else 0.0f
			//Velocity(f) = 1.0f
		}
		for (f <- faces(mesh)) {
			VelocityTemp(f) = Velocity(f)
		}
	
		val cellwidth = 1.0f
		var deltat = 1.f
		val smallest_t = 0.0001f
		var t = 0.f
		
		
		
		var ll = Vec(MAX_FLOAT,MAX_FLOAT,MAX_FLOAT)
		var ur = Vec(MIN_FLOAT,MIN_FLOAT,MIN_FLOAT)
		for(v <- vertices(mesh)) {
		  ll *<*= Prelude.position(v)
		  ur *>*= Prelude.position(v)
		}
		val mesh_center = .5f * (ll + ur)
		for(f <- interior_set) {
		  val center = Prelude.face_centroid(f)
		  // val x = center.x - mesh_center.x
		  // val y = center.y - mesh_center.y
		  // val z = center.z - mesh_center.z
		  //Velocity(f) = center.x
		}
		
		Print("Precomputing Jacobian")
		for (c <- cells(mesh)) {
			var c_jacobian = 0.f //what is the change in Flux if we change the pressure? df/dp
			for (f <- faces(c)) {
				if (is_in_interior_set(f) == 1) {
						c_jacobian += 1
				}
			}
			Jacobian(c) = c_jacobian
		}
	
		Print("Starting simulation loop")
		var starttime = wall_time()
		var nsteps = 0
		while(t < 5.0f) {
		
			deltat = 0.1f
			var temp = 0.0f
			for (f <- faces(mesh)) {
				temp *>*= Velocity(f)
			}
			deltat = deltat min cellwidth/temp*0.5f
			deltat = deltat max smallest_t
			Print("Time ", t, ", starting step ", nsteps, " with delta_t: ", deltat)
			
			for (c <- cells(mesh)) {
				var vel = Vec(0.f,0.f,0.f)
				for (f <- faces(c)) {
					vel += Velocity(f)*Prelude.face_unit_normal(f)/2.f
				}
				VelocityFieldCellsMag(c) = length(vel)
				VelocityFieldCells(c) = vel
			}
			
			//ADVECTION
			for (f <- interior_set) {
				val n = Prelude.face_unit_normal(f);
			
				var v = Vec(0.f, 0.f, 0.f)
				//interpolate velocity
				for (c <- cells(f)) {
					for (f_c <- faces(c)) {
						val n_c = Prelude.face_unit_normal(f_c);
						if (dot(n, n_c) < 0.1) {
							v += Velocity(f_c)*n_c
						}
					}
				}
				v *= 0.25f
				v += Velocity(f)*n
				
				
				VelocityFieldFaces(f) = v
				//Print("Normal: ", Prelude.face_unit_normal(f), ", Velocity: ", v)
				
				val dtv = -deltat*v //distance into past
				
				//calculate gradient
				var vn = 0.f
				var w_other = 0.f
				for (e <- edges(f)) {
					for (f_c <- faces(e)) {
						if (ID(f_c) != ID(f)) {
							val n_c = Prelude.face_unit_normal(f_c)
							if (dot(n, n_c) > 0.9) {
								
								val centroid_diff = Prelude.face_centroid(f_c) - Prelude.face_centroid(f)
								val centroid_diff_normalized = normalize(centroid_diff)
								val dir = dot(centroid_diff_normalized,dtv)/cellwidth
								if (dir > 0) {
									w_other += dir
									vn += Velocity(f_c)*dir
								}
								
							}
						}
					}
				}
				for (c <- cells(f)) {
					for (f_c <- faces(c)) {
						if (ID(f) != ID(f_c)) {
							val n_c = Prelude.face_unit_normal(f_c)
							if (dot(n, n_c) > 0.9) {
								
								val centroid_diff = Prelude.face_centroid(f_c) - Prelude.face_centroid(f)
								val centroid_diff_normalized = normalize(centroid_diff)
								val dir = dot(centroid_diff_normalized,dtv)/cellwidth
								if (dir > 0) {
									w_other += dir
									vn += Velocity(f_c)*dir
								}
								
							}
						}
					}
				}
				vn += Velocity(f)*(1-w_other)
			
				VelocityTemp(f) = vn
			
			}
			
			//PROJECTION (Jacobi method)
			var maxdiverg = 1.0f
			var iter = 0
			while (maxdiverg > 0.05f && iter < 2000) {
				maxdiverg = 0.0f
				
				for (f <- interior_set) {
					Velocity(f) = VelocityTemp(f)
				}
				
				for (c <- cells(mesh)) {
					var c_divergence = 0.f //how many units we need to change the velocity by
					for (f <- faces(c)) {
						val into = Prelude.cell_centroid(c) - Prelude.face_centroid(f)
						val dir = dot(into, Prelude.face_unit_normal(f))
						if (dir < 0) {
							c_divergence -= Velocity(f)
						} else {
							c_divergence += Velocity(f)
						}
					}
					maxdiverg *>*= c_divergence
				
					//update velocities according to the jacobian
					for (f <- faces(c)) {
						if (is_in_interior_set(f) == 1) {
							val into = Prelude.cell_centroid(c) - Prelude.face_centroid(f)
							val dir = dot(into, Prelude.face_unit_normal(f))
							if (dir < 0) {
								VelocityTemp(f) += 0.66f*c_divergence/Jacobian(c)
							} else {
								VelocityTemp(f) -= 0.66f*c_divergence/Jacobian(c)
							}
						}
					}
				}
				
				Print("  Step ", iter, " Max divergence: ", maxdiverg)
				iter += 1
			}
			t += deltat
			
			for(c<-cells(mesh)) {
				TimeHack(c) = t
			}
			
			nsteps += 1
		}
		Print("Total time: ", wall_time() - starttime, ", steps: ", nsteps)
	}
}
