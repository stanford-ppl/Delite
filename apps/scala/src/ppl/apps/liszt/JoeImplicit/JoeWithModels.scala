package ppl.apps.liszt.JoeWithModels

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object JoeWithModelsRunner extends DeLisztApplicationRunner with JoeWithModels

object JoeWithModels extends DeLisztApplication {

	val rhs_rho = FieldWithConst[Cell,Float](0.f) ;
        val rhs_rhou = FieldWithConst[Cell,Vec[_3,Float]](Constants.float3_zero) ;
        val rhs_rhoE = FieldWithConst[Cell,Float](0.f) ;

	// notice that this would originally be called in JoeWithModels.initialHook()
        val rho = FieldWithConst[Cell,Float](IC.rho_init) ;
        val rhou = FieldWithConst[Cell,Vec[_3,Float]](IC.rho_init * IC.u_init) ;
        val rhoE = FieldWithConst[Cell,Float](IC.p_init / (IC.GAMMA - 1.f)  +  0.5f * IC.rho_init * dot( IC.u_init, IC.u_init )) ;

	val AplMatrixStorageField = FieldWithConst[Face,Mat[_5,_5,Float]]( Constants.zeroDenseFiveMatrix ) ;
	val AmiMatrixStorageField = FieldWithConst[Face,Mat[_5,_5,Float]]( Constants.zeroDenseFiveMatrix ) ;

	val zone_interior = BoundarySet[Face]("zone_interior")
	val zone_boundary = BoundarySet[Face]("zone_boundary")
	val symmetry = BoundarySet[Face]("symmetry")
	val wall = BoundarySet[Face]("wall")
	val other_boundaries = BoundarySet[Face]("other_boundaries")
	val cbc = BoundarySet[Face]("cbc")
	val hook = BoundarySet[Face]("hook")
	val cbc_subsonic_outlet = BoundarySet[Face]("cbc_subsonic_outlet")

	// for implicit method
	val A = new SparseMatrix[Float] ;
	val dq = new SparseVector[Float] ;
	val rhs = new SparseVector[Float] ;

	def init() { Print("JoeWithModels()") ;}
	def initialHook() {Print("setInitialConditions()") ;}


	def getInsideBoundaryCell( f : Face ) : Cell = {
		val c = if ( isCellOutsideRef(inside(f)) ) outside(f) else inside(f) ;
		return c ;
	}

	def isFaceBoundary( f: Face) : Boolean = {
		if ( isCellOutsideRef(inside(f)) || isCellOutsideRef(outside(f)) ) { 
			return true ;
		}
		return false ;		
	}	

	// TODO(Montse):: HACK!!!! NEED TO MODIFY THE REFERENCE OUTSIDE CELL!!!! MIGHT NOT BE ZERO!!!!
	def isCellOutsideRef( c: Cell ) : Boolean = {
		if ( ID(c) == 0 ) {
			return true ;
		}
		return false ;
	}

	def run() {
		initialHook() ;
		
		if ( IC.navierStokesSolver == NavierStokesSolvers.EXPL_EULER ){ 
			runForwardEuler() ;
		} else if ( IC.navierStokesSolver == NavierStokesSolvers.IMPL_EULER ){
			runBackwardEuler() ;
		} else if ( IC.navierStokesSolver == NavierStokesSolvers.EXPL_RK ){
		} else { Print("ERROR: no or wrong time integration scheme specified !") ; }
	}

	def runBackwardEuler() {

		UgpWithCvCompFlow.calcRansStateVarAndMaterialProperties() ;
		
		setNavierStokesBC() ;
		
		// HERE TIME SHOULD START TICKING !!!!!!
		val start_time = MPI_Wtime() ;
		var step = 0 ;
		var isDone = false ;

		val tmp_time = MPI_Wtime() ;
			for ( c <- cells(mesh) ) {
				val c_id = ID(c);
				IntegerSorter.setNum( c_id ) ;
				if ( ID(c) % 1000 == 0 ) {
					Print( "done with cells: ", c_id ) ;	
				}

				for ( cc <- cells(c) ) {
					IntegerSorter.setNum( ID(cc) ) ;
				}
				
				var j = 0 ;
				while ( j < IntegerSorter.numberOfNeighbors ) {
					val cellId = IntegerSorter.getNextMin() ;
					if ( cellId == c_id ) {
						val tmp = MeshGeometryCalc.cv_volume(c) / UgpWithCvCompFlow.local_dt(c) ;
						val diagMatrix = Mat( Vec(tmp,0.f,0.f,0.f,0.f), Vec(0.f,tmp,0.f,0.f,0.f), Vec(0.f,0.f,tmp,0.f,0.f), Vec(0.f,0.f,0.f,tmp,0.f), Vec(0.f,0.f,0.f,0.f,tmp) ) ;
						A(c,c) = diagMatrix ;
					} else {
						for ( cc <- cells(c) ) {
							if ( !isCellOutsideRef(cc) && ID(cc) == cellId ) {
								A(c,cc) = Constants.zeroDenseFiveMatrix ;
							}
						}
					}
					j = j + 1 ;
				}
				IntegerSorter.reset() ;
				dq(c) = Constants.zeroFiveVec ;

			}
		Print("time for initiallization of the matrix loop: ", MPI_Wtime() - tmp_time  ) ;

		while (step < Constants.nsteps && !isDone ) {
			Print("looping: ", step) ;
			step += 1 ;

			if ( (step >= IC.startIncCFL) && (step % IC.intervalIncCFL == 0) && (IC.cfl < IC.maxCFL) ) {
				IC.cfl = IC.cfl * IC.incCFL ; 
			}
			var dtMin = UgpWithCvCompFlow.calcDt(IC.cfl) ; 
			for ( c <- cells(mesh) ) {
				for ( cc <- cells(c) ) {
					if ( !isCellOutsideRef(cc)) {
						A(c,cc) = Constants.zeroDenseFiveMatrix ;
					}
				}
				val tmp = MeshGeometryCalc.cv_volume(c) / UgpWithCvCompFlow.local_dt(c) ;
				val diagMatrix = Mat( Vec(tmp,0.f,0.f,0.f,0.f), Vec(0.f,tmp,0.f,0.f,0.f), Vec(0.f,0.f,tmp,0.f,0.f), Vec(0.f,0.f,0.f,tmp,0.f), Vec(0.f,0.f,0.f,0.f,tmp) ) ;
				A(c,c) = diagMatrix ;
//if (ID(c) == 6805) {
//	Print( "added to A(", ID(c), "," ,ID(c) , "): ", diagMatrix ) ;
//}
				dq(c) = Constants.zeroFiveVec ;
			}

			var my_resid_rho = 0.f ;
			var my_resid_rhou = Constants.float3_zero ;
			var my_resid_rhoE = 0.f ;

			calcRhs(true) ; // note that this function originally would take rhs_rho, rhs_rhou, rhs_rhoE, rho, rhou, rhoE fields as arguments
			
			for( c <- cells(mesh) ) {
				val rhsVec = IC.underRelax * Vec ( rhs_rho(c), rhs_rhou(c).x, rhs_rhou(c).y, rhs_rhou(c).z, rhs_rhoE(c) ) ;
				rhs(c) = rhsVec ;
//if (ID(c) == 6805) {
//	Print( "rhs(" ,ID(c), "): ", rhsVec ) ;
//}
			}

			solveCoupledLinSysNS() ;

			for( c <- cells(mesh) ) {
				rho(c) += dq(c & 0) ;
				rhou(c) += Vec( dq(c & 1), dq(c & 2), dq(c & 3) ) ;
				rhoE(c) += dq(c & 4) ;
			}

			UgpWithCvCompFlow.calcRansStateVarAndMaterialProperties() ;

			setNavierStokesBC() ;

			for( c <- cells(mesh) ) {
				my_resid_rho += fabs(rhs_rho(c)) ;
				val absVec = Vec( fabs(rhs_rhou(c).x), fabs(rhs_rhou(c).y), fabs(rhs_rhou(c).z) ) ;
				my_resid_rhou += absVec ;
				my_resid_rhoE += fabs(rhs_rhoE(c)) ;
			}

			val my_resid = Vec( my_resid_rho, my_resid_rhou.x, my_resid_rhou.y, my_resid_rhou.z, my_resid_rhoE ) ;

			if ( step % IC.check_interval == 0 ) {
				if ( step % ( IC.check_interval * 10 ) == 0 ) {
					Print( "" ) ; // print nothing to simulate a \n !!!!
					Print( "done step: " , step , ", cfl: " , IC.cfl , ", min. dt:   " , dtMin ) ; 
				}

				showResidue( my_resid, step ) ;
			} 

			if ( my_resid(_4) <= IC.resid_energ_th ) {
				isDone = true ;
			} 

		}

		 Print( "TIME_FOR_LOOP: ", MPI_Wtime() - start_time ) ;
	}

	def runForwardEuler() {

		UgpWithCvCompFlow.calcRansStateVarAndMaterialProperties() ;
		
 
		setNavierStokesBC() ;
		
		// HERE TIME SHOULD START TICKING !!!!!!
		val start_time = MPI_Wtime() ;
		var step = 0 ;
		while (step < Constants.nsteps ) {
			Print("looping: ", step) ;
			step += 1 ;

			var my_resid_rho = 0.f ;
			var my_resid_rhou = Constants.float3_zero ;
			var my_resid_rhoE = 0.f ;

			var dtMin = UgpWithCvCompFlow.calcDt(IC.cfl) ;

			calcRhs(false) ; // note that this function originally would take rhs_rho, rhs_rhou, rhs_rhoE, rho, rhou, rhoE fields as arguments
			
			for( c <- cells(mesh) ) {
				val tmp = UgpWithCvCompFlow.local_dt(c) / MeshGeometryCalc.cv_volume(c) ;
				rho(c) += tmp * rhs_rho(c) ;
				rhou(c) += tmp * rhs_rhou(c) ;
				rhoE(c) += tmp * rhs_rhoE(c) ;
			}

			for( c <- cells(mesh) ) {
				my_resid_rho += fabs(rhs_rho(c)) ;
				val absVec = Vec( fabs(rhs_rhou(c).x), fabs(rhs_rhou(c).y), fabs(rhs_rhou(c).z) ) ;
				my_resid_rhou += absVec ;
				my_resid_rhoE += fabs(rhs_rhoE(c)) ;
			}

			val my_resid = Vec( my_resid_rho, my_resid_rhou.x, my_resid_rhou.y, my_resid_rhou.z, my_resid_rhoE ) ;

			if ( step % IC.check_interval == 0 ) {
				if ( step % ( IC.check_interval * 10 ) == 0 ) {
					Print( "" ) ; // print nothing to simulate a \n !!!!
					Print( "done step: " , step , ", cfl: " , IC.cfl , ", min. dt:   " , dtMin ) ; 
				}

				showResidue( my_resid, step ) ;
			} 

			UgpWithCvCompFlow.calcRansStateVarAndMaterialProperties() ;

			setNavierStokesBC() ;
		}

		 Print( "TIME_FOR_LOOP: ", MPI_Wtime() - start_time ) ;
			
	}


	def calcRhs( isImplicit : Boolean ) {
		for( c <- cells(mesh) ) {
			rhs_rho(c) = 0.f ;
			rhs_rhou(c) =  Constants.float3_zero ;
			rhs_rhoE(c) = 0.f ;
		}

		calcEulerFlux( isImplicit ) ;

		// construct matrix A
		if( isImplicit ) {
			for ( c <- cells(mesh) ) {
				for ( f <- faces(c) ) {
					val Apl = AplMatrixStorageField(f) ;
					if ( isFaceBoundary(f) ) {
						A(c,c) += Apl ;	
//if (ID(c) == 6805) {
//	Print( "added to A(", ID(c), "," ,ID(c) , "): ", Apl ) ;
//}				
					} else {
						val Ami = AmiMatrixStorageField(f) ;
						val cc = inside(f) ; 
						if ( ID(c) == ID(cc) ) {
							val ccc = outside(f) ;
							A(c,c) -= Apl ;
							A(c,ccc) -= Ami ;
//if (ID(c) == 6805) {
//	Print( "added to A(", ID(c), "," ,ID(c) , "): ", -Apl ) ;
//	Print( "added to A(", ID(c), "," ,ID(ccc) , "): ", -Ami ) ;
//}
						} else {
							A(c,c) += Apl ;
							A(c,cc) += Ami ;
//if (ID(c) == 6805) {
//	Print( "added to A(", ID(c), "," ,ID(c) , "): ", Apl ) ;
//	Print( "added to A(", ID(c), "," ,ID(cc) , "): ", Ami ) ;
//}
						}
										
					}

				}
			}
		}
		
	}

	def calcEulerFlux( isImplicit : Boolean ) {
		for (f <- zone_interior) { calcRhsInterior(f, isImplicit) }
		for (f <- symmetry) { calcRhsSymmetry(f, isImplicit) }
		// Ignoring wall
		for (f <- other_boundaries) { calcRhsOtherBoundaries(f, isImplicit) }
	}


	def setNavierStokesBC() {
		var bc_err = false ;

		// Print( "Applying HOOK		: " )
		for (f <- hook) { UgpWithCvCompFlow.ComputeBCProperties_T(f) }
		// TODO(mbarrien): Is there a need for a different BoundaryInfo for subranges
		// inside a larger boundary?
		val vector_bc = Vec( IC.u_init.x, IC.u_init.y, IC.u_init.z, IC.T_init, IC.p_init )
		val T_bc = vector_bc(_3)
		val p_bc = vector_bc(_4)
		val u_bc = Vec( vector_bc.x, vector_bc.y, vector_bc.z )
		if (IC.first) {
			Print( "Applying CBC		\t u_bc: ", u_bc, " T_bc: ", T_bc, " p_bc: ", p_bc ) ;
		}
		for (f <- cbc) {
			CBCBoundary(f, T_bc, p_bc, u_bc)
		}
		for (f <- cbc) {
			UgpWithCvCompFlow.ComputeBCProperties_T(f)
		}
		// Ignore cbc_subsonic_inlet
		// TODO(mbarrien): This vec should probably not be hardcoded like this.
		val cbc_p_bc = Vec( 0.f, 0.f, 0.f, 0.f, 0.f )(_4)
		if (IC.first) {
			Print ( "Applying CBC_SUBSONIC_OUTLET \t pOut: ", cbc_p_bc ) ;
		}
		for (f <- cbc_subsonic_outlet) {
			CBCSubsonicOutletBoundary(f, cbc_p_bc);
		}
		for (f <- cbc_subsonic_outlet) {
			UgpWithCvCompFlow.ComputeBCProperties_T(f)
		}
		if (IC.first) {
			Print ( "Applying SYMMETRY	: " )
		}
		for (f <- symmetry) {
			SymmetryBoundary(f);
		}
		// Ignore neumann
		// Ignore wall
		for (f <- zone_boundary) {
			setRhoFa(f)
		}
		IC.first = false ;
	}

	
	def showResidue( rhsResid: Vec[_5,Float], step: Int ) {
		if ( step % (IC.check_interval * 10 ) == 0 ) {
			Print( "		rho		rhou-X		rhou-Y		rhou-Z		rhoE		") ;
		}
		Print( "RESID: ", step, "		", rhsResid(_0), "		", rhsResid(_1), "		", rhsResid(_2), "		", rhsResid(_3), "		", rhsResid(_4) ) ;
	}

	
	def CBCBoundary( f: Face, T_bc : Float, p_bc : Float, u_bc : Vec[_3,Float] ) {

//	if ( ID(f) == 95000 ) {
//		Print( "CBC face with Id: ", ID(f), " T: ", T_bc, " u: ", u_bc, " p: ", p_bc ) ;
//	}

		UgpWithCvCompFlow.T_fa(f) = T_bc ;
		UgpWithCvCompFlow.vel_fa(f) = u_bc ;
		UgpWithCvCompFlow.p_fa(f) = p_bc ;
	}

	def CBCSubsonicOutletBoundary( f: Face, p_bc : Float ) {
		val icv0 = getInsideBoundaryCell(f) ;
		UgpWithCvCompFlow.T_fa(f) = UgpWithCvCompFlow.temp(icv0) ;
		UgpWithCvCompFlow.vel_fa(f) = rhou(icv0) / rho(icv0) ;
		UgpWithCvCompFlow.p_fa(f) = p_bc ;
	}

	def SymmetryBoundary( f: Face ) {
		val icv0 = getInsideBoundaryCell(f) ;
		var nVec = MeshGeometryCalc.fa_normal(f) ;
		//val area = nVec.length() ;
		nVec = normalize(nVec) ;
		
		// flip u, APPROXIMATION ---> take velocity at the cell center
		val u0 = UgpWithCvCompFlow.vel(icv0) ;
		val un = dot( nVec, u0 ) ;
		UgpWithCvCompFlow.vel_fa(f) = u0 - 1.f * un * nVec ;

		UgpWithCvCompFlow.T_fa(f) = UgpWithCvCompFlow.temp(icv0) ;
		UgpWithCvCompFlow.p_fa(f) = UgpWithCvCompFlow.press(icv0) ;
		UgpWithCvCompFlow.RoM_fa(f) = UgpWithCvCompFlow.RoM(icv0) ;
		UgpWithCvCompFlow.gam_fa(f) = UgpWithCvCompFlow.gamma(icv0) ;
		UgpWithCvCompFlow.mu_fa(f) = UgpWithCvCompFlow.muLam(icv0) ;
		UgpWithCvCompFlow.lamOcp_fa(f) = UgpWithCvCompFlow.LambdaOverCp(icv0) ;
		UgpWithCvCompFlow.h_fa(f) = UgpWithCvCompFlow.enthalpy(icv0) ;	

//		if ( ID(f) == 17777 || ID(f) == 95500 || ID(f) == 94000 || ID(f) == 94500 || ID(f) == 60000 ) {
//			Print( "Symmetry face: ", ID(f), " vel: ", u0 - 1.f * un * nVec, " T: ", UgpWithCvCompFlow.temp(icv0), " p: ", UgpWithCvCompFlow.press(icv0), " RoM: ", UgpWithCvCompFlow.RoM(icv0), "gam: ", UgpWithCvCompFlow.gamma(icv0), "mu: ", UgpWithCvCompFlow.muLam(icv0), " lamOcp: ", UgpWithCvCompFlow.LambdaOverCp(icv0), " h: ", UgpWithCvCompFlow.enthalpy(icv0) ) ;
//		}

	}

	def setRhoFa( f: Face ) {
		UgpWithCvCompFlow.rho_fa(f) = UgpWithCvCompFlow.p_fa(f) / ( UgpWithCvCompFlow.RoM_fa(f) * UgpWithCvCompFlow.T_fa(f) ) ;
	}

	def calcRhsInterior( f: Face, isImplicit: Boolean ) {
		val icv0 = outside(f) ;
		val icv1 = inside(f) ;

		val rho0 = rho(icv0) ;
		val u0 = UgpWithCvCompFlow.vel(icv0) ;
		val p0 = UgpWithCvCompFlow.press(icv0) ;
		val h0 = UgpWithCvCompFlow.enthalpy(icv0) ;
		val T0 = UgpWithCvCompFlow.temp(icv0) ;
		val gam0 = UgpWithCvCompFlow.gamma(icv0) ;
		val R0 = UgpWithCvCompFlow.RoM(icv0) ;

		val rho1 = rho(icv1) ;
		val u1 = UgpWithCvCompFlow.vel(icv1) ;
		val p1 = UgpWithCvCompFlow.press(icv1) ;
		val h1 = UgpWithCvCompFlow.enthalpy(icv1) ;
		val T1 = UgpWithCvCompFlow.temp(icv1) ;
		val gam1 = UgpWithCvCompFlow.gamma(icv1) ;
		val R1 = UgpWithCvCompFlow.RoM(icv1) ;

		// face unit normal and area...
		var nVec = MeshGeometryCalc.fa_normal(f) ;
		val area = sqrt(dot(nVec,nVec)) ;
		nVec = normalize(nVec) ;
	
		val kine0 = UgpWithCvCompFlow.kine(icv0) ;
		val kine1 = UgpWithCvCompFlow.kine(icv1) ;

		//Frho5[_0] = Frho, Frho5[_1] = Frhou.x , Frho5[_2] = Frhou.y, Frho5[_3] = Frhou.y, Frho5[_4] = FrhoE
		val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(rho0, u0, p0, h0, gam0, rho1, u1, p1, h1, gam1, area, nVec, 0.f, kine0, kine1) ;
		
		rhs_rho(icv0) -= Frho5(_0) ;
		rhs_rhou(icv0) -= Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv0) -= Frho5(_4) ;

		rhs_rho(icv1) += Frho5(_0) ;
		rhs_rhou(icv1) += Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv1) += Frho5(_4) ;

		
		if (isImplicit) {
			val jacobians = UgpWithCvCompFlow.calcEulerFluxMatrices_HLLC(true, true, rho0, u0, p0, T0, h0, R0, gam0, kine0, rho1, u1, p1, T1, h1, R1, gam1, kine1, area, nVec, 0.f) ;
			AplMatrixStorageField(f) = jacobians.x ;
			AmiMatrixStorageField(f) = jacobians.y ;	
		}

	}

	def calcRhsSymmetry( f: Face, isImplicit: Boolean) {
		val icv0 = getInsideBoundaryCell(f) ;

		var nVec = MeshGeometryCalc.fa_normal(f) ;
		val area = sqrt(dot(nVec,nVec)) ;
		nVec = normalize(nVec) ;
	
		val kine0 = UgpWithCvCompFlow.kine(icv0) ;

		val rho0 = UgpWithCvCompFlow.rho_fa(f) ;
		val u0 = UgpWithCvCompFlow.vel_fa(f) ;
		val p0 = UgpWithCvCompFlow.p_fa(f) ;
		val h0 = UgpWithCvCompFlow.h_fa(f) ;
		val gam0 = UgpWithCvCompFlow.gam_fa(f) ;
		val T0 = UgpWithCvCompFlow.T_fa(f) ;
		val R0 = UgpWithCvCompFlow.RoM_fa(f) ;

		val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(rho0, u0, p0, h0, gam0, rho0, u0, p0, h0, gam0, area, nVec, 0.f, kine0, kine0) ;

		rhs_rho(icv0) -= Frho5(_0) ;
		rhs_rhou(icv0) -= Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv0) -= Frho5(_4) ;

		if ( isImplicit ) {
			val jacobians = UgpWithCvCompFlow.calcEulerFluxMatrices_HLLC(true, false, rho0, u0, p0, T0, h0, R0, gam0, kine0, rho0, u0, p0, T0, h0, R0, gam0, kine0, area, nVec, 0.f) ;
			AplMatrixStorageField(f) = jacobians.x ;
			AmiMatrixStorageField(f) = jacobians.y ;
		}
	
	}

	def calcRhsOtherBoundaries( f: Face, isImplicit: Boolean ) {
		val icv0 = getInsideBoundaryCell(f) ;

		val rho0 = rho(icv0) ;
		val u0 = UgpWithCvCompFlow.vel(icv0) ;
		val p0 = UgpWithCvCompFlow.press(icv0) ;
		val h0 = UgpWithCvCompFlow.enthalpy(icv0) ;
		val gam0 = UgpWithCvCompFlow.gamma(icv0) ;
		val T0 = UgpWithCvCompFlow.temp(icv0) ;
		val R0 = UgpWithCvCompFlow.RoM(icv0) ;

		val rho1 = UgpWithCvCompFlow.rho_fa(f) ;
		val u1 = UgpWithCvCompFlow.vel_fa(f) ;
		val p1 = UgpWithCvCompFlow.p_fa(f) ;
		val h1 = UgpWithCvCompFlow.h_fa(f) ;
		val gam1 = UgpWithCvCompFlow.gam_fa(f) ;
		val T1 = UgpWithCvCompFlow.T_fa(f) ;
		val R1 = UgpWithCvCompFlow.RoM_fa(f) ;

		// face unit normal and area...
		var nVec = MeshGeometryCalc.fa_normal(f) ;
		val area = sqrt(dot(nVec,nVec)) ;
		nVec = normalize(nVec) ;
	
		val kine0 = UgpWithCvCompFlow.kine(icv0) ;
		val kine1 = UgpWithCvCompFlow.kine(icv0) ;

		//Frho5[_0] = Frho, Frho5[_1] = Frhou.x , Frho5[_2] = Frhou.y, Frho5[_3] = Frhou.y, Frho5[_4] = FrhoE
		val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(rho0, u0, p0, h0, gam0, rho1, u1, p1, h1, gam1, area, nVec, 0.f, kine0, kine1) ;
		
		rhs_rho(icv0) -= Frho5(_0) ;
		rhs_rhou(icv0) -= Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv0) -= Frho5(_4) ;

		if ( isImplicit ) {
			val jacobians = UgpWithCvCompFlow.calcEulerFluxMatrices_HLLC(true, false, rho0, u0, p0, T0, h0, R0, gam0, kine0, rho1, u1, p1, T1, h1, R1, gam1, kine1, area, nVec, 0.f) ;
			AplMatrixStorageField(f) = jacobians.x ;
			AmiMatrixStorageField(f) = jacobians.y ;
		}

	}

	// this function really belongs to UgpWithCvCompFlow, but we cannot read and write fields in the same loop (dq), so we have to place the function here for the moment
	def solveCoupledLinSysNS() {
		if ( IC.linearSolverNS == LinearSolversNS.PETSC_GMRES ) {
			val solution = A / rhs ;
			for( c <- cells(mesh) ) {
				dq(c) = Vec( solution(c & 0), solution(c & 1), solution(c & 2), solution(c & 3), solution(c & 4) ) ;
			}
		} else if ( IC.linearSolverNS == LinearSolversNS.BCGSTAB ) {
			// not implemented
		} else {
			Print( "Error: unrecognized solver: ", IC.linearSolverNS ) ;
		}
	}


}

