package ppl.apps.liszt.Joe

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

	val zone_interior = BoundarySet[Face]("zone_interior")
	val zone_boundary = BoundarySet[Face]("zone_boundary")
	val symmetry = BoundarySet[Face]("symmetry")
	val wall = BoundarySet[Face]("wall")
	val other_boundaries = BoundarySet[Face]("other_boundaries")
	val cbc = BoundarySet[Face]("cbc")
	val hook = BoundarySet[Face]("hook")
	val cbc_subsonic_outlet = BoundarySet[Face]("cbc_subsonic_outlet")

	val ScatterCount = FieldWithConst[Cell,Int](0);
	
	def init() { Print("JoeWithModels()") ;}
	def initialHook() {Print("setInitialConditions()") ;}


	// TODO(Montse):: HACK!!!! NEED TO MODIFY THE REFERENCE OUTSIDE CELL!!!! MIGHT NOT BE ZERO!!!!
	def getInsideBoundaryCell( f : Face ) : Cell = {
		val c = if ( ID(inside(f)) == 0 ) outside(f) else inside(f) ;
		return c ;
	}	

	def run() {
		initialHook() ;
		
		if ( IC.navierStokesSolver == NavierStokesSolvers.EXPL_EULER ){ 
			runExplicitBackwardEuler() ;
		} else if ( IC.navierStokesSolver == NavierStokesSolvers.IMPL_EULER ){
		} else if ( IC.navierStokesSolver == NavierStokesSolvers.EXPL_RK ){
		} else { Print("ERROR: no or wrong time integration scheme specified !") ; }
	}

	def runExplicitBackwardEuler() {

		// TODO(Montse):: HACK, HACK, HACK!!!! CHANGE THAT VEC< 5, VEC< 5,FLOAT>> TO MATRIX<5,5,FLOAT> !!!!!!!!!!!!!!!!!!!!!
		val zeroFiveVec = Vec(0.f, 0.f, 0.f, 0.f, 0.f) ;
		val dummyA = Vec( zeroFiveVec, zeroFiveVec, zeroFiveVec, zeroFiveVec, zeroFiveVec) ;

		UgpWithCvCompFlow.calcRansStateVarAndMaterialProperties() ;
		
 
		setNavierStokesBC() ;
		
		// HERE TIME SHOULD START TICKING !!!!!!
		var start_time = 0.0;
		var step = 0 ;
		while (step < Constants.nsteps ) {
			if (step == 1) {
				start_time = wall_time();
			}
			//Print("looping: ", step) ;
			step += 1 ;

			var my_resid_rho = 0.f ;
			var my_resid_rhou = Constants.float3_zero ;
			var my_resid_rhoE = 0.f ;

			var dtMin = UgpWithCvCompFlow.calcDt(IC.cfl) ;
			calcRhs(dummyA) ; // note that this function originally would take rhs_rho, rhs_rhou, rhs_rhoE, rho, rhou, rhoE fields as arguments
			
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

		 Print( "TIME_FOR_LOOP: ", wall_time() - start_time ) ;
			
	}



	def calcRhs( A : Vec[_5, Vec[_5,Float]] ) {
		for( c <- cells(mesh) ) {
			rhs_rho(c) = 0.f ;
			rhs_rhou(c) =  Constants.float3_zero ;
			rhs_rhoE(c) = 0.f ;
		}
  		// count how switched back to first order due to extrapolated negative pressure at the faces 

		for (f <- zone_interior) { calcRhsInterior(f) }
		for (f <- symmetry) { calcRhsSymmetry(f) }
		// Ignore wall...
		for (f <- other_boundaries) { calcRhsOtherBoundaries(f) }
	}




	def setNavierStokesBC() {
		//IC.first = true ;
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
		var sCount = 0;
		var checkCount = 0;
		for (c <- cells(mesh)) {
			sCount += ScatterCount(c)
			checkCount += size(faces(c))
		}
		if ( step % (IC.check_interval * 10 ) == 0 ) {
			Print( "		rho		rhou-X		rhou-Y		rhou-Z		rhoE		ScatterCounts") ;
		}
		Print( "RESID: ", step, "		", rhsResid(_0), "		", rhsResid(_1), "		", rhsResid(_2), "		", rhsResid(_3), "		", rhsResid(_4), "		", sCount, "==", checkCount ) ;
	}

	
	def CBCBoundary( f: Face, T_bc : Float, p_bc : Float, u_bc : Vec[_3,Float] ) {
//if ( ID(f) == 95000 ) {
//	Print( "CBC face with Id: ", ID(f), " T: ", T_bc, " u: ", u_bc, " p: ", p_bc ) ;
//}
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

//if ( ID(f) == 17777 || ID(f) == 95500 || ID(f) == 94000 || ID(f) == 94500 || ID(f) == 60000 ) {
//Print( "Symmetry face: ", ID(f), " vel: ", u0 - 1.f * un * nVec, " T: ", UgpWithCvCompFlow.temp(icv0), " p: ", UgpWithCvCompFlow.press(icv0), " RoM: ", UgpWithCvCompFlow.RoM(icv0), "gam: ", UgpWithCvCompFlow.gamma(icv0), "mu: ", UgpWithCvCompFlow.muLam(icv0), " lamOcp: ", UgpWithCvCompFlow.LambdaOverCp(icv0), " h: ", UgpWithCvCompFlow.enthalpy(icv0) ) ;
//}
	}

	def setRhoFa( f: Face ) {
		UgpWithCvCompFlow.rho_fa(f) = UgpWithCvCompFlow.p_fa(f) / ( UgpWithCvCompFlow.RoM_fa(f) * UgpWithCvCompFlow.T_fa(f) ) ;
	}

	def calcRhsInterior( f: Face ) {
		val icv0 = outside(f) ;
		val icv1 = inside(f) ;

		val rho0 = rho(icv0) ;
		val u0 = UgpWithCvCompFlow.vel(icv0) ;
		val p0 = UgpWithCvCompFlow.press(icv0) ;
		val h0 = UgpWithCvCompFlow.enthalpy(icv0) ;

		val rho1 = rho(icv1) ;
		val u1 = UgpWithCvCompFlow.vel(icv1) ;
		val p1 = UgpWithCvCompFlow.press(icv1) ;
		val h1 = UgpWithCvCompFlow.enthalpy(icv1) ;

		// face unit normal and area...
		var nVec = MeshGeometryCalc.fa_normal(f) ;
		val area = sqrt(dot(nVec,nVec)) ;
		nVec = normalize(nVec) ;
	
		val kine0 = UgpWithCvCompFlow.kine(icv0) ;
		val kine1 = UgpWithCvCompFlow.kine(icv1) ;

		//Frho5[_0] = Frho, Frho5[_1] = Frhou.x , Frho5[_2] = Frhou.y, Frho5[_3] = Frhou.y, Frho5[_4] = FrhoE
		val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(rho0, u0, p0, h0, UgpWithCvCompFlow.gamma(icv0), rho1, u1, p1, h1, UgpWithCvCompFlow.gamma(icv1), area, nVec, 0.f, kine0, kine1) ;
		
		rhs_rho(icv0) -= Frho5(_0) ;
		rhs_rhou(icv0) -= Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv0) -= Frho5(_4) ;

		rhs_rho(icv1) += Frho5(_0) ;
		rhs_rhou(icv1) += Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv1) += Frho5(_4) ;

		ScatterCount(icv0) += 1;
		ScatterCount(icv1) += 1;

	}

	def calcRhsSymmetry( f: Face) {
		val icv0 = getInsideBoundaryCell(f) ;

		var nVec = MeshGeometryCalc.fa_normal(f) ;
		val area = sqrt(dot(nVec,nVec)) ;
		nVec = normalize(nVec) ;
	
		val kine0 = UgpWithCvCompFlow.kine(icv0) ;
		val kine1 = UgpWithCvCompFlow.kine(icv0) ;

		val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(UgpWithCvCompFlow.rho_fa(f), UgpWithCvCompFlow.vel_fa(f), UgpWithCvCompFlow.p_fa(f), UgpWithCvCompFlow.h_fa(f), UgpWithCvCompFlow.gam_fa(f), 
			UgpWithCvCompFlow.rho_fa(f), UgpWithCvCompFlow.vel_fa(f), UgpWithCvCompFlow.p_fa(f), UgpWithCvCompFlow.h_fa(f), UgpWithCvCompFlow.gam_fa(f), area, nVec, 0.f, kine0, kine1) ;

		rhs_rho(icv0) -= Frho5(_0) ;
		rhs_rhou(icv0) -= Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv0) -= Frho5(_4) ;
		
		ScatterCount(icv0) += 1;
	
	}

	def calcRhsOtherBoundaries( f: Face ) {
		val icv0 = getInsideBoundaryCell(f) ;

		val rho0 = rho(icv0) ;
		val u0 = UgpWithCvCompFlow.vel(icv0) ;
		val p0 = UgpWithCvCompFlow.press(icv0) ;
		val h0 = UgpWithCvCompFlow.enthalpy(icv0) ;
		val gam0 = UgpWithCvCompFlow.gamma(icv0) ;


		// face unit normal and area...
		var nVec = MeshGeometryCalc.fa_normal(f) ;
		val area = sqrt(dot(nVec,nVec)) ;
		nVec = normalize(nVec) ;
	
		val kine0 = UgpWithCvCompFlow.kine(icv0) ;
		val kine1 = UgpWithCvCompFlow.kine(icv0) ;

		//Frho5[_0] = Frho, Frho5[_1] = Frhou.x , Frho5[_2] = Frhou.y, Frho5[_3] = Frhou.y, Frho5[_4] = FrhoE
		val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(rho0, u0, p0, h0, gam0, UgpWithCvCompFlow.rho_fa(f), UgpWithCvCompFlow.vel_fa(f), UgpWithCvCompFlow.p_fa(f), UgpWithCvCompFlow.h_fa(f), UgpWithCvCompFlow.gam_fa(f), area, nVec, 0.f, kine0, kine1) ;
		
		rhs_rho(icv0) -= Frho5(_0) ;
		rhs_rhou(icv0) -= Vec(Frho5(_1), Frho5(_2), Frho5(_3)) ;
		rhs_rhoE(icv0) -= Frho5(_4) ;

		ScatterCount(icv0) += 1;


	}

}
