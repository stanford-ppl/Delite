package ppl.apps.liszt.Joe

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object UgpWithCvCompFlowRunner extends DeLisztApplicationRunner with UgpWithCvCompFlow

object UgpWithCvCompFlow extends DeLisztApplication {
	
	val vel = FieldWithConst[Cell,Vec[_3,Float]](Constants.float3_zero) ;
	val press = FieldWithConst[Cell,Float](0.f) ;
	val temp = FieldWithConst[Cell,Float](0.f) ;
	val enthalpy = FieldWithConst[Cell,Float](0.f) ;
	val LambdaOverCp = FieldWithConst[Cell,Float](0.f) ;
	val sos = FieldWithConst[Cell,Float](0.f) ;
	val kine = FieldWithConst[Cell,Float](0.f) ;
	val local_dt = FieldWithConst[Cell,Float](0.f) ;

	// these fields should be build in "initialHookScalarRansCombModel(), so we got rid of the whole function
	val RoM = FieldWithConst[Cell,Float](IC.R_gas) ;
	val gamma = FieldWithConst[Cell,Float](IC.GAMMA) ;
	val muLam = FieldWithConst[Cell,Float](0.f) ;
	
	// this should be called in runExplicitBackwardEuler
	val gam_fa = FieldWithConst[Face,Float](0.f) ;
	val RoM_fa = FieldWithConst[Face,Float](0.f) ;
	val h_fa = FieldWithConst[Face,Float](0.f) ;
	val T_fa = FieldWithConst[Face,Float](0.f) ;
	val vel_fa = FieldWithConst[Face,Vec[_3,Float]](Constants.float3_zero) ;
	val p_fa = FieldWithConst[Face,Float](0.f) ;
	val mu_fa = FieldWithConst[Face,Float](0.f) ;
	val lamOcp_fa = FieldWithConst[Face,Float](0.f) ;
	val rho_fa = FieldWithConst[Face,Float](0.f) ;

	def calcRansStateVarAndMaterialProperties() {
		for ( c <- cells(mesh) ) {
			if ( JoeWithModels.rho(c) <= 0.0f ) {
				Print( "negative density at xcv: " , MeshGeometryCalc.x_cv(c) , " " , ID(c) ) ;
			}

			vel(c) = JoeWithModels.rhou(c) / JoeWithModels.rho(c) ;
			
			var kinecv = 0.f ;
			kinecv = kine(c) ;

			val pr = ( gamma(c) - 1.f ) * ( JoeWithModels.rhoE(c) - 0.5f * dot(JoeWithModels.rhou(c),JoeWithModels.rhou(c)) / JoeWithModels.rho(c) - JoeWithModels.rho(c) * kinecv ) ;
			if ( pr <= 0.f ) {
				Print( "negative pressure at xcv: " , MeshGeometryCalc.x_cv(c), " ", ID(c) ) ;
			} else {
				press(c) = pr ;
			}
			
			val tp = pr / ( JoeWithModels.rho(c) * RoM(c) ) ;
			temp(c) = tp ;
			enthalpy(c) = gamma(c) * RoM(c) / ( gamma(c) - 1.f ) * tp ;
			sos(c) = sqrt( gamma(c) * pr / JoeWithModels.rho(c) ) ;
//if (ID(c) == 6806) {
// 	Print( "CALCRANSSTATE c: " , ID(c), "	x_cv: ", MeshGeometryCalc.x_cv(c), " gamma: ", gamma(c), " press: ", pr, " rho: ", JoeWithModels.rho(c), " rhou: ", JoeWithModels.rhou(c), " rhoE: ", JoeWithModels.rhoE(c), " RoM: ", RoM(c), " kine: ", kine(c), " temp: ", tp, " enthalpy ", gamma(c) * RoM(c) / ( gamma(c) - 1.f ) * tp, " sos: ", sqrt( gamma(c) * pr / JoeWithModels.rho(c) ) );
//}
		}
	} 

	def ComputeBCProperties_T( f: Face ) {
		gam_fa(f) = IC.GAMMA ;
		RoM_fa(f) = IC.R_gas ;
		h_fa(f) = IC.GAMMA * IC.R_gas / ( IC.GAMMA - 1.f ) * T_fa(f) ;
	}
	

	def ComputeGeometry(){
		for( f <- faces(mesh) ){
			MeshGeometryCalc.calcFaceGeom(f, true) ;
		}
		for( c <- cells(mesh) ){
			MeshGeometryCalc.calcCellGeom(c) ;
		}  
	}

	def init() {
		Print("UgpWithCvCompFlow()") ;

		// ----------------------------------------------------------------------------------------
    		// write some parameters on the screen
    		// ----------------------------------------------------------------------------------------
		
		Print("") ;
		Print("--------------------------------------------") ;
		Print( "Gas properties         " ) ;
		Print("    GAMMA            : ", IC.GAMMA );
      		Print("    R_GAS            : ", IC.R_gas );
      		Print("    P_REF            : ", IC.p_init );
      		Print("    RHO_REF          : ", IC.rho_init );
      		Print("    T_REF            : ", IC.T_init );
      		Print("    SOS_REF          : ", sqrt(IC.GAMMA*IC.R_gas*IC.T_init) );
      		Print("Solver settings        ");
      		Print("    nsteps           : ", Constants.nsteps);
      		Print("    timeStepMode     : ", IC.timeStepMode);
      		Print("--------------------------------------------") ;
		Print("") ;
		ComputeGeometry() ;
		
		// HACK!!!!
		// TODO(Montse):: Remove this once Zach fixes the bug for initiallizing Fields with a constant different to zero!!!!

		for ( c <- cells(mesh) ) {
			gamma(c) = IC.GAMMA ;
			RoM(c) = IC.R_gas ;

			JoeWithModels.rho(c) = IC.rho_init ;
        		JoeWithModels.rhou(c) = IC.rho_init * IC.u_init ;
        		JoeWithModels.rhoE(c) = IC.p_init / (IC.GAMMA - 1.f)  +  0.5f * IC.rho_init * dot( IC.u_init, IC.u_init ) ;
		}	
	}

	def calcEulerFlux_HLLC( rhoL : Float, uL : Vec[_3,Float], pL : Float, h0 : Float, gammaL : Float, rhoR : Float, uR : Vec[_3,Float], pR : Float, h1 : Float, gammaR : Float, area : Float, nVec : Vec[_3,Float], surfVeloc : Float, kL : Float, kR : Float ) : Vec[_5,Float] = {
		
		var Frho = 0.f ;
		var Frhou = Constants.float3_zero ;
		var FrhoE = 0.f ;		
		
		val unL = dot(uL,nVec) ;
		val uLuL = dot(uL,uL) ;
		val cL = sqrt( gammaL * pL / rhoL ) ;
		val hL = gammaL / ( gammaL - 1.f ) * pL / rhoL + 0.5f * uLuL + kL ;
		val eL = hL * rhoL - pL ;

		val unR = dot(uR,nVec) ;
		val uRuR = dot(uR,uR) ;
		val cR = sqrt( gammaR * pR / rhoR ) ;
		val hR = gammaR / ( gammaR - 1.f ) * pR / rhoR + 0.5f * uRuR + kR ;
		val eR = hR * rhoR - pR ;

		// Roe's averaging
		val Rrho = sqrt( rhoR / rhoL ) ;
		val tmp = 1.f / ( 1.f + Rrho ) ;
		val velRoe = tmp * ( uL + uR *Rrho ) ;
		val uRoe = dot( velRoe, nVec ) ;
		//val hRoe = tmp * ( hL + hR * Rrho ) ;
		
		//val cRoe = sqrt( (gammaL - 1.f) * ( hRoe - 0.5f * dot( velRoe, velRoe ) ) ) ;
		val gamPdivRho = tmp * ( (gammaL * pL / rhoL + 0.5f * (gammaL - 1.f) * uLuL ) + (gammaR * pR / rhoR + 0.5f * (gammaR - 1.f) * uRuR ) * Rrho ) ;
		val cRoe = sqrt( gamPdivRho - ((gammaL + gammaR) * 0.5f - 1.f ) * 0.5f * dot( velRoe, velRoe ) );

		// speed of sound at L and R 
		val sL = (uRoe - cRoe).min(unL - cL) ;
		val sR = (uRoe + cRoe).max(unR + cR) ;

		// speed of contact surface
		val sM = (pL - pR - rhoL * unL * (sL - unL) + rhoR * unR * (sR - unR) ) / (rhoR * (sR - unR) - rhoL * (sL - unL)) ;

		// pressure at right and left (pR =pL ) side contact surfaca
		val pStar = rhoR * (unR - sR) * (unR - sM) + pR ;

		if (sM >= 0.f) {
			if (sL > 0.f) {
				Frho = rhoL * unL ;
				Frhou = rhoL * unL * uL + pL * nVec ;
				FrhoE = (eL + pL) * unL ;
			} else {
				val invSLmSs = 1.f / (sL - sM ) ;
				val sLmuL = sL - unL ;
				val rhoSL = rhoL * sLmuL * invSLmSs ;
				val rhouSL = (( rhoL * sLmuL) * uL + (pStar - pL) * nVec) * invSLmSs ;
				val eSL = ( sLmuL * eL - pL * unL + pStar * sM ) * invSLmSs ;

				Frho = rhoSL * sM ;
				Frhou = rhouSL * sM + pStar * nVec ;
				FrhoE = ( eSL + pStar ) * sM ; 
			}
		} else {
			if ( sR >= 0.f ) {
				val invSRmSs = 1.f / (sR - sM ) ;
				val sRmuR = sR - unR ;
				val rhoSR = rhoR * sRmuR * invSRmSs ;
				val rhouSR = (( rhoR * sRmuR) * uR + (pStar - pR) * nVec) * invSRmSs ;
				val eSR = ( sRmuR * eR - pR * unR + pStar * sM ) * invSRmSs ;

				Frho = rhoSR * sM ;
				Frhou = rhouSR * sM + pStar * nVec ;
				FrhoE = ( eSR + pStar ) * sM ;
			} else {
				Frho = rhoR * unR ;
				Frhou = rhoR * unR * uR + pR * nVec ;
				FrhoE = (eR + pR) * unR ;
			}
		}

		Frho *= area ;
		Frhou *= area ;
		FrhoE *= area ;
		
		return Vec(Frho, Frhou.x, Frhou.y, Frhou.z, FrhoE) ;		
	}

	def calcDt( cfl_target : Float ) : Float = {
		var dt = 0.f ;

		if ( IC.timeStepMode == 0 ) {
			return IC.const_dt ;
		}
		if ( IC.timeStepMode == 1 ) {
			dt = IC.DT ;
			IC.const_dt = IC.DT ;
			for( c <- cells(mesh) ) {
				local_dt(c) = dt ;
			}
			IC.timeStepMode = 0 ;
		}
		if ( IC.timeStepMode == 2 ) {
			for( icv <- cells(mesh) ) {
				var dt_cv = MAX_FLOAT ;
				var lambdaMax = 0.f ;

				val c = sqrt( gamma(icv) * press(icv) / JoeWithModels.rho(icv) ) ;

				for ( f <- faces(icv) ) {
					var nVec = MeshGeometryCalc.fa_normal(f) ;
					val area = sqrt(dot(nVec, nVec)) ;
					nVec = normalize(nVec) ;

					val Uk = dot( JoeWithModels.rhou(icv), nVec ) / JoeWithModels.rho(icv) ;
					val lambda = ( fabs(Uk) + c ) * area ;
					lambdaMax = lambdaMax.max(lambda) ;
				}
		
				dt_cv = cfl_target * MeshGeometryCalc.cv_volume(icv) / lambdaMax ;
				IC.dt_minCPU = IC.dt_minCPU.min(dt_cv) ;
				local_dt(icv) = dt_cv ;
			}
			dt = IC.dt_minCPU ;
		}
		return dt ;
	}

}


