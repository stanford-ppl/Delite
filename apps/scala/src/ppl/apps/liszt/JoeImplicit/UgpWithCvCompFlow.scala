package ppl.apps.liszt.JoeImplicit

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object UgpWithCvCompFlowRunner extends DeLisztApplicationRunner with UgpWithCvCompFlow

object UgpWithCvCompFlow extends DeLisztExp {
	
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

	def assignFiveFiveMatrix( v0: Vec[_5,Float], v1: Vec[_5,Float], v2: Vec[_5,Float], v3: Vec[_5,Float], v4: Vec[_5,Float] ) : Mat[_5,_5,Float] = {
		return Mat( Vec( v0(_0), v0(_1), v0(_2), v0(_3), v0(_4)),
				Vec( v1(_0), v1(_1), v1(_2), v1(_3), v1(_4)),
				Vec( v2(_0), v2(_1), v2(_2), v2(_3), v2(_4)),
				Vec( v3(_0), v3(_1), v3(_2), v3(_3), v3(_4)),
				Vec( v4(_0), v4(_1), v4(_2), v4(_3), v4(_4)) ) ;
	}

	def calcRansStateVarAndMaterialProperties() {
		for ( c <- cells(mesh) ) {
			if ( JoeWithModels.rho(c) <= 0.0 ) {
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
// DEBUGGING!!!!!
//if (ID(c) == 6805) {
 //	Print( "CALCRANSSTATE c: " , ID(c), " x_cv: ", MeshGeometryCalc.x_cv(c)," gamma: ", gamma(c), " press: ", pr, " rho: ", JoeWithModels.rho(c), " rhou: ", JoeWithModels.rhou(c), " rhoE: ", JoeWithModels.rhoE(c), " RoM: ", RoM(c), " kine: ", kine(c), " temp: ", tp, " enthalpy ", gamma(c) * RoM(c) / ( gamma(c) - 1.f ) * tp, " sos: ", sqrt( gamma(c) * pr / JoeWithModels.rho(c) ) );
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


	def calcEulerFluxMatrices_HLLC(computeA_L: Boolean, computeA_R: Boolean, rhoL : Float, uL : Vec[_3,Float], pL : Float, TL : Float, h0 : Float, R0 : Float, gammaL : Float, kL : Float, 
		rhoR : Float, uR : Vec[_3,Float], pR : Float, TR : Float, h1 : Float, R1 : Float, gammaR : Float, kR : Float, area : Float, nVec : Vec[_3,Float], surfVeloc : Float) : Vec[_2, Mat[_5,_5,Float]] = {

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
		val hRoe = tmp * ( hL + hR * Rrho ) ;		
		
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

		var A_L = Constants.zeroDenseFiveMatrix ;
		var A_R = Constants.zeroDenseFiveMatrix ;

		if (sM >= 0.f) {
			if (sL > 0.f) {
				if ( computeA_L ) {
					val nVecArea = area*nVec ;
					A_L = calcJacobianA( uL, pL, rhoL, nVecArea, 0.5f * (gammaL + gammaR), 0.f ) ;
				}
			} else {
				val invSLmSs = 1.f / (sL - sM ) ;
				val sLmuL = sL - unL ;
				val rhoSL = rhoL * sLmuL * invSLmSs ;
				val rhouSL = (( rhoL * sLmuL) * uL + (pStar - pL) * nVec) * invSLmSs ;
				val eSL = ( sLmuL * eL - pL * unL + pStar * sM ) * invSLmSs ;
				val gammaLM1 = gammaL - 1.f ;
				val gammaRM1 = gammaR - 1.f ;
				val invrhotld = 1.f / ( rhoR * ( sR - unR ) - rhoL * ( sL - unL ) ) ;

				var dSMdUL = Constants.zeroFiveVec ;
				var dpsdUL = Constants.zeroFiveVec ;
				var dSMdUR = Constants.zeroFiveVec ;
				var dpsdUR = Constants.zeroFiveVec ;

				if (computeA_L) {
					val tmp0 = -1.f * unL * unL + uLuL * gammaLM1 / 2.f + sM * sL ;
					val tmp1 = nVec * ( 2.f * unL - sL - sM ) - gammaLM1 * uL ;
					dSMdUL = invrhotld * Vec( tmp0  , tmp1.x, tmp1.y, tmp1.z, gammaLM1 ) ;
					dpsdUL = rhoR * ( sR - unR ) * dSMdUL ;
				}

				if (computeA_R) {
					val tmp0 = unR * unR - uRuR * gammaRM1 / 2.f - sM * sR ;
					val tmp1 = -1.f * nVec * ( 2.f * unR - sR - sM ) + gammaRM1 * uR ;
					dSMdUR = invrhotld * Vec( tmp0 , tmp1.x, tmp1.y, tmp1.z, -gammaRM1 ) ;
					dpsdUR = rhoL * ( sL - unL ) * dSMdUR ;
//Print( "nVec: ", nVec, " unR: ", unR, " sR: ", sR, " sM: ", sM, " gammaRM1: ", gammaRM1, " uR: ", uR ) ;
//Print( "tmp1: ", tmp1 ) ;
//Print ( "dSMdUR: ", dSMdUR ) ;
				}

				val jacobeans = calcSubSonicJacobeanHLLC( computeA_L, computeA_R, rhoL, uL, pL, eL, unL, uLuL, sL, rhoSL, rhouSL, eSL, dSMdUL, dSMdUR, dpsdUL, dpsdUR, sM, pStar, 0.5f * ( gammaL + gammaR ), nVec ) ;
				A_L = jacobeans.x ;
				A_R = jacobeans.y ;

				if (computeA_L) {
					val tmp0 = Vec(A_L(_0,_0), A_L(_0,_1), A_L(_0,_2), A_L(_0,_3), A_L(_0,_4))*sM + dSMdUL * rhoSL ;
					val tmp1 = Vec(A_L(_1,_0), A_L(_1,_1), A_L(_1,_2), A_L(_1,_3), A_L(_1,_4))*sM + dSMdUL * rhouSL.x + dpsdUL * nVec.x ;
					val tmp2 = Vec(A_L(_2,_0), A_L(_2,_1), A_L(_2,_2), A_L(_2,_3), A_L(_2,_4))*sM + dSMdUL * rhouSL.y + dpsdUL * nVec.y ;
					val tmp3 = Vec(A_L(_3,_0), A_L(_3,_1), A_L(_3,_2), A_L(_3,_3), A_L(_3,_4))*sM + dSMdUL * rhouSL.z + dpsdUL * nVec.z ;
					val tmp4 = ( Vec(A_L(_4,_0), A_L(_4,_1), A_L(_4,_2), A_L(_4,_3), A_L(_4,_4)) + dpsdUL )*sM + (eSL + pStar)*dSMdUL ;
					A_L =  area * assignFiveFiveMatrix(tmp0, tmp1, tmp2, tmp3, tmp4 ) ;
				}
				if (computeA_R) {
					val tmp0 = Vec(A_R(_0,_0), A_R(_0,_1), A_R(_0,_2), A_R(_0,_3), A_R(_0,_4))*sM + dSMdUR * rhoSL ;
					val tmp1 = Vec(A_R(_1,_0), A_R(_1,_1), A_R(_1,_2), A_R(_1,_3), A_R(_1,_4))*sM + dSMdUR * rhouSL.x + dpsdUR * nVec.x ;
					val tmp2 = Vec(A_R(_2,_0), A_R(_2,_1), A_R(_2,_2), A_R(_2,_3), A_R(_2,_4))*sM + dSMdUR * rhouSL.y + dpsdUR * nVec.y ;
					val tmp3 = Vec(A_R(_3,_0), A_R(_3,_1), A_R(_3,_2), A_R(_3,_3), A_R(_3,_4))*sM + dSMdUR * rhouSL.z + dpsdUR * nVec.z ;
					val tmp4 = ( Vec(A_R(_4,_0), A_R(_4,_1), A_R(_4,_2), A_R(_4,_3), A_R(_4,_4)) + dpsdUR )*sM + (eSL + pStar)*dSMdUR ;
					A_R = area * assignFiveFiveMatrix( tmp0, tmp1, tmp2, tmp3, tmp4 ) ;
				}
			}
		} else {
			if ( sR >= 0.f ) {
				val invSRmSs = 1.f / (sR - sM ) ;
				val sRmuR = sR - unR ;
				val rhoSR = rhoR * sRmuR * invSRmSs ;
				val rhouSR = ( rhoR * sRmuR * uR + (pStar - pR) * nVec) * invSRmSs ;
				val eSR = ( sRmuR * eR - pR * unR + pStar * sM ) * invSRmSs ;
				val gammaLM1 = gammaL - 1.f ;
				val gammaRM1 = gammaR - 1.f ;
				val invrhotld = 1.f / ( rhoR * ( sR - unR ) - rhoL * ( sL - unL ) ) ;

				var dSMdUL = Constants.zeroFiveVec ;
				var dpsdUL = Constants.zeroFiveVec ;
				var dSMdUR = Constants.zeroFiveVec ;
				var dpsdUR = Constants.zeroFiveVec ;

				if (computeA_L) {
					val tmp0 = -1.f * unL * unL + uLuL * gammaLM1 / 2.f + sM * sL ;
					val tmp1 = nVec * ( 2.f * unL - sL - sM ) - gammaLM1 * uL ;
					dSMdUL =  invrhotld * Vec( tmp0 , tmp1.x, tmp1.y, tmp1.z, gammaLM1 ) ;
					dpsdUL = rhoR * ( sR - unR ) * dSMdUL ;
				}

				if (computeA_R) {
					val tmp0 = unR * unR - uRuR * gammaRM1 / 2.f - sM * sR ;
					val tmp1 = -1.f * nVec * ( 2.f * unR - sR - sM ) + gammaRM1 * uR ;
					dSMdUR = invrhotld * Vec( tmp0 , tmp1.x, tmp1.y, tmp1.z, -gammaRM1 ) ;
					dpsdUR = rhoL * ( sL - unL ) * dSMdUR ;
				}
				
				val jacobeans = calcSubSonicJacobeanHLLC( computeA_L, computeA_R, rhoR, uR, pR, eR, unR, uRuR, sR, rhoSR, rhouSR, eSR, dSMdUR, dSMdUL, dpsdUR, dpsdUL, sM, pStar, 0.5f * ( gammaL + gammaR ), nVec ) ;
				A_L = jacobeans.x ;
				A_R = jacobeans.y ;

				if (computeA_L) {
					val tmp0 = Vec(A_L(_0,_0), A_L(_0,_1), A_L(_0,_2), A_L(_0,_3), A_L(_0,_4))*sM + dSMdUL * rhoSR ;
					val tmp1 = Vec(A_L(_1,_0), A_L(_1,_1), A_L(_1,_2), A_L(_1,_3), A_L(_1,_4))*sM + dSMdUL * rhouSR.x + dpsdUL * nVec.x ;
					val tmp2 = Vec(A_L(_2,_0), A_L(_2,_1), A_L(_2,_2), A_L(_2,_3), A_L(_2,_4))*sM + dSMdUL * rhouSR.y + dpsdUL * nVec.y ;
					val tmp3 = Vec(A_L(_3,_0), A_L(_3,_1), A_L(_3,_2), A_L(_3,_3), A_L(_3,_4))*sM + dSMdUL * rhouSR.z + dpsdUL * nVec.z ;
					val tmp4 = ( Vec(A_L(_4,_0), A_L(_4,_1), A_L(_4,_2), A_L(_4,_3), A_L(_4,_4)) + dpsdUL )*sM + (eSR + pStar)*dSMdUL ;
					A_L = area * assignFiveFiveMatrix( tmp0, tmp1, tmp2, tmp3, tmp4 ) ;
				}
				if (computeA_R) {
					val tmp0 = Vec(A_R(_0,_0), A_R(_0,_1), A_R(_0,_2), A_R(_0,_3), A_R(_0,_4))*sM + dSMdUR * rhoSR ;
					val tmp1 = Vec(A_R(_1,_0), A_R(_1,_1), A_R(_1,_2), A_R(_1,_3), A_R(_1,_4))*sM + dSMdUR * rhouSR.x + dpsdUR * nVec.x ;
					val tmp2 = Vec(A_R(_2,_0), A_R(_2,_1), A_R(_2,_2), A_R(_2,_3), A_R(_2,_4))*sM + dSMdUR * rhouSR.y + dpsdUR * nVec.y ;
					val tmp3 = Vec(A_R(_3,_0), A_R(_3,_1), A_R(_3,_2), A_R(_3,_3), A_R(_3,_4))*sM + dSMdUR * rhouSR.z + dpsdUR * nVec.z ;
					val tmp4 = ( Vec(A_R(_4,_0), A_R(_4,_1), A_R(_4,_2), A_R(_4,_3), A_R(_4,_4)) + dpsdUR )*sM + (eSR + pStar)*dSMdUR ;
					A_R = area * assignFiveFiveMatrix( tmp0, tmp1, tmp2, tmp3, tmp4 ) ;
				}

			} else {
				if ( computeA_R ) {
					val nVecArea = area*nVec ;
					A_R = calcJacobianA( uR, pR, rhoR, nVecArea, 0.5f * (gammaL + gammaR), 0.f ) ;
				}
			}
		}
		return Vec(A_L, A_R) ;		
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

	def calcJacobianA( vel: Vec[_3,Float], pp: Float, rrho: Float, nV: Vec[_3,Float], gamma: Float, surfVeloc: Float ) : Mat[_5, _5,Float] = {	// nV is not normalized
		
		val kapm1 = gamma - 1.f ;

		val nVel : Vec[_3,Float] = vel * nV ;		
		val U_k = nVel.x + nVel.y + nVel.z ;
		val vSquHlf = 0.5f * dot(vel,vel) ;
		val c = sqrt( gamma * pp / rrho ) ;
		val inv_kap_m1 = 1.f / kapm1 ;

		val A_0 = Vec( -surfVeloc, nV.x, nV.y, nV.z, 0.f ) ;

		val tmp10 = -vel.x * ( nVel.y + nVel.z ) + nV.x * (kapm1 * vSquHlf - vel.x*vel.x) ;
		val tmp11 = ( 2.f - gamma ) * nVel.x + U_k - surfVeloc ;
		val tmp12 = vel.x*nV.y - kapm1 * vel.y * nV.x ;
		val tmp13 = vel.x*nV.z - kapm1 * vel.z * nV.x ;
		val A_1 = Vec( tmp10, tmp11, tmp12, tmp13, kapm1*nV.x ) ;

		val tmp20 = -vel.y * ( nVel.x + nVel.z ) + nV.y * (kapm1 * vSquHlf - vel.y*vel.y ) ;
		val tmp21 = -kapm1 * vel.x * nV.y + vel.y * nV.x ;
		val tmp22 = ( 2.f - gamma ) * nVel.y + U_k - surfVeloc ;
		val tmp23 = vel.y * nV.z - kapm1 * vel.z * nV.y ;
		val A_2 = Vec( tmp20, tmp21, tmp22, tmp23, kapm1 * nV.y ) ;

		val tmp30 = -vel.z * ( nVel.x + nVel.y ) + nV.z * ( kapm1 * vSquHlf - vel.z * vel.z ) ;
		val tmp31 = -kapm1 * vel.x * nV.z + vel.z * nV.x ;
		val tmp32 = -kapm1 * vel.y * nV.z + vel.z * nV.y ;
		val tmp33 = ( 2.f - gamma ) * nVel.z + U_k - surfVeloc ;
		val A_3 = Vec( tmp30, tmp31, tmp32, tmp33, kapm1 * nV.z ) ;

		val tmp40 = U_k * ((gamma - 2.f) * vSquHlf - c*c*inv_kap_m1 ) ;
		val tmp41 = c*c*inv_kap_m1*nV.x - kapm1*vel.x*( nVel.y + nVel.z ) - (kapm1*vel.x*vel.x - vSquHlf) * nV.x ;
		val tmp42 = c*c*inv_kap_m1*nV.y - kapm1*vel.y*( nVel.x + nVel.z ) - (kapm1*vel.y*vel.y - vSquHlf) * nV.y ;
		val tmp43 = c*c*inv_kap_m1*nV.z - kapm1*vel.z*( nVel.x + nVel.y ) - (kapm1*vel.z*vel.z - vSquHlf) * nV.z ;
		val A_4 = Vec( tmp40, tmp41, tmp42, tmp43, gamma * U_k - surfVeloc ) ;

		return assignFiveFiveMatrix( A_0, A_1, A_2, A_3, A_4 ) ;
	}

	def calcSubSonicJacobeanHLLC( computeA_L: Boolean, computeA_R: Boolean, rhoL: Float, uL: Vec[_3,Float], pL: Float, eL: Float, qL: Float, psiL: Float, SL: Float, rhoSL: Float, rhouSL: Vec[_3,Float], eSL: Float, dSMdUL: Vec[_5,Float], 
		dSMdUR: Vec[_5,Float], dpsdUL: Vec[_5,Float], dpsdUR: Vec[_5,Float], SM: Float, pS: Float, gamma: Float, nV: Vec[_3,Float] ) : Vec[_2, Mat[_5,_5,Float]] = {   // nV is not normalized

		val gammaMinus1 = gamma - 1.f ;
		val omL = 1.f / ( SL - SM ) ;

		var A_L = Constants.zeroDenseFiveMatrix ;
		var A_R = Constants.zeroDenseFiveMatrix ;
		if ( computeA_L ) {
			val A_L0 = Vec( SL, -nV.x, -nV.y, -nV.z, 0.f ) + rhoSL * dSMdUL ;

			val tmp10 = 	 qL * uL.x        - nV.x * psiL * gammaMinus1 / 2.f ;
			val tmp11 = SL - qL               + nV.x * ( gamma - 2.f ) * uL.x ;
			val tmp12 =          -uL.x * nV.y + nV.x * gammaMinus1 * uL.y ;
			val tmp13 =          -uL.x * nV.z + nV.x * gammaMinus1 * uL.z ;
			val A_L1 = Vec( tmp10, tmp11, tmp12, tmp13, -gammaMinus1 * nV.x ) + nV.x * dpsdUL + rhouSL.x * dSMdUL ; 

			val tmp20 = 	 qL * uL.y 	  - nV.y * psiL * gammaMinus1 / 2.f ;
			val tmp21 = 	     -uL.y * nV.x + nV.y * gammaMinus1 * uL.x ;
			val tmp22 = SL - qL 		  + nV.y * ( gamma - 2.f ) * uL.y ;
			val tmp23 = 	     -uL.y * nV.z + nV.y * gammaMinus1 * uL.z ;
			val A_L2 = Vec( tmp20, tmp21, tmp22, tmp23, -gammaMinus1 * nV.y ) + nV.y * dpsdUL + rhouSL.y * dSMdUL ; 

			val tmp30 = 	 qL * uL.z 	  - nV.z * psiL * gammaMinus1 / 2.f ;
			val tmp31 = 	     -uL.z * nV.x + nV.z * gammaMinus1 * uL.x ;
			val tmp32 = 	     -uL.z * nV.y + nV.z * gammaMinus1 * uL.y ;
			val tmp33 = SL - qL 		  + nV.z * ( gamma - 2.f ) * uL.z ;
			val A_L3 = Vec( tmp30, tmp31, tmp32, tmp33, -gammaMinus1 * nV.z ) + nV.z * dpsdUL + rhouSL.z * dSMdUL ; 

			val tmp40 = qL * ( eL + pL ) / rhoL - qL * psiL * ( gamma - 1.f ) / 2.f ;
			val tmp4_ = -nV * (eL + pL) / rhoL + gammaMinus1 * uL * qL ;
			val A_L4 = Vec( tmp40, tmp4_.x, tmp4_.y, tmp4_.z, SL - qL * gamma ) + SM * dpsdUL + (pS + eSL) * dSMdUL ;

			A_L = omL * assignFiveFiveMatrix( A_L0, A_L1, A_L2, A_L3, A_L4 ) ;
	
		}
		if ( computeA_R ) {
			val tmp0 = rhoSL*dSMdUR ;
			val tmp1 = nV.x * dpsdUR + rhouSL.x*dSMdUR ;
			val tmp2 = nV.y * dpsdUR + rhouSL.y*dSMdUR ;
			val tmp3 = nV.z * dpsdUR + rhouSL.z*dSMdUR ;
			val tmp4 = dpsdUR * SM + (pS + eSL)*dSMdUR ;
			
			A_R = omL * assignFiveFiveMatrix( tmp0, tmp1, tmp2, tmp3, tmp4 ) ;
		}
		return Vec( A_L, A_R ) ;
	}

}

