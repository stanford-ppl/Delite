package ppl.apps.liszt.Joe

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object JoeRunner extends DeLisztApplicationRunner with Joe

trait Joe extends DeLisztApplication {  
  object NavierStokesSolvers {
    var EXPL_EULER : Rep[Int] = null
    var IMPL_EULER : Rep[Int] = null
    var EXPL_RK : Rep[Int] = null
    var IMPL_EULER_POINT : Rep[Int] = null
    
    def init() {
      EXPL_EULER = 0 
      IMPL_EULER = 1 
      EXPL_RK = 2 
      IMPL_EULER_POINT = 3 
    }
  }
  
  object Constants {
    var float3_zero : Rep[Vec[_3,Float]] = null
    var iterations : Rep[Int] = null
    
    def init() {
      float3_zero = Vec(0.f,0.f,0.f) 
      iterations = 10
    }
  }

  object IC {
    var rho_init : Rep[Float] = null
    var p_init : Rep[Float] = null
    var u_init : Rep[Vec[_3,Float]] = null
    var T_init : Rep[Float] = null
    var GAMMA : Rep[Float] = null
    var cfl : Rep[Float] = null
    var navierStokesSolver : Rep[Int] = null
    var check_interval : Rep[Int] = null
    var R_gas : Rep[Float] = null
    var first : Rep[Boolean] = null
    var timeStepMode : Rep[Int] = null  // notice here that this depends on cfl
    var const_dt : Rep[Float] = null
    var DT : Rep[Float] = null
    var dt_minCPU : Rep[Float] = null
    
    def init() = {
      rho_init = 1.f
      p_init = 0.119783502243532f
      u_init = Vec(1.f, 0.f, 0.f)
      T_init = 0.119783502243532f
      GAMMA = 1.4f 
      cfl = 0.5f 
      navierStokesSolver = NavierStokesSolvers.EXPL_EULER 
      check_interval = 1 
      R_gas = p_init / rho_init / T_init 
      first = true 
      timeStepMode = 2  // notice here that this depends on cfl
      const_dt = 0.f 
      DT = 0.001f 
      dt_minCPU = MIN_FLOAT 
    }
  }
  
  object MeshGeometryCalc {
    var position : Rep[Field[Vertex,Vec[_3, Float]]] = null
    var x_fa : Rep[Field[Face,Vec[_3, Float]]] = null
    var x_cv : Rep[Field[Cell,Vec[_3, Float]]] = null
    var fa_normal : Rep[Field[Face,Vec[_3, Float]]] = null
    var cv_volume : Rep[Field[Cell,Float]] = null
    
    def init() {
      position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")
      x_fa = FieldWithConst[Face, Vec[_3, Float]](Constants.float3_zero)
      x_cv = FieldWithConst[Cell, Vec[_3, Float]](Constants.float3_zero)	
      fa_normal = FieldWithConst[Face, Vec[_3, Float]](Constants.float3_zero)
      cv_volume = FieldWithConst[Cell, Float](0.f)
    }

    def calcFaceCenter( f: Rep[Face] ) : Rep[Vec[_3, Float]] = {
      var center = Constants.float3_zero
      for(v <- vertices(f) ) {
        center = center + position(v)
      }
      center = center / size(vertices(f))
      return center
    }

    def calcCellCenter( c: Rep[Cell] ) : Rep[Vec[_3,Float]] = {
            var center = Constants.float3_zero
            for (v <- vertices(c)) {
                    center = center + position(v)
            }
            center = center / size(vertices(c))
            return center
    }


    def calcFaceGeom( f: Rep[Face], flipNormal: Rep[Boolean] ) {
      val approxCenter = calcFaceCenter(f)
      var normal = Constants.float3_zero
      for ( e <- edgesCCW(f) ) {
        val v0 = position(head(e)) - approxCenter
        val v1 = position(tail(e)) - approxCenter
        normal = normal + cross(v1,v0)
      }
      normal = normalize(normal)
      
      var center = Constants.float3_zero
      var area = 0.f
      for(e <- edgesCCW(f)) {
        val v0 = position(head(e)) - approxCenter
        val v1 = position(tail(e)) - approxCenter
        val tmp_area = dot(normal,cross(v1,v0))
        area += tmp_area
        center = center + ( approxCenter + position(head(e)) + position(tail(e)) ) * tmp_area
      }
      x_fa(f) = center/ (area * 3.f)
      var farea = area / 2.f
      var tmpNormal = normal * farea
      if ( flipNormal )
        tmpNormal = -tmpNormal
      fa_normal(f) = tmpNormal
    }

    def calcCellGeom( c: Rep[Cell] ) {
      val approxCenter = calcCellCenter(c)
      var volume = 0.f
      var center = Constants.float3_zero
      for( f <- faces(c) ) {
        val v0 = x_fa(f) - approxCenter
        for( e <- edgesCCW(towards(f,c)) ) {
          val v1 = position(head(e)) - approxCenter
          val v2 = position(tail(e)) - approxCenter
          val tetVol = dot(v0, cross(v1,v2))
          volume += tetVol
          center = center + (approxCenter + x_fa(f) + position(head(e)) + position(tail(e))) * tetVol
        }
      }
      x_cv(c) = center/ (volume * 4.f)
      cv_volume(c) = volume/ 6.f
    }

  }
  
  object UgpWithCvCompFlow {
    var vel : Rep[Field[Cell,Vec[_3,Float]]] = null
    var press : Rep[Field[Cell,Float]] = null
    var temp : Rep[Field[Cell,Float]] = null
    var enthalpy : Rep[Field[Cell,Float]] = null
    var LambdaOverCp : Rep[Field[Cell,Float]] = null
    var sos : Rep[Field[Cell,Float]] = null
    var kine : Rep[Field[Cell,Float]] = null
    var local_dt : Rep[Field[Cell,Float]] = null

    // these fields should be build in "initialHookScalarRansCombModel(), so we got rid of the whole function
    var RoM : Rep[Field[Cell,Float]] = null
    var gamma : Rep[Field[Cell,Float]] = null
    var muLam : Rep[Field[Cell,Float]] = null
    
    // this should be called in runExplicitBackwardEuler
    var gam_fa : Rep[Field[Face,Float]] = null
    var RoM_fa : Rep[Field[Face,Float]] = null
    var h_fa : Rep[Field[Face,Float]] = null
    var T_fa : Rep[Field[Face,Float]] = null
    var vel_fa : Rep[Field[Face,Vec[_3,Float]]] = null
    var p_fa : Rep[Field[Face,Float]] = null
    var mu_fa : Rep[Field[Face,Float]] = null
    var lamOcp_fa : Rep[Field[Face,Float]] = null
    var rho_fa : Rep[Field[Face,Float]] = null

    def calcRansStateVarAndMaterialProperties() {
      for ( c <- cells(mesh) ) {
        if ( JoeWithModels.rho(c) <= 0.0f ) {
          Print( "negative density at xcv: " , MeshGeometryCalc.x_cv(c) , " " , ID(c) )
        }

        vel(c) = JoeWithModels.rhou(c) / JoeWithModels.rho(c)
        
        var kinecv = 0.f
        kinecv = kine(c)

        val pr = ( gamma(c) - 1.f ) * ( JoeWithModels.rhoE(c) - 0.5f * dot(JoeWithModels.rhou(c),JoeWithModels.rhou(c)) / JoeWithModels.rho(c) - JoeWithModels.rho(c) * kinecv )
        press(c) = pr
        if ( pr <= 0.f ) {
          Print( "negative pressure at xcv: " , MeshGeometryCalc.x_cv(c), " ", ID(c) )
        }
        
        val tp = pr / ( JoeWithModels.rho(c) * RoM(c) )
        temp(c) = tp
        enthalpy(c) = gamma(c) * RoM(c) / ( gamma(c) - 1.f ) * tp
        sos(c) = sqrtf( gamma(c) * pr / JoeWithModels.rho(c) )
  //if (ID(c) == 6806) {
  // 	Print( "CALCRANSSTATE c: " , ID(c), "	x_cv: ", MeshGeometryCalc.x_cv(c), " gamma: ", gamma(c), " press: ", pr, " rho: ", JoeWithModels.rho(c), " rhou: ", JoeWithModels.rhou(c), " rhoE: ", JoeWithModels.rhoE(c), " RoM: ", RoM(c), " kine: ", kine(c), " temp: ", tp, " enthalpy ", gamma(c) * RoM(c) / ( gamma(c) - 1.f ) * tp, " sos: ", sqrtf( gamma(c) * pr / JoeWithModels.rho(c) ) );
  //}
      }
    } 

    def ComputeBCProperties_T( f: Rep[Face] ) {
      gam_fa(f) = IC.GAMMA
      RoM_fa(f) = IC.R_gas
      h_fa(f) = IC.GAMMA * IC.R_gas / ( IC.GAMMA - 1.f ) * T_fa(f)
    }
    

    def ComputeGeometry(){
      for( f <- faces(mesh) ){
        MeshGeometryCalc.calcFaceGeom(f, true)
      }
      for( c <- cells(mesh) ){
        MeshGeometryCalc.calcCellGeom(c)
      }  
    }

    def init() {
      vel = FieldWithConst[Cell,Vec[_3,Float]](Constants.float3_zero)
      press = FieldWithConst[Cell,Float](0.f)
      temp = FieldWithConst[Cell,Float](0.f)
      enthalpy = FieldWithConst[Cell,Float](0.f)
      LambdaOverCp = FieldWithConst[Cell,Float](0.f)
      sos = FieldWithConst[Cell,Float](0.f)
      kine = FieldWithConst[Cell,Float](0.f)
      local_dt = FieldWithConst[Cell,Float](0.f)
      // these fields should be build in "initialHookScalarRansCombModel(), so we got rid of the whole function
      RoM = FieldWithConst[Cell,Float](IC.R_gas)
      gamma = FieldWithConst[Cell,Float](IC.GAMMA)
      muLam = FieldWithConst[Cell,Float](0.f)
      
      // this should be called in runExplicitBackwardEuler
      gam_fa = FieldWithConst[Face,Float](0.f)
      RoM_fa = FieldWithConst[Face,Float](0.f)
      h_fa = FieldWithConst[Face,Float](0.f)
      T_fa = FieldWithConst[Face,Float](0.f)
      vel_fa = FieldWithConst[Face,Vec[_3,Float]](Constants.float3_zero)
      p_fa = FieldWithConst[Face,Float](0.f)
      mu_fa = FieldWithConst[Face,Float](0.f)
      lamOcp_fa = FieldWithConst[Face,Float](0.f)
      rho_fa = FieldWithConst[Face,Float](0.f)
    
      Print("UgpWithCvCompFlow()")

      // ----------------------------------------------------------------------------------------
          // write some parameters on the screen
          // ----------------------------------------------------------------------------------------
      
      Print("")
      Print("--------------------------------------------")
      Print( "Gas properties         " )
      Print("    GAMMA            : ", IC.GAMMA );
            Print("    R_GAS            : ", IC.R_gas );
            Print("    P_REF            : ", IC.p_init );
            Print("    RHO_REF          : ", IC.rho_init );
            Print("    T_REF            : ", IC.T_init );
            Print("    SOS_REF          : ", sqrtf(IC.GAMMA*IC.R_gas*IC.T_init) );
            Print("Solver settings        ");
            Print("    nsteps           : ", Constants.iterations);
            Print("    timeStepMode     : ", IC.timeStepMode);
            Print("--------------------------------------------")
      Print("")
      ComputeGeometry()
      
      // HACK!!!!
      // TODO(Montse):: Remove this once Zach fixes the bug for initiallizing Fields with a constant different to zero!!!!

      for ( c <- cells(mesh) ) {
        gamma(c) = IC.GAMMA
        RoM(c) = IC.R_gas

        JoeWithModels.rho(c) = IC.rho_init
              JoeWithModels.rhou(c) = IC.u_init * IC.rho_init
              JoeWithModels.rhoE(c) = IC.p_init / (IC.GAMMA - 1.f)  +  0.5f * IC.rho_init * dot( IC.u_init, IC.u_init )
      }	
    }

    def calcEulerFlux_HLLC( rhoL : Rep[Float], uL : Rep[Vec[_3,Float]], pL : Rep[Float], h0 : Rep[Float], gammaL : Rep[Float], rhoR : Rep[Float], uR : Rep[Vec[_3,Float]], pR : Rep[Float], h1 : Rep[Float], gammaR : Rep[Float], area : Rep[Float], nVec : Rep[Vec[_3,Float]], surfVeloc : Rep[Float], kL : Rep[Float], kR : Rep[Float] ) : Rep[Vec[_5,Float]] = {
      var Frho = 0.f
      var Frhou = Constants.float3_zero
      var FrhoE = 0.f		
      
      val unL = dot(uL,nVec)
      val uLuL = dot(uL,uL)
      val cL = sqrtf( gammaL * pL / rhoL )
      val hL = gammaL / ( gammaL - 1.f ) * pL / rhoL + 0.5f * uLuL + kL
      val eL = hL * rhoL - pL

      val unR = dot(uR,nVec)
      val uRuR = dot(uR,uR)
      val cR = sqrtf( gammaR * pR / rhoR )
      val hR = gammaR / ( gammaR - 1.f ) * pR / rhoR + 0.5f * uRuR + kR
      val eR = hR * rhoR - pR

      // Roe's averaging
      val Rrho = sqrtf( rhoR / rhoL )
      val tmp = 1.f / ( 1.f + Rrho )
      val velRoe = ( uL + uR *Rrho ) * tmp
      val uRoe = dot( velRoe, nVec )
      //val hRoe = tmp * ( hL + hR * Rrho )
      
      //val cRoe = sqrtf( (gammaL - 1.f) * ( hRoe - 0.5f * dot( velRoe, velRoe ) ) )
      val gamPdivRho = tmp * ( (gammaL * pL / rhoL + 0.5f * (gammaL - 1.f) * uLuL ) + (gammaR * pR / rhoR + 0.5f * (gammaR - 1.f) * uRuR ) * Rrho )
      val cRoe = sqrtf( gamPdivRho - ((gammaL + gammaR) * 0.5f - 1.f ) * 0.5f * dot( velRoe, velRoe ) );

      // speed of sound at L and R 
      val sL = (uRoe - cRoe).min(unL - cL)
      val sR = (uRoe + cRoe).max(unR + cR)

      // speed of contact surface
      val sM = (pL - pR - rhoL * unL * (sL - unL) + rhoR * unR * (sR - unR) ) / (rhoR * (sR - unR) - rhoL * (sL - unL))

      // pressure at right and left (pR =pL ) side contact surfaca
      val pStar = rhoR * (unR - sR) * (unR - sM) + pR

      // Calculate the nested code first.
      val sL_gt_0 = sL > 0.f;
      val sR_ge_0 = sR >= 0.f;
      val sM_ge_0 = sM >= 0.f;

      val top_nest = (sM_ge_0 && !sL_gt_0);
      val btm_nest = (!sM_ge_0 && sR_ge_0);
      val either_nest = (top_nest || btm_nest);

      val unSide = if (sM_ge_0) unL else unR;
      val pSide = if (sM_ge_0) pL else pR;
      val eSide = if (sM_ge_0) eL else eR;
      val rhoSide = if (sM_ge_0) rhoL else rhoR;
      val uSide = if (sM_ge_0) uL else uR;

      if (either_nest) {
        val sSide = if (sM_ge_0) sL else sR;

        val inv_diff = 1.f / (sSide - sM);
        val sSide_minus_other = sSide - unSide;
        val scaled_rho = rhoSide * sSide_minus_other * inv_diff;
        val scaled_more = (uSide * rhoSide * sSide_minus_other + nVec * (pStar - pSide)) * inv_diff
        val eS_side = (sSide_minus_other * eSide - pSide * unSide + pStar * sM) * inv_diff
        
        Frho = scaled_rho * sM;
        Frhou = scaled_more * sM + nVec * pStar
        FrhoE = (eS_side + pStar) * sM;
      } else {

        Frho = rhoSide * unSide;
        Frhou = uSide * rhoSide * unSide + nVec * pSide
        FrhoE = (eSide + pSide) * unSide;
      }

      Frho = Frho * area
      Frhou = Frhou * area
      FrhoE = FrhoE * area
      
      return Vec(Frho, Frhou.x, Frhou.y, Frhou.z, FrhoE)		
    }

    def calcDt( cfl_target : Rep[Float] ) : Rep[Float] = {
      var dt = 0.f

      if ( IC.timeStepMode == 0 ) {
        return IC.const_dt
      }
      if ( IC.timeStepMode == 1 ) {
        dt = IC.DT
        IC.const_dt = IC.DT
        for( c <- cells(mesh) ) {
          local_dt(c) = dt
        }
        IC.timeStepMode = 0
      }
      if ( IC.timeStepMode == 2 ) {
        for( icv <- cells(mesh) ) {
          var lambdaMax = 0.f

          val c = sqrtf( gamma(icv) * press(icv) / JoeWithModels.rho(icv) )

          for ( f <- faces(icv) ) {
            var nVec = MeshGeometryCalc.fa_normal(f)
            val area = sqrtf(dot(nVec, nVec))
            nVec = normalize(nVec)

            val Uk = dot( JoeWithModels.rhou(icv), nVec ) / JoeWithModels.rho(icv)
            val lambda = ( fabsf(Uk) + c ) * area
            lambdaMax = lambdaMax.max(lambda)
          }
      
          val dt_cv = cfl_target * MeshGeometryCalc.cv_volume(icv) / lambdaMax
          IC.dt_minCPU = IC.dt_minCPU.min(dt_cv)
          local_dt(icv) = dt_cv
        }
        dt = IC.dt_minCPU
      }
      return dt
    }

  }
  
  object JoeWithModels {
    val rhs_rho = FieldWithConst[Cell,Float](0.f)
    val rhs_rhou = FieldWithConst[Cell,Vec[_3,Float]](Constants.float3_zero)
    val rhs_rhoE = FieldWithConst[Cell,Float](0.f)

    // notice that this would originally be called in JoeWithModels.initialHook()
    val rho = FieldWithConst[Cell,Float](IC.rho_init)
    val rhou = FieldWithConst[Cell,Vec[_3,Float]](IC.u_init * IC.rho_init)
    val rhoE = FieldWithConst[Cell,Float](IC.p_init / (IC.GAMMA - 1.f)  +  0.5f * IC.rho_init * dot( IC.u_init, IC.u_init ))

    val zone_interior = BoundarySet[Face]("zone_interior")
    val zone_boundary = BoundarySet[Face]("zone_boundary")
    val symmetry = BoundarySet[Face]("symmetry")
    //val wall = BoundarySet[Face]("wall")
    val other_boundaries = BoundarySet[Face]("other_boundaries")
    val cbc = BoundarySet[Face]("cbc")
    //val hook = BoundarySet[Face]("hook")
    //val cbc_subsonic_outlet = BoundarySet[Face]("cbc_subsonic_outlet")
    
    def init() { Print("JoeWithModels()")}
    def initialHook() {Print("setInitialConditions()")}


    // TODO(Montse):: HACK!!!! NEED TO MODIFY THE REFERENCE OUTSIDE CELL!!!! MIGHT NOT BE ZERO!!!!
    def getInsideBoundaryCell( f : Rep[Face] ) : Rep[Cell] = {
            // NOTE(boulos): Sorry the "old" code pisses off my
            // vectorized return statements (and I've currently
            // forgotten why). This equivalent code works though.
            val inside_cell = inside(f);
      val outside_cell = outside(f);
      val result = if (ID(inside_cell) == 0) outside_cell else inside_cell;
            return result;
      //val c = if ( ID(inside(f)) == 0 ) outside(f) else inside(f)
      //return c
    }	

    def run() {
      initialHook()
      
      if ( IC.navierStokesSolver == NavierStokesSolvers.EXPL_EULER ){ 
        runExplicitBackwardEuler()
      } else if ( IC.navierStokesSolver == NavierStokesSolvers.IMPL_EULER ){
      } else if ( IC.navierStokesSolver == NavierStokesSolvers.EXPL_RK ){
      } else { Print("ERROR: no or wrong time integration scheme specified !") }
    }

    def runExplicitBackwardEuler() {

      UgpWithCvCompFlow.calcRansStateVarAndMaterialProperties()
      
   
      setNavierStokesBC()
      
      // HERE TIME SHOULD START TICKING !!!!!!
      var start_time = 0.0;
      var step = 0
      while (step < Constants.iterations ) {
        if (step == 1) {
          start_time = wall_time();
        }
        //Print("looping: ", step)
        step += 1
        
        var dtMin = UgpWithCvCompFlow.calcDt(IC.cfl)
        calcRhs() // note that this function originally would take rhs_rho, rhs_rhou, rhs_rhoE, rho, rhou, rhoE fields as arguments
        
        for( c <- cells(mesh) ) {
          val tmp = UgpWithCvCompFlow.local_dt(c) / MeshGeometryCalc.cv_volume(c)
          rho(c) = rho(c) + rhs_rho(c) * tmp
          rhou(c) = rhou(c) + rhs_rhou(c) * tmp
          rhoE(c) = rhoE(c) + rhs_rhoE(c) * tmp
        }

        if ( step % IC.check_interval == 0 ) {
          if ( step % ( IC.check_interval * 10 ) == 0 ) {
            Print( "" ) // print nothing to simulate a \n !!!!
            Print( "done step: " , step , ", cfl: " , IC.cfl , ", min. dt:   " , dtMin ) 
          }
          var my_resid = Vec(0.f,0.f,0.f,0.f,0.f)
          for( c <- cells(mesh) ) {
            my_resid = my_resid + Vec(fabsf(rhs_rho(c)),
                            fabsf(rhs_rhou(c).x),
                            fabsf(rhs_rhou(c).y),
                            fabsf(rhs_rhou(c).z),
                            fabsf(rhs_rhoE(c)))
          }
          showResidue( my_resid, step )
        } 

        UgpWithCvCompFlow.calcRansStateVarAndMaterialProperties()

        setNavierStokesBC()
      }

       Print( "TIME_FOR_LOOP: ", wall_time() - start_time )
        
    }



    def calcRhs() {
      for( c <- cells(mesh) ) {
        rhs_rho(c) = 0.f
        rhs_rhou(c) =  Constants.float3_zero
        rhs_rhoE(c) = 0.f
      }
        // count how switched back to first order due to extrapolated negative pressure at the faces 

      for (f <- zone_interior) { calcRhsInterior(f) }
      for (f <- symmetry) { calcRhsSymmetry(f) }
      // Ignore wall...
      for (f <- other_boundaries) { calcRhsOtherBoundaries(f) }
    }




    def setNavierStokesBC() {
      //IC.first = true
      var bc_err = false

      // Print( "Applying HOOK		: " )
      //for (f <- hook) { UgpWithCvCompFlow.ComputeBCProperties_T(f) }
      // TODO(mbarrien): Is there a need for a different BoundaryInfo for subranges
      // inside a larger boundary?
      val vector_bc = Vec( IC.u_init.x, IC.u_init.y, IC.u_init.z, IC.T_init, IC.p_init )
      val T_bc = vector_bc(_3)
      val p_bc = vector_bc(_4)
      val u_bc = Vec( vector_bc.x, vector_bc.y, vector_bc.z )
      if (IC.first) {
        Print( "Applying CBC		\t u_bc: ", u_bc, " T_bc: ", T_bc, " p_bc: ", p_bc )
      }
      for (f <- cbc) {
        CBCBoundary(f, T_bc, p_bc, u_bc)
      }
      for (f <- cbc) {
        UgpWithCvCompFlow.ComputeBCProperties_T(f)
      }

      // Ignore cbc_subsonic_inlet
      // TODO(mbarrien): This vec should probably not be hardcoded like this.
      val cbc_p_bc = 0.f
      if (IC.first) {
        Print ( "Applying CBC_SUBSONIC_OUTLET \t pOut: ", cbc_p_bc )
      }
      /*
      for (f <- cbc_subsonic_outlet) {
        CBCSubsonicOutletBoundary(f, cbc_p_bc);
      }
      for (f <- cbc_subsonic_outlet) {
        UgpWithCvCompFlow.ComputeBCProperties_T(f)
      }*/

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
      IC.first = false
    }

    
    def showResidue( rhsResid: Rep[Vec[_5,Float]], step: Rep[Int] ) {
      var sCount = 0;
      if ( step % (IC.check_interval * 10 ) == 0 ) {
        Print( "		rho		rhou-X		rhou-Y		rhou-Z		rhoE")
      }
      Print( "RESID: ", step, "		", rhsResid(_0), "		", rhsResid(_1), "		", rhsResid(_2), "		", rhsResid(_3), "		", rhsResid(_4))
    }

    
    def CBCBoundary( f: Rep[Face], T_bc : Rep[Float], p_bc : Rep[Float], u_bc : Rep[Vec[_3,Float]] ) {
  //if ( ID(f) == 95000 ) {
  //	Print( "CBC face with Id: ", ID(f), " T: ", T_bc, " u: ", u_bc, " p: ", p_bc )
  //}
      UgpWithCvCompFlow.T_fa(f) = T_bc
      UgpWithCvCompFlow.vel_fa(f) = u_bc
      UgpWithCvCompFlow.p_fa(f) = p_bc
    }
    /*

    def CBCSubsonicOutletBoundary( f: Face, p_bc : Float ) {
      val icv0 = getInsideBoundaryCell(f)
      UgpWithCvCompFlow.T_fa(f) = UgpWithCvCompFlow.temp(icv0)
      UgpWithCvCompFlow.vel_fa(f) = rhou(icv0) / rho(icv0)
      UgpWithCvCompFlow.p_fa(f) = p_bc
    }
    */
    
    def SymmetryBoundary( f: Rep[Face] ) {
      val icv0 = getInsideBoundaryCell(f)
      var nVec = MeshGeometryCalc.fa_normal(f)
      //val area = nVec.length()
      nVec = normalize(nVec)
      
      // flip u, APPROXIMATION ---> take velocity at the cell center
      val u0 = UgpWithCvCompFlow.vel(icv0)
      val un = dot( nVec, u0 )
      UgpWithCvCompFlow.vel_fa(f) = u0 - nVec * un

      UgpWithCvCompFlow.T_fa(f) = UgpWithCvCompFlow.temp(icv0)
      UgpWithCvCompFlow.p_fa(f) = UgpWithCvCompFlow.press(icv0)
      UgpWithCvCompFlow.RoM_fa(f) = UgpWithCvCompFlow.RoM(icv0)
      UgpWithCvCompFlow.gam_fa(f) = UgpWithCvCompFlow.gamma(icv0)
      UgpWithCvCompFlow.mu_fa(f) = UgpWithCvCompFlow.muLam(icv0)
      UgpWithCvCompFlow.lamOcp_fa(f) = UgpWithCvCompFlow.LambdaOverCp(icv0)
      UgpWithCvCompFlow.h_fa(f) = UgpWithCvCompFlow.enthalpy(icv0)	

  //if ( ID(f) == 17777 || ID(f) == 95500 || ID(f) == 94000 || ID(f) == 94500 || ID(f) == 60000 ) {
  //Print( "Symmetry face: ", ID(f), " vel: ", u0 - 1.f * un * nVec, " T: ", UgpWithCvCompFlow.temp(icv0), " p: ", UgpWithCvCompFlow.press(icv0), " RoM: ", UgpWithCvCompFlow.RoM(icv0), "gam: ", UgpWithCvCompFlow.gamma(icv0), "mu: ", UgpWithCvCompFlow.muLam(icv0), " lamOcp: ", UgpWithCvCompFlow.LambdaOverCp(icv0), " h: ", UgpWithCvCompFlow.enthalpy(icv0) )
  //}
    }

    def setRhoFa( f: Rep[Face] ) {
      UgpWithCvCompFlow.rho_fa(f) = UgpWithCvCompFlow.p_fa(f) / ( UgpWithCvCompFlow.RoM_fa(f) * UgpWithCvCompFlow.T_fa(f) )
    }

    def calcRhsInterior( f: Rep[Face] ) {
      val icv0 = outside(f)
      val icv1 = inside(f)

      val rho0 = rho(icv0)
      val u0 = UgpWithCvCompFlow.vel(icv0)
      val p0 = UgpWithCvCompFlow.press(icv0)
      val h0 = UgpWithCvCompFlow.enthalpy(icv0)

      val rho1 = rho(icv1)
      val u1 = UgpWithCvCompFlow.vel(icv1)
      val p1 = UgpWithCvCompFlow.press(icv1)
      val h1 = UgpWithCvCompFlow.enthalpy(icv1)

      // face unit normal and area...
      var nVec = MeshGeometryCalc.fa_normal(f)
      val area = sqrtf(dot(nVec,nVec))
      //nVec = normalize(nVec)
      nVec = nVec / area
    
      val kine0 = UgpWithCvCompFlow.kine(icv0)
      val kine1 = UgpWithCvCompFlow.kine(icv1)

      //Frho5[_0] = Frho, Frho5[_1] = Frhou.x , Frho5[_2] = Frhou.y, Frho5[_3] = Frhou.y, Frho5[_4] = FrhoE
      val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(rho0, u0, p0, h0, UgpWithCvCompFlow.gamma(icv0), rho1, u1, p1, h1, UgpWithCvCompFlow.gamma(icv1), area, nVec, 0.f, kine0, kine1)
      
      rhs_rho(icv0) = rhs_rho(icv0) - Frho5(_0)
      rhs_rhou(icv0) = rhs_rhou(icv0) - Vec(Frho5(_1), Frho5(_2), Frho5(_3))
      rhs_rhoE(icv0) = rhs_rhoE(icv0) - Frho5(_4)

      rhs_rho(icv1) = rhs_rho(icv1) + Frho5(_0)
      rhs_rhou(icv1) = rhs_rhou(icv1) + Vec(Frho5(_1), Frho5(_2), Frho5(_3))
      rhs_rhoE(icv1) = rhs_rhoE(icv1) + Frho5(_4)

    }

    def calcRhsSymmetry( f: Rep[Face]) {
      val icv0 = getInsideBoundaryCell(f)

      var nVec = MeshGeometryCalc.fa_normal(f)
      val area = sqrtf(dot(nVec,nVec))
      //nVec = normalize(nVec)
                  nVec = nVec / area
    
      val kine0 = UgpWithCvCompFlow.kine(icv0)
      val kine1 = UgpWithCvCompFlow.kine(icv0)

      val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(UgpWithCvCompFlow.rho_fa(f), UgpWithCvCompFlow.vel_fa(f), UgpWithCvCompFlow.p_fa(f), UgpWithCvCompFlow.h_fa(f), UgpWithCvCompFlow.gam_fa(f), 
        UgpWithCvCompFlow.rho_fa(f), UgpWithCvCompFlow.vel_fa(f), UgpWithCvCompFlow.p_fa(f), UgpWithCvCompFlow.h_fa(f), UgpWithCvCompFlow.gam_fa(f), area, nVec, 0.f, kine0, kine1)

      rhs_rho(icv0) = rhs_rho(icv0) - Frho5(_0)
      rhs_rhou(icv0) = rhs_rhou(icv0) - Vec(Frho5(_1), Frho5(_2), Frho5(_3))
      rhs_rhoE(icv0) = rhs_rhoE(icv0) - Frho5(_4)
    
    }

    def calcRhsOtherBoundaries( f: Rep[Face] ) {
      val icv0 = getInsideBoundaryCell(f)

      val rho0 = rho(icv0)
      val u0 = UgpWithCvCompFlow.vel(icv0)
      val p0 = UgpWithCvCompFlow.press(icv0)
      val h0 = UgpWithCvCompFlow.enthalpy(icv0)
      val gam0 = UgpWithCvCompFlow.gamma(icv0)


      // face unit normal and area...
      var nVec = MeshGeometryCalc.fa_normal(f)
      val area = sqrtf(dot(nVec,nVec))
      nVec = normalize(nVec)
    
      val kine0 = UgpWithCvCompFlow.kine(icv0)
      val kine1 = UgpWithCvCompFlow.kine(icv0)

      //Frho5[_0] = Frho, Frho5[_1] = Frhou.x , Frho5[_2] = Frhou.y, Frho5[_3] = Frhou.y, Frho5[_4] = FrhoE
      val Frho5 = UgpWithCvCompFlow.calcEulerFlux_HLLC(rho0, u0, p0, h0, gam0, UgpWithCvCompFlow.rho_fa(f), UgpWithCvCompFlow.vel_fa(f), UgpWithCvCompFlow.p_fa(f), UgpWithCvCompFlow.h_fa(f), UgpWithCvCompFlow.gam_fa(f), area, nVec, 0.f, kine0, kine1)
      
      rhs_rho(icv0) = rhs_rho(icv0) - Frho5(_0)
      rhs_rhou(icv0) = rhs_rhou(icv0) - Vec(Frho5(_1), Frho5(_2), Frho5(_3))
      rhs_rhoE(icv0) = rhs_rhoE(icv0) - Frho5(_4)
    }

  }

  
	def main() {
    NavierStokesSolvers.init()
    Constants.init()
    IC.init()
    MeshGeometryCalc.init()
    
		UgpWithCvCompFlow.init() 
		JoeWithModels.init() 
		Print("MyJoe()") 
    JoeWithModels.run()
	}
}
