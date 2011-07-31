package ppl.apps.liszt.Joe

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object EulerHyshotRunner extends DeLisztApplicationRunner with EulerHyshotBound

object Constants extends DeLisztExp {
	val float3_zero = Vec(0.f,0.f,0.f) ;
	val nsteps = 1000 ; //10000
}


object NavierStokesSolvers extends DeLisztExp {
	val EXPL_EULER = 0 ;
	val IMPL_EULER = 1 ;
	val EXPL_RK = 2 ;
	val IMPL_EULER_POINT = 3 ;
}


object IC extends DeLisztExp {
	val rho_init = 1.f;
	val p_init = 0.119783502243532f;
	val u_init = Vec(1.f, 0.f, 0.f);
	val T_init = 0.119783502243532f;
	val GAMMA = 1.4f ;
	val cfl = 0.5f ;
	val navierStokesSolver = NavierStokesSolvers.EXPL_EULER ;
	val check_interval = 1 ;
	val R_gas = p_init / rho_init / T_init ;
	var first = true ;
	var timeStepMode = 2 ; // notice here that this depends on cfl
	var const_dt = 0.f ;
	val DT = 0.001f ;
	var dt_minCPU = MIN_FLOAT ;
}


object MyJoe extends DeLisztExp {
	def init() {
		UgpWithCvCompFlow.init() ;
		JoeWithModels.init() ;
		Print("MyJoe()") ;
	}

	def run() { JoeWithModels.run() ; }
}


object Joe extends DeLisztApplication {
	def main() {
		MyJoe.init() ;
		MyJoe.run() ;
	}
}
