package ppl.apps.liszt.JoeImplicit

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object JoeImplicitRunner extends DeLisztApplicationRunner with JoeImplicit

object Constants extends DeLisztExp {
	val float3_zero = Vec(0.f,0.f,0.f) ;
	val zeroFiveVec = Vec(0.f, 0.f, 0.f, 0.f, 0.f) ;
// TODO(Montse):: HACK, HACK, HACK!!!! CHANGE THAT VEC< 5, VEC< 5,FLOAT>> TO MATRIX<5,5,FLOAT> !!!!!!!!!!!!!!!!!!!!!
	val zeroDenseFiveMatrix = UgpWithCvCompFlow.assignFiveFiveMatrix( zeroFiveVec, zeroFiveVec, zeroFiveVec, zeroFiveVec, zeroFiveVec ) ;
	val nsteps = 1000 ; //10000
}


object NavierStokesSolvers extends DeLisztExp {
	val EXPL_EULER = 0 ;
        val IMPL_EULER = 1 ;
        val EXPL_RK = 2 ;
	val IMPL_EULER_POINT = 3 ;
}


object LinearSolversNS extends DeLisztExp {
	val PETSC_GMRES = 0 ;
	val BCGSTAB = 1 ;
}


object IC extends DeLisztExp {
	val rho_init = 1.f;
	val p_init = 0.119783502243532f;
	val u_init = Vec(1.f, 0.f, 0.f);
	val T_init = 0.119783502243532f;
	val GAMMA = 1.4f ;
	var cfl = 0.5f ;
	val navierStokesSolver = NavierStokesSolvers.IMPL_EULER ;
	val check_interval = 1 ;
	val R_gas = p_init / rho_init / T_init ;
	var first = true ;
	var timeStepMode = 2 ; // notice here that this depends on cfl
	var const_dt = 0.f ;
	val DT = 0.001f ;
	var dt_minCPU = MAX_FLOAT ;

	//implicit
	val underRelax = 0.5f ;
	val maxIterLS = 30 ;
	val zeroAbsLS = 1.f * 1e-8 ;
	val zeroRelLS = 0.01f ;
	val startIncCFL = 100 ;
	val intervalIncCFL = 10 ;
	val incCFL = 1.f ;
	val maxCFL = 0.5f ;
	val resid_energ_th = 1.f * 1e-20 ;
	val linearSolverNS = LinearSolversNS.PETSC_GMRES ;
}


object IntegerSorter extends DeLisztExp {

	val MAX_INT = 100000 ;

	var num0 = MAX_INT ;
	var num1 = MAX_INT ;
	var num2 = MAX_INT ;
	var num3 = MAX_INT ;
	var num4 = MAX_INT ;
	var num5 = MAX_INT ;

	var numberOfNeighbors = 0 ;

	def setNum( v: Int ) {
		var value = v ;
		if ( value == 0 ) value = MAX_INT ; // outside cell
		else {
			if ( numberOfNeighbors == 0 ) num0 = value ;
		        else if ( numberOfNeighbors == 1 ) num1 = value ;
		        else if ( numberOfNeighbors == 2 ) num2 = value ;
		        else if ( numberOfNeighbors == 3 ) num3 = value ;
		        else if ( numberOfNeighbors == 4 ) num4 = value ;
		        else if ( numberOfNeighbors == 5 ) num5 = value ;
			numberOfNeighbors = numberOfNeighbors + 1 ;
		}
	}

	def getNextMin(): Int = {
		var minNumber = MAX_INT ;
		var position = -1 ;
		if ( num0 < minNumber ) {
			minNumber = num0 ;
			position = 0 ;
		}
                if ( num1 < minNumber ) {
			minNumber = num1 ;
			position = 1 ;
		}
		if ( num2 < minNumber ) {
			minNumber = num2 ;
			position = 2 ;
		}
		if ( num3 < minNumber ) {
			minNumber = num3 ;
			position = 3 ;
		}
		if ( num4 < minNumber ) {
			minNumber = num4 ;
			position = 4 ;
		}
		if ( num5 < minNumber ) {
			minNumber = num5 ;
			position = 5 ;
		}

		if ( position == 0 ) num0 = MAX_INT ;
                if ( position == 1 ) num1 = MAX_INT ;
                if ( position == 2 ) num2 = MAX_INT ;
                if ( position == 3 ) num3 = MAX_INT ;
                if ( position == 4 ) num4 = MAX_INT ;
                if ( position == 5 ) num5 = MAX_INT ;

		return minNumber ;
	}

	def reset() {
		num0 = MAX_INT ;
		num1 = MAX_INT ;
		num2 = MAX_INT ;
		num3 = MAX_INT ;
		num4 = MAX_INT ;
		num5 = MAX_INT ;
		numberOfNeighbors = 0 ;

	}


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

