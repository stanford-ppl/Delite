package ppl.apps.liszt.Joe

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object MeshGeometryCalc extends DeLisztExp {

	val position = FieldWithLabel[Vertex,Vec[_3,Float]]("position") ;
	val x_fa = FieldWithConst[Face, Vec[_3, Float]](Constants.float3_zero) ;
	val x_cv = FieldWithConst[Cell, Vec[_3, Float]](Constants.float3_zero) ;	
	val fa_normal = FieldWithConst[Face, Vec[_3, Float]](Constants.float3_zero) ;
	val cv_volume = FieldWithConst[Cell, Float](0.f) ;

	def calcFaceCenter( f: Face ) : Vec[_3, Float] = {
		var center = Constants.float3_zero ;
		for(v <- vertices(f) ) {
			center += position(v) ;
		}
		center = center / size(vertices(f)) ;
		return center ;
	}

        def calcCellCenter( c: Cell ) : Vec[_3,Float] = {
                var center = Constants.float3_zero ;
                for (v <- vertices(c)) {
                        center += position(v) ;
                }
                center = center / size(vertices(c)) ;
                return center ;
        }


	def calcFaceGeom( f: Face, flipNormal: Boolean ) {
		val approxCenter = calcFaceCenter(f) ;
		var normal = Constants.float3_zero ;
		for ( e <- edgesCCW(f) ) {
			val v0 = position(head(e)) - approxCenter ;
			val v1 = position(tail(e)) - approxCenter ;
			normal += cross(v1,v0) ;
		}
		normal = normalize(normal) ;
		
		var center = Constants.float3_zero ;
		var area = 0.f ;
		for(e <- edgesCCW(f)) {
			val v0 = position(head(e)) - approxCenter ;
			val v1 = position(tail(e)) - approxCenter ;
			val tmp_area = dot(normal,cross(v1,v0)) ;
			area += tmp_area ;
			center += tmp_area * ( approxCenter + position(head(e)) + position(tail(e)) ) ;
		}
		x_fa(f) = center/ (3.f * area ) ;
		var farea = area / 2.f ;
		var tmpNormal = normal * farea ;
		if ( flipNormal )
			tmpNormal = -tmpNormal ;
		fa_normal(f) = tmpNormal ;
	}

	def calcCellGeom( c: Cell ) {
		val approxCenter = calcCellCenter(c) ;
		var volume = 0.f ;
		var center = Constants.float3_zero ;
		for( f <- faces(c) ) {
			val v0 = x_fa(f) - approxCenter ;
			for( e <- edgesCCW(towards(f,c)) ) {
				val v1 = position(head(e)) - approxCenter ;
				val v2 = position(tail(e)) - approxCenter ;
				val tetVol = dot(v0, cross(v1,v2)) ;
				volume += tetVol ;
				center += tetVol * (approxCenter + x_fa(f) + position(head(e)) + position(tail(e))) ;
			}
		}
		x_cv(c) = center/ (4.f * volume) ;
		cv_volume(c) = volume/ 6.f ;
	}

}

