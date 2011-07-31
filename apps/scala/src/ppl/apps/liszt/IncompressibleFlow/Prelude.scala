package ppl.apps.liszt.IncompressibleFlow

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object Prelude extends DeLisztExp {
	val position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")	
	val face_centroid = FieldWithConst[Face,Vec[_3,Float]](Vec(0.0f,0.0f,0.0f))
	val face_area = FieldWithConst[Face,Float](0.f)
	val face_normal = FieldWithConst[Face,Vec[_3,Float]](Vec(0.0f,0.0f,0.0f))
	val face_unit_normal = FieldWithConst[Face,Vec[_3,Float]](Vec(0.0f,0.0f,0.0f))
	
	val cell_centroid = FieldWithConst[Cell,Vec[_3,Float]](Vec(0.0f,0.0f,0.0f))
	val cell_volume = FieldWithConst[Cell,Float](0.f)
	
	
	def calcFaceCenter(f : Face) : Vec[_3,Float] = {
		var center = Vec(0.f,0.f,0.f)
		val test = Vec(0.f,0.f,0.f,0.f)
		for(v <- vertices(f)) {
		  center += position(v)
		}
		center = center / size(vertices(f))
		return center
	}
	
	def calcCellCenter(c : Cell) : Vec[_3,Float] = {
		var center = Vec(0.f,0.f,0.f)
		for(v <- vertices(c)) {
		  center += position(v)
		}
		center = center / size(vertices(c))
		return center
	}
	def calcFaceGeom(f : Face) : Unit = {
		val approxCenter = calcFaceCenter(f)
		var normal = Vec(0.f,0.f,0.f)
		for(e <- edgesCCW(f)) {
		  val v0 = position(head(e)) - approxCenter
		  val v1 = position(tail(e)) - approxCenter
		  normal += cross(v1,v0)
		}
		normal = normalize(normal)
		var center = Vec(0.f,0.f,0.f)
		var area = 0.f
		for(e <- edgesCCW(f)) {
		  val v0 = position(head(e)) - approxCenter
		  val v1 = position(tail(e)) - approxCenter
		  val tmp_area = dot(normal,cross(v1,v0))
		  area += tmp_area
		  center += tmp_area * ( approxCenter + position(head(e)) + position(tail(e)))
		}  
		face_centroid(f) = center / (3.f * area)
		val farea = area / 2.f
		face_area(f) = farea
		face_normal(f) = normal*farea
		face_unit_normal(f) = normal
	}

	def calcCellGeom(c : Cell) : Unit = {
		val approxCenter = calcCellCenter(c)
		var volume = 0.f
		var center = Vec(0.f,0.f,0.f)
		for(f <- faces(c)) {
		  val v0 = face_centroid(f) - approxCenter
		  for(e <- edgesCCW(towards(f,c))) {
			val v1 = position(head(e)) - approxCenter
			val v2 = position(tail(e)) - approxCenter
			val tetVol = dot(v0,cross(v1,v2))
			volume += tetVol
			center += tetVol * ( approxCenter + face_centroid(f) + position(head(e)) + position(tail(e)))
		  }
		}
		cell_centroid(c) = center / (volume * 4.f)
		cell_volume(c) = volume / 6.f
	}
	
	def calcMeshGeometry() {
	for(f <- faces(mesh)) {
		// if(ID(outside(f)) < ID(inside(f))) {
		// 	calcFaceGeom(flip(f))
		// } else {
			calcFaceGeom(f)
		// }
	}
		for(c <- cells(mesh)) {
		  calcCellGeom(c)
		}
		for(f <- faces(mesh)) {
		  Print(ID(f),"FaceArea: ",face_area(f),"normal: ",face_unit_normal(f),"face_centroid",face_centroid(f))
		}
		for(c <- cells(mesh)) {
			Print("c: ",ID(c)," ",cell_volume(c)," ",cell_centroid(c))
		}
	}
	

}