package ppl.apps.liszt.mat_test

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object MatTestRunner extends DeLisztApplicationRunner with MatTest

object MatTest extends DeLisztApplication {
	def main() {
		val m = Mat(Vec(1,2),Vec(3,4))
		val a = 3 * m
		val b = 3 / m
		val c = m * 3
		val d = m / 3
		
		val e = m + m
		val f = m - m
		val g = -m 
		val h = m(_1,_1)
		val v = Vec(1,2)
		val mv = m * v
		
		val i = Mat(Vec(1))
		val j = Mat(Vec(1,2),Vec(3,4))
		val k = Mat(Vec(1,2,3),Vec(4,5,6),Vec(7,8,9))
		val l = Mat(Vec(1,2,3,4),Vec(1,2,3,4),Vec(1,2,3,4),Vec(1,2,3,4))
		val n = Mat(Vec(1,2,3,4),Vec(1,2,3,4),Vec(1,2,3,4),Vec(1,2,3,4),Vec(1,2,3,4))
		
		val o = Mat(Vec(1,2,3),Vec(4,5,6))
		val vv = Vec(7,8,9)
		val r = o * vv
		val s = m * r
		Print(m,a,b,c,d,e,f,g,h,v,mv,i,j,k,l,n,o,vv,r,s)
		
		val sx = new SparseVector[Float]
		val sm = new SparseMatrix[Float]
		
		for(c <- cells(mesh)) {
		    for(c2 <- cells(c))
		        sm(c,c2) = 1
		}
		for(c <- cells(mesh)) {
			//sx(c & 0) = ID(c)
			//sx(c & 1) = ID(c)
			sx(c) = ID(c)
			//sx(c,_2)
			//sx(c) = Vec(0.f + ID(c),1.f + ID(c))
			for(c2 <- cells(c)) {
				//sm(c,c2) = 1
				//sm(c & 0,c2 & 0) = 1
				//sm(c & 0,c2 & 1) = 1
				//sm(c & 1,c2 & 0) = 2
				//sm(c & 1,c2 & 1) = 2
			    //sm(c,c2) = Mat(Vec(1.f,1.f),Vec(2.f,2.f))
			    sm(c,c2) += 2
			}
		}
		val sb = sm * sx
		for(c <- cells(mesh)) {
			var id_sum = 0
			for(c2 <- cells(c)) {
				id_sum += ID(c2)
			}
			//Print(ID(c),": ", id_sum, " ", sb(c & 0), sb(c & 1))
			//Print(ID(c),": ", id_sum, " ", sb(c,_2))
			Print(ID(c),": ", id_sum, " ", sb(c))
		}
		
		val sm2 = new SparseMatrix[Float]
		val sv2 = new SparseVector[Float]
		
		for(c <- cells(mesh)) {
			sm2(c,c) = 10
			sv2(c) = ID(c)
		}
		var ii = 0
		while(ii < 3) {
		val newx = sm2 / sv2
		for(c <- cells(mesh)) {
			Print("New X ",ID(c),": ",newx(c))
		}
		ii += 1
		}
	}
}