/*
def printMesh {
	println("Printing Mesh")
	
	println(nvertices)
	println(nedges)
	println(nfaces)
	println(ncells)
	
	for(crs <- List(vtov,vtoe,vtof,vtoc,etov,etof,etoc,ftov,ftoe,ftoc,ctov,ctoe,ctof,ctoc)) {

		if(crs.isInstanceOf[CRSImpl]) {
			println(crs.asInstanceOf[CRSImpl].rows.length)
			for(elem <- crs.asInstanceOf[CRSImpl].rows) print(elem + " ")
			println("")
			println(crs.asInstanceOf[CRSImpl].values.length)
			for(elem <- crs.asInstanceOf[CRSImpl].values) print(elem + " ")
			println("")
		}
		else {
			val rows_length = 1 + (crs.asInstanceOf[CRSConst].values.length / crs.asInstanceOf[CRSConst].mult) 
			println(rows_length)
			var idx = 0
			while(idx < rows_length) { print(crs.row(idx) + " "); idx = idx + 1; }
			println("")
			println(crs.asInstanceOf[CRSConst].values.length)
			for(elem <- crs.asInstanceOf[CRSConst].values) print(elem + " ")
			println("")
		}
	}
	println("Printing Mesh done")
}

def printMeshSet {
  println("Print x4:")
  println(1)
  println(x4.size)
  var idx = 0
  while(idx < x4.size) { print(x4.apply(idx) + " "); idx = idx + 1; }
  println("")
  println("x4 print done")
}
*/
