/*
 * MeshData.h
 *
 *  Created on: Sep 26, 2009
 *      Author: zd
 */

#ifndef CRSMESHDATA_H_
#define CRSMESHDATA_H_

namespace CRSMeshPrivate {
struct MeshData {
	size_t nvertices;
	size_t nedges;
	size_t nfaces;
	size_t ncells;

	CRS vtov;
	CRS vtoe;
	CRS vtof;
	CRS vtoc;

	CRSConst etov;
	CRS etof;
	CRS etoc;

	CRS ftov;
	CRS ftoe;
	CRSConst ftoc;

	CRS ctov;
	CRS ctoe;
	CRS ctof;
	CRS ctoc;
};

}

#endif /* MESHDATA_H_ */
