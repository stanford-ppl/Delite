#ifndef _MESH_H_
#define _MESH_H_

#include <cuda.h>
#include "CRS.h"
#include "MeshSet.h"
#include "MeshObj.h"

class Mesh {
public:
  const static int OUTSIDE = 0;
  const static int INSIDE = 1;
  const static int HEAD = 0;
  const static int TAIL = 1;
  const static int id = 0;

  int nvertices;
  int nedges;
  int nfaces;
  int ncells;

  CRS vtov;
  CRS vtoe;
  CRS vtof;
  CRS vtoc;

  CRS etov;
  CRS etof;
  CRS etoc;

  CRS ftov;
  CRS ftoe;
  CRS ftoc;

  CRS ctov;
  CRS ctoe;
  CRS ctof;
  CRS ctoc;

  // Use special CellSetImpl, don't expose 0 cell
  MeshSet<Cell> cells;
  MeshSet<Edge> edges;
  MeshSet<Face> faces;
  MeshSet<Vertex> vertices;

  __device__ __host__ Cell outside(Face f) {
    int offset = reversed(f) ? 1 : 0;
    return ftoc.apply(f, offset);
  }

  __device__ __host__ Cell inside(Face f) {
    int offset = reversed(f)? 0 : 1;
    return ftoc.apply(f, offset);
  }

  __device__ __host__ MeshSet<Cell> cellsFace(Face f) {
    MeshSet<Cell> ret;
    ret.data = ftoc.values + ftoc.row(internal(f));
    ret.size = ftoc.row(internal(f)+1) - ftoc.row(internal(f));
    return ret;
  }

};

#endif