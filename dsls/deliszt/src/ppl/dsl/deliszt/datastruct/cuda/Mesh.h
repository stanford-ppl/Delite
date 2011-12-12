#ifndef _MESH_H_
#define _MESH_H_

#include <cuda.h>
#include "CRS.h"
#include "MeshSet.h"
#include "MeshObj.h"
#include "BitReverse.h"

class Mesh {
public:
  const static int OUTSIDE = 0;
  const static int INSIDE = 1;
  const static int HEAD = 0;
  const static int TAIL = 1;
  const static int FORWARD = 1;
  const static int REVERSE = -1;
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

  // TODO: Transfer below structures from CPU side
  MeshSet<Cell> cells;
  MeshSet<Edge> edges;
  MeshSet<Face> faces;
  MeshSet<Vertex> vertices;


  /* Return MeshSet<Vertex> given MeshObj T */
  __device__ __host__ MeshSet<Vertex> verticesMesh(void) {
    return vertices;
  }
  __device__ __host__ MeshSet<Vertex> verticesVertex(Vertex v) {
    MeshSet<Vertex> ret;
    ret.data = vtov.values + vtov.row(internal(v));
    ret.size = vtov.row(internal(v)+1) - vtov.row(internal(v));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Vertex> verticesEdge(Edge e) {
    MeshSet<Vertex> ret;
    ret.data = etov.values + etov.row(internal(e));
    ret.size = etov.row(internal(e)+1) - etov.row(internal(e));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Vertex> verticesFace(Face f) {
    MeshSet<Vertex> ret;
    ret.data = ftov.values + ftov.row(internal(f));
    ret.size = ftov.row(internal(f)+1) - ftov.row(internal(f));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Vertex> verticesCell(Cell c) {
    MeshSet<Vertex> ret;
    ret.data = ctov.values + ctov.row(internal(c));
    ret.size = ctov.row(internal(c)+1) - ctov.row(internal(c));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Vertex> verticesCCW(Face f) {
    MeshSet<Vertex> ret;
    Cell c = outside(f);
    if(ftoc.apply(internal(f), OUTSIDE) == internal(c)) {
      ret.data = ftov.values + ftov.row(internal(f));
      ret.size = ftov.row(internal(f)+1) - ftov.row(internal(f));
      ret.dir = FORWARD;
    } else {
      ret.data = ftov.values + ftov.row(internal(f)+1)-1;
      ret.size = ftov.row(internal(f)+1) - ftov.row(internal(f));
      ret.dir = REVERSE;
    }
    return ret;
  }
  __device__ __host__ MeshSet<Vertex> verticesCW(Face f) {
    MeshSet<Vertex> ret;
    Cell c = outside(f);
    if(ftoc.apply(internal(f), INSIDE) == internal(c)) {
      ret.data = ftov.values + ftov.row(internal(f));
      ret.size = ftov.row(internal(f)+1) - ftov.row(internal(f));
      ret.dir = FORWARD;
    } else {
      ret.data = ftov.values + ftov.row(internal(f)+1)-1;
      ret.size = ftov.row(internal(f)+1) - ftov.row(internal(f));
      ret.dir = REVERSE;
    }
    return ret;
  }
  //TODO: etov? ctov? Which one is correct?
  __device__ __host__ Vertex vertex(int eIdx, int i) {
    return ctov.apply(eIdx, i);
  }

  /* Return MeshSet<Cell> given MeshObj T */
  __device__ __host__ MeshSet<Cell> cellsMesh(void) {
    return cells;
  }
  __device__ __host__ MeshSet<Cell> cellsVertex(Vertex v) {
    MeshSet<Cell> ret;
    ret.data = vtoc.values + vtoc.row(internal(v));
    ret.size = vtoc.row(internal(v)+1) - vtoc.row(internal(v));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Cell> cellsEdge(Edge e) {
    MeshSet<Cell> ret;
    ret.data = etoc.values + etoc.row(internal(e));
    ret.size = etoc.row(internal(e)+1) - etoc.row(internal(e));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Cell> cellsFace(Face f) {
    MeshSet<Cell> ret;
    ret.data = ftoc.values + ftoc.row(internal(f));
    ret.size = ftoc.row(internal(f)+1) - ftoc.row(internal(f));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Cell> cellsCell(Cell c) {
    MeshSet<Cell> ret;
    ret.data = ctoc.values + ctoc.row(internal(c));
    ret.size = ctoc.row(internal(c)+1) - ctoc.row(internal(c));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Cell> cellsCCW(Edge e) {
    MeshSet<Cell> ret;
    Cell c = head(e);
    if(etov.apply(internal(e), HEAD) == internal(c)) {
      ret.data = etoc.values + etoc.row(internal(e));
      ret.size = etoc.row(internal(e)+1) - etoc.row(internal(e));
      ret.dir = FORWARD;
    } else {
      ret.data = etoc.values + etoc.row(internal(e)+1)-1;
      ret.size = etoc.row(internal(e)+1) - etoc.row(internal(e));
      ret.dir = REVERSE;
    }
    return ret;
  }
  __device__ __host__ MeshSet<Cell> cellsCW(Edge e) {
    MeshSet<Cell> ret;
    Cell c = head(e);
    if(etov.apply(internal(e), TAIL) == internal(c)) {
      ret.data = etoc.values + etoc.row(internal(e));
      ret.size = etoc.row(internal(e)+1) - etoc.row(internal(e));
      ret.dir = FORWARD;
    } else {
      ret.data = etoc.values + etoc.row(internal(e)+1)-1;
      ret.size = etoc.row(internal(e)+1) - etoc.row(internal(e));
      ret.dir = REVERSE;
    }
    return ret;
  }


  /* Return MeshSet<Edge> given MeshObj T */
  __device__ __host__ MeshSet<Edge> edgesMesh(void) {
    return edges;
  }
  __device__ __host__ MeshSet<Edge> edgesVertex(Vertex v) {
    MeshSet<Edge> ret;
    ret.data = vtoe.values + vtoe.row(internal(v));
    ret.size = vtoe.row(internal(v)+1) - vtoe.row(internal(v));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Edge> edgesFace(Face f) {
    MeshSet<Edge> ret;
    ret.data = ftoe.values + ftoe.row(internal(f));
    ret.size = ftoe.row(internal(f)+1) - ftoe.row(internal(f));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Edge> edgesCell(Cell c) {
    MeshSet<Edge> ret;
    ret.data = ctoe.values + ctoe.row(internal(c));
    ret.size = ctoe.row(internal(c)+1) - ctoe.row(internal(c));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Edge> edgesCCW(Face f) {
    MeshSet<Edge> ret;
    Cell c = outside(f);
    if(ftoc.apply(internal(f), HEAD) == internal(c)) {
      ret.data = ftoe.values + ftoe.row(internal(f));
      ret.size = ftoe.row(internal(f)+1) - ftoe.row(internal(f));
      ret.dir = FORWARD;
    } else {
      ret.data = ftoe.values + ftoe.row(internal(f)+1)-1;
      ret.size = ftoe.row(internal(f)+1) - ftoe.row(internal(f));
      ret.dir = REVERSE;
    }
    return ret;
  }
  __device__ __host__ MeshSet<Edge> edgesCW(Face f) {
    MeshSet<Edge> ret;
    Cell c = outside(f);
    if(ftoc.apply(internal(f), TAIL) == internal(c)) {
      ret.data = ftoe.values + ftoe.row(internal(f));
      ret.size = ftoe.row(internal(f)+1) - ftoe.row(internal(f));
      ret.dir = FORWARD;
    } else {
      ret.data = ftoe.values + ftoe.row(internal(f)+1)-1;
      ret.size = ftoe.row(internal(f)+1) - ftoe.row(internal(f));
      ret.dir = REVERSE;
    }
    return ret;
  }


  /* Return MeshSet<Face> given MeshObj T */
  __device__ __host__ MeshSet<Face> facesMesh(void) {
    return faces;
  }
  __device__ __host__ MeshSet<Face> facesVertex(Vertex v) {
    MeshSet<Face> ret;
    ret.data = vtof.values + vtof.row(internal(v));
    ret.size = vtof.row(internal(v)+1) - vtof.row(internal(v));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Face> facesEdge(Edge e) {
    MeshSet<Face> ret;
    ret.data = etof.values + etof.row(internal(e));
    ret.size = etof.row(internal(e)+1) - etof.row(internal(e));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Face> facesCell(Cell c) {
    MeshSet<Face> ret;
    ret.data = ctof.values + ctof.row(internal(c));
    ret.size = ctof.row(internal(c)+1) - ctof.row(internal(c));
    ret.dir = FORWARD;
    return ret;
  }
  __device__ __host__ MeshSet<Face> facesCCW(Edge e) {
    MeshSet<Face> ret;
    Vertex v = head(e);
    if(ftoc.apply(internal(e), HEAD) == internal(v)) {
      ret.data = etof.values + etof.row(internal(e));
      ret.size = etof.row(internal(e)+1) - etof.row(internal(e));
      ret.dir = FORWARD;
    } else {
      ret.data = etof.values + etof.row(internal(e)+1)-1;
      ret.size = etof.row(internal(e)+1) - etof.row(internal(e));
      ret.dir = REVERSE;
    }
    return ret;
  }
  __device__ __host__ MeshSet<Face> facesCW(Edge e) {
    MeshSet<Face> ret;
    Vertex v = head(e);
    if(ftoc.apply(internal(e), TAIL) == internal(v)) {
      ret.data = etoc.values + etoc.row(internal(e));
      ret.size = etoc.row(internal(e)+1) - etoc.row(internal(e));
      ret.dir = FORWARD;
    } else {
      ret.data = etoc.values + etoc.row(internal(e)+1)-1;
      ret.size = etoc.row(internal(e)+1) - etoc.row(internal(e));
      ret.dir = REVERSE;
    }
    return ret;
  }

  __device__ __host__ Face face(int eIdx, int i) {
    return etof.apply(eIdx,i);
  }

  __device__ __host__ int head(Edge e) {
    return etov.apply(internal(e), (reversed(e)) ? 1 : 0);
  }
  __device__ __host__ int tail(Edge e) {
    return etov.apply(internal(e), (reversed(e)) ? 0 : 1);
  }

  __device__ __host__ Cell outside(Face f) {
    int offset = reversed(f) ? 1 : 0;
    return ftoc.apply(internal(f), offset);
  }

  __device__ __host__ Cell inside(Face f) {
    int offset = reversed(f) ? 0 : 1;
    return ftoc.apply(internal(f), offset);
  }

  __device__ __host__ Edge towardsEdgeVertex(Edge e, Vertex v) {
    bool facing = internal(etov.apply(internal(e), HEAD)) == internal(v);
    return ((facing) ? e : flip(e));
  }

  __device__ __host__ Face towardsFaceCell(Face f, Cell c) {
    bool facing = internal(ftoc.apply(internal(f), OUTSIDE)) == internal(c);
    return ((facing) ? f : flip(f));
  }

};

#endif
