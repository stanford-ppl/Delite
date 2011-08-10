#ifndef _MESHOBJIMPL_H_
#define _VECIMPL_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

class MeshObj {
public:
    size_t id;
    static const uint32_t MESH_DIRECTION_FLAG = 0x80000000;
    
    __host__ __device__ MeshObj() {
        id = 0;
    }

    __host__ __device__ MeshObj(size_t id) : id(id) {
    }
    
    __host__ __device__ uint32_t internalId() {
      return id & ~MESH_DIRECTION_FLAGl
    }
}

class Cell : public MeshObj {
    __host__ __device__ Cell() : MeshObj() {
    }

    __host__ __device__ Cell(size_t id) : MeshObj(id) {
    }
}

class Face : public MeshObj {
    __host__ __device__ Face() : MeshObj() {
    }

    __host__ __device__ Face(size_t id) : MeshObj(id) {
    }
}

class Edge : public MeshObj {
    __host__ __device__ Edge() : MeshObj() {
    }

    __host__ __device__ Edge(size_t id) : MeshObj(id) {
    }
}

class Vertex : public MeshObj {
    __host__ __device__ Vertex() : MeshObj() {
    }

    __host__ __device__ Vertex(size_t id) : MeshObj(id) {
    }
}

#endif