#ifndef _MESHOBJIMPL_H_
#define _VECIMPL_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

class MeshObj {
public:
    size_t id;
    static const uint32_t MESH_DIRECTION_FLAG = 0x80000000;
    
    MeshObj() {
        id = 0;
    }

    MeshObj(size_t id) : id(id) {
    }
    
    uint32_t internalId() {
      return id & ~MESH_DIRECTION_FLAGl
    }
}

class Cell : public MeshObj {
    Cell() : MeshObj() {
    }

    Cell(size_t id) : MeshObj(id) {
    }
}

class Face : public MeshObj {
    Face() : MeshObj() {
    }

    Face(size_t id) : MeshObj(id) {
    }
}

class Edge : public MeshObj {
    Edge() : MeshObj() {
    }

    Edge(size_t id) : MeshObj(id) {
    }
}

class Vertex : public MeshObj {
    Vertex() : MeshObj() {
    }

    Vertex(size_t id) : MeshObj(id) {
    }
}

#endif