#ifndef CRSMESH_H_
#define CRSMESH_H_
#include <cstdlib>
#include <cassert>
#include <cstring>
#include "Liszt/ElemTypes.h"

//Forward Decls
namespace CRSMeshPrivate {

using ::LisztPrivate::ElemTypes::id_type;
class MeshObjectData;
typedef MeshObjectData VertexData;
typedef MeshObjectData EdgeData;
typedef MeshObjectData FaceData;
typedef MeshObjectData CellData;
typedef MeshObjectData FacetEdgeData;

class MeshObject;
class Vertex;
class Edge;
class Face;
class Cell;
class FacetEdge;
class Mesh;

}

//Interface Decls
#define __MESH_IMPL_NAMESPACE_PRIVATE__ CRSMeshPrivate

#include "MeshInterface/Types.h"
#include "CRSMesh/CRS.h"
#include "CRSMesh/Iterators.h"

#include "CRSMesh/DataObjects.h"

#include "CRSMesh/MeshObject.h"
#include "MeshInterface/FacetEdge.h"
#include "MeshInterface/Vertex.h"
#include "MeshInterface/Edge.h"
#include "MeshInterface/Face.h"
#include "MeshInterface/Cell.h"

#include "CRSMesh/MeshData.h"
#include "MeshInterface/Mesh.h"

#include "ForAll.h"

#undef __MESH_IMPL_NAMESPACE_PRIVATE__

#include "MeshSet.h"

//Implementation Files
namespace CRSMeshPrivate {

#define SUBCLASS_SIZEDMESHSET(outer,name,it) \
    class outer::name : public InternalSizeMeshSet<it> { \
    public: \
        name() {} \
        name(const it & i) : InternalSizeMeshSet<it>(i) {} \
    };

typedef CRSIterator<Vertex> vertex_it;
typedef CRSIterator<Edge> edge_it;
typedef CRSIterator<Face> face_it;
typedef CRSIterator<Cell> cell_it;
typedef CRSIterator<FacetEdge> facet_it;

#include "CRSMesh/Edge.cpp"
#include "CRSMesh/Cell.cpp"
#include "CRSMesh/Face.cpp"
#include "CRSMesh/Vertex.cpp"
#include "CRSMesh/Mesh.cpp"

#undef SUBCLASS_SIZEDMESHSET
}

//expose the interface classes here
namespace CRSMesh {
using CRSMeshPrivate::Vertex;
using CRSMeshPrivate::Edge;
using CRSMeshPrivate::Face;
using CRSMeshPrivate::Cell;
using CRSMeshPrivate::FacetEdge;
using CRSMeshPrivate::Mesh;
}
#endif /* FACETEDGEMESH_H_ */
