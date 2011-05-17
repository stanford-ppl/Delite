/*
 * Edge.h
 *
 *  Created on: Mar 8, 2009
 *      Author: zdevito
 */
//#include "../EdgeData.h"
#include "MeshSet.h"
#include "Liszt/ElemTypes.h"

namespace __MESH_IMPL_NAMESPACE_PRIVATE__ {

class Edge : public MeshObject {
public:
    typedef ::LisztPrivate::ElemTypes::EdgeType type;
    Edge() {}
    Edge(const EdgeData & data);
    Edge(const EdgeData & data, const EdgeInfo & fe_);
    friend class FacetEdge;

    Vertex head() const;
    Vertex tail() const;
    Vertex opposite(const Vertex & v) const;
    void flip();
    void towards(const Vertex & v);
    FacetEdge with(const Face & f) const;
    class vertex_set;
    class face_set;
    class cell_set;
    class facet_set;

    vertex_set vertices() const;


    face_set faces() const;

    face_set facesCCW() const;
    face_set facesCCW(const Vertex & v) const;

    face_set facesCW() const;
    face_set facesCW(const Vertex & v) const;


    cell_set cells() const;

    cell_set cellsCCW() const;
    cell_set cellsCCW(const Vertex & v) const;

    cell_set cellsCW() const;
    cell_set cellsCW(const Vertex & v) const;


    facet_set facets() const;
private:
    EdgeInfo info;
};

}
