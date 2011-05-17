/*
 * Face.h
 *
 *  Created on: Mar 8, 2009
 *      Author: zdevito
 */

#include "MeshSet.h"
#include "Liszt/ElemTypes.h"

namespace __MESH_IMPL_NAMESPACE_PRIVATE__ {

class Face : public MeshObject {
public:
    typedef ::LisztPrivate::ElemTypes::FaceType type;
    Face() {}
    Face(const FaceData & data);
    Face(const FaceData & data, const FaceInfo & fe_);
    friend class FacetEdge;

    Cell outside() const;
    Cell inside() const;
    Cell opposite(const Cell & v) const;
    void flip();
    void towards(const Cell & v);
    FacetEdge with(const Edge & f) const;

    class vertex_set;
    class edge_set;
    class cell_set;
    class facet_set;

    vertex_set vertices() const;
    vertex_set verticesCW() const;
    vertex_set verticesCW(const Cell & c) const;
    vertex_set verticesCCW() const;
    vertex_set verticesCCW(const Cell & c) const;

    edge_set edges() const;
    edge_set edgesCW() const;
    edge_set edgesCW(const Cell & c) const;
    edge_set edgesCCW() const;
    edge_set edgesCCW(const Cell & c) const;
    cell_set cells() const;
    facet_set facets() const;

private:
    FaceInfo info;
};

}

