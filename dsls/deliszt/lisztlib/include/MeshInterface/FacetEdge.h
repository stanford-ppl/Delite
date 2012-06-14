/*
 * FacetEdge.h
 *
 *  Created on: Mar 8, 2009
 *      Author: zdevito
 */
#include <iostream>
#include "Liszt/ElemTypes.h"

namespace __MESH_IMPL_NAMESPACE_PRIVATE__ {
using LisztPrivate::ElemTypes::id_type;
class FacetEdge {
public:
    FacetEdge();
    FacetEdge(const FacetEdgeData * i);
    FacetEdge(const FacetEdgeData * i, CellSide e, VertexSide f);
    FacetEdge(const FacetEdge & fe);

    void reset(const FacetEdgeData * i, CellSide e, VertexSide f);
    void reset(const FacetEdge & f);

    FacetEdge flipEdge() const;
    FacetEdge flipFace() const;
    FacetEdge flipIfMismatched(CellSide ed, VertexSide fd) const;

    FacetEdge towards(const Cell & c) const;    // Make outside cell of returned facetedge be the given cell.
    FacetEdge towards(const Face & f) const;    // Make outside cell of returned facetedge be the cell that shares the given face.
    FacetEdge towards(const Edge & e) const;    // Make head vertex of returned facetedge be the vertex that shares the given edge.
    FacetEdge towards(const Vertex & v) const;  // Make head vertex of returned facetedge be the given vertex.

    bool operator==(const FacetEdge & rhs) const;
    bool operator!=(const FacetEdge & rhs) const;

    Vertex head() const;
    Vertex tail() const;
    Edge  edge() const;
    Face  face() const;
    Cell outside() const;
    Cell inside() const;

    Cell opposite(const Cell & c) const;
    Vertex opposite(const Vertex & v) const;

#define MAKE_ADJ(name,ret,around,dir) \
    ret name() const; \
    ret name##Towards(const around & d) const;

    MAKE_ADJ(faceCWAroundEdge,Face,Vertex,fd);
    MAKE_ADJ(faceCCWAroundEdge,Face,Vertex,fd);

    MAKE_ADJ(cellCWAroundEdge,Cell,Vertex,fd);
    MAKE_ADJ(cellCCWAroundEdge,Cell,Vertex,fd);

    MAKE_ADJ(edgeCWAroundFace,Edge,Cell,ed);
    MAKE_ADJ(edgeCCWAroundFace,Edge,Cell,ed);

    MAKE_ADJ(vertexCWAroundFace,Vertex,Cell,ed);
    MAKE_ADJ(vertexCCWAroundFace,Vertex,Cell,ed);

    MAKE_ADJ(facetedgeCWAroundEdge,FacetEdge,Vertex,fd);
    MAKE_ADJ(facetedgeCCWAroundEdge,FacetEdge,Vertex,fd);

    MAKE_ADJ(facetedgeCWAroundFace,FacetEdge,Cell,ed);
    MAKE_ADJ(facetedgeCCWAroundFace,FacetEdge,Cell,ed);

#undef MAKE_ADJ
    id_type ID() const;
    const FacetEdgeData * internal() const;
private:
    // Unlike all other implementations, all the private methods of FacetEdge
    // use an absolute direction scheme, with the concepts of head, tail,
    // inside, and outside remaining fixed regardless of the current
    // user-perceived orientation using cd and vd. With this fixed scheme,
    // clockwise and counter-clockwise around an edge/face also remain fixed.

    // ..........
    // ^ head   .
    // |        .  Imagine an English language book with the front cover facing
    // |   IN   .  the viewer,  with the spine on the left side (so the text is
    // |        .  right-side up). The edge is the spine of the book, with the
    // |   TUO  .  head at the top and the tail at the  bottom. The front cover
    // |        .  touches the inside cell, while the back cover touches the
    // | tail   .  outside cell.
    // ..........

    // By convention, all AroundEdge functions maintain the same head/tail
    // vertex orientation in the returned FacetEdge, while the cell adjacent
    // to this face becomes the inside of the next face (i.e. the outside
    // "points" out on the FacetEdge returned.
    // Similarly, by convention all AroundFace functions maintain the same
    // inside/outside cell orientation (i.e. the cell that was outside is
    // still the outside cell in the FacetEdge returned), while the edge of
    // the return value will always have the tail vertex be the one that was
    // shared with this edge (i.e. the head always points "away" from this FE).

    Face faceCWAroundEdgeTowards(VertexSide dir) const;
    Face faceCCWAroundEdgeTowards(VertexSide dir) const;
    Cell cellCCWAroundEdgeTowards(VertexSide dir) const;
    Cell cellCWAroundEdgeTowards(VertexSide dir) const;
    Edge edgeCCWAroundFaceTowards(CellSide dir) const;
    Edge edgeCWAroundFaceTowards(CellSide dir) const;
    Vertex vertexCCWAroundFaceTowards(CellSide dir) const;
    Vertex vertexCWAroundFaceTowards(CellSide dir) const;

    FacetEdge facetedgeCWAroundEdgeTowards(VertexSide dir) const;
    FacetEdge facetedgeCCWAroundEdgeTowards(VertexSide dir) const;
    FacetEdge facetedgeCWAroundFaceTowards(CellSide dir) const;
    FacetEdge facetedgeCCWAroundFaceTowards(CellSide dir) const;

    VertexSide directionVertex(id_type v) const;
    CellSide directionCell(id_type c) const;
private:
    const FacetEdgeData * data;
    CellSide cd;   // Marks which side of the edge is vert[0]
    VertexSide vd; // Marks which side of the face is face[0]

    friend std::ostream & operator<<(std::ostream & o, const FacetEdgeData & ife);
    friend std::ostream & operator<<(std::ostream & o, const FacetEdge & ife);
};
inline std::ostream & operator<<(std::ostream & o, const FacetEdge & ie);
}  // namespace __MESH_IMPL_NAMESPACE_PRIVATE__

