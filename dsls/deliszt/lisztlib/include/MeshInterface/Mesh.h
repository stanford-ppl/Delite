/*
 * Mesh.h
 *
 *  Created on: Mar 9, 2009
 *      Author: zdevito
 */

#include "Liszt/ElemTypes.h"
#include "Liszt/Range.h"

#include "MeshIO/FacetEdgeBuilder.h"
#include "Liszt/Util.h"

#include "noncopyable.h"

namespace __MESH_IMPL_NAMESPACE_PRIVATE__ {
using ::LisztPrivate::ElemTypes::id_type;
using ::LisztPrivate::Range;

class Mesh : public Liszt::noncopyable {
public:
    typedef Vertex vertex_t;
    typedef Edge edge_t;
    typedef Face face_t;
    typedef Cell cell_t;
    Mesh() {}
    template<typename LM> explicit Mesh(LM & m);
    template<typename LM> void init(LM & m);
    
    void initFromFacetEdgeBuilder(MeshIO::FacetEdgeBuilder * builder);
    ~Mesh();

    template<typename MO> MO get(id_type id) const;
    template<typename ElemType> typename ::LisztPrivate::MeshObjectForType<Mesh, ElemType>::set get() const;
    // Returns set of vertices with ID in the range of [i,j).
    template<typename ElemType> typename ::LisztPrivate::MeshObjectForType<Mesh, ElemType>::set get(id_type id, id_type j) const;
    template<typename ElemType> typename ::LisztPrivate::MeshObjectForType<Mesh, ElemType>::set get(const Range<ElemType>& range) const;

    class vertex_set;
    class edge_set;
    class face_set;
    class cell_set;

    vertex_set vertices() const;
    vertex_set vertices(id_type i, id_type j) const;
    vertex_set vertices(const Range<vertex_t::type> & range) const;

    edge_set edges() const;
    edge_set edges(id_type i, id_type j) const;
    edge_set edges(const Range<edge_t::type> & range) const;

    face_set faces() const;
    face_set faces(id_type i, id_type j) const;
    face_set faces(const Range<face_t::type> & range) const;

    cell_set cells() const;
    cell_set cells(id_type i, id_type j) const;
    cell_set cells(const Range<cell_t::type> & range) const;

    MeshData data;
private:
    friend class Vertex;
    friend class Edge;
    friend class Face;
    friend class Cell;
};

template<typename ElemType>
inline typename ::LisztPrivate::MeshObjectForType<Mesh, ElemType>::set
Mesh::get() const {
    return MeshFuncForType<Mesh, ElemType>::get(*this);
}
template<typename ElemType>
inline typename ::LisztPrivate::MeshObjectForType<Mesh, ElemType>::set
Mesh::get(id_type i, id_type j) const {
    return MeshFuncForType<Mesh, ElemType>::get(*this, i, j);
}

template<typename ElemType>
inline typename ::LisztPrivate::MeshObjectForType<Mesh, ElemType>::set
Mesh::get(const Range<ElemType>& range) const {
    return MeshFuncForType<Mesh, ElemType>::get(*this, range);
}

}  // namespace __MESH_IMPL_NAMESPACE_PRIVATE__
