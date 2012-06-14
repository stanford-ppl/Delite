/*
 * Vertex.h
 *
 *  Created on: Mar 8, 2009
 *      Author: zdevito
 */

#include "MeshSet.h"
#include "Liszt/ElemTypes.h"

namespace __MESH_IMPL_NAMESPACE_PRIVATE__ {

class Vertex : public MeshObject {
public:
    typedef ::LisztPrivate::ElemTypes::VertexType type;
    Vertex() {}
    Vertex(const VertexData & data);
    class cell_set;
    class edge_set;
    class face_set;
    class vertex_set;
    vertex_set vertices() const;
    edge_set edges() const;
    face_set faces() const;
    cell_set cells() const;

};

}
