/*
 * Cell.h
 *
 *  Created on: Mar 8, 2009
 *      Author: zdevito
 */

#include "MeshSet.h"
#include "Liszt/ElemTypes.h"

namespace __MESH_IMPL_NAMESPACE_PRIVATE__ {


class Cell : public MeshObject {
public:
    typedef ::LisztPrivate::ElemTypes::CellType type;
    Cell() {}
    Cell(const CellData & data);
    class cell_set;
    class edge_set;
    class face_set;
    class vertex_set;
    vertex_set vertices() const;
    edge_set edges() const;
    face_set faces() const;
    cell_set cells() const;
    id_type _set_id() const;
};

}

