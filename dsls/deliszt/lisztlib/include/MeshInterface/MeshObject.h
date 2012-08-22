/*
 * MeshObject.h
 *
 *  Created on: Mar 8, 2009
 *      Author: zdevito
 */
//#include "../MeshObjectData.h"
#include "Liszt/ElemTypes.h"

namespace __MESH_IMPL_NAMESPACE_PRIVATE__ {

using LisztPrivate::ElemTypes::id_type;

class Mesh;

class MeshObject {
public:
    typedef Mesh mesh_type;
    MeshObject() {}
    MeshObject(const MeshObjectData & data__) : data_(&data__) {}
    id_type ID() const { return data_->id; }
    bool operator==(const MeshObject & rhs) const { return ID() == rhs.ID(); }
    bool operator!=(const MeshObject & rhs) const { return !(*this == rhs); }
protected:
    const MeshObjectData * data() const { return data_; }
private:
    const MeshObjectData * data_;
};

}
