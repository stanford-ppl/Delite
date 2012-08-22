/*
 * MeshObject.h
 *
 *  Created on: Sep 27, 2009
 *      Author: zdevito
 */

#ifndef CRSMESHOBJECT_H_
#define CRSMESHOBJECT_H_

namespace CRSMeshPrivate {
    class Mesh;
}
#include "DataObjects.h"



namespace CRSMeshPrivate {

using LisztPrivate::ElemTypes::id_type;

class Mesh;

class MeshObject {
public:
    typedef Mesh mesh_type;
    MeshObject() {}
    MeshObject(const MeshObjectData & data_)
    : data(data_){}
    id_type ID() const {
        return ~FLIP_DIRECTION_BIT & data.id;
    }
    bool operator==(const MeshObject & rhs) const { return ID() == rhs.ID(); }
    bool operator!=(const MeshObject & rhs) const { return !(*this == rhs); }
    id_type get_set_id() const {
        return data.set_id ;
    }
protected:
    MeshObjectData data;
};

}

#endif /* MESHOBJECT_H_ */
