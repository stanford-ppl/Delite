/*
 * DataObjects.h
 *
 *  Created on: Sep 27, 2009
 *      Author: zdevito
 */

#ifndef CRSDATAOBJECTS_H_
#define CRSDATAOBJECTS_H_

namespace CRSMeshPrivate {

struct MeshObjectData {
    MeshObjectData() {}
    MeshObjectData(const Mesh * m, id_type i)
    : mesh(m), id(i) {}
    MeshObjectData(const Mesh * m, id_type i, id_type set_id_)
    : mesh(m), id(i), set_id(set_id_) {}
    const Mesh * mesh;
    id_type id;
    id_type set_id;
};

struct EmptyInfo {};
typedef EmptyInfo FaceInfo;
typedef EmptyInfo EdgeInfo;

}

#endif /* DATAOBJECTS_H_ */
