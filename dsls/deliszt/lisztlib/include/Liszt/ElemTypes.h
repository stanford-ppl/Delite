/*
 * ElementRef.h
 *
 *  Created on: Jul 21, 2009
 *      Author: zdevito
 */

#ifndef ELEMTYPES_H_
#define ELEMTYPES_H_

namespace LisztPrivate {

namespace ElemTypes {

enum ElemType {VERTEX,EDGE,FACE,CELL,ELEM_TYPE_SIZE};

struct VertexType {
    static const ElemType value = VERTEX;
};
struct EdgeType {
    static const ElemType value = EDGE;
};
struct FaceType {
    static const ElemType value = FACE;
};
struct CellType {
    static const ElemType value = CELL;
};
struct AnyType {
    // This class is used in cases where element type is unimportant,
    // such as in DescriptorMap storage
    // Intentionally omitting "value"
};


typedef unsigned int id_type;
typedef int rank_type;

template<typename T>
class ElemRef {
public:
    ElemRef() : id(0) {}
    ElemRef(id_type i)
    : id(i) {}
    typedef T elem_type;
    static const ElemType elem_value = elem_type::value;
    id_type ID() const {
        return id;
    }
private:
    id_type id;
};

template<typename T>
inline ElemRef<typename T::type> GetRef(const T & t) {
    return ElemRef<typename T::type>(t.ID());
}

typedef ElemRef<VertexType> VertexRef;
typedef ElemRef<EdgeType>   EdgeRef;
typedef ElemRef<FaceType>   FaceRef;
typedef ElemRef<CellType>   CellRef;


template <typename Mesh, typename ElemType>
struct MeshObjectForType;

template <typename Mesh>
struct MeshObjectForType<Mesh, VertexType> {
    typedef typename Mesh::vertex_t type;
    typedef typename Mesh::vertex_set set;
};
template <typename Mesh>
struct MeshObjectForType<Mesh, EdgeType> {
    typedef typename Mesh::edge_t type;
    typedef typename Mesh::edge_set set;
};
template <typename Mesh>
struct MeshObjectForType<Mesh, FaceType> {
    typedef typename Mesh::face_t type;
    typedef typename Mesh::face_set set;
};
template <typename Mesh>
struct MeshObjectForType<Mesh, CellType> {
    typedef typename Mesh::cell_t type;
    typedef typename Mesh::cell_set set;
};

}

using namespace ElemTypes;
}

#endif /* ELEMENTREF_H_ */
