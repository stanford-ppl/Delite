#ifndef _LISZT_UTIL_H
#define _LISZT_UTIL_H

#include "Liszt/ElemTypes.h"
#include "Liszt/Range.h"
#include "MeshIO/LisztFormat.h"
#include "Vector.h"
#include "Matrix.h"

static inline LisztPrivate::ElemTypes::ElemType IOToElemType(MeshIO::IOElemType io) {
	using namespace LisztPrivate;
	switch(io) {
		case MeshIO::VERTEX_T: return ElemTypes::VERTEX;
		case MeshIO::EDGE_T: return ElemTypes::EDGE;
		case MeshIO::FACE_T: return ElemTypes::FACE;
		case MeshIO::CELL_T: return ElemTypes::CELL;
		default: exit(1);
	}
}

static inline MeshIO::IOElemType ElemTypeToIO(LisztPrivate::ElemTypes::ElemType io) {
	using namespace LisztPrivate;
	switch(io) {
		case ElemTypes::VERTEX: return MeshIO::VERTEX_T;
		case ElemTypes::EDGE: return MeshIO::EDGE_T;
		case ElemTypes::FACE: return MeshIO::FACE_T;
		case ElemTypes::CELL: return MeshIO::CELL_T;
		default: exit(1);
	}
}

template <typename MeshType, typename ElemType> 
struct MeshFuncForType;
#define MESH_FUNC_LOOKUP(tag,func) \
template <typename MeshType> struct MeshFuncForType<MeshType, tag> { \
    static inline typename LisztPrivate::ElemTypes::MeshObjectForType<MeshType, tag>::set get(const MeshType& m) { return m.func(); } \
    static inline typename LisztPrivate::ElemTypes::MeshObjectForType<MeshType, tag>::set get(const MeshType& m, LisztPrivate::id_type i, LisztPrivate::id_type j) { return m.func(i, j); } \
    static inline typename LisztPrivate::ElemTypes::MeshObjectForType<MeshType, tag>::set get(const MeshType& m, const LisztPrivate::Range<tag>& range) { return m.func(range); } \
};
MESH_FUNC_LOOKUP(LisztPrivate::ElemTypes::VertexType,vertices)
MESH_FUNC_LOOKUP(LisztPrivate::ElemTypes::EdgeType,edges)
MESH_FUNC_LOOKUP(LisztPrivate::ElemTypes::FaceType,faces)
MESH_FUNC_LOOKUP(LisztPrivate::ElemTypes::CellType,cells)


//meta function that converts a C++ type into its representative LisztType object
template<typename T>
struct GetLisztType;

template<>
struct GetLisztType<int> {
	static MeshIO::LisztType type() { MeshIO::LisztType ret = { MeshIO::LISZT_INT, 0, {0, 0} }; return ret; }
};

template<>
struct GetLisztType<float> {
	static MeshIO::LisztType type() { MeshIO::LisztType ret = { MeshIO::LISZT_FLOAT, 0, {0, 0} }; return ret; }
};

template<>
struct GetLisztType<double> {
	static MeshIO::LisztType type() { MeshIO::LisztType ret = { MeshIO::LISZT_DOUBLE, 0, {0, 0} }; return ret; }
};

template<>
struct GetLisztType<bool> {
	static MeshIO::LisztType type() { MeshIO::LisztType ret = { MeshIO::LISZT_BOOL, 0, {0, 0} }; return ret; }
};

template<size_t n, typename T>
struct GetLisztType< vec<n,T> > {
	static MeshIO::LisztType type() {
		MeshIO::LisztType base = GetLisztType<T>::type();
		assert(base.flags == 0);
		MeshIO::LisztType ret = { base.type, MeshIO::LISZT_VEC_FLAG, {(char) n, 0} };
		return ret;
	}
};

template<size_t r, size_t c, typename T>
struct GetLisztType< matrix<r,c,T> > {
	static MeshIO::LisztType type() {
		MeshIO::LisztType base = GetLisztType<T>::type();
		assert(base.flags == 0);
		MeshIO::LisztType ret = { base.type, MeshIO::LISZT_MAT_FLAG, {(char) r, (char) c} };
		return ret;
	}
};
#endif
