#ifndef _LISZT_FORMAT_H
#define _LISZT_FORMAT_H

#include <stdint.h>
#include <string>
#include <stdexcept>

namespace MeshIO {

typedef uint64_t file_ptr;
typedef uint32_t lsize_t;
typedef uint32_t id_t;
static const uint32_t LISZT_MAGIC_NUMBER = 0x18111022;

class MeshLoadException : public std::runtime_error {
public:
    MeshLoadException(const std::string& message) :
            std::runtime_error(message) {
    }
    ;
};

struct LisztHeader {
	uint32_t magic_number; //LISZT_MAGIC_NUMBER
	lsize_t nV,nE,nF,nC,nFE, nBoundaries;
	file_ptr position_table;
	file_ptr facet_edge_table;
	file_ptr boundary_set_table;
} __attribute__((packed));

struct FileFacetEdge { //
	struct HalfFacet {
		id_t cell;
		id_t vert;
	} __attribute__((packed));
	id_t face;
	id_t edge;
	HalfFacet hf[2];
} __attribute__((packed));

struct PositionTable {
	double data[0][3]; //c++ magic, position at the beginning of the Position table and then,
	//index this.data[face_num][0-2] to get the data out again
} __attribute__((packed));

//This ordering pairs the duals, which is useful when writing methods that work on the
//mesh and the dual mesh
enum IOElemType {
    VERTEX_T = 0,
    CELL_T = 1,
    EDGE_T = 2,
    FACE_T = 3,
    TYPE_SIZE = 4,
    AGG_FLAG = 1 << 7  // Set high bit if aggregating multiple element types
};

struct BoundarySet {
	IOElemType type; //type of boundary set
	union {
	    id_t start; //inclusive
	    lsize_t left_id; // number of entries from beginning of table
	};
	union {
	    id_t end; //exclusive
	    lsize_t right_id; // number of entries from beginning of table
	};
	file_ptr name_string; //offset to null-terminated string
} __attribute__((packed));

//The following structures are used to represent fields
//currently there is not space in a liszt mesh file for fields,
//but we may add one later.  For now, each field will have its own file.

//constant_length base types that we can serialize to a file
//if the VEC or MAT flag is set then the type is a vector

const char LISZT_INT = 0;
const char LISZT_FLOAT = 1;
const char LISZT_DOUBLE = 2;
const char LISZT_BOOL = 3;
const char LISZT_VEC_FLAG = 1; //if set data[0] is the size of the vector
const char LISZT_MAT_FLAG = 3; //if set data[0] is number of rows, data[1] is number of columns

struct LisztType {
	char type;
	char flags;
	char data[2];
} __attribute__((packed));


struct FileField {
	IOElemType domain;
	LisztType range;
	lsize_t nElems;
	file_ptr name; //null-terminated string in file
	file_ptr data;
} __attribute__((packed));


} // namespace MeshIO
#endif
