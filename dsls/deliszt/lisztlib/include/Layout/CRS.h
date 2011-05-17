#ifndef _LAYOUT_CRS_H
#define _LAYOUT_CRS_H

namespace Layout {

typedef unsigned int idx_type;
using LisztPrivate::ElemTypes::id_type;

struct CRS {
	idx_type * row_idx;  // the index into values array where each element's values start.
	id_type * values;    // the total number of values we store
};
struct IDPair {
	id_type data[2];
};
struct CRSConst {
	IDPair * values;
};

} /*namespace CRS*/

#endif
