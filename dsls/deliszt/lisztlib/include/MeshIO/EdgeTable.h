#ifndef _EDGE_TABLE_H
#define _EDGE_TABLE_H

#include "MeshIO/Common.h"

namespace MeshIO {

class EdgeTable {
public:
	EdgeTable() : next_edge(0) {}
	id_t get(id_t v0, id_t v1, bool * added = NULL) {
		if(v0 > v1)
			return get(v1,v0,added);
		else {
			std::pair<id_t,id_t> e(v0,v1);
			table_t::iterator it = table.find(e);
			if(it == table.end()) {
				id_t eid = next_edge++;
				table[e] = eid;
				if(added)
					*added = true;
				return eid;
			} else {
				if(added)
					*added = false;
				return it->second;
			}
		}
	}
	size_t size() const {
		return next_edge;
	}
private:
	template<typename T1,typename T2>
	struct PairHash {
		size_t operator()(const std::pair<T1,T2> & pair) const {
			std::hash<T1> h1;
			std::hash<T2> h2;
			return h1(pair.first) + h2(pair.second);
		}
	};
	typedef std::hash_map< std::pair<id_t,id_t>, id_t, PairHash<id_t,id_t> > table_t; 
	id_t next_edge;
	table_t table;
};
} // namespace MeshIO
#endif
