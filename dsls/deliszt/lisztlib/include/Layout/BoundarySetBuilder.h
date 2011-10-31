#ifndef _BOUNDARY_SET_BUILDER_H
#define _BOUNDARY_SET_BUILDER_H
#include <map>
#include <set>
#include <string>
#include "liszt_memory.h"

#include "MeshIO/LisztFileReader.h"

#include "Liszt/Util.h"
#include "Liszt/ElemTypes.h"

using namespace ::LisztPrivate::ElemTypes;

class BoundarySetBuilder {
public:
	BoundarySetBuilder() {}
	
	//takes ownership of boundaries
	void init(size_t nB, MeshIO::BoundarySetEntry * boundaries) {
		bounds.size = nB;
		bounds.array.reset(new auto_ptr_array_adapter<MeshIO::BoundarySetEntry>(boundaries));
		//construct name -> table entry mapping
		for(size_t i = 0; i < bounds.size; i++) {
			if((*bounds.array)[i].name.size() > 0)
				name_table[(*bounds.array)[i].name] = &(*bounds.array)[i];
		}
	}
	
	
	template<typename MO, typename BS>
	bool load(const char * name, 
	          BS * bset) {
		if(loaded_sets.count(bset) > 0)
			return true; //set is already loaded
		
		range_list_t ranges;
		size_t total_size;
		load_internal<MO>(name,&ranges,&total_size);
		bset->beginInit(total_size);
		
		for(range_list_t::iterator it = ranges.begin(), end = ranges.end(); it != end; it++)
			bset->addSet(it->first,it->second);
		bset->endInit();
		loaded_sets.insert(bset);
		
		return true;
	}
	
	template<typename MO, typename BS, typename Filter>
	bool loadWithFilter(const char * name,
	                    Filter & filter,
	                    BS * bset) {
		if(loaded_sets.count(bset) > 0)
			return true; //set is already loaded
		
		
		
		range_list_t ranges;
		size_t total_size;
		
		load_internal<MO>(name,&ranges,&total_size);
		
		bset->beginInit(total_size);
		
		if(total_size > 0) {
			id_type * expanded = new MeshIO::id_t[total_size];
			
			size_t e = 0;
			for(range_list_t::iterator it = ranges.begin(), end = ranges.end(); it != end; it++) {
				for(MeshIO::id_t i = it->first; i < it->second; i++) {
					expanded[e++] = filter(ElemTypeToIO(MO::value),i);
				}
			}
			
			assert(e == total_size);
			
			//now build the new set
			std::sort(expanded,expanded + total_size);
			MeshIO::id_t start = expanded[0];
			for(size_t i = 1; i < total_size; i++) {
				MeshIO::id_t rl_next = expanded[i-1] + 1;
				if(rl_next != expanded[i]) {
					bset->addSet(start,rl_next);
					start = expanded[i];
				}
			}
			
			bset->addSet(start,expanded[total_size-1] + 1);
			
			delete [] expanded;
		}
		
		bset->endInit();
		loaded_sets.insert(bset);
		return true;
	}
	
	
private:
	typedef std::vector< std::pair<MeshIO::id_t, MeshIO::id_t> > range_list_t;
	typedef std::set<void *> loaded_sets_t;
	typedef std::map< std::string, MeshIO::BoundarySetEntry *> name_table_t;
	bool addSet(MeshIO::IOElemType t, MeshIO::BoundarySetEntry * b, std::vector<MeshIO::id_t> * points) {
		if( (b->type & ~MeshIO::AGG_FLAG) != t) {
			printf("boundary set aggregate changed type, file format error %d, expected %d",t,b->type & ~MeshIO::AGG_FLAG);
			return false;
		}
		if(b->type & MeshIO::AGG_FLAG) {
			return addSet(t,entry(b->start),points) && 
			       addSet(t,entry(b->end),points);
		} else {
			//we represent start and end ranges together in the same list
			//by shifting everyting left 1 and making start ranging even
			//and end ranges odd.
			//this makes end ranges slightly greater than start ranges
			//making it easy to march though and extract non-overlapping ranges
			points->push_back(b->start << 1);
			points->push_back( (b->end << 1) | 1);
			return true;
		}
	}
	bool sameType(ElemType a, MeshIO::IOElemType b) {
		return a == IOToElemType(b);
	}
	MeshIO::BoundarySetEntry * entry(size_t i) {
		assert(i < bounds.size);
		return &(*bounds.array)[i];
	}
	MeshIO::BoundarySetEntry * entry(const std::string & name) {
		name_table_t::iterator it = name_table.find(name);
		if(it == name_table.end())
			return NULL;
		else
			return it->second;
	}
	template<typename MO>
	bool load_internal(const char * name, 
	                   range_list_t * ranges, 
	                   size_t * size = NULL) {
		if(size)
			*size = 0;
		MeshIO::BoundarySetEntry * e = entry(name);
		if(!e) {
			printf("warning: boundary set not in file, it will be initialized as empty: %s\n",name);
			return true;
		}
		MeshIO::IOElemType typ = (MeshIO::IOElemType)(e->type & ~MeshIO::AGG_FLAG);
		if(!sameType(MO::value,typ)) {
			printf("boundary set has wrong type: %d, expected %d",typ,MO::value);
			exit(1);
			return false;
		}
		std::vector<MeshIO::id_t> points;
		if(!addSet(typ,e,&points)) {
			exit(1);
			return false;
		}
		
		std::sort(points.begin(),points.end());
		
		id_t depth = 0;
		id_t start = 0;
		for(std::vector<MeshIO::id_t>::iterator it = points.begin(),end = points.end(); 
		    it != end; 
		    it++) {
		    bool is_end = *it & 1;
			id_t n = *it >> 1;
			if(is_end) {
				depth--;
				if(depth == 0) {
					ranges->push_back(std::make_pair(start,n)); //bset->addSet(MeshFuncForType<MeshT,MO>::get(*mesh,start,n));
					if(size)
						*size += (n - start);
				}
			} else {
				if(depth == 0)
					start = n;
				depth++;
			}
		}
		assert(depth == 0);
		return true;
	}
	name_table_t name_table;
	loaded_sets_t loaded_sets; //the set of already loaded boundaries
	struct {
		::std::auto_ptr<auto_ptr_array_adapter<MeshIO::BoundarySetEntry> > array;
		size_t size;
	} bounds;
};
#endif