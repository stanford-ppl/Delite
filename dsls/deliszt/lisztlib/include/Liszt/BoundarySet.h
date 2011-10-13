/*
 * BoundarySet.h
 *
 *  Created on: May 15, 2010
 *      Author: mbarrien
 */
#ifndef BOUNDARYSET_H_
#define BOUNDARYSET_H_
#include <vector>
#include <cstdlib>
#include <cassert>
#include "liszt_memory.h"
#include "Liszt/ElemTypes.h"
#include "Liszt/Util.h"
#include "MeshSet.h"
#include <utility>

namespace LisztPrivate {

class BoundarySet {
public:
    typedef std::pair<size_t, size_t> range_t;
    typedef std::vector<range_t> ranges_t;
    ranges_t ranges;
    typedef std::vector<range_t>::const_iterator range_it;

    //called before any calls to addSet
    void beginInit(size_t num_elems) {
    }
    void addSet(size_t start, size_t end) {
        ranges.push_back(std::make_pair(start, end));
    }
    //called after all calls to addSet
    void endInit() {
    }

    const ranges_t& getRanges() const {
        return ranges;
    }
};

}

 // namespace LisztPrivate
#endif /* BOUNDARY_SET_H_*/
