/*
 * Range.h
 *
 *  Created on: Jul 28, 2009
 *      Author: zdevito
 */

#ifndef RANGE_H_
#define RANGE_H_

#include <algorithm>
#include "Liszt/ElemTypes.h"
#include "Liszt/IteratorFacade.h"

using namespace std;

namespace LisztPrivate {
//represents a range of mesh elements with IDs in range [start,finish)
template<typename T>
class Range {
public:
    typedef T elem_type;
    typedef ElemRef<elem_type> ref_type;
    Range() : start(0), finish(0) {}
    Range(id_type s, id_type e) : start(s), finish(e) {
        assert(s <= e);
    }
    Range(const Range<AnyType>& any_range) : start(any_range.internal_begin()), finish(any_range.internal_end()) { }

    class const_iterator : public liszt_iterator_facade<const_iterator,ref_type> {
    public:
        const_iterator() : id(0) {}
        const_iterator(id_type i) : id(i) {}
        void increment() {
            ++id;
        }
        void decrement() {
            --id;
        }
        void advance(ptrdiff_t t) {
            id += t;
        }
        ptrdiff_t distance_to(const const_iterator & other) const {
            return other.id - id;
        }
        ref_type dereference() const {
            return ref_type(id);
        }
        bool equal(const const_iterator & other) const {
            return id == other.id;
        }
        id_type id;
    };
    const_iterator begin() const {
        return const_iterator(start);
    }
    const_iterator end() const {
        return const_iterator(finish);
    }
    id_type internal_begin() const {
        return start;
    }
    id_type internal_end() const {
        return finish;
    }
    bool contains(const ref_type& ref) const {
        return ref.ID() >= start && ref.ID() < finish;
    }
    Range<T> intersection(const Range<T>& other) const {
        if (other.finish <= start || other.start >= finish) {
            // No overlap, empty range so actual value doesn't matter.
            return Range<T>(start, start);
        }
        return Range<T>(std::max(start, other.start), std::min(finish, other.finish));
    }
    size_t size() const {
        return finish - start;
    }
    // This is a "semi-hack" to be able to get an untyped range class for
    // storage in a DescriptorMap.
    Range<AnyType> getUntypedRange() const {
        return Range<AnyType>(start, finish);
    }
private:
    id_type start;
    id_type finish;
};

}
#endif /* RANGE_H_ */
