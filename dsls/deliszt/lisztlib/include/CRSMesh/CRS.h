/*
 * CRS.h
 *
 *  Created on: Sep 26, 2009
 *      Author: zd
 */

#ifndef CRS_H_
#define CRS_H_

#include <iostream>
#include <algorithm>
#include <strings.h>

namespace CRSMeshPrivate {
//to prevent cache contention for mesh objects, we allocated each to its own page, the page size
//should be adjusted to make the size of the largest cache line size for which the space/wasted memory tradeoff makes sense
static const int LOG_LISZE_PAGE_SIZE = 12;
//can't use const because it angers __attribute__
#define LISZE_PAGE_SIZE 4096
typedef unsigned int idx_type;
struct PageAlloc {
        unsigned char data[LISZE_PAGE_SIZE];
        static void * malloc(size_t nbytes) {
                size_t npages = (((nbytes - 1) & ~(LISZE_PAGE_SIZE - 1)) >> LOG_LISZE_PAGE_SIZE) + 1;
                void * data = (void *) new PageAlloc[npages];
                //std::cout << "Requested " << nbytes << " bytes, allocated " << npages * LISZE_PAGE_SIZE << " bytes (" << npages << " pages) at" << data << std::endl;
            return data;
        }
        static void free(void * ptr) {
                PageAlloc * pages = (PageAlloc*) ptr;
                delete [] pages;
        }
}  __attribute__((aligned(LISZE_PAGE_SIZE)));

struct CRS {
        idx_type * row_idx;
        id_type * values;
        CRS() : row_idx(NULL), values(NULL) {}
private:
        CRS(const CRS& c) {}
        void operator=(const CRS& c) {}
public:
        void initIndices(size_t size) {
                // Allocate 1 extra entry to accumulate total number of values
                row_idx = (idx_type *) PageAlloc::malloc((size + 1)*sizeof(idx_type));
        }
        void initValues(size_t size) {
                assert(row_idx);
                idx_type nvalues = row_idx[size];
                values = (id_type *) PageAlloc::malloc(nvalues * sizeof(id_type));
        }
        ~CRS() {
                if(row_idx)
                        PageAlloc::free(row_idx);
                if(values)
                        PageAlloc::free(values);
        }
};

struct IDPair {
        id_type data[2];
        id_type & operator[](unsigned int i) {
                return data[i];
        }
        const id_type & operator[](unsigned int i) const {
                return data[i];
        }
};
//for edges to vertices and faces to cells where the indicies are constant
struct CRSConst {
        IDPair * values;
        void initValues(size_t size) {
                values = (IDPair*) PageAlloc::malloc(size * sizeof(IDPair));
        }
};

//class used to wrap initialization of a CRS
class CRSInit {
public:
        CRSInit(const size_t & sz, CRS & c) : size(sz), crs(c) {
                set_sizes = new idx_type[size];
                bzero(set_sizes,size *sizeof(idx_type));
        }
        void found(id_type id) {
                assert(id < size);
                set_sizes[id]++;
        }
        void writeIndices() {
                assert(crs.row_idx == NULL);
                crs.initIndices(size);
                sizes_to_indices();
                crs.initValues(size);
        }
        void add(id_type e, id_type v) {
                assert(e < size);
                assert(set_sizes[e] != 0);
                idx_type idx = crs.row_idx[e+1] - set_sizes[e];
                crs.values[idx] = v;
                set_sizes[e]--;
        }
        void assert_finished_and_remove_duplicates() {
                size_check();
                for(unsigned int i = 0; i < size; i++) {
                        id_type * start = crs.values + crs.row_idx[i];
                        id_type * end = crs.values + crs.row_idx[i+1];
                        std::sort(start,end);
                        id_type * last = std::unique(start,end);
                        set_sizes[i] = last - start;
                }

                id_type * old_values = crs.values;
                //we now allocate space for the non-dup values
                idx_type nelems = crs.row_idx[size];
                crs.values = (id_type*) PageAlloc::malloc(nelems * sizeof(id_type));
                id_type * elem = crs.values;
                //copy the values over
                for(unsigned int i = 0; i < size; i++) {
                        id_type * begin = old_values + crs.row_idx[i];
                        for(unsigned int j = 0; j < set_sizes[i]; j++, elem++) {
                                *elem = begin[j];
                        }
                }
                //write the new indices into row_idx
                sizes_to_indices();
                //free the old values and clean up the initializer
                PageAlloc::free(old_values);
                delete [] set_sizes;
                set_sizes = NULL;
        }
        void assert_finished() {
                size_check();
                delete [] set_sizes;
                set_sizes = NULL;
        }
        ~CRSInit() {
                assert(set_sizes == NULL);
        }
private:
        void sizes_to_indices() {
                crs.row_idx[0] = 0;
                // Index is based on running total of sizes
                for(unsigned int i = 1; i <= size; i++) {
                        crs.row_idx[i] = crs.row_idx[i-1] + set_sizes[i-1];
                }
        }
        void size_check() {
#ifndef NDEBUG
                for(unsigned int i = 0; i < size; i++) {
                        assert(set_sizes[i] == 0);
                }
#endif
        }
        idx_type * set_sizes;
        const size_t & size;
        CRS & crs;
};
static const unsigned int FLIP_DIRECTION_SHIFT = 31;
static const unsigned int FLIP_DIRECTION_BIT = 1u << FLIP_DIRECTION_SHIFT;

}
#endif /* CRS_H_ */
