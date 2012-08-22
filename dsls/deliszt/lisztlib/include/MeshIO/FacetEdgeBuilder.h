#ifndef _FACET_EDGE_BUILDER
#define _FACET_EDGE_BUILDER
#include <stdint.h>
#include "MeshIO/Common.h"
#include "MeshIO/LisztFormat.h"
#include <iostream>
//Warning: Silly pointer hacks to make these structures 64 bytes
//vert_cell[0]'s high bit holds if it is hf 1 or one.
//this enables use to store pointers to HalfFacets that can recover the pointer
//to their member facet edge
//After allocation all use should be through the HalfFacet structures


//edges/faces vertices/cells are always stored with their dual in the same
//member, hopefully this will allow me to write half the code rather than generating
//dual methods

namespace MeshIO {
const int OTHER_BIT = 31;
const id_t OTHER_BIT_FLAG = 1u << 31u;
const id_t THIS_BIT_FLAG = 1u << 30u;  // For visit ID
const id_t BOTH_BIT_FLAG = OTHER_BIT_FLAG | THIS_BIT_FLAG;
const id_t MAX_VISIT_TAG = THIS_BIT_FLAG - 1;
struct FacetEdge {
        struct HalfFacet {
                HalfFacet * ccwAroundEF[2];
                // vert_cell[0] is vertex ID, vert_cell[1] is cell ID
                // with high bit set of ID set if this is HalfFacet #1 (not set if #0)
                id_t vert_cell[2];
                HalfFacet() {
                        ccwAroundEF[0] = NULL;
                        ccwAroundEF[1] = NULL;
                }
                id_t vertOrCell(int i) const {
                        return ~OTHER_BIT_FLAG & vert_cell[i];
                }

                id_t vert() const { return vertOrCell(0); }
                id_t cell() const { return vertOrCell(1); }

                HalfFacet * ccwAroundEdge() const {
                        return ccwAroundEF[0];
                }
                HalfFacet * ccwAroundFace() const {
                        return ccwAroundEF[1];
                }
                id_t ID() const {
                        return me()->id;
                }
                id_t edgeOrFace(int i) const {
                        return me()->edge_face[i];
                }
                id_t edge() const {
                        return edgeOrFace(0);
                }
                id_t face() const {
                        return edgeOrFace(1);
                }
                bool wasVisited(id_t visit_id) const {
                        return (me()->visited & (isOther() ? ~THIS_BIT_FLAG : ~OTHER_BIT_FLAG)) ==
                                        (visit_id | (isOther() ? OTHER_BIT_FLAG : THIS_BIT_FLAG));
                }
                bool visit(id_t visit_id) {
                        assert(!(visit_id & BOTH_BIT_FLAG));
                        // If bottom 30 bits contain same visit_id and the proper high bit is
                        // set, then we have already visited
                        if(wasVisited(visit_id)) {
                                return true;
                        } else {
                                // If visit_id already matches, preserve any set flags, otherwise
                                // clear flags (using flag-less visit_id) and reset visited ID.
                                // Resets visit ID for both this HalfFacet and the complement.
                                me()->visited =
                                        ((me()->visited & ~BOTH_BIT_FLAG) == visit_id ?
                                                me()->visited : visit_id) |
                                        (isOther() ? OTHER_BIT_FLAG : THIS_BIT_FLAG);
                                return false;
                        }
                }
                //clear the visit bits, making this thing unvisited for visit_id
                void unvisit() {
                        me()->visited &= ~(isOther() ? OTHER_BIT_FLAG : THIS_BIT_FLAG);
                }
                HalfFacet * flip() const {
                        return (HalfFacet*) this - (static_cast<int>(isOther() << 1) - 1);
                }
                bool isOther() const {
                        return vert_cell[0] >> OTHER_BIT;
                }
                FacetEdge * me() {
                        // Relying on the HalfFacet array being the first member of a FacetEdge
                        return reinterpret_cast<FacetEdge*>(this - isOther());
                }
                const FacetEdge * me() const {
                        // Relying on the HalfFacet array being the first member of a FacetEdge
                        return reinterpret_cast<const FacetEdge*>(this - isOther());
                }
        } __attribute__((packed));
        FacetEdge() {
                visited = 0;
        }
        HalfFacet hf[2];
        id_t edge_face[2]; // edge_face[0] is edge ID, edge_face[1] is face ID.
        id_t id;
        id_t visited;   // Lower 30 bits is visit ID, topmost bit set if hf[1] visited
                                    // 2nd MSB set if hf[0] visited. If both HalfFacets are to be
                                        // visited, they must both be visited before a new visit ID is
                                        // used on either HalfFacet.
} __attribute__((packed));



static const unsigned int FACET_POOL_PAGE_SIZE = 1024;
struct FacetPool {
        FacetPool() : next_free_entry(FACET_POOL_PAGE_SIZE), last_entry_freed(false) {}
        FacetEdge *  alloc() {
                if(last_entry_freed) {
                        assert(next_free_entry != 0);
                        last_entry_freed = false;
                        return entry(next_free_entry - 1);
                }
                assert(next_free_entry <= FACET_POOL_PAGE_SIZE);

                if(next_free_entry == FACET_POOL_PAGE_SIZE) {
                        next_free_entry = 0;
                        blocks.push_back(new unsigned char[FACET_POOL_PAGE_SIZE * sizeof(FacetEdge)]);

                }
                return entry(next_free_entry++);
        }
        //releases all the FacetEdge memory, no destrutors are called
        void purge_memory() {
                for(unsigned int i = 0; i < blocks.size(); i++)
                        delete [] blocks[i];
                blocks.clear();
                next_free_entry = FACET_POOL_PAGE_SIZE;
                last_entry_freed = false;
        }
        //frees the last facet allocated, this is the only per-element free allowed
        void freeLastFacet() {
                last_entry_freed = true;
        }

        //iterator that gives you access to all allocated FacetEdges in the builder
        //use like:
        //FacetEdge * fe;
        //iterator_t it = this->iterator();
        //while(this->next(&fe)) { ... }
        class iterator_t {
        public:
                iterator_t() {}
                iterator_t(FacetPool * p) : pool(p), i(0), j(0) {}
                bool next(FacetEdge ** fe) {
                        if(j == FACET_POOL_PAGE_SIZE) {
                                j = 0;
                                i++;
                        }
                        if((i == pool->blocks.size()) ||
                           (i + 1 == pool->blocks.size() && j + (pool->last_entry_freed ? 1 : 0) == pool->next_free_entry) ) {
                                return false;
                        } else {
                                *fe = pool->entry(i,j++);
                                return true;
                        }
                }
        private:
                FacetPool * pool;
                unsigned int i;
                unsigned int j;
        };
        iterator_t iterator() {
                return iterator_t(this);
        }
private:
        FacetEdge * entry(unsigned int b, unsigned int j) {
                return reinterpret_cast<FacetEdge *>(blocks[b]) + j;
        }
        FacetEdge * entry(unsigned int i) {
                return reinterpret_cast<FacetEdge *>(blocks.back()) + i;
        }
        unsigned int next_free_entry;
        bool last_entry_freed;
        std::vector<unsigned char *> blocks;
};


static inline std::ostream & operator<<(std::ostream & out, FacetEdge::HalfFacet & hf) {
        out << "ID: " << hf.ID() << " Edge: " << hf.edge() << " Face: "
            << hf.face() << " Vert: " << hf.vert() << "Cell: " << hf.cell();
        if(hf.ccwAroundEdge())
          out << "ccwAroundEdge: " << hf.ccwAroundEdge()->ID();
        if(hf.ccwAroundFace())
          out << "ccwAroundFace: " << hf.ccwAroundFace()->ID();
        return out << std::endl;
}

class FacetEdgeBuilder {
        typedef FacetEdge::HalfFacet HalfFacet;
public:
        FacetEdgeBuilder() {
                //assert(sizeof(FacetEdge) == 64 || !"Structured is unexpected size, is this 64-bit? I need 64-bit");
        }

        //simple tells us the total possible sizes, we assume we will have a sparse subset
        //we don't need to know how many facet_edges there are while building, but we will need
        //to know by the end.   Use setFE() later on to correct the number if you don't know it up front.
        void init(lsize_t nV_,lsize_t nE_, lsize_t nF_, lsize_t nC_, lsize_t nFE_ = 0) {
                nV() = nV_;
                nE() = nE_;
                nF() = nF_;
                nC() = nC_;
                nFE() = nFE_;
        }
        void setNFE(lsize_t nFE_) {
                nFE() = nFE_;
        }
        void insert(lsize_t start_id, lsize_t end_id, const FileFacetEdge * ffes) {
                lsize_t nFEs = end_id - start_id;
                for(lsize_t i = 0; i < nFEs; i++) {
                        insert(start_id + i,&ffes[i]);
                }
        }
        bool insert(id_t id, const FileFacetEdge * ffe) {
                FacetEdge * fe = facet_pool.alloc();
                if(!setupFE(id,ffe,fe)) {
                        facet_pool.freeLastFacet();
                        return false;
                } else {
                        return true;
                }
        }
        //dump all memory, fast due to pools
        void clear() {
                for(unsigned int i = 0; i < TYPE_SIZE; i++) {
                        elem_maps[i].clear();
                }
                facet_pool.purge_memory();
        }
        lsize_t nElems[5];
        lsize_t & nV() { return nElems[VERTEX_T]; }
        lsize_t & nE() { return nElems[EDGE_T]; }
        lsize_t & nF() { return nElems[FACE_T]; }
        lsize_t & nC() { return nElems[CELL_T]; }
        lsize_t & nFE() { return nElems[TYPE_SIZE]; }
        const lsize_t & nV() const { return nElems[VERTEX_T]; }
        const lsize_t & nE() const { return nElems[EDGE_T]; }
        const lsize_t & nF() const { return nElems[FACE_T]; }
        const lsize_t & nC() const { return nElems[CELL_T]; }
        const lsize_t & nFE() const { return nElems[TYPE_SIZE]; }


        void validateFullMesh() {
                for(int i = 0; i < TYPE_SIZE; i++) {
                        if(nElems[i] != elem_maps[i].size()) {
                                fprintf(stderr,"%d: reported and discovered size mismatch %d reported, %zu discovered\n",i,nElems[i], elem_maps[i].size());
                        }
                }
                for(unsigned int i = 0; i < 2; i++) {
                    for(lsize_t j = 0; j < nElems[i+2]; j++) {
                                //printf("checking %s #%d\n", i == 0 ? "edge" : "face", j);
                                //printRingE(i,it->first);
                                validRing(i,canonicalFacet(static_cast<IOElemType>(i+2), j));
                        }
                }
        }

        template<typename Filter>
        void filter(Filter & filter) {
                //dump the element maps, they must be rebuilt during the filter process
                //as the numbering of the elements will change
                for(unsigned int i = 0; i < TYPE_SIZE; i++) {
                        elem_maps[i].clear();
                }
                FacetPool::iterator_t it = facet_pool.iterator();
                FacetEdge * fe;
                while(it.next(&fe)) {
                        filterFacet(filter,fe);
                }
        }

#define NEXT(fe) (fe)->ccwAroundEF[type]
#define VALUE(fe) (fe)->vertOrCell(!type)
#define DVALUE(fe) (fe)->vertOrCell(type)
#define PREV(fe)  NEXT( (fe)->flip() )
#define IS_HOLE(fe) (VALUE(fe) != VALUE(NEXT(fe)->flip()))

        void printRing(int type, HalfFacet * head) {
                const char * elem = type ? "f" : "e";
                const char * val = type ? "v" : "c";
                const char * dval = type ? "c" : "v";
                HalfFacet * cur = head;
                fprintf(stderr,"ring(%d) for %s%d ccwAround %s%d: ",type,elem,head->edgeOrFace(type),dval,DVALUE(head));
                do {
                        fprintf(stderr,"(%s%d) - fe%d - (%s%d)", val,VALUE(cur->flip()), cur->ID(), val,VALUE(cur));
                        cur = NEXT(cur);
                } while(cur != head);
                fprintf(stderr,"\n");
        }
        void printRingE(int type, id_t id) {
                printRing(type,edge_face_m()[type][id]);
        }
        FacetPool::iterator_t facets() {
                return facet_pool.iterator();
        }
private:

        void validRing(int type, HalfFacet * head) {
                HalfFacet * cur = head;
                do {
                        if(VALUE(cur) != VALUE(NEXT(cur)->flip()) ||
                           DVALUE(cur) != DVALUE(NEXT(cur))) {
                                printf("warning: hole found in mesh\n");
                                printRing(type,head);
                                //assert(!"hole found in mesh");
                        }
                        cur = NEXT(cur);
                } while(cur != head);
        }
        bool setupFE(id_t id, const FileFacetEdge * ffe, FacetEdge * fe) {
                //printf("attempting adding fe = %d\n",id);
                new (fe) FacetEdge;
                fe->id = id;
                fe->edge_face[0] = ffe->edge;
                fe->edge_face[1] = ffe->face;
                for(int i = 0; i < 2; i++) {
                        fe->hf[i].vert_cell[0] = (i << OTHER_BIT) | ffe->hf[i].vert;
                        fe->hf[i].vert_cell[1] = (i << OTHER_BIT) | ffe->hf[i].cell;
                }

                //addToRing will return false if we already have FE in the mesh
                if(addToRing(0,&fe->hf[0])) { //rings use hf[0] since this was the default direction listed in the files,
                                        //and we must preserve this default

                    assert(fe->hf[0].ccwAroundEF[0] != NULL);
                    assert(fe->hf[1].ccwAroundEF[0] != NULL);

                        addToRing(1,&fe->hf[0]);

                        assert(fe->hf[0].ccwAroundEF[1] != NULL);
                    assert(fe->hf[1].ccwAroundEF[1] != NULL);

                        //add or replace the mappings for edges/cells
                        HalfFacet * hf = fe->hf;
                        // std::cout << "Adding " << hf->vert() << " to " << VERTEX_T << std::endl;
                        // std::cout << "Adding " << hf->cell() << " to " << CELL_T << std::endl;
                        vertMap()[hf->vert()] = hf;
                        cellMap()[hf->cell()] = hf;

                        hf = hf->flip();
                        // std::cout << "Adding " << hf->vert() << " to " << VERTEX_T << std::endl;
                        // std::cout << "Adding " << hf->cell() << " to " << CELL_T << std::endl;
                        vertMap()[hf->vert()] = hf;
                        cellMap()[hf->cell()] = hf;

                        return true;
                } else {
                        return false;
                }
        }

        //NOTE: this is setup to do both an operation and its dual; The comments
        //will refer to setting up the ring of faces around an edge.  This also sets up edges around a face
        //using the exact same code

        //add to ring preserves the invarient that given the ordered list of faces around an edge
        //if this ring contains any sub ranges of the total list, those subranges will be
        //in order and contiguous.  Furthermore all HalfFacet's Verts must match (i.e. they are not flipped)

        //for example if faces 1 2 3 4 5 6 7 are around an edge and we have 1 2 4 6 locally then we may have ring
        //1-2-6-4, or 4-6-1-2, but not 2-1-6-4

        //when we have all edges, this invarient means we have the edges in order
        bool addToRing(int type, HalfFacet * hf) {
                //is this this first time the ring has been seen?
                map_t & elemMap = edge_face_m()[type];
                id_t elemID = hf->edgeOrFace(type);
                map_t::iterator it = elemMap.find(elemID);
                if(it == elemMap.end()) {
                        //std::cout << "Adding " << elemID << " to " << type + 2 << std::endl;
                        elemMap[elemID] = hf;
                        //base-case preserves the invarient by linking its pointers to itself, establishing
                        //a ring
                        NEXT(hf) = hf;
                        PREV(hf) = hf->flip();
                } else {
                        HalfFacet * ring_head = it->second;
                        //if this HF points toward the opposite vert we need to flip it
                        if(DVALUE(ring_head) != DVALUE(hf)) {
                                hf = hf->flip();
                                assert(DVALUE(ring_head) == DVALUE(hf));
                        }
                        //printRing(type,ring_head);
                        //we now make a list of disjoint segments of the ring
                        HalfFacet * cur = ring_head;
                        HalfFacet * hole = NULL;
                        do {
                            //      cur links to fe
                            //      fe links to cur
                            //      none (valid link)
                            //      none (invalid link)
                            //      cur is fe
                            //and cur links to fe will occur before cur == fe
                                if(cur->ID() == hf->ID()) {
                    return false; //we already have the fe, so just return
                } else if (VALUE(cur) == VALUE(hf->flip())) {
                                return insertAndFix(type,cur,hf);
                            } else if (VALUE(hf) == VALUE(cur->flip())) {
                                return insertAndFix(type,cur->flip(),hf->flip());
                            } else if (IS_HOLE(cur)) {
                                //a hole is found
                                hole = cur;
                            }
                                cur = NEXT(cur);
                        } while(cur != ring_head);
                        //did not find any links in the list
                        //lets place fe in a hole, one must exist, otherwise FE can't be in this list
                        assert(hole != NULL);
                        insertAtHole(type,hole,hf,hf);
                }
                return true; //we added the FE

        }
        bool insertAndFix(int type, HalfFacet * head, HalfFacet * hf) {
                assert(VALUE(head) == VALUE(hf->flip()));
                if(NEXT(head)->ID() == hf->ID())
                        return false; //we already have fe

        insertAtHole(type, head, hf, hf);

        HalfFacet * start = NEXT(hf);
        HalfFacet * cur = start;

                if(VALUE(hf) != VALUE(cur->flip())) //the next link is not valid, let's see if we can find a valid link
                while(cur != hf) {
                        if(VALUE(hf) == VALUE(cur->flip())) { //we have found a valid link
                                HalfFacet * end = PREV(cur)->flip();

                                NEXT(hf) = cur;
                                NEXT(cur->flip()) = hf->flip();

                HalfFacet * hole = NULL;
                                while(hole == NULL && cur != hf) { //we need to place list start-end somewhere now that the correct link has displaced it
                                        if(IS_HOLE(cur))
                                                hole = cur;
                                        cur = NEXT(cur);
                                }
                                assert(hole != NULL);
                                insertAtHole(type,hole,start,end);
                                return true;
                        }
                        cur = NEXT(cur);
                }
                return true;
        }
        void insertAtHole(int type, HalfFacet * hole, HalfFacet * start, HalfFacet * end) {
                //insert fe between hole and its successor, fix backwards pointers as well
                HalfFacet * after = NEXT(hole);
                NEXT(end) = after;
                NEXT(hole) = start;
                NEXT(after->flip()) = end->flip();
                NEXT(start->flip()) = hole->flip();
        }
#undef NEXT
#undef PREV
#undef VALUE
#undef DVALUE
#undef IS_HOLE

        template<typename Filter>
        void filterHalfFacet(Filter & filter, FacetEdge::HalfFacet * hf, int i) {
                hf->vert_cell[0] = filter(VERTEX_T,hf->vert());
                hf->vert_cell[1] = filter(CELL_T,hf->cell());
                vertMap()[hf->vert()] = hf;
                cellMap()[hf->cell()] = hf;

                //set the correct other bits
                hf->vert_cell[0] |= i << OTHER_BIT;
                hf->vert_cell[1] |= i << OTHER_BIT;
        }
        template<typename Filter>
        void filterFacet(Filter & filter, FacetEdge * fe) {
                filterHalfFacet(filter,&fe->hf[0],0);
                filterHalfFacet(filter,&fe->hf[1],1);
                fe->edge_face[0] = filter(EDGE_T,fe->edge_face[0]);
                fe->edge_face[1] = filter(FACE_T,fe->edge_face[1]);
                edgeMap()[fe->edge_face[0]] = &fe->hf[0];
                faceMap()[fe->edge_face[1]] = &fe->hf[0];
        }

public:
        typedef std::hash_map<id_t,HalfFacet *> map_t;

        map_t elem_maps[4]; //duals are paired
        map_t * vert_cell_m() { return elem_maps; }
        map_t * edge_face_m() { return elem_maps + 2; }
        const map_t * vert_cell_m() const { return elem_maps; }
        const map_t * edge_face_m() const { return elem_maps + 2; }

        map_t & vertMap() { return elem_maps[VERTEX_T]; }
        map_t & cellMap() { return elem_maps[CELL_T]; }

        map_t & edgeMap() { return elem_maps[EDGE_T]; }
        map_t & faceMap() { return elem_maps[FACE_T]; }

        const map_t & vertMap() const { return elem_maps[VERTEX_T]; }
        const map_t & cellMap() const { return elem_maps[CELL_T]; }

        const map_t & edgeMap() const { return elem_maps[EDGE_T]; }
        const map_t & faceMap() const { return elem_maps[FACE_T]; }

        HalfFacet* canonicalFacet(IOElemType type, id_t id) const {
                map_t::const_iterator iter = elem_maps[type].find(id);
                assert(iter != elem_maps[type].end());
                return iter->second;
        }
private:
        FacetPool facet_pool;
};
} // namespace MeshIO
#endif
