/*
 * Iterators.h
 *
 *  Created on: Sep 27, 2009
 *      Author: zd
 */

#ifndef CRSITERATORS_H_
#define CRSITERATORS_H_

#include "CRSMesh/CRS.h"

namespace CRSMeshPrivate {
template<typename MO>
class CRSIterator {
public:
        typedef MO value_type;
    id_type iterNumber;
  // NOTE(boulos): Empty ctors are the devil.

  //CRSIterator() : iterNumber(0), mesh(NULL), cur(NULL), end(NULL) {}
  CRSIterator() {}

        CRSIterator(const Mesh * m, const CRS & crs, id_type id)
        : iterNumber(0), mesh(m){
                cur = crs.values + crs.row_idx[id];
                end = crs.values + crs.row_idx[id+1];
        }
        bool hasNext() const {
                return cur != end;
        }
        value_type next() {
            value_type ret = value_type(MeshObjectData(mesh,*cur++, iterNumber++)) ;
            return ret;
        }
        value_type operator[](id_type id) {
                return value_type(MeshObjectData(mesh,cur[id], iterNumber));
        }
        size_t size() const {
            return end - cur;
        }
private:
        const Mesh * mesh;
        id_type * cur;
        id_type * end;
};

template<typename MO>
class CRSDirectedIterator {
public:
        typedef MO value_type;
  //CRSDirectedIterator() : mesh(NULL), cur(NULL), end(NULL), dir(CCW) {}
  CRSDirectedIterator() {}
        CRSDirectedIterator(const Mesh * m, const CRS & crs, id_type id, Rotation d)
        : mesh(m), dir(d) {
                if(dir == CCW) {
                        cur = crs.values + crs.row_idx[id];
                        end = crs.values + crs.row_idx[id+1];
                } else {
                        cur = crs.values + crs.row_idx[id+1] - 1;
                        end = crs.values + crs.row_idx[id] - 1;
                }
        }
        bool hasNext() const {
                return cur != end;
        }
        value_type next() {
            //if the direction is negative, then we need to flip the resulting object, which
            //is done by flipping its direction bit, note this will flip cells and vertices
            //as well but this will have no effect on them
                value_type vt = value_type(MeshObjectData(mesh, (dir & FLIP_DIRECTION_BIT) ^ *cur));
                cur += dir;
                return vt;
        }
        value_type operator[](int idx) {
                if(dir == CCW)
                        return value_type(MeshObjectData(mesh,cur[idx]));
                else
                        return value_type(MeshObjectData(mesh,FLIP_DIRECTION_BIT ^ cur[-idx]));
        }
        size_t size() const {
            return abs(end - cur);
        }
private:
        const Mesh * mesh;
        id_type * cur;
        id_type * end;
        Rotation dir;
};

template<typename MO>
class CRSConstIterator {
public:
        typedef MO value_type;
        CRSConstIterator() {}
        CRSConstIterator(const Mesh * m, const CRSConst & crs, id_type id)
        : mesh(m) {
                cur = crs.values[id].data;
                end = crs.values[id].data + 2;
        }
        bool hasNext() const {
                return cur != end;
        }
        value_type next() {
                return value_type(MeshObjectData(mesh,*cur++));
        }
        value_type operator[](int idx) {
                return value_type(MeshObjectData(mesh,cur[idx]));
        }
        size_t size() const {
            return 2;
        }
private:
        const Mesh * mesh;
        id_type * cur;
        id_type * end;
};

template<typename MO>
class MeshIterator {
public:
        typedef MO value_type;
  //MeshIterator() : mesh(NULL), cur(0), end(0) {}
  MeshIterator() {}
        MeshIterator(const Mesh * m, size_t nElem)
        : mesh(m) {
                cur = 0;
                end = nElem;
        }
        MeshIterator(const Mesh * m, id_type s, id_type e)
        : mesh(m), cur(s), end(e) {}
        bool hasNext() const {
                return cur != end;
        }
        value_type next() {
                return value_type(MeshObjectData(mesh,cur++));
        }
        value_type operator[](int idx) {
                return value_type(MeshObjectData(mesh,cur + idx));
        }
        size_t size() const {
            return end - cur;
        }
        //return a new iterator with a subset of the entire range
        //Ideally this should be handled by the set returning this iterator
        //but this change is much easier to make right now (Zach)
        //[s,s+sz) must be a subset of [cur,end)
        MeshIterator<MO> with(id_type off, id_type sz) const {
                size_t s = cur + off;
                size_t e = s + sz;
                assert(s >= cur && s <= end );
                assert(e <= end && e >= cur );
                assert(s <= e);
                return MeshIterator(mesh,s,e);
        }
private:
        const Mesh * mesh;
        id_type cur;
        id_type end;
};

}
#endif /* ITERATORS_H_ */
