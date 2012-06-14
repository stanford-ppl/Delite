#ifndef MESH_SET_H_
#define MESH_SET_H_

template <typename Iter>
class MeshSet {
public:
    typedef Iter iterator;
    typedef typename Iter::value_type value_type;
    MeshSet() {}
	MeshSet(const Iter & it_) : it(it_) {}
	MeshSet(const MeshSet<Iter> & rhs) : it(rhs.it) {}
	Iter iter() const {
		return it; //copies the iterator
	}
	//size is not known upfront, but we still provide
	//the slow lookup here to meet more of the interface
	size_t size() const {
	    Iter it2 = iter();
	    size_t i = 0;
	    for(; it2.hasNext(); i++)
	        it2.next();
	    return i;
	}
	//retrieves one arbitrary element from the set
	value_type element() const{
	    Iter it2 = it;
	    assert(it2.hasNext());
	    return it2.next();
	}

protected:
	Iter it;
};

template<typename Iter>
class SizedMeshSet : public MeshSet<Iter> {
public:
    SizedMeshSet() : s(0) {}
	SizedMeshSet(const Iter & it_, size_t size_) : MeshSet<Iter>(it_), s(size_) {}
	size_t size() const { return s; }
	SizedMeshSet(const SizedMeshSet<Iter> & rhs) : MeshSet<Iter>(rhs), s(rhs.s) {}
private:
	size_t s;
};

template<typename Iter>
class InternalSizeMeshSet : public MeshSet<Iter> {
public:
    InternalSizeMeshSet() {}
    InternalSizeMeshSet(const Iter & it_)
    : MeshSet<Iter>(it_) {}
    size_t size() const {
        return this->it.size();
    }
    Iter iter() const { return this->it; }
    Iter iter(size_t s, size_t sz) const {
		return this->it.with(s,sz); //returns new iterator from [s,s+sz)
	}
	Iter iter(size_t s) const { return this->iter(s,this->size() - s); }
};


template<typename Set1, typename Set2>
class EitherSet {
public:
    class iterator {
    public:
        typedef typename Set1::iterator it1_t;
        typedef typename Set2::iterator it2_t;
        
        typedef typename Set1::value_type value_type;
        iterator() {}
        iterator(const it1_t & s) {
            data.i1 = s;
            isLeft = true;
        }
        iterator(const it2_t & s) {
            data.i2 = s;
            isLeft = false;
        }
        bool hasNext() const {
            if(isLeft)
                return data.i1.hasNext();
            else
                return data.i2.hasNext();
        }
        value_type next() {
            if(isLeft)
                return data.i1.next();
            else
                return data.i2.next();
        }
    private:
        struct {
            it1_t i1;
            it2_t i2;
        } data;
        char buf[sizeof(Set1) > sizeof(Set2) ? sizeof(Set1) : sizeof(Set2)];
        bool isLeft;
    };
    typedef typename iterator::value_type value_type;
    EitherSet() {}
    EitherSet(const Set1 & s) {
        data.s1 = s;
        isLeft = true;
    }
    EitherSet(const Set2 & s) {
        data.s2 = s;
        isLeft = false;
    }
    size_t size() const {
        if(isLeft)
            return data.s1.size();
        else
            return data.s2.size();
    }
    value_type element() const {
        if(isLeft)
            return data.s1.element();
        else
            return data.s2.element();
    }
    iterator iter() const {
        if(isLeft)
            return iterator(data.s1.iter());
        else
            return iterator(data.s2.iter());
    }
    iterator iter(size_t s, size_t sz) const {
        if(isLeft)
            return iterator(data.s1.iter(s,sz));
        else
            return iterator(data.s2.iter(s,sz));
    }
    iterator iter(size_t s) const {
        if(isLeft)
            return iterator(data.s1.iter(s));
        else
            return iterator(data.s2.iter(s));
    }
private:
    bool isLeft;
    struct {
        Set1 s1;
        Set2 s2;
    } data;
};

#endif /* MESH_SET_H_ */
