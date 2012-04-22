/*
 * SetAdaptor.h
 *
 *  Created on: Mar 10, 2009
 *      Author: zdevito
 */

#ifndef SETADAPTOR_H_
#define SETADAPTOR_H_
#include "liszt_memory.h"

template<typename T>
class IteratorAdaptor {
public:
    typedef T value_type;
    template<typename Impl> IteratorAdaptor(const Impl & impl_)
    : impl(new iterator<Impl>(impl_)) {}
    IteratorAdaptor() : impl() {}
    virtual bool hasNext() const {
        assert(impl.get() != NULL);
        return impl->hasNext();
    }
    virtual ~IteratorAdaptor() {}
    virtual T next() {
        assert(impl.get() != NULL);
        return impl->next();
    }
private:
    class interface {
    public:
        typedef T value_type;
        virtual bool hasNext() const = 0;
        virtual T next() = 0;
        virtual ~interface() {}
    };
    template<typename Impl> class iterator : public interface {
    public:
        iterator(const Impl & impl_) : impl(impl_) {}
        virtual bool hasNext() const { return impl.hasNext(); }
        virtual T next() { return impl.next(); }
    private:
        Impl impl;
    };
    ::std::tr1::shared_ptr<interface> impl;
};

template<typename T> class SetAdaptor {
public:
    typedef IteratorAdaptor<T> iterator;
    typedef T value_type;
    template<typename Impl> SetAdaptor(const Impl & impl_) :
    impl(new set<Impl>(impl_)) {}
    SetAdaptor() : impl() {}
    virtual ~SetAdaptor() {}
    virtual iterator iter() const {
        assert(impl.get() != NULL);
        return impl->iter();
    }
    virtual size_t size() const {
        assert(impl.get() != NULL);
        return impl->size();
    }
private:
    class interface {
    public:
        typedef T value_type;
        virtual IteratorAdaptor<T> iter() const = 0;
        virtual size_t size() const = 0;
        virtual ~interface() {}
    };
    template<typename Impl> class set : public interface {
    public:
        set(const Impl & impl_) : impl(impl_) {}
        virtual IteratorAdaptor<T> iter() const {
            return IteratorAdaptor<T>(impl.iter());
        }
        virtual size_t size() const { return impl.size(); }
    private:
        Impl impl;
    };
    ::std::tr1::shared_ptr<interface> impl;
};

#endif /* SETADAPTOR_H_ */
