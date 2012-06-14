#ifndef NONCOPYABLE_H_
#define NONCOPYABLE_H_

namespace Liszt {

class noncopyable {
	protected:
		noncopyable () {}
		~noncopyable () {} /// Protected non-virtual destructor
	private: 
		noncopyable (const noncopyable &);
		noncopyable & operator = (const noncopyable &);
};
	
}

#endif /* NONCOPYABLE */