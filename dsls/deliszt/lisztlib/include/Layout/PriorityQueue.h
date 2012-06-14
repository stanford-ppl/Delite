#ifndef _PRIORITY_QUEUE_H
#define _PRIORITY_QUEUE_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits>
#include <algorithm>

template<typename T>
class PriorityQueue {
public:
	PriorityQueue() : heap(NULL) {}
	void init(size_t n_max) {
		n_max_elems = n_max;
		heap = new T[n_max_elems + 1];
		max_pos = 0;
	}
	template<typename Iter>
	void init(Iter begin, Iter end) {
		n_max_elems = max_pos = end - begin;
		heap = new T[n_max_elems + 1];
		for(size_t i = 0; begin != end; i++, begin++) {
			heap[i + 1] = begin;
			position(heap[i + 1]) = i + 1;
		}
		heapify();
	}
	void insert(T t) {
		assert(max_pos < n_max_elems);
		size_t pos = ++max_pos;
		heap[pos] = t;
		swim(pos);
	}
	//note that if pop is called with no interleaving calls to insert
	//then the heap array will be in reverse sorted order
	bool pop(T * min_pri) {
		if(max_pos > 0) {
			*min_pri = heap[1];
			swap(1,max_pos);
			max_pos--;
			sink(1);
			return true;
		} else return false;
	}
	//make T be more important
	void prioritize(T t, size_t pri) {
		assert(pri <= priority(t));
		priority(t) = pri;
		swim(position(t));
	}
	T * internal_array() { return heap + 1; }
	~PriorityQueue() {
		if(heap) {
			delete [] heap;
			heap = NULL;
		}
	}
private:
	void heapify() {
		for(size_t p = max_pos; p > 0; p--) {
			sink(p);
		}
	}
	size_t left(size_t i) { return 2 * i; }
	size_t right(size_t i) { return 2 * i + 1; }
	size_t parent(size_t i) { return i / 2; }
	size_t & priority(T t) { return t->priority; }
	size_t & position(T t) { return t->position; }
	void sink(size_t c) {
		while(true) {
			size_t l = left(c);
			size_t r = right(c);
			size_t c_pri = priority(heap[c]);
			size_t l_pri = l > max_pos ? std::numeric_limits<size_t>::max() : priority(heap[l]);
			size_t r_pri = r > max_pos ? std::numeric_limits<size_t>::max() : priority(heap[r]);
			if(l_pri < r_pri) {
				if(c_pri > l_pri) {
					swap(c,l);
					//tail call
					//sink(l);
					c = l;
				} else return;
			} else {
				if(c_pri > r_pri) {
					swap(c,r);
					//tail call
					//sink(r);
					c = r;
				} else return;
			}
		}
	}
	void swim(size_t c) {
		while(c > 1) {
			size_t p = parent(c);
			size_t p_pri = priority(heap[p]);
			size_t c_pri = priority(heap[c]);
			if(c_pri < p_pri) {
				swap(c,p);
				c = p;
			} else return;
		}
	}
	void swap(size_t a, size_t b) {
		//printf("swap (%ld) and (%ld)\n",a,b);
		assert( a <= max_pos);
		assert( b <= max_pos);
		std::swap(heap[a],heap[b]);
		position(heap[a]) = a;
		position(heap[b]) = b;
	}
	T * heap;
	size_t max_pos;
	size_t n_max_elems;
	public:
	void print() {
		for(size_t i = 1; i <= max_pos; i++) {
			printf("%ld ",heap[i]->priority);
		}
		printf("\n");
	}
};

struct PQTest {
	struct TestObj {
		size_t priority;
		size_t position;
	};
	
	void drain(PriorityQueue<TestObj *> & p) {
		TestObj * obj;
		size_t last_pri = std::numeric_limits<size_t>::min();
		for(int i = 0; p.pop(&obj); i++) {
			printf("%d: %ld\n", i, obj->priority);
			assert(obj->priority >= last_pri);
			last_pri = obj->priority;
		}
	}
	int main(int argc, char ** argv) {
		PriorityQueue<TestObj *> pq1;
		PriorityQueue<TestObj *> pq2;
		size_t sz = argc - 1;
		pq1.init(sz);
		TestObj * arr = new TestObj[sz];
		for(size_t i = 0; i < sz; i++) {
			arr[i].priority = atoi(argv[i + 1]);
			pq1.insert(&arr[i]);
			pq1.print();
		}
		drain(pq1);
		printf("STEP 2\n");
		pq2.init(arr,arr + sz);
		pq2.print();
		drain(pq2);
		return 0;
	}
};

#endif