package ppl.dsl.deliszt.analysis

object UpdateablePriorityQueue {
}

class NodeInfo[T](val obj: T, var priority: Int) {
  var position = 0
}

class UpdateablePriorityQueue[T](objs: Array[NodeInfo[T]], val max: Int) {
  val heap = initData
  var end = if(objs != null) objs.length else 0
  
  def initData = {
    val nodes = new Array[NodeInfo[T]](max+1)
    
    for(i <- 0 until objs.length) {
      objs(i).position = i+1
      nodes(i+1) = objs(i)
    }
    
    heapify()
    nodes
  }
  
  def left(i: Int) = 2 * i
	def right(i: Int) = 2*i + 1
	def parent(i: Int) = i/2

	def insert(obj: NodeInfo[T], pri: Int) {
		assert(end < max)
    
    end += 1
    
    obj.position = end+1
    obj.priority = pri
    
		heap(end) = obj
		swim(end)
	}
  
	//note that if pop is called with no interleaving calls to insert
	//then the heap array will be in reverse sorted order
	def pop() = {
		if(end > 0) {
			val obj = heap(1).obj
			swap(1,end)
			end -= 1
			sink(1)
			Some(obj)
		}
    else {
      None
    }
	}
  
	//make T be more important
	def prioritize(t: NodeInfo[T], pri: Int) {
		assert(pri <= t.priority)
		t.priority = pri
		swim(t.position)
	}
  
	//def internal_array() { return heap + 1; }
  def internal_array() = heap

	def heapify() {
    for(p <- end until 0 by -1) {
      sink(p)
    }
	}
  
	def sink(_c: Int) {
    var c = _c
    var done = false
  
		while(!done) {
			val l = left(c)
			val r = right(c)
			val c_pri = heap(c).priority
			val l_pri = if(l > end) Int.MaxValue else heap(l).priority
			val r_pri = if(r > end) Int.MaxValue else heap(r).priority
			if(l_pri < r_pri) {
				if(c_pri > l_pri) {
					swap(c,l)
					c = l
				}
        else {
          done = true
        }
			} else {
				if(c_pri > r_pri) {
					swap(c,r)
					c = r
				}
        else {
          done = true
        }
			}
		}
	}
  
	def swim(_c: Int) {
    var c = _c
    var done = false
  
		while(c > 1 && !done) {
			val p = parent(c)
			val p_pri = heap(p).priority
			val c_pri = heap(c).priority
			if(c_pri < p_pri) {
				swap(c,p)
				c = p
			}
      else {
        done = true
      }
		}
	}
  
	def swap(a: Int, b: Int) {
		// println("swap " + a + " and " + b)
		assert(a <= end)
		assert(b <= end)
    
    val temp = heap(a)
    heap(a) = heap(b)
    heap(b) = temp
    
		heap(a).position = a
		heap(b).position = b
	}
	
	/*
  def print() {
    for(i < 1 to end) {
      print(heap(i).priority + " ")
    }
    println()
	} */
}
