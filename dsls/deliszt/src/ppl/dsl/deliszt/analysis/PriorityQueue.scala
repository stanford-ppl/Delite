package ppl.dsl.deliszt.analysis

object UpdateablePriorityQueue {
  def apply[T](objs: IndexedSeq[NodeInfo[T]], max: Int) = {
    val heap = new Array[NodeInfo[T]](max+1)
    
    for(i <- 0 until objs.length) {
      objs(i).position = i+1
      heap(i+1) = objs(i)
    }
    
    val pq = new UpdateablePriorityQueue(heap, objs.length, max)
    pq.heapify()
    pq
  }
  
  def apply[T](max: Int) : UpdateablePriorityQueue[T] = {
    apply(new Array[NodeInfo[T]](0), max)
  }
}

class NodeInfo[T](val obj: T, var priority: Int) {
  var position = 0

  override def toString() : String = {
    "Node(val: " + obj + ", pos: " + position + ", pri: " + priority + ")"
  }
}

class UpdateablePriorityQueue[T](val heap: Array[NodeInfo[T]], var end: Int, val max: Int) {
  def left(i: Int) = 2 * i
	def right(i: Int) = 2*i + 1
	def parent(i: Int) = i/2

	def insert(obj: NodeInfo[T]) : Unit = insert(obj, obj.priority)

	def insert(obj: NodeInfo[T], pri: Int) : Unit = {
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
			val obj = heap(1)
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
	
  def print() {
    for(i <- 1 to end) {
      System.out.print(heap(i).priority + " ")
    }
    
    System.out.println()
	}
}

object PQTest {
	def drain(p: UpdateablePriorityQueue[Int]) {
		var last_pri = Int.MinValue
		
		var node: Option[NodeInfo[Int]] = None
		var i = 0
		
		while({ node = p.pop(); node.isDefined }) {
		  node match {
		    case Some(node) => {
		      val priority = node.priority
		      System.out.println(i + ": " + priority)
		      assert(priority >= last_pri)
			    last_pri = priority
		      i += 1
	      }
	      case _ =>
		  }
		}
	}
	
	def main(args: Array[String]) {
	  val numNodes = args.size
	  	  
		val nodes = ((0 until numNodes) map { i: Int =>
      new NodeInfo(i, 0)
    }).toArray
    
    val pq1 = UpdateablePriorityQueue[Int](numNodes)
		
		for(i <- 0 until numNodes) {
  		nodes(i).priority = Integer.parseInt(args(i))
			pq1.insert(nodes(i))
			pq1.print();
		}
		
		drain(pq1)
		
		System.out.println("STEP 2")
		
		val pq2 = UpdateablePriorityQueue(nodes, numNodes)
		pq2.print()
		drain(pq2)
	}
}
