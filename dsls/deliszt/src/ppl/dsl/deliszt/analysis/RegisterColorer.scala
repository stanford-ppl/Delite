package ppl.dsl.deliszt.analysis

import scala.collection.mutable.ArrayBuffer

class RegisterColorer extends Colorer {
  class Colors {
    val _colors = new ArrayBuffer[Boolean](0)
    
    def get(): Int = {
      val i = _colors.indexOf(false)
      if(i < 0) {
        _colors += false
        _colors.size - 1
      }
      else {
        i
      }
		}
    
    def clear() {
      for(i <- 0 until _colors.size) {
        _colors(i) = false
      }
    }
    
    def apply(i: Int) = _colors(i)
    def update(i: Int, b: Boolean) {_colors(i) = b}
    def length = _colors.length
    override def toString = _colors.map(_.toString + " ").mkString
  }
	
	// numNodes = number of nodes
	// size_t sz = pair of start and end pointers for each node's edge list in eg
	// size_t eg = 
	// size_t color_out = color number for each node
	// returns the number of colors used
  def color(numNodes: Int, sizes: Array[Int], edges: Array[Int]) = {
    val color_out = new Array[Int](numNodes)
  
		val nodes = ((0 until numNodes) map { i: Int =>
      new NodeInfo(i, sizes(i+1) - sizes(i))
    }).toArray
    
    val colors = new Colors()
    
    val pq = UpdateablePriorityQueue(nodes, numNodes)
    val removed = Array.fill[Boolean](numNodes) {false}
		
    var node: Option[NodeInfo[Int]] = None
    
		while({ node = pq.pop(); node.isDefined }) {
      node match {
        case Some(node) => {
          removed(node.obj) = true
          for(i <- sizes(node.obj) until sizes(node.obj+1)) {
            val otherNode = edges(i)
            if(!removed(otherNode)) {
              pq.prioritize(nodes(otherNode),nodes(otherNode).priority - 1)
            }
          }
        }
      }
		}
    
		//now pq's internal array is reverse sorted in the order we removed stuff
		//now add the stuff back, coloring as we go
    for(c <- 0 until numNodes) {
			colors.clear()
			val ni = pq.heap(c+1)
			assert(removed(ni.obj));
			for(i <- sizes(ni.obj) until sizes(ni.obj + 1)) {
				val otherNode = edges(i)
				if(!removed(otherNode)) {
					colors(color_out(otherNode)) = true
				}
			}
			removed(ni.obj) = false
			color_out(ni.obj) = colors.get()
		}

		(color_out, colors.length)
	}
}
