package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{HashMap, Map, MutableList}

/**
 * Directed/undirected multi-graph
 */

class Graph(val isDirected: Boolean)  {
  /** Flag indicating whether the graph can be mutated or not */
  var immutable = false
  
  /** Mutable graph structures */
  protected var adjMap = HashMap[Node, MutableList[(Edge,Node)]]()
  protected var adjMapReversed = HashMap[Node, MutableList[(Edge,Node)]]()
  protected var edgeList = new MutableList[Edge]()
  
  /** Immutable graph structures */
  // graph nodes
  protected var _nodes : Array[Node] = null
  // graph edges
  protected var _edges : Array[Edge] = null
  // out edges associated with each node
  protected var nodeOutEdges : Array[GIterable[Edge]] = null
  // in edges associated with each node
  protected var nodeInEdges : Array[GIterable[Edge]] = null
  // out neighbors of each node
  protected var nodeOutNeighbors : Array[GIterable[Node]] = null
  // in neighbors of each node
  protected var nodeInNeighbors : Array[GIterable[Node]] = null

  /** Basic graph lookup operations (mutable/immutable graphs) */
  def nodes = {
    if(immutable) {
      new GIterable[Node](_nodes)
    }
    else {
      new GIterable[Node](adjMap.keySet.toArray)
    }
  }

  def edges = {
    if(immutable) {
      new GIterable[Edge](_edges)
    }
    else {
      new GIterable[Edge](edgeList.toArray)
    }
  }
  
  def numEdges: Int = {
    if(!immutable) {
      edgeList.size
    }
    else {
      _edges.length
    }
  }
  def numNodes: Int = {
    if(!immutable) {
      adjMap.keySet.size
    }
    else {
      _nodes.length
    }
  }
  
  /** Graph construction operations (mutable graphs only) */
  
  def addNode: Node = {
    if (immutable) {
      throw new RuntimeException("Cannot add a new node to an immutable graph")
    }
    val n:Node = new Node(this)
    adjMap(n) = new MutableList[(Edge, Node)]()
    
    if(isDirected) {
      adjMapReversed(n) = new MutableList[(Edge, Node)]()
    }
    n
  }
  
  // note: multiple edges between same nodes are allowed
  def addEdge(from: Node, to: Node): Edge = {
    if (immutable) {
      throw new RuntimeException("Cannot add a new edge to an immutable graph")
    }
    val e:Edge = new Edge(this, from, to)
    
    edgeList += e
    adjMap(e.from) += Pair(e, e.to)
    if(isDirected) {
      adjMapReversed(e.to) += Pair(e, e.from)
    }
    e
  }
   
  // freezes the current graph, making it immutable
  def freeze() : Unit = {
    if (immutable) {
      throw new RuntimeException("Cannot freeze an immutable graph")
    }
    immutable = true   
    _nodes = adjMap.keySet.toArray
    _edges = edgeList.toArray
    var id = 0
    while(id < numNodes) {
      _nodes(id).id = id
      id += 1
    }
    id = 0
    while(id < numEdges) {
      _edges(id).id = id
      id += 1
    }
    val sorted = _nodes map {adjMap(_).sortBy{case (e,n) => n.id}}
    nodeOutEdges = sorted map {(l: MutableList[(Edge,Node)]) => new GIterable[Edge]((l map {_._1}).toArray)}
    nodeOutNeighbors = sorted map {(l: MutableList[(Edge,Node)]) => new GIterable[Node]((l map {_._2}).toArray)}
    if(isDirected) {
      val sortedReversed = _nodes map {adjMapReversed(_).sortBy{case (e,n) => n.id}}
      nodeInEdges = sortedReversed map {(l: MutableList[(Edge,Node)]) => new GIterable[Edge]((l map {_._1}).toArray)}
      nodeInNeighbors = sortedReversed map {(l: MutableList[(Edge,Node)]) => new GIterable[Node]((l map {_._2}).toArray)}
    }
    adjMap = null
    adjMapReversed = null
    edgeList = null
  }
  
  // returns an immutable snapshot of the current graph
  def snapshot() : Graph = {
    if (immutable) {
      throw new RuntimeException("Cannot snapshot an immutable graph")
    }
    val immutableSnapshot = new Graph(isDirected) 
    immutableSnapshot.immutable = true   
    // make a structural copy of the graph
    // preserving the appropriate node relations
    immutableSnapshot._nodes = new Array[Node](numNodes)
    immutableSnapshot._edges = new Array[Edge](numEdges)
    val nodesToCopy = this.adjMap.keySet.toArray
    val edgesToCopy = this.edgeList.toArray
    // assign a correspondence with copied nodes and edges
    val nodeToCopyMap = HashMap[Node, Node]()
    val edgeToCopyMap = HashMap[Edge, Edge]()
    var i = 0
    while(i < numNodes) {
      immutableSnapshot._nodes(i) = new Node(immutableSnapshot)
      immutableSnapshot._nodes(i).id = i
      nodeToCopyMap(nodesToCopy(i)) = immutableSnapshot._nodes(i)
      i += 1
    }
    i = 0
    while(i < numEdges) {
      val e = edgesToCopy(i)
      immutableSnapshot._edges(i) = new Edge(immutableSnapshot, nodeToCopyMap(e.from), nodeToCopyMap(e.to))
      immutableSnapshot._edges(i).id = i
      edgeToCopyMap(e) = immutableSnapshot._edges(i)
      i += 1
    }
    
    val sorted = nodesToCopy map {this.adjMap(_).sortBy{case (e,n) => nodeToCopyMap(n).id}}
    immutableSnapshot.nodeOutEdges = sorted map {(l: MutableList[(Edge,Node)]) => new GIterable[Edge]((l map {(t: (Edge, Node)) => edgeToCopyMap(t._1)}).toArray)}
    immutableSnapshot.nodeOutNeighbors = sorted map {(l: MutableList[(Edge,Node)]) => new GIterable[Node]((l map {(t: (Edge, Node)) => {nodeToCopyMap(t._2)}}).toArray)}
    
    if(isDirected) {
      val sortedReversed = nodesToCopy map {this.adjMapReversed(_).sortBy{case (e,n) => nodeToCopyMap(n).id}}
      immutableSnapshot.nodeInEdges = sortedReversed map {(l: MutableList[(Edge,Node)]) => new GIterable[Edge]((l map {(t: (Edge, Node)) => edgeToCopyMap(t._1)}).toArray)}
      immutableSnapshot.nodeInNeighbors = sortedReversed map {(l: MutableList[(Edge,Node)]) => new GIterable[Node]((l map {(t: (Edge, Node)) => nodeToCopyMap(t._2)}).toArray)}
    }
    
    immutableSnapshot
  }
  
  def isImmutable: Boolean = {this.immutable}
  
  /** Graph analysis operations (immutable graphs only) */
  
  def getNode(nId: Int): Node = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    _nodes(nId)
  }
  
  def getEdge(eId: Int): Edge = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    _edges(eId)
  }
  
  // "flip" the edge directions, to be used during traversals
  // (note: does not currently modify the actual Edge objects To/From fields)
  // TODO: ensure this is used during traversals/iterations only or update the Edges
  // to reflect a true flip
  def reverse: Graph = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    val rg = new Graph(isDirected)
    rg.immutable = true
    
    rg._nodes = this._nodes
    rg._edges = this._edges
    rg.nodeInEdges = this.nodeOutEdges
    rg.nodeOutEdges = this.nodeInEdges
    rg.nodeInNeighbors = this.nodeOutNeighbors
    rg.nodeOutNeighbors = this.nodeInNeighbors
    
    rg 
  }
  
  // only available after construction finalization
  def outNeighbors(n: Node) = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")      
    nodeOutNeighbors(n.id)
  }
  
  def inNeighbors(n: Node) = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    if(isDirected) {
      nodeInNeighbors(n.id)
    } else {
      nodeOutNeighbors(n.id)
    }
  }

  def outEdges(n: Node) = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    nodeOutEdges(n.id)
  }
  
  def inEdges(n: Node) = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    if(isDirected) {
      nodeInEdges(n.id)
    } else {
      nodeOutEdges(n.id)
    }
  }
  
  def inDegree(n: Node) = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    if(isDirected) {
      nodeInEdges(n.id).length
    } else {
      nodeOutEdges(n.id).length
    }
  }
  
  def outDegree(n: Node) = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    nodeOutEdges(n.id).length
  }
  
  def upNeighbors(n: Node, visited: Array[Int]): GIterable[Node] = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    
    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }
    
    val inNbrs = inNeighbors(n)
    val upNbrs = collection.mutable.HashSet[Node]()
    var i = 0
    while (i < inNbrs.length) {
      if ((visited(inNbrs(i).id) < visited(n.id)) && (visited(inNbrs(i).id) != 0)) {
        upNbrs.add(inNbrs(i))
      }
      i += 1
    }
    
    new GIterable[Node](upNbrs.toArray)
  }
   
  def downNeighbors(n: Node, visited: Array[Int]): GIterable[Node] = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    
    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }
    
    val outNbrs = outNeighbors(n)
    val downNbrs = collection.mutable.HashSet[Node]()
    var i = 0
    while (i < outNbrs.length) {
      if (visited(outNbrs(i).id) > visited(n.id) || visited(outNbrs(i).id) == 0) {
        downNbrs.add(outNbrs(i))
      }
      i += 1
    }
    
    new GIterable[Node](downNbrs.toArray)
  }
  
  def upEdges(n: Node, visited: Array[Int]): GIterable[Edge] = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    
    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }
    
    val inEdgs = inEdges(n)
    val upEdges = new MutableList[Edge]()
    var i = 0
    while (i < inEdgs.length) {
      if ((visited(inEdgs(i).from.id) < visited(n.id)) && (visited(inEdgs(i).from.id) != 0)) {
        upEdges += inEdgs(i)
      }
      i += 1
    }
    new GIterable[Edge](upEdges.toArray)
  }
   
  def downEdges(n: Node, visited: Array[Int]): GIterable[Edge] = {
    if (!immutable) throw new RuntimeException("Operation avaliable for immutable graphs only")
    
    if(visited == null) {
      throw new RuntimeException("Operation available during BFS traversal only")
    }
    
    val outEdgs = outEdges(n)
    val downEdges = new MutableList[Edge]()
    var i = 0
    while (i < outEdgs.length) {
      if (visited(outEdgs(i).to.id) > visited(n.id) || visited(outEdgs(i).to.id) == 0) {
        downEdges += outEdgs(i)
      }
      i += 1
    }
    
    new GIterable[Edge](downEdges.toArray)
  }
}