package ppl.dsl.optiml.datastruct.scala


// lots of violations of the LSP here, that can end up exposed to the user (TODO: fix)

/**
 * Vector
 */

trait Vector[@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
  def length: Int
  def isRow: Boolean
  def mtrans: Vector[T]  
  def apply(n: Int): T
  def update(index: Int, x: T)
  def cloneL: Vector[T]
  def toList: List[T]
  def sort(implicit o: Ordering[T]): Vector[T] // because we use the underlying data field to sort
  def copyFrom(pos: Int, xs: Vector[T])
  def insert(pos: Int, x: T)
  def insertAll(pos: Int, xs: Vector[T])
  def removeAll(pos: Int, len: Int)
  def clear()
  def trim
    
  // DeliteCollection
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
  def dcSize = length
  
  // value equality is needed for DeliteCollection zero
  override def equals(rhs: Any): Boolean = {
    if (!rhs.isInstanceOf[Vector[T]]) return false
        
    val rv = rhs.asInstanceOf[Vector[T]]
    if (dcSize != rv.dcSize) return false
    var i = 0
    while (i < rv.dcSize) {
      if (dcApply(i) != rv.dcApply(i)) return false
      i += 1        
    }
    true    
  }
  
  // TO BE MOVED TO DenseVector
  def data: Array[T]
  def unsafeSetData(xs: Array[T], len: Int)
}

// TODO: enable this after switching VectorOps to DenseVectorOps
trait DenseVector[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T] {
//   def data: Array[T]
//   def unsafeSetData(xs: Array[T], len: Int)  
}

trait ZeroVector[T] extends DenseVector[T]

trait EmptyVector[T] extends Vector[T] {
  def length : Int = 0
  def isRow: Boolean = true // shouldn't matter
  def mtrans = this  
  def toList = List[T]()
  def cloneL = new EmptyVectorImpl[T]  
  def trim = ()
  def clear() = ()  
  def sort(implicit o: Ordering[T]) = this
  
  def apply(i: Int): T = throw new IndexOutOfBoundsException()
  def update(index: Int, x: T) = throw new IndexOutOfBoundsException()
  def removeAll(pos: Int, len: Int) = throw new IndexOutOfBoundsException()
  def copyFrom(pos: Int, xs: Vector[T]) = throw new IndexOutOfBoundsException()    
  
  // TODO: implement these - return a non-empty Vector  
  def insert(pos: Int, x: T) = throw new UnsupportedOperationException()
  def insertAll(pos: Int, xs: Vector[T]) = throw new UnsupportedOperationException()
  
  // to be removed
  def data: Array[T] = throw new UnsupportedOperationException()
  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException()
}

trait VectorView[@specialized(Boolean, Int, Long, Float, Double) T] extends DenseVector[T]

trait MatrixRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
  def index: Int
}
trait MatrixCol[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
  def index: Int
}

trait StreamRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
  def index: Int
}

trait RangeVector extends DenseVector[Int]

trait IndexVector extends DenseVector[Int]

trait IndexVector2 {
  def rowInd: IndexVector
  def colInd: IndexVector
}

trait IndexVectorWC extends IndexVector {
  override def length : Int = 0
  def apply(i: Int) = throw new UnsupportedOperationException()
  def isRow : Boolean = throw new UnsupportedOperationException()
  def update(index: Int, x: Int) = throw new UnsupportedOperationException()
  def data = throw new UnsupportedOperationException()
  def toList = throw new UnsupportedOperationException()
  def mtrans = throw new UnsupportedOperationException()
  def sort(implicit o: Ordering[Int]) = throw new UnsupportedOperationException()
  def insert(pos: Int, x: Int) = throw new UnsupportedOperationException()
  def insertAll(pos: Int, xs: Vector[Int]) = throw new UnsupportedOperationException()
  def copyFrom(pos: Int, xs: Vector[Int]) = throw new UnsupportedOperationException()
  def removeAll(pos: Int, len: Int) = throw new UnsupportedOperationException()
  def trim = throw new UnsupportedOperationException()
  def clear() = throw new UnsupportedOperationException()
  def cloneL = throw new UnsupportedOperationException()
  def unsafeSetData(xs: Array[Int], len: Int) = throw new UnsupportedOperationException()
}

/**
 * Matrix
 */
trait Matrix[@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
  def numRows: Int
  def numCols: Int
  def size: Int
  def apply(i: Int, j: Int): T
  def update(row: Int, col: Int, x: T)
  
  def data: Array[T]  
  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T]
  def getRow(i: Int): MatrixRow[T]
  def getCol(j: Int): MatrixCol[T]
  def insertRow(pos: Int, x: Vector[T])
  def insertAllRows(pos: Int, xs: Matrix[T])
  def insertCol(pos: Int, x: Vector[T])
  def insertAllCols(pos: Int, xs: Matrix[T])
  def removeRows(pos: Int, len: Int)
  def removeCols(pos: Int, len: Int)
  def cloneL: Matrix[T]

  // DeliteCollection
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T): Unit
  def dcSize = size
  
  // value equality is needed for DeliteCollection zero
  override def equals(rhs: Any): Boolean = {
    if (!rhs.isInstanceOf[Matrix[T]]) return false
        
    val rv = rhs.asInstanceOf[Matrix[T]]
    if (size != rv.size) return false
    var i = 0
    while (i < rv.size) {
      if (dcApply(i) != rv.dcApply(i)) return false
      i += 1        
    }
    true    
  }
}

trait SymmetricMatrix[@specialized(Boolean, Int, Float, Double) T] extends Matrix[T] 

/**
 * TrainingSet
 */

trait Labels[@specialized(Boolean, Int, Long, Float, Double) L] extends Vector[L] {
  def numLabels = length
}

trait TrainingSet[@specialized(Boolean, Int, Long, Float, Double) T,@specialized(Boolean, Int, Long, Float, Double) L] extends Matrix[T] {
  def numSamples = numRows
  def numFeatures = numCols
  def labels: Labels[L]

  def transposed: TrainingSet[T,L]
  override def update(row: Int, col: Int, x: T) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertRow(pos: Int, x: Vector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertAllRows(pos: Int, xs: Matrix[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertCol(pos: Int, x: Vector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertAllCols(pos: Int, xs: Matrix[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def removeRows(pos: Int, len: Int) = throw new UnsupportedOperationException("Training sets are immutable")
  override def removeCols(pos: Int, len: Int) = throw new UnsupportedOperationException("Training sets are immutable")
}

/**
 * Image
 */

trait Image[@specialized(Int,Float,Double) T] extends Matrix[T]

trait GrayscaleImage extends Image[Int]

/**
 * Graph
 */

// no covariance here, since Graph is mutable.
trait Graph[V <: Vertex, E <: Edge] {
  def vertices: Vertices[V]
  def edges: Edges[E]
  //def adjacent(a: V, b: V): Boolean
  def neighborsOf(a: V): Vertices[V]
  def neighborsSelfOf(a: V): Vertices[V]
  def edgesOf(a: V): Edges[E]
  def containsEdge(e: E): Boolean
  def containsVertex(v: V): Boolean

  def addVertex(v: V)
  def addEdge(e: E, a: V, b: V)
  //def removeEdge(a: V, b: V)
  def freeze(): Unit
  def frozen: Boolean
}

// this is really not pretty. this class hierarchy needs to be thought about more than i've had a chance to.
trait Vertex {
  type V <: Vertex
  type E <: Edge
  type G = Graph[V,E]

  def graph: G
  def edges: Edges[E]
  def neighbors: Vertices[V]
  def neighborsSelf: Vertices[V]
  def addTask(v: V): Unit
  def tasks: Vertices[V]
  def clearTasks(): Unit
}

trait Edge {
  type V <: Vertex
  type E <: Edge
  type G = Graph[V,E]

  def graph: G
}

trait Vertices[V <: Vertex] extends Vector[V] {
  def cloneV: Vertices[V]
  def printBeliefs(): Unit
}

trait Edges[E <: Edge] extends Vector[E]


/**
 * Bidirectional graph
 */

trait InOutEdges[E <: Edge] extends Vector[(E,E)]

trait MessageVertex extends Vertex {
  type V = MessageVertex
  type E = MessageEdge

  def data: MessageData
  def edges: Edges[E] 
  def target(e: MessageEdge): MessageVertex
}

trait MessageEdge extends Edge {
  type V = MessageVertex
  type E = MessageEdge

  def in(v: MessageVertex): MessageData
  def out(v: MessageVertex): MessageData
  def target(source: MessageVertex): MessageVertex
}

trait MessageData


/**
 * Stream
 */

trait Stream[@specialized(Boolean, Int, Long, Float, Double) T] {
  def numRows: Int
  def numCols: Int
  def chunkSize: Int
  def initRow(row: Int, offset: Int)
  def isPure: Boolean
  def chunkRow(idx: Int, offset: Int): StreamRow[T]
  def rawElem(idx: Int): T
  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T]
}