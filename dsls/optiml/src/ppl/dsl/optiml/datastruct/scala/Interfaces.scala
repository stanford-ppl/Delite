package ppl.dsl.optiml.datastruct.scala

// TODO: putting everything in one file was convenient at first, but now that this is growing larger
// it should be refactored into a more logical organization.

//TR: Note that scalac's specialize phase took 15s (!) on this file (on my MacBook).
// No wonder since DeliteOpZipWith and DeliteOpZipWithReduce each had 1000 specialized variants.
// Adding @specialized(Boolean, Int, Long, Float, Double) reduced the time to 5s.
// Note also that DeliteOpMultiLoop does not have that problem because it always
// uses a custom activation record class for each use.

// I changed some of the traits to abstract classes, that might yield a slight speed gain
// at runtime (invokevirtual vs invokeinterface)

/**
 * Vector
 */

// TODO: vector and matrix should not expose "data" fields. This needs to be refactored.
trait Vector[@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.datastruct.scala.DeliteCollection[T] {
  // methods required on real underlying data structure impl
  // we need these for:
  //   1) accessors to data fields
  //   2) setters to data fields (alternatively, methods that can mutate data fields)
  //   3) methods that the runtime expects
  def length : Int
  def isRow : Boolean
  def apply(n: Int) : T
  def update(index: Int, x: T)
  def data: Array[T]
  def toList: List[T]

  def mtrans: Vector[T]
  def sort(implicit o: Ordering[T]): Vector[T] // because we use the underlying data field to sort
  def copyFrom(pos: Int, xs: Vector[T])
  def insert(pos: Int, x: T)
  def insertAll(pos: Int, xs: Vector[T])
  def removeAll(pos: Int, len: Int)
  def clear()
  def trim
  def cloneL: Vector[T]
  def unsafeSetData(xs: Array[T], len: Int)

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
}

trait ZeroVector[T] extends Vector[T]

trait EmptyVector[T] extends Vector[T] {
  def length : Int = 0
  def isRow: Boolean = true // shouldn't matter  
  
  def apply(i: Int): T = throw new UnsupportedOperationException()
  def update(index: Int, x: T) = throw new UnsupportedOperationException()
  def data = throw new UnsupportedOperationException()
  def toList = throw new UnsupportedOperationException()

  def mtrans = throw new UnsupportedOperationException()
  def sort(implicit o: Ordering[T]) = throw new UnsupportedOperationException()
  def insert(pos: Int, x: T) = throw new UnsupportedOperationException()
  def insertAll(pos: Int, xs: Vector[T]) = throw new UnsupportedOperationException()
  def copyFrom(pos: Int, xs: Vector[T]) = throw new UnsupportedOperationException()
  def removeAll(pos: Int, len: Int) = throw new UnsupportedOperationException()
  def trim = throw new UnsupportedOperationException()
  def clear() = throw new UnsupportedOperationException()
  def cloneL = throw new UnsupportedOperationException()
  def unsafeSetData(xs: Array[T], len: Int) = throw new UnsupportedOperationException()
}

trait VectorView[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T]

trait MatrixRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
  def index: Int
}
trait MatrixCol[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
  def index: Int
}

trait StreamRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
  def index: Int
}

trait RangeVector extends Vector[Int]

trait IndexVector extends Vector[Int]

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
  // fields required on real underlying data structure impl
  def numRows: Int
  def numCols: Int
  def size: Int
  def data: Array[T]

  def apply(i: Int, j: Int): T
  def update(row: Int, col: Int, x: T)
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