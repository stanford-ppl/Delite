package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

// lots of violations of the LSP here, that can end up exposed to the user (TODO: fix)

/**
 * Vector
 */


trait StreamRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
  def index: Int
}

trait IndexVector extends Vector[Int] //extends DenseVector[Int]

trait IndexVectorDense extends IndexVector
trait IndexVectorRange extends IndexVector

// trait IndexVector2 {
//   def rowInd: IndexVector
//   def colInd: IndexVector
// }

// trait IndexVectorWC extends IndexVector {
//   def length : Int = 0
//   def apply(i: Int) = throw new UnsupportedOperationException()
//   def isRow : Boolean = throw new UnsupportedOperationException()
//   def update(index: Int, x: Int) = throw new UnsupportedOperationException()
//   def data = throw new UnsupportedOperationException()
//   def toList = throw new UnsupportedOperationException()
//   def mtrans = throw new UnsupportedOperationException()
//   def sort(implicit o: Ordering[Int]) = throw new UnsupportedOperationException()
//   def insert(pos: Int, x: Int) = throw new UnsupportedOperationException()
//   def insertAll(pos: Int, xs: Vector[Int]) = throw new UnsupportedOperationException()
//   def copyFrom(pos: Int, xs: Vector[Int]) = throw new UnsupportedOperationException()
//   def removeAll(pos: Int, len: Int) = throw new UnsupportedOperationException()
//   def trim = throw new UnsupportedOperationException()
//   def clear() = throw new UnsupportedOperationException()
//   def cloneL = throw new UnsupportedOperationException()
//   def unsafeSetData(xs: Array[Int], len: Int) = throw new UnsupportedOperationException()
// }

/**
 * TrainingSet
 */

trait Labels[L] extends DenseVector[L] {
  def numLabels = length
}

trait TrainingSet[@specialized(Boolean, Int, Long, Float, Double) T,@specialized(Boolean, Int, Long, Float, Double) L] extends Matrix[T] {
  def numSamples = numRows
  def numFeatures = numCols
  def labels: Labels[L]

  def transposed: TrainingSet[T,L]
  override def update(row: Int, col: Int, x: T) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertRow(pos: Int, x: DenseVector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertAllRows(pos: Int, xs: Matrix[T]) = throw new UnsupportedOperationException("Training sets are immutable")
  override def insertCol(pos: Int, x: DenseVector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
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

trait Vertices[V <: Vertex] extends DenseVector[V] {
  def cloneV: Vertices[V]
  def printBeliefs(): Unit
}

trait Edges[E <: Edge] extends DenseVector[E]


/**
 * Bidirectional graph
 */

trait InOutEdges[E <: Edge] extends DenseVector[(E,E)]

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
