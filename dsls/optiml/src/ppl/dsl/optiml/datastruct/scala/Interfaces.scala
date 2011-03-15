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
 * Delite
 */

abstract class DeliteOpMultiLoop[A] {
  def size: Int
  def alloc: A
  def split(rhs: A): A
  def process(__act: A, idx: Int): Unit
  def combine(__act: A, rhs: A): Unit
}



/**
 * @tparam CR  A subtype of DeliteCollection[B]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
abstract class DeliteOpMap[@specialized(Boolean, Int, Long, Float, Double) A, 
                  @specialized(Boolean, Int, Long, Float, Double) B, CR] {
  def in: DeliteCollection[A]
  def alloc: CR
  def map(a: A): B
}

/**
 * @tparam CR  A subtype of DeliteCollection[R]; passed as a separate parameter to avoid requiring a higher kinded type.
 */
abstract class DeliteOpZipWith[@specialized(Boolean, Int, Long, Float, Double) A,
                      @specialized(Boolean, Int, Long, Float, Double) B, 
                      @specialized(Boolean, Int, Long, Float, Double) R, CR] {
  def inA: DeliteCollection[A]
  def inB: DeliteCollection[B]
  def alloc: CR
  def zip(a: A, b: B): R
}

abstract class DeliteOpReduce[@specialized(Boolean, Int, Long, Float, Double) R] {
  def in: DeliteCollection[R]
  def reduce(r1: R, r2: R): R
}

abstract class DeliteOpMapReduce[@specialized(Boolean, Int, Long, Float, Double) A, 
                        @specialized(Boolean, Int, Long, Float, Double) R] {
  def in: DeliteCollection[A]
  def map(elem: A): R
  def reduce(r1: R, r2: R): R

  /**
   * default implementation of map-reduce is simply to compose the map and reduce functions
   * A subclass can override to fuse the implementations
   */
  def mapreduce(acc: R, elem: A): R = reduce(acc, map(elem))
}

abstract class DeliteOpZipWithReduce[@specialized(Boolean, Int, Long, Float, Double) A, 
                            @specialized(Boolean, Int, Long, Float, Double) B, 
                            @specialized(Boolean, Int, Long, Float, Double) R] {
  def inA: DeliteCollection[A]
  def inB: DeliteCollection[B]
  def zip(a: A, b: B): R
  def reduce(r1: R, r2: R): R

  /**
   * default implementation of zip-reduce is simply to compose the zip and reduce functions
   * A subclass can override to fuse the implementations
   */
  def zipreduce(acc: R, a: A, b: B): R = reduce(acc, zip(a,b))
}

abstract class DeliteOpForeach[@specialized(Boolean, Int, Long, Float, Double) A] {
  def in: DeliteCollection[A]
  def foreach(elem: A): Unit
  def sync(idx: Int): List[Any]
}

trait DeliteCollection[@specialized(Boolean, Int, Long, Float, Double) T] {
  def size: Int
  def dcApply(idx: Int): T
  def dcUpdate(idx: Int, x: T)
}

/**
 * Vector
 */

// TODO: vector and matrix should not expose "data" fields. This needs to be refactored.

trait Vector[@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.DeliteCollection[T] {
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

  // DeliteCollection
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
  def size = length
}

trait NilVector[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T] {
  def length : Int = 0
  def apply(i: Int) = throw new UnsupportedOperationException()
  def isRow : Boolean = throw new UnsupportedOperationException()
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
}

trait VectorView[@specialized(Boolean, Int, Long, Float, Double) T] extends Vector[T]

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
}

/**
 * Matrix
 */
trait Matrix[@specialized(Boolean, Int, Long, Float, Double) T] extends ppl.delite.framework.DeliteCollection[T] {
  // fields required on real underlying data structure impl
  def numRows: Int
  def numCols: Int
  def size: Int
  def data: Array[T]

  def apply(i: Int) : VectorView[T]
  def apply(i: Int, j: Int): T
  def update(row: Int, col: Int, x: T)
  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T]
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

}

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

trait Vertices[V <: Vertex] extends Vector[V]
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
  def initChunk(offset: Int)
  def initRow(row: Int, offset: Int)
  def rowsIn(offset: Int): Int
  def isPure: Boolean
  def chunkRow(idx: Int): VectorView[T]
  def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T]
}


/**
 * Ref
 */

case class Ref[@specialized(Boolean, Int, Long, Float, Double) T](v: T) {
  private[this] var _v = v

  def get = _v
  def set(v: T) = _v = v
}
