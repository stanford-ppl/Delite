package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

// lots of violations of the LSP here, that can end up exposed to the user (TODO: fix)

/**
 * Vector
 */


// trait StreamRow[@specialized(Boolean, Int, Long, Float, Double) T] extends VectorView[T] {
//   def index: Int
// }

//trait IndexVector //extends Vector[Int] //extends DenseVector[Int]
//trait IndexVectorDense extends IndexVector
//trait IndexVectorRange extends IndexVector

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
//   def Clone = throw new UnsupportedOperationException()
//   def unsafeSetData(xs: Array[Int], len: Int) = throw new UnsupportedOperationException()
// }

/**
 * TrainingSet
 */

// trait Labels[L] extends DenseVector[L] {
//   def numLabels = length
// }

// trait TrainingSet[@specialized(Boolean, Int, Long, Float, Double) T,@specialized(Boolean, Int, Long, Float, Double) L] extends DenseMatrix[T] {
//   def numSamples = _numRows
//   def numFeatures = _numCols
//   def labels: Labels[L]
// 
//   def transposed: TrainingSet[T,L]
//   override def update(row: Int, col: Int, x: T) = throw new UnsupportedOperationException("Training sets are immutable")
//   override def insertRow(pos: Int, x: DenseVector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
//   override def insertAllRows(pos: Int, xs: DenseMatrix[T]) = throw new UnsupportedOperationException("Training sets are immutable")
//   override def insertCol(pos: Int, x: DenseVector[T]) = throw new UnsupportedOperationException("Training sets are immutable")
//   override def insertAllCols(pos: Int, xs: DenseMatrix[T]) = throw new UnsupportedOperationException("Training sets are immutable")
//   override def removeRows(pos: Int, len: Int) = throw new UnsupportedOperationException("Training sets are immutable")
//   override def removeCols(pos: Int, len: Int) = throw new UnsupportedOperationException("Training sets are immutable")
// }

/**
 * Image
 */

//trait Image[@specialized(Int,Float,Double) T] //extends DenseMatrix[T]

//trait GrayscaleImage extends Image[Int]

/**
 * Graph
 */

// trait Graph {
//   def vertices: DenseVector[Vertex]
//   def edges: DenseVector[Edge]
//   //def adjacent(a: Vertex, b: Vertex): Boolean
//   def neighborsOf(a: Vertex): DenseVector[Vertex]
//   def neighborsSelfOf(a: Vertex): DenseVector[Vertex]
//   def edgesOf(a: Vertex): DenseVector[Edge]
//   def containsEdge(e: Edge): Boolean
//   def containsVertex(v: Vertex): Boolean
// 
//   def addVertex(v: Vertex)
//   def addEdge(e: Edge, a: Vertex, b: Vertex)
//   //def removeEdge(a: Vertex, b: Vertex)
//   def freeze(): Unit
//   def frozen: Boolean
// }

// this is really not pretty. this class hierarchy needs to be thought about more than i've had a chance to.
// trait Vertex {
//   // type V <: Vertex
//   // type E <: Edge
//   // type G = Graph[V,E]
//   
//   def graph: Graph
//   def data: Option[MessageData]
//   def edges: Edges[Edge] 
//   def target(e: Edge): Vertex
//   def neighbors: DenseVector[Vertex]
//   def neighborsSelf: DenseVector[Vertex]
//   def addTask(v: Vertex): Unit
//   def tasks: DenseVector[Vertex]
//   def clearTasks(): Unit
// }
// 
// trait Edge {
//   // type V <: Vertex
//   // type E <: Edge
//   // type G = Graph[V,E]
// 
//   def graph: Graph
//   def in(v: Vertex): Option[MessageData]
//   def out(v: Vertex): Option[MessageData]
//   def target(source: Vertex): Vertex  
// }

// trait Vertices[V <: Vertex] extends DenseVector[V] {
//   def Clone: Vertices[V]
//   def printBeliefs(): Unit
// }

//trait Edges[E <: Edge] extends DenseVector[E]


/**
 * Bidirectional graph
 */

// trait InOutEdges[E <: Edge] extends DenseVector[(E,E)]
// 
// trait MessageVertex extends Vertex {
//   type V = MessageVertex
//   type E = MessageEdge
// 
// }

// trait MessageEdge extends Edge {
//   type V = MessageVertex
//   type E = MessageEdge
// 
// }

//trait MessageData


/**
 * Stream
 */

// trait Stream[@specialized(Boolean, Int, Long, Float, Double) T] {
//   def numRows: Int
//   def numCols: Int
//   def chunkSize: Int
//   def initRow(row: Int, offset: Int)
//   def isPure: Boolean
//   def chunkRow(idx: Int, offset: Int): StreamRow[T]
//   def rawElem(idx: Int): T
//   def vview(start: Int, stride: Int, length: Int, isRow: Boolean): VectorView[T]
// }
