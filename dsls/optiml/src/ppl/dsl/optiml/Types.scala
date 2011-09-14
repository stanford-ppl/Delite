package ppl.dsl.optiml

import ppl.delite.framework.datastruct.scala.DeliteCollection

/**
 * This class defines abstract types that are used in the OptiML compiler (and their
 * sub-typing relationships). Ops are statically dispatched based upon these types,
 * i.e. any op supported by Vector should be supported by all of Vector's subtypes.  
 */
 
/**
 * Vector 
 */

// TODO: all current instantiations of Vector should be switched to DenseVector
trait Vector[T] extends DeliteCollection[T]
trait DenseVector[T] extends Vector[T]
trait SparseVector[T] extends Vector[T]

// TODO: add row and column vectors as compiler types only (not in generated code)

trait ZeroVector[T] extends DenseVector[T]
trait EmptyVector[T] extends DenseVector[T]
trait RangeVector extends DenseVector[Int]
trait VectorView[T] extends DenseVector[T]
trait Labels[T] extends DenseVector[T] 

trait MatrixRow[T] extends VectorView[T]
trait MatrixCol[T] extends VectorView[T] 
trait StreamRow[T] extends VectorView[T]

/**
 * IndexVector 
 *  
 * used for accessing elements; can have different shapes...
 */
trait IndexVector extends DenseVector[Int]
trait IndexVectorWC extends IndexVector 
trait IndexVector2


/**
 * Matrix
 */
 
trait Matrix[T] extends DeliteCollection[T] 
trait DenseMatrix[T] extends Matrix[T]
trait SparseMatrix[T] extends Matrix[T]

trait SymmetricMatrix[T] extends DenseMatrix[T] 
trait TrainingSet[T,L] extends DenseMatrix[T] 
trait Image[T] extends DenseMatrix[T]

trait GrayscaleImage extends Image[Int]


/**
 * Graph
 */

// no covariance here, since Graph is mutable.
trait Graph[V <: Vertex, E <: Edge]

// this is really not pretty. this class hierarchy needs to be thought about more than i've had a chance to.
trait Vertex {
  type V <: Vertex
  type E <: Edge
  type G = Graph[V,E]
}

trait Edge {
  type V <: Vertex
  type E <: Edge
  type G = Graph[V,E]
}

trait Vertices[V <: Vertex] extends DenseVector[V]
trait Edges[E <: Edge] extends DenseVector[E]


/**
 * Bidirectional graph
 */

trait InOutEdges[E <: Edge] extends DenseVector[(E,E)]
trait MessageVertex extends Vertex 
trait MessageEdge extends Edge
trait MessageData


/**
 * Stream
 */

trait Stream[T]