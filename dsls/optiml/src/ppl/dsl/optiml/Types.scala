package ppl.dsl.optiml

/**
 * OptiML compiler types
 */


//////////////////
// OptiML

/**
 * Vector 
 */
//trait Labels[T] extends DenseVector[T] 
trait StreamRow[T] extends VectorView[T]
trait IndexVector extends Vector[Int] with VectorRow[Int]
trait IndexVectorRange extends IndexVector with RangeVector
trait IndexVectorDense extends IndexVector with DenseVector[Int]
//trait IndexVectorWC extends IndexVector
trait IndexVector2


/**
 *  Matrix
 */
trait Image[T] extends DenseMatrix[T]
trait GrayscaleImage extends Image[Int]


/**
 *  Graph
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


/**
 * TrainingSet 
 */
 
abstract class TrainingSet[T] //extends DenseMatrix[T]
trait SupervisedTrainingSet[T,L] extends TrainingSet[T]
trait UnsupervisedTrainingSet[T] extends TrainingSet[T]

