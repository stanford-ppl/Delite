package ppl.dsl.optiml

/**
 * OptiML compiler types
 */


//////////////////
// OptiML

trait Labels[T] extends DenseVector[T] // ! to be temporarily removed to simplify things for pldi
trait StreamRow[T] extends VectorView[T]
trait IndexVector extends Vector[Int] with RowVector[Int]
trait IndexVectorRange extends IndexVector
trait IndexVectorDense extends IndexVector
//trait IndexVectorWC extends IndexVector
trait IndexVector2

trait TrainingSet[T,L] extends Matrix[T]
trait Image[T] extends Matrix[T]
trait GrayscaleImage extends Image[Int]

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
