package ppl.dsl

package object CVX {
  /**
   * OptiML compiler types 
   */
  
  /////////////////////////
  // inherited from OptiLA 
  //
  // is there a less manual way of doing this?
  // 
  // type Vector[T] = optila.Vector[T]
  // type DenseVector[T] = optila.DenseVector[T]
  // //type SparseVector[T] = optila.SparseVector[T] 
  // type RowVector[T] = optila.RowVector[T]
  // type ColVector[T] = optila.ColVector[T]
  // //type ZeroVector[T] = optila.ZeroVector[T]
  // //type EmptyVector[T] = optila.EmptyVector[T]
  // type RangeVector = optila.RangeVector
  // type VectorView[T] = optila.VectorView[T]
  // type MatrixRow[T] = optila.MatrixRow[T] 
  // type MatrixCol[T] = optila.MatrixCol[T] 
  // 
  // type Matrix[T] = optila.Matrix[T]
  //type DenseMatrix[T] = optila.DenseMatrix[T]
  //type SparseMatrix[T] = optila.SparseMatrix[T]
  //type SymmetricMatrix[T] = optila.SymmetricMatrix[T]
  
  
  //////////////////
  // OptiML

  // trait Labels[T] extends DenseVector[T] // ! to be temporarily removed to simplify things for pldi
  //   trait StreamRow[T] extends VectorView[T] 
  //   trait IndexVector extends Vector[Int] with RowVector[Int]
  //   trait IndexVectorRange extends IndexVector
  //   trait IndexVectorDense extends IndexVector
  //   //trait IndexVectorWC extends IndexVector 
  //   trait IndexVector2
  // 
  //   trait TrainingSet[T,L] extends Matrix[T] 
  //   trait Image[T] extends Matrix[T]
  //   trait GrayscaleImage extends Image[Int]
  // 
  //   // no covariance here, since Graph is mutable.
  //   trait Graph[V <: Vertex, E <: Edge]
  // 
  //   // this is really not pretty. this class hierarchy needs to be thought about more than i've had a chance to.
  //   trait Vertex {
  //     type V <: Vertex
  //     type E <: Edge
  //     type G = Graph[V,E]
  //   }
  // 
  //   trait Edge {
  //     type V <: Vertex
  //     type E <: Edge
  //     type G = Graph[V,E]
  //   }
  // 
  //   trait Vertices[V <: Vertex] extends DenseVector[V]
  //   trait Edges[E <: Edge] extends DenseVector[E]
  // 
  // 
  //   /**
  //    * Bidirectional graph
  //    */
  //   trait InOutEdges[E <: Edge] extends DenseVector[(E,E)]
  //   trait MessageVertex extends Vertex 
  //   trait MessageEdge extends Edge
  //   trait MessageData
  // 
  // 
  //   /**
  //    * Stream
  //    */
  // 
  //   trait Stream[T]
}
