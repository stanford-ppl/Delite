package ppl.dsl.optiml

import ppl.dsl.optila.OptiLATypes

/**
 * OptiML compiler types
 */


//////////////////
// OptiML

trait OptiMLTypes extends OptiLATypes {
  this: OptiML =>
  
  /**
   * Vector 
   */
  trait StreamRow[T] extends DenseVectorView[T]
  trait IndexVector extends Vector[Int] with RowVector[Int]
  trait IndexVectorRange extends IndexVector with RangeVector
  trait IndexVectorDense extends DenseVector[Int] with IndexVector 

  trait IndexVector2 extends Vector[(Int,Int)]
  trait IndexVectorTriangular extends IndexVector2

  /**
   *  Matrix
   */
  trait Image[T] extends DenseMatrix[T]
  trait GrayscaleImage extends Image[Double]


  /**
   *  Graph
   */
  trait Graph[VD,ED]
  trait Vertex[VD,ED] 
  trait Edge[VD,ED] 

  /**
   * Stream
   */
  trait Stream[T]


  /**
   * TrainingSet 
   */
  abstract class TrainingSet[T] 
  trait SupervisedTrainingSet[T,L] extends TrainingSet[T]
  trait UnsupervisedTrainingSet[T] extends TrainingSet[T]
}
