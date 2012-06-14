package ppl.dsl.optiml

/**
 * OptiML compiler types
 */


//////////////////
// OptiML

/**
 * Vector 
 */
trait StreamRow[T] extends DenseVectorView[T]
trait IndexVector extends Vector[Int] with RowVector[Int]
trait IndexVectorRange extends IndexVector with RangeVector
trait IndexVectorDense extends IndexVector with DenseVector[Int]

trait IndexVector2 extends Vector[(Int,Int)]
trait IndexVectorTriangular extends IndexVector2

/**
 *  Matrix
 */
trait Image[T] extends DenseMatrix[T]
trait GrayscaleImage extends Image[Int]


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

