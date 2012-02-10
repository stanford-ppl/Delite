import ppl.dsl.optiml.datastruct.scala._
import ppl.dsl.optila.datastruct.scala._

// object BinarizedGradientTemplate {
//   def apply(val radius: Int, ...) = newStruct(("radius","Int", radius),
// }

class BinarizedGradientTemplate (
  // In the reduced image. The side of the template square is then 2*r+1.
  val radius: Int,

  // Holds a tighter bounding box of the object in the original image scale
  val rect: Rect,
  val mask_list: DenseVector[Int],

  // Pyramid level of the template (reduction_factor = 2^level)
  val level: Int,

  // The list of gradients in the template
  val binary_gradients: DenseVector[Int],

  // indices to use for matching (skips zeros inside binary_gradients)
  val match_list: IndexVectorDense,

  // This is a match list of list of sub-parts. Currently unused.
  val occlusions: DenseVector[DenseVector[Int]],

  val templates: DenseVector[BinarizedGradientTemplate],

  val hist: DenseVector[Float]
)
 
