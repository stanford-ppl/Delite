package ppl.dsl.optiml.application

import ppl.dsl.optiml.{OptiMLExp, OptiML}

trait ApplicationOps extends PairwiseRatingOps with SimilarityOps with BiGGDetectionOps with BinarizedGradientPyramidOps with BinarizedGradientTemplateOps with RectOps { this: OptiML => }
trait ApplicationOpsExp extends PairwiseRatingOpsExp with SimilarityOpsExp with BiGGDetectionOpsExp with BinarizedGradientPyramidOpsExp with BinarizedGradientTemplateOpsExp with RectOpsExp { this: OptiMLExp => }
trait ScalaGenApplicationOps extends ScalaGenPairwiseRatingOps with ScalaGenSimilarityOps with ScalaGenBiGGDetectionOps with ScalaGenBinarizedGradientPyramidOps with ScalaGenBinarizedGradientTemplateOps with ScalaGenRectOps 

// abstract types for internal application data structures
trait PairwiseRating
trait Similarity
trait BiGGDetection
trait BinarizedGradientPyramid
trait BinarizedGradientTemplate
trait Rect
