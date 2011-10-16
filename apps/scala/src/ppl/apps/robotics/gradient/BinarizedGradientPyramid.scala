package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.application._
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientPyramidFuncs {
  this: OptiMLApplication =>


  def makePyramid(gradientImage: Rep[GrayscaleImage]) = {
    var crt = gradientImage
    var currentLevel = 0
    val pyramid = BinarizedGradientPyramid(DenseVector[GrayscaleImage](0, true), 3, 1, 3)

    while (currentLevel < pyramid.start_level + pyramid.levels) {
      if (currentLevel >= pyramid.start_level) {
        pyramid.pyramid += crt //TODO TR non-mutable write
      }
      if (currentLevel != (pyramid.start_level + pyramid.levels - 1)) {
        crt = varToGrayscaleImageOps(crt).bitwiseOrDownsample()
      }
      currentLevel += 1
    }
    pyramid
  }

  def getIndex(pyramid: Rep[BinarizedGradientPyramid], index: Rep[Int]) = pyramid.pyramid(index - pyramid.start_level)
}