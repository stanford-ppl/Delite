package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala._
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientPyramidFuncs {
  this: OptiMLExp =>

  def makePyramid(gradientImage: Rep[GrayscaleImage]) = {
    var crt = gradientImage
    var currentLevel = unit(0)
    val pyramid = BinarizedGradientPyramid(Vector[GrayscaleImage](), 3, 1, 3)

    while (currentLevel < pyramid.start_level + pyramid.levels) {
      if (currentLevel >= pyramid.start_level) {
        pyramid.pyramid += crt
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