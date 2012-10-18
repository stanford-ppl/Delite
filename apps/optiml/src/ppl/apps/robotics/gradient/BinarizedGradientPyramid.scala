package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.application._
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientPyramidFuncs {
  this: OptiMLApplication =>


  def makePyramid(gradientImage: Rep[GrayscaleImage]) = {
    var crt = gradientImage
    var currentLevel = 0
    
    val pyramid = DenseVector[GrayscaleImage](0, true)
    val startLevel = 3
    val levels = 1
    val fixedLevelIndex = 3
    
    while (currentLevel < startLevel + levels) {
      if (currentLevel >= startLevel) {
        pyramid <<= crt
      }
      if (currentLevel != (startLevel + levels - 1)) {
        crt = crt.bitwiseOrDownsample
      }
      currentLevel += 1
    }
    
    BinarizedGradientPyramid(pyramid, startLevel, levels, fixedLevelIndex)
  }

  def getIndex(pyramid: Rep[BinarizedGradientPyramid], index: Rep[Int]) = pyramid.pyramid(index - pyramid.start_level)
}