package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,GrayscaleImage}
import ppl.delite.framework.DeliteApplication

trait BinarizedGradientPyramid {
  // TODO: how do we clean this up in app code?
  val IR: DeliteApplication with OptiMLExp
  import IR._

  def makePyramid(gradientImage: Rep[GrayscaleImage]) = {
    var crt = gradientImage
    var currentLevel = 0
    val pyramid = BinarizedGradientPyramid(Vector[GrayscaleImage](), 3, 1, 3)

    while (currentLevel < pyramid.start_level + pyramid.levels) {
      if (currentLevel >= pyramid.start_level) {
        pyramid.pyramid += crt
      }
      if (currentLevel != (pyramid.start_level + pyramid.levels - 1)) {
        crt = crt.bitwiseOrDownsample()
      }
      currentLevel += 1
    }
    pyramid
  }

  def getIndex(pyramid: Rep[BinarizedGradientPyramid], index: Rep[Int]) = pyramid.pyramid(index - pyramid.start_level)
}