package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala._
import ppl.delite.framework.DeliteApplication

object gradient extends DeliteApplication with OptiMLExp
  with BinarizedGradientGridFuncs with BinarizedGradientPyramidFuncs with BinarizedGradientTemplateFuncs {

  def main() = {

    val image = MLInputReader.readGrayscaleImage(args(0))
    val all_templates = MLInputReader.readTemplateModels(args(1))

    tic
    var imgs = unit(0)
    while (imgs < 3) {
      detectAllObjects(all_templates, image)
      imgs += 1
    }
    toc
  }
}
