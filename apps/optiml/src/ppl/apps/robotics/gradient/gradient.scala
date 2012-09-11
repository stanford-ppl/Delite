package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.application._
import ppl.delite.framework.DeliteApplication

object gradientRunner extends OptiMLApplicationRunner with gradient

trait gradient extends OptiMLApplication
  with BinarizedGradientGridFuncs with BinarizedGradientPyramidFuncs with BinarizedGradientTemplateFuncs {

  def main() = {
    val image = readGrayscaleImage(args(0))
    val all_templates = readTemplateModels(args(1))    
    tic()
    detectAllObjects(all_templates, image)
    toc()
  }
}
