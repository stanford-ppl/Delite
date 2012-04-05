package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.application._
import ppl.delite.framework.DeliteApplication

object gradientRunner extends OptiMLApplicationRunner with gradient

trait gradient extends OptiMLApplication
  with BinarizedGradientGridFuncs with BinarizedGradientPyramidFuncs with BinarizedGradientTemplateFuncs {

  def main() = {

//    println("gradientapp.1")
    val image = readGrayscaleImage(args(0))
//    println("gradientapp.2")
    val all_templates = readTemplateModels(args(1))
//    println("gradientapp.3: " + all_templates.length)

    tic()
//println("gradientapp.4")
    var imgs = 0
//println("gradientapp.5")
    while (imgs < 3) {
//println("gradientapp.6")
      detectAllObjects(all_templates, image)
//println("gradientapp.7")
      imgs += 1
//println("gradientapp.8")
    }
//println("gradientapp.9")
    toc()
  }
}
