package ppl.apps.robotics.gradient

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,GrayscaleImage}
import ppl.delite.framework.DeliteApplication

object gradient extends DeliteApplication with OptiMLExp {
  def main() = {

    val BinarizedGradientGrid = new BinarizedGradientGrid { val IR = gradient.this }

    val image = MLInputReader.readGrayscaleImage(args(0))
    val templateFiles = Vector[String]()
    new java.io.File(args(1)).getCanonicalFile.listFiles.map{
      file => templateFiles += file.getPath()
    }

    val all_templates = modelFilenames.map { f =>
      println("Loading model: " + f)
      ModelReader.loadModels(f)
    }

    tic
    var imgs = 0
    while (imgs < 3) {
      BinarizedGradientGrid.detectAllObjects(all_templates, image)
      imgs += 1
    }
    toc
  }
}
