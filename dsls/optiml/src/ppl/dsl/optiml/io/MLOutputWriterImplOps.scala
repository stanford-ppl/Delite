package ppl.dsl.optiml.io

import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.ScalaOpsPkg
import ppl.dsl.optiml._

trait MLOutputWriterImplOps { this: OptiML =>
  def mloutput_write_img_pgm_impl(img: Rep[GrayscaleImage], filename: Rep[String]): Rep[Unit]
}

trait MLOutputWriterImplOpsStandard extends MLOutputWriterImplOps {
  this: OptiMLCompiler with OptiMLLift =>
  
  ///////////////
  // kernels

  def mloutput_write_img_pgm_impl(img: Rep[GrayscaleImage], filename: Rep[String]): Rep[Unit] = {
    val xfs = BufferedWriter(FileWriter(filename))

    xfs.write("P2\n")
    xfs.write(String.valueOf(img.numCols))
    xfs.write(" ")
    xfs.write(String.valueOf(img.numRows) + "\n")
    xfs.write("255\n")

    val min = img.min
    val max = img.max

    img.foreachRow( vec => {
      vec.foreach( p => {
        if (min != max) {
          val a: Rep[Double] = unit(255.0) * (p - min) // TR M7: internal compiler errors for whole expression
          val pixel = (a / (max - min)).AsInstanceOf[Int]
          xfs.write(pixel + "")
        }
        else
          xfs.write("0")

        xfs.write("\\t")
      })
    xfs.write("\n")
    })

    xfs.close()
  }
}
