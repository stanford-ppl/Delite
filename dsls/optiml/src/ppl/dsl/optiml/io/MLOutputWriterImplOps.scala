package ppl.dsl.optiml.io

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,TrainingSet}
import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.ScalaOpsPkg
import ppl.dsl.optiml.OptiML

trait MLOutputWriterImplOps { this: Base =>
  def mloutput_write_impl[A:Manifest](m: Rep[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
  def mloutput_write_vector_impl[A:Manifest](v: Rep[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
  def mloutput_write_img_pgm_impl(img: Rep[Matrix[Double]], filename: Rep[String]): Rep[Unit]
}

trait MLOutputWriterImplOpsStandard extends MLOutputWriterImplOps {
  this: OptiML =>
  
  ///////////////
  // kernels

  def mloutput_write_impl[A:Manifest](m: Rep[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit] = {
    val xfs = BufferedWriter(FileWriter(filename))
    m.foreachRow( vec => {
      vec.foreach( e => {
        xfs.write(String.valueOf(conv(e)))
        xfs.write("  ")
      })
    xfs.write("\n")
    })
    xfs.close()
  }

  def mloutput_write_vector_impl[A:Manifest](v: Rep[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit] = {
    val xfs = BufferedWriter(FileWriter(filename))

    v.foreach( e => {
      xfs.write(String.valueOf(conv(e)) + "\n")
    })
    xfs.close()
  }

  def mloutput_write_img_pgm_impl(img: Rep[Matrix[Double]], filename: Rep[String]): Rep[Unit] = {
    val xfs = BufferedWriter(FileWriter(filename))

    xfs.write("P2\n")
    xfs.write(String.valueOf(img.numCols) + " " + String.valueOf(img.numRows) + "\n")
    xfs.write(255 + "\n")

    val min = img.min
    val max = img.max

    for (i <- 0 until img.numRows) {
      var first = unit(true)
      for (j <- 0 until img.numCols) {
        if(!first) {
          xfs.write("\t")
        }

        if (min != max) {
          val pixel = (255.0 * (img(i, j) - min) / (max - min)).asInstanceOfL[Int] + "\n"
          xfs.write(pixel)
        }
        else
          xfs.write("0\n")
      }
    }
    xfs.close()
  }
}