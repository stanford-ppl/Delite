package ppl.dsl.optiml.io

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,TrainingSet}
import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.ScalaOpsPkg
import ppl.dsl.optiml.OptiML

trait MLOutputWriterImplOps { this: Base =>
  def mloutput_write_impl[A:Manifest](m: Rep[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
  def mloutput_write_vector_impl[A:Manifest](v: Rep[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
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
}