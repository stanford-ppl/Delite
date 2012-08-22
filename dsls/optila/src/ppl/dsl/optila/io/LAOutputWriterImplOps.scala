package ppl.dsl.optila.io

import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.ScalaOpsPkg
import ppl.delite.framework.Interfaces
import ppl.dsl.optila._

trait LAOutputWriterImplOps { this: Interfaces =>
  def laoutput_write_impl[A:Manifest](m: Interface[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
  def laoutput_write_vector_impl[A:Manifest](v: Interface[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit]
}

trait LAOutputWriterImplOpsStandard extends LAOutputWriterImplOps {
  this: OptiLACompiler with OptiLALift =>
  
  ///////////////
  // kernels

  def laoutput_write_impl[A:Manifest](m: Interface[Matrix[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit] = {
    val xfs = BufferedWriter(FileWriter(filename))
    m.foreachRow( vec => {
      vec.foreach( e => {
        xfs.write(conv(e).ToString)
        xfs.write("  ")
      })
    xfs.write("\\n")
    })
    xfs.close()
  }

  def laoutput_write_vector_impl[A:Manifest](v: Interface[Vector[A]], filename: Rep[String], conv: Rep[A] => Rep[Double]): Rep[Unit] = {
    val xfs = BufferedWriter(FileWriter(filename))

    v.foreach( e => {
      xfs.write(conv(e).ToString + "\\n")
    })
    xfs.close()
  }
}
