package ppl.dsl.optiml

import scala.virtualization.lms.common.Base
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.embedded.scala.ScalaOpsPkg3

trait MLInputReaderImplOps { this: DeliteApplication =>
  def mlinput_read_impl(filename: Rep[String]) : Rep[Matrix[Double]]
  def mlinput_read_vector_impl(filename : Rep[String]) : Rep[Vector[Double]]
}

trait MLInputReaderImplOpsStandard extends MLInputReaderImplOps {
  this: DeliteApplication with MLInputReaderOps with MatrixOps with VectorOps with ScalaOpsPkg3 =>
  
  ///////////////
  // kernels

  def mlinput_read_impl(filename: Rep[String]) = {
    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    // TODO: somehow the assigment above is not getting scheduled before the split, despite reflectEffect
    // TODO: weirdness with StringOps, losing a \        
    var dbls = line.split("\\\\s+")
    val x = Matrix[Double](0, dbls.length)

    while (line != null){
      val v = Vector[Double](dbls.length)
      for (i <- 0 until dbls.length){
        v(i) = Double.parseDouble(dbls(i))
      }
      x += v

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        dbls = line.split("\\\\s+")
      }
    }
    xfs.close()

    x
  }

  def mlinput_read_vector_impl(filename: Rep[String]) = {
    val x = Vector[Double](0)

    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    while (line != null){
      line = line.trim()
      val dbl = Double.parseDouble(line)
      x += dbl

      line = xfs.readLine()
    }
    xfs.close()

    x
  }

}