package ppl.dsl.optila.io

import java.io._
import scala.virtualization.lms.common.Base
import ppl.dsl.optila._

trait LAInputReaderImplOps { this: Base =>
  def lainput_read_impl(filename: Rep[String], delim: Rep[String]) : Rep[DenseMatrix[Double]]
  def lainput_read_vector_impl(filename : Rep[String]): Rep[DenseVector[Double]]
}

trait LAInputReaderImplOpsStandard extends LAInputReaderImplOps {
  this: OptiLACompiler with OptiLALift =>
  
  ///////////////
  // kernels

  def lainput_read_impl(filename: Rep[String], delim: Rep[String]) = {
    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    // TODO: weirdness with StringOps, losing a \        
    var dbls = line.split(delim)
    //var dbls = line.split("\\\\s+")
    val x = DenseMatrix[Double](0, dbls.length)

    while (line != null){
      val v = (0::dbls.length) map { i => Double.parseDouble(dbls(i)) } 
      x += v

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        dbls = line.split(delim)
      }
    }
    xfs.close()

    x.unsafeImmutable
  }

  def lainput_read_vector_impl(filename: Rep[String]) = {
    val x = DenseVector[Double](0, true)

    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    while (line != null){
      line = line.trim()
      val dbl = Double.parseDouble(line)
      x += dbl

      line = xfs.readLine()
    }
    xfs.close()

    x.unsafeImmutable
  }
}
