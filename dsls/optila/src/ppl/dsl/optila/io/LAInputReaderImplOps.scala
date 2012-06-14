package ppl.dsl.optila.io

import java.io._
import scala.virtualization.lms.common.Base
import ppl.dsl.optila._

trait LAInputReaderImplOps { this: Base =>
  def lainput_read_matrix_impl[Elem:Manifest](filename: Rep[String], schemaBldr: Rep[String] => Rep[Elem], delim: Rep[String]): Rep[DenseMatrix[Elem]]
  def lainput_read_vector_impl[Row:Manifest](filename: Rep[String], schemaBldr: Rep[DenseVector[String]] => Rep[Row], delim: Rep[String]): Rep[DenseVector[Row]]  
}

trait LAInputReaderImplOpsStandard extends LAInputReaderImplOps {
  this: OptiLACompiler with OptiLALift =>
  
  ///////////////
  // kernels
 
  def lainput_read_matrix_impl[Elem:Manifest](filename: Rep[String], schemaBldr: Rep[String] => Rep[Elem], delim: Rep[String]) = {
    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    var elems = line.split(delim)
    val x = DenseMatrix[Elem](0, elems.length)
  
    while (line != null){
      val v = (0::elems.length) map { i => schemaBldr(elems(i)) } 
      x += v
  
      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        elems = line.split(delim)
      }
    }
    xfs.close()
  
    x.unsafeImmutable
  }
  
  
  def lainput_read_vector_impl[Row:Manifest](filename: Rep[String], schemaBldr: Rep[DenseVector[String]] => Rep[Row], delim: Rep[String]) = {
    val x = DenseVector[Row](0, true)

    val xfs = BufferedReader(FileReader(filename))
    var line = xfs.readLine()
    while (line != null){      
      line = line.trim()
      val elems = line.split(delim)
      val v = (0::elems.length) map { i => elems(i) }
      val row = schemaBldr(v)
      x += row

      line = xfs.readLine()
    }
    xfs.close()

    x.unsafeImmutable
  }
  
}
