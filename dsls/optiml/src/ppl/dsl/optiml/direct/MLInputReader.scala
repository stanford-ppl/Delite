package ppl.dsl.optiml.direct

import java.io.{BufferedReader, FileReader}
import scala.collection.mutable.ArrayBuffer
import ppl.dsl.optiml.direct.Matrix

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)
object MLInputReader{

  def read(filename: String) : Matrix[Double] = {
    val xfs = new BufferedReader(new FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    var dbls = line.split("\\s+")
    val x = Matrix[Double](0, dbls.length)

    while (line != null){
      val v = Vector[Double](dbls.length)
      for (i <- 0 until dbls.length){
        v(i) = java.lang.Double.parseDouble(dbls(i))
      }
      x += v

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        dbls = line.split("\\s+")
      }
    }
    xfs.close()

    x
  }

   def readVector(filename: String) : Vector[Double] = {
    val x = Vector[Double](0)

    val xfs = new BufferedReader(new FileReader(filename))
    var line = xfs.readLine()
    while (line != null){
      line = line.trim()
      val dbl = java.lang.Double.parseDouble(line)
      x += dbl

      line = xfs.readLine()
    }
    xfs.close()

    x
  }

}
