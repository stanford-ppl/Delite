package ppl.apps.ml.lbp

import util.Random
import java.io.{BufferedReader, FileReader}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/08/2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LBPImage {
  def load(filename: String) : LBPImage = {
    val xfs = new BufferedReader(new FileReader(filename))

    var line = xfs.readLine()
    line = line.trim()
    var rowscols = line.split("\\s+")

    val rows = java.lang.Integer.parseInt(rowscols(0))
    val cols = java.lang.Integer.parseInt(rowscols(1))

    val img = new LBPImage(rows, cols)

    line = xfs.readLine()

    var i = 0
    while (line != null){
      line = line.trim()
      val pixels = line.split("\\s+")

      for (j <- 0 until Math.min(cols, pixels.length)){
        img.data(img.vertid(i, j)) = java.lang.Double.parseDouble(pixels(j))
      }

      line = xfs.readLine()
      i += 1
    }
    xfs.close()

    img
  }
}

class LBPImage(val rows: Int, val cols: Int) {
  val data = Array.fill(rows * cols)(0.0)
  val pixels = rows * cols

  def copy() : LBPImage = {
    val img = new LBPImage(rows, cols)
    Array.copy(data, 0, img.data, 0, rows*cols)
    img
  }

  def vertid(i: Int, j: Int) = {
    assert(i < rows)
    assert(j < cols)

    i * cols + j
  }

  def paintSunset(numRings: Int) = {
    val centerR = rows / 2.0
    val centerC = cols / 2.0
    val maxRadius = Math.min(rows, cols) / 2.0

    for(r <- 0 until rows) {
      for(c <- 0 until cols) {
        val distance = Math.sqrt((r-centerR)*(r-centerR) + (c-centerC)*(c-centerC))

        // If on top of image
        if(r < rows / 2) {
          // Compute ring of sunset
          val ring = Math.floor(Math.min(1.0, distance/maxRadius) * (numRings - 1))

          data(vertid(r,c)) = ring
        }
        else {
          data(vertid(r,c)) = 0
        }
      }
    }
  }

  // Corrupt the image with Gaussian noise
  def corrupt(sigma: Double) = {
    for(i <- 0 until rows * cols) {
      data(i) += Random.nextGaussian * sigma
    }
  }

  def save(filename: String) = {
    var out_file = new java.io.FileOutputStream(filename) 
    var out_stream = new java.io.PrintStream(out_file)

    out_stream.println("P2")
    out_stream.println(cols + " " + rows)
    out_stream.println(255)

    val min = data.reduceLeft((a, b) => Math.min(a, b))
    val max = data.reduceLeft((a, b) => Math.max(a, b))

    for(i <- 0 until rows) {
       out_stream.println((0 until cols).map((j : Int) => {if(min != max) (255.0 * (data(vertid(i, j)) - min) / (max - min)).toInt else 0}
        ).mkString("\t"))
    }

    out_stream.close
  }
}