package ppl.apps.ml.lbpdenoise

import ppl.dsl.optiml.OptiMLApplicationRunner

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LBPDenoiseRawRunner extends OptiMLApplicationRunner with LBPDenoiseRaw

trait LBPDenoiseRaw extends LBPDenoise {
  override def print_usage = {
    println("Usage: LBPDenoiseRaw <raw file>")
    println("Example: LBPDenoiseRaw noisy200x200.raw")
    exit(-1)
  }
  
  override def loadImage(args: Rep[Array[String]], colors: Rep[Int], sigma: Rep[Int]) = {
    val img = Image(readMatrix(args(0)))
    MLOutputWriter.writeImgPgm(img, "check.pgm")
    img
  }
}
