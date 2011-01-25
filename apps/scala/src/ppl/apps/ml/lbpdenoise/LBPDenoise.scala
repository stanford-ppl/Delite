package ppl.apps.ml.lbpdenoise

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.OptiMLExp
import ppl.dsl.optiml.datastruct.scala._
import scala.util.Random

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LBPDenoise extends DeliteApplication with OptiMLExp {
  var colors = 5
  var damping = 0.1
  var bound = 1E-15
  var rows = 100
  var cols = 100
  var sigma = 2
  var lambda = 10
  var smoothing = "laplace"
  var pred_type = "map";

  var count = 1

  val edgePotential = Matrix[Double](colors, colors)

  /* Residual printing
  var out_file = new java.io.FileOutputStream("residuals_scala.txt")
  var out_stream = new java.io.PrintStream(out_file)
*/

  def print_usage = {
    println("Usage: GraphLBP <rows> <cols>")
    println("Example: GraphLBP 100 100")
    exit(-1)
  }

  def run(args: Array[String]) = {
    // rows and cols arguments
    try {
      if (args.length > 0) rows = java.lang.Integer.parseInt(args(0))
      if (args.length > 1) cols = java.lang.Integer.parseInt(args(1))
    }
    catch {
      case _: Exception => print_usage
    }

    // Generate image
    val img = Matrix[Double](rows, cols)
    imgPaintSunset(img, colors)
    MLOutputWriter.writeImgPgm(img, "src.pgm")
    imgCorrupt(img, sigma)
    MLOutputWriter.writeImgPgm(img, "noise.pgm")

    // Load in a raw image that we generated from GraphLab
    //val img = LBPImage.load(raw)

    // Make sure we read in the raw file correctly
    // img.save("check_img.pgm")

    val num = 5
    for (i <- 0 until num) {
      // Clean up the image and save it
      //PerformanceTimer.start("LBP")
      val cleanImg = denoise(img)
      //PerformanceTimer.stop("LBP")
      //PerformanceTimer.print("LBP")
      MLOutputWriter.writeImgPgm(cleanImg, "pred.pgm")
    }

    /* PerformanceTimer2.summarize("BM")
   PerformanceTimer2.summarize("CD")
   PerformanceTimer2.summarize("OC")
   PerformanceTimer2.summarize("D")
   PerformanceTimer2.summarize("R") */

    /*out_stream.println(count)
    out_stream.close*/
  }

  def denoise(imgIn: Rep[Matrix[Double]]): Rep[Matrix[Double]] = {
    // Make a copy of the image
    val img = imgIn.cloneL()

    // Construct graph from image
    val g = constructGraph(img, colors, sigma)

    if (smoothing == "laplace") {
      binaryFactorSetLaplace(edgePotential, lambda)
    }
    else if (smoothing == "square") {
      binaryFactorSetAgreement(edgePotential, lambda)
    }

    //var count = 1

    def toFreeze(v: MessageVertex) {
      v.neighbors()
    }

    untilconverged(g) {
      v =>
        v.data.belief = v.data.potential.clone()

        // Multiply belief by messages
        for (e <- v.in_edges) {
          v.data.belief = unaryFactorTimes(v.data.belief, e.in(v).old_message)
        }

        // Normalize the belief
        v.data.belief = unaryFactorNormalize(v.data.belief)

        // Send outbound messages
        for ((in, out) <- v.edges) {
          // Compute the cavity
          val cavity = unaryFactorNormalize(unaryFactordivide(v.data.belief.clone(), in.message))

          // Convolve the cavity with the edge factor
          var msg = unaryFactorNormalize(unaryFactorconvolve(edgePotential, cavity))

          // Damp the message
          msg = unaryFactorSamp(msg, outEdge.message, damping)

          // Compute message residual
          val residual = unaryFactorResidual(msg, out.data.message)

          // Set the message
          out.data.message = msg

          /*  if(count % 10000 == 0) {
           println(count + " " + residual)
         } */

          // Enqueue update function on target vertex if residual is greater than bound
          if (residual > bound) {
            v.addTask(e.target(v))
          }
        }

      //count += 1
    }

    // Predict the image!
    g.vertices foreach { v =>
      img.data(v.id) = unaryFactorMaxAsg(v.belief);
    }

    img
  }

  def constructGraph(img: Rep[Matrix[Double]], numRings: Rep[Int], sigma: Rep[Double]): Graph[MessageVertex, MessageEdge] = {
    val g = new UndirectedGraphImpl[MessageVertex, MessageEdge]()

    // Same belief for everyone
    val belief = unaryFactorUniform(numRings)

    val sigmaSq = sigma * sigma

    val vertices = Array.ofDim[MessageVertex](img.numRows, img.numCols)

    // Set vertex potential based on image
    for (i <- 0 until img.numRows) {
      for (j <- 0 until img.numCols) {
        val pixelId = LBPImage.pixelId(img, i, j)
        val potential = Vector.zeros(numRings)

        val obs = img.data(pixelId)

        for (pred <- 0 until numRings) {
          potential(pred) = -(obs - pred) * (obs - pred) / (2.0 * sigmaSq)
        }

        potential.normalize()

        val data = new DenoiseVertexDataImpl(pixelId, potential, belief.copy())
        val vertex = MessageVertex(g, data)

        vertices(i)(j) = vertex
        g.addVertex(vertex)
      }
    }

    val edgeData = new DenoiseEdgeDataImpl(unaryFactorUniform(numRings), unaryFactorUniform(numRings))

    // Add bidirectional edges between neighboring pixels
    for (i <- 0 until img.numRows - 1) {
      for (j <- 0 until img.numCols - 1) {
        val edgeDown = MessageEdge(g, edgeData.cloneL(), edgeData.cloneL(), vertices(i)(j), vertices(i)(j + 1))
        g.addEdge(edgeDown, vertices(i)(j), vertices(i)(j + 1))

        val edgeRight = MessageEdge(g, edgeData.cloneL(), edgeData.cloneL(), vertices(i)(j), vertices(i)(j + 1))
        g.addEdge(edgeRight, vertices(i)(j), vertices(i + 1)(j))
      }
    }

    g
  }

  def imgPixels(img: Rep[Matrix[Double]]) = {
    img.numRows * img.numCols
  }

  def imgPixelId(img: Rep[Matrix[Double]], i: Rep[Int], j: Rep[Int]) = {
    i * img.numCols + j
  }

  def imgPaintSunset(img: Rep[Matrix[Double]], numRings: Int) = {
    val centerR = img.numRows.asInstanceOfL[Double] / 2.0
    val centerC = img.numCols.asInstanceOfL[Double] / 2.0
    val maxRadius = Math.min(img.numRows, img.getCows) / 2.0

    for (r <- 0 until img.numRows) {
      for (c <- 0 until img.numCols) {
        val distance = Math.sqrt((r - centerR) * (r - centerR) + (c - centerC) * (c - centerC))

        // If on top of image
        if (r < img.numRows / 2) {
          // Compute ring of sunset
          val ring = Math.floor(Math.min(1.0, distance / maxRadius) * (numRings - 1))

          img(r, c) = ring
        }
        else {
          img(r, c) = 0
        }
      }
    }
  }

  // Corrupt the image with Gaussian noise
  def imgCorrupt(img: Rep[Matrix[Double]], sigma: Rep[Double]) = {
    for (r <- 0 until img.numRows) {
      for (c <- 0 until img.numCols) {
        img(r, c) = img(r, c) + Random.nextGaussian * sigma
      }
    }
  }

  def imgSave(img: Rep[Matrix[Double]], filename: Rep[String]) = {

  }

  def binaryFactorSetAgreement(bf: Rep[Matrix[Double]], lambda: Rep[Double]) = {
    for (i <- 0 until bf.numRows) {
      for (j <- 0 until bf.numCols) {
        if (i != j)
          bf(i, j) = 0.0 - lambda
        else
          bf(i, j) = 0
      }
    }
  }

  def binaryFactorSetLaplace(bf: Rep[Matrix[Double]], lambda: Rep[Double]) = {
    for (i <- 0 until bf.numRows) {
      for (j <- 0 until bf.numCols) {
        bf(i, j) = 0.0 - Math.abs(i - j) * lambda;
      }
    }
  }

  def unaryFactorUniform(arity: Rep[Int]) = {
    unaryFactorNormalize(Vector.zeros(arity))
  }

  def unaryFactorNormalize(uf: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    val logZ = Math.log(uf.exp.sum)
    uf map {_ - logZ}
  }

  // Multiply elementwise by other factor
  def unaryFactorTimes(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    a + b
  }

  // Add other factor elementwise
  def unaryFactorPlus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) = {
    (a.exp + b.exp) map {Math.log(_)}
  }

  // Divide elementwise by other factor
  def unaryFactorDivide(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    a - b
  }

  def unaryFactorConvolve(bf: Matrix[Double], other: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    val indices = Vector.range(0, bf.getCols)
    val colSums = indices map {(bf.getCol(_) + other).sum} map {if (_ == 0) Double.MinValue else _}
    colSums map {Math.log(_)}
  }

  /**This = other * damping + this * (1-damping) */
  def unaryFactorDamp(a: Rep[Vector[Double]], b: Rep[Vector[Double]], damping: Rep[Double]) = {
    if (damping != 0) {
      (damping * b.exp + (1.0 - damping) * a.exp) map {Math.log(_)}
    }
    else {
      a
    }
  }

  /**Compute the residual between two unary factors */
  def unaryFactorResidual(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Double = {
    (a.exp - b.exp).abs.sum
  }

  // Max assignment
  def unaryFactorMaxAsg(uf: Rep[Vector[Double]]): Int = {
    var max_asg = 0;
    var max_value = uf(0);

    var asg = 0
    while (asg < uf.length) {
      if (uf(asg) > max_value) {
        max_value = a(asg)
        max_asg = asg
      }
      asg += 1
    }

    max_asg
  }

  def unaryFactorExpectation(uf: Rep[Vector[Double]]): Double = {
    val indices = Vector.range(0, uf.length)

    (indices * uf.exp).sum / uf.exp.sum
  }
}
