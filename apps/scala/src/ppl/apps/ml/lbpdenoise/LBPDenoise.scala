package ppl.apps.ml.lbpdenoise

import ppl.delite.framework.DeliteApplication
import ppl.delite.runtime.profiler.PerformanceTimer
import ppl.delite.runtime.Delite
import ppl.apps.ml.lbp.LBPImage
import ppl.dsl.optiml.datastruct.scala.{BidirectionalGraphImpl, MessageData, Vector}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LBPDenoise extends DeliteApplication {
  object GraphLBP extends DeliteApplication {
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

  val edgePotential = Matrix(colors, colors)

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

    Delite.init = true

    // Generate image
    val img = new LBPImage(rows, cols)
    img.paintSunset(colors)
    img.save("src.pgm")
    img.corrupt(sigma)
    img.save("noise.pgm")

    // Load in a raw image that we generated from GraphLab
    //val img = LBPImage.load(raw)

    // Make sure we read in the raw file correctly
    // img.save("check_img.pgm")

    Delite.init = false

    val num = 5
    for (i <- 0 until num) {
      // Clean up the image and save it
      PerformanceTimer.start("LBP")
      val cleanImg = denoise(img)
      PerformanceTimer.stop("LBP")
      PerformanceTimer.print("LBP")
      cleanImg.save("pred.pgm")
    }

    /* PerformanceTimer2.summarize("BM")
   PerformanceTimer2.summarize("CD")
   PerformanceTimer2.summarize("OC")
   PerformanceTimer2.summarize("D")
   PerformanceTimer2.summarize("R") */

    /*out_stream.println(count)
    out_stream.close*/
  }

  def denoise(imgIn: LBPImage): LBPImage = {
    // Make a copy of the image
    val img = imgIn.copy()

    // Construct graph from image
    val g = constructGraph(img, colors, sigma)

    if (smoothing == "laplace") {
      BinaryFactor.setLaplace(edgePotential, lambda)
    }
    else if (smoothing == "square") {
      BinaryFactor.setAgreement(edgePotential, lambda)
    }
    
    //var count = 1

    def toFreeze(v: DenoiseVertex) {
      v.neighbors()
    }

    g.untilConverged(Consistency.Edge) {
      v =>
        v.data.belief = v.data.potential.clone()

        // Multiply belief by messages
        for (e <- v.in_edges) {
          v.data.belief = UnaryFactor.times(v.data.belief, e.in(v).old_message)
        }

        // Normalize the belief
        v.data.belief =  UnaryFactor.normalize(v.data.belief)

        // Send outbound messages
        for ((in, out) <- v.edges) {
          // Compute the cavity
          val cavity = UnaryFactor.normalize(UnaryFactor.divide(v.data.belief.clone(), in.message))

          // Convolve the cavity with the edge factor
          var msg = UnaryFactor.normalize(UnaryFactor.convolve(edgePotential, cavity))

          // Damp the message
          msg = UnaryFactor.damp(msg, outEdge.message, damping)

          // Compute message residual
          val residual = UnaryFactor.residual(msg, out.data.message)

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
    if (pred_type == "map") {
      for (v <- g.vertexSet) {
        img.data(v.id) = UnaryFactor.max_asg(v.belief);
      }
    }
    else if (pred_type == "exp") {
      for (v <- g.vertexSet) {
        img.data(v.id) = UnaryFactor.max_asg(v.belief);
      }
    }

    img
  }

  def constructGraph(img: LBPImage, numRings: Int, sigma: Double): BidirectionalGraph[DenoiseVertex, DenoiseEdge] = {
    val g = new BidirectionalGraphImpl[DenoiseVertex, DenoiseEdge]()

    // Same belief for everyone
    val belief = UnaryFactor.uniform(numRings)

    val sigmaSq = sigma * sigma

    val vertices = Array.ofDim[DenoiseVertex](img.rows, img.cols)

    // Set vertex potential based on image
    for (i <- 0 until img.rows) {
      for (j <- 0 until img.cols) {
        val pixelId = img.vertid(i, j)
        val potential = Vector.zeros(numRings)

        val obs = img.data(pixelId)

        for (pred <- 0 until numRings) {
          potential.data(pred) = -(obs - pred) * (obs - pred) / (2.0 * sigmaSq)
        }

        potential.normalize()

        val vertex = new DenoiseVertex(pixelId, potential, belief.copy())

        vertices(i)(j) = vertex
        g.addVertex(vertex)
      }
    }

    val edge = new DenoiseEdge(UnaryFactor.uniform(numRings))

    // Add bidirectional edges between neighboring pixels
    for (i <- 0 until img.rows - 1) {
      for (j <- 0 until img.cols - 1) {
        g.addEdge(DenoiseEdge.copy(edge), DenoiseEdge.copy(edge), vertices(i)(j), vertices(i)(j + 1))
        g.addEdge(DenoiseEdge.copy(edge), DenoiseEdge.copy(edge), vertices(i)(j), vertices(i + 1)(j))
      }
    }

    g
  }
}
  
}

class DenoiseVertex(val id : Int, var belief : Vector[Double], var potential : Vector[Double]) extends MessageData

object DenoiseEdge {
  def copy(edge: DenoiseEdge) = {
    new DenoiseEdge(edge.message)
  }
}

class DenoiseEdge(var message : Vector[Double]) extends MessageData