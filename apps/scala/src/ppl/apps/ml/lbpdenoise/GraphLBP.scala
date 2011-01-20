package ppl.apps.ml.lbp

import ppl.delite.dsl.optiml.MessageGraph
import ppl.delite.dsl.optiml.Graph.{Consistency}
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.metrics.PerformanceTimer

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/27/2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class EdgeData(var message: UnaryFactor, var old_message: UnaryFactor) {
  def copy(): EdgeData = {
    new EdgeData(message.copy(), old_message.copy())
  }
}
class VertexData(var potential: UnaryFactor, var belief: UnaryFactor)

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

  val edgePotential = new BinaryFactor(0, colors, 0, colors)

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
      edgePotential.setLaplace(lambda)
    }
    else if (smoothing == "square") {
      edgePotential.setAgreement(lambda)
    }

    println(edgePotential)
    implicit val pFact = new MessageGraph.ProxyFactory[VertexData, EdgeData]

   //var count = 1

    g.untilConvergedData2(Consistency.Edge) {
      v =>
      // Flip messages on in edges
        for (e <- v.edges) {
          e.in(v).old_message = e.in(v).message
        }

        v.data.belief = v.data.potential.copy()

        // Multiply belief by messages
        for (e <- v.edges) {
          UnaryFactor.times(v.data.belief, e.in(v).old_message)
        }

        // Normalize the belief
        UnaryFactor.normalize(v.data.belief)

        // Send outbound messages
        for (e <- v.edges) {
          // Compute the cavity
          val cavity = v.data.belief.copy()
          UnaryFactor.divide(cavity, e.in(v).old_message)
          UnaryFactor.normalize(cavity)

          // Convolve the cavity with the edge factor
          val outEdge = e.out(v)
          val outMsg = new UnaryFactor(outEdge.message.v, outEdge.message.arity)
          UnaryFactor.convolve(outMsg, edgePotential, cavity)
          UnaryFactor.normalize(outMsg)

          // Damp the message
          UnaryFactor.damp(outMsg, outEdge.message, damping)

          outEdge.message = outMsg

          // Compute message residual
          val residual = UnaryFactor.residual(outMsg, outEdge.old_message)

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

    // Predict the image! Well as of now we don't even get to this point, so fuck
    if (pred_type == "map") {
      for (v <- g.vertexSet) {
        img.data(v.belief.v) = UnaryFactor.max_asg(v.belief);
      }
    }
    else if (pred_type == "exp") {
      for (v <- g.vertexSet) {
        img.data(v.belief.v) = UnaryFactor.max_asg(v.belief);
      }
    }

    img
  }

  def constructGraph(img: LBPImage, numRings: Int, sigma: Double): MessageGraph[VertexData, EdgeData] = {
    val g = new MessageGraph[VertexData, EdgeData]

    // Same belief for everyone
    val belief = new UnaryFactor(0, numRings)
    belief.uniform()
    belief.normalize()

    val sigmaSq = sigma * sigma

    val vertices = Array.ofDim[VertexData](img.rows, img.cols)

    // Set vertex potential based on image
    for (i <- 0 until img.rows) {
      for (j <- 0 until img.cols) {
        val pixelId = img.vertid(i, j)
        val potential = new UnaryFactor(pixelId, numRings)

        val obs = img.data(pixelId)

        for (pred <- 0 until numRings) {
          potential.data(pred) = -(obs - pred) * (obs - pred) / (2.0 * sigmaSq)
        }

        potential.normalize()

        val vertex = new VertexData(potential, belief.copy(pixelId))

        vertices(i)(j) = vertex
        g.addVertex(vertex)
      }
    }

    val message = new UnaryFactor(0, numRings)
    message.uniform()
    message.normalize()

    val oldMessage = message.copy()

    val message2 = message.copy()
    val oldMessage2 = message.copy()

    val templateEdge = new EdgeData(message, oldMessage)
    val baseEdge = new EdgeData(message2, oldMessage2)

    // Add bidirectional edges between neighboring pixels
    for (i <- 0 until img.rows - 1) {
      for (j <- 0 until img.cols - 1) {
        message.v = img.vertid(i, j + 1)
        oldMessage.v = img.vertid(i, j + 1)

        message2.v = img.vertid(i, j)
        oldMessage2.v = img.vertid(i, j)

        val edgeRight = templateEdge.copy()

        g.addEdge(edgeRight, baseEdge.copy(), vertices(i)(j), vertices(i)(j + 1))

        message.v = img.vertid(i + 1, j)
        oldMessage.v = img.vertid(i + 1, j)

        val edgeDown = templateEdge.copy()

        g.addEdge(edgeDown, baseEdge.copy(), vertices(i)(j), vertices(i + 1)(j))
      }
    }

    g
  }
}