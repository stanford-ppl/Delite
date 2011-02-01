package ppl.apps.ml.lbpdenoise

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.OptiMLExp
import ppl.dsl.optiml.datastruct.scala._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LBPDenoise extends DeliteApplication with OptiMLExp {
  def print_usage = {
    println("Usage: GraphLBP <rows> <cols>")
    println("Example: GraphLBP 100 100")
    exit(-1)
  }

  def main() = {
    if (args.length < 1) print_usage
  
    val colors = unit(5)
    val damping = unit(0.1)
    val bound = unit(1E-15)
    var rows = unit(100)
    var cols = unit(100)
    val sigma = unit(2)
    val lambda = unit(10)
    var smoothing = unit("laplace")
    val pred_type = unit("map")

    val edgePotential = Matrix[Double](colors, colors)
  
    // rows and cols arguments
    /*rows = Integer.parseInt(args(0))
    cols = Integer.parseInt(args(1))

    // Generate image
    val img = Matrix[Double](rows, cols)
    imgPaintSunset(img, colors)
    MLOutputWriter.writeImgPgm(img, "src.pgm")
    imgCorrupt(img, sigma)
    MLOutputWriter.writeImgPgm(img, "noise.pgm") */
    
    // Load in a raw image that we generated from GraphLab

    // Make sure we read in the raw file correctly
    val img = MLInputReader.read(args(0))
    MLOutputWriter.writeImgPgm(img, "checkImg.pgm")

    //val num = unit(1)
    //for (i <- 0 until num) {
      // Clean up the image and save it
      //PerformanceTimer.start("LBP")
      
      // Make a copy of the image
      val cleanImg = img.cloneL

      // Construct graph from image
      val g = constructGraph(cleanImg, colors, sigma)

      if (smoothing == "laplace") {
        binaryFactorSetLaplace(edgePotential, lambda)
      }
      else if (smoothing == "square") {
        binaryFactorSetAgreement(edgePotential, lambda)
      }
      
      edgePotential.pprint

      var count = unit(1)
      
      g.freeze()

      untilconverged(g) {
        v =>
          val vdata = v.data.asInstanceOfL[DenoiseVertexData]
          vdata.belief.copyFrom(0, vdata.potential)

          // Multiply belief by messages
          for (e <- v.edges) {
            val in = e.asInstanceOfL[MessageEdge].in(v).asInstanceOfL[DenoiseEdgeData]
            unaryFactorTimesM(vdata.belief, in.message)
          }

          // Normalize the belief
          vdata.setBelief(unaryFactorNormalize(vdata.belief))
          // THIS FAILS HORRIBLY
          // unaryFactorNormalizeM(vdata.belief)

	//println("mult")
	//vdata.belief.pprint

          // Send outbound messages
          for (e <- v.edges) {
            val in = e.asInstanceOfL[MessageEdge].in(v).asInstanceOfL[DenoiseEdgeData]
            val out = e.asInstanceOfL[MessageEdge].out(v).asInstanceOfL[DenoiseEdgeData]
            
            // Compute the cavity
            val cavity = vdata.belief.cloneL
            unaryFactorDivideM(cavity, in.message)
            val ncavity = unaryFactorNormalize(cavity)

            // Convolve the cavity with the edge factor
            val msg = unaryFactorNormalize(unaryFactorConvolve(edgePotential, ncavity))

            // Damp the message (MUTATE IN PLACE)
            /* unaryFactorDampM(msg, out.message, damping)
            // Compute message residual
            val residual = unaryFactorResidual(msg, out.message)
            
            // Set the message
           out.message.copyFrom(0, msg) */
           
            val dampMsg = unaryFactorDamp(msg, out.message, damping)
            // Compute message residual
            val residual = unaryFactorResidual(dampMsg, out.message)
            
            // Set the message
            out.setMessage(msg)

            /*if(count % 100000 == 0) {
            print("damping")
            msg.pprint
             out.message.pprint
             dampMsg.pprint
             }*/
            
             if(count % 100000 == 0) {
             println(count)
             println(residual)
             }
           
            // Enqueue update function on target vertex if residual is greater than bound
            if (residual > bound) {
              v.addTask(e.asInstanceOfL[MessageEdge].target(v))
            }
          }
        count += 1
      }
      
      println("Update functions ran: " + count)

      // Predict the image!
      g.vertices foreach { v =>
        imgUpdate(cleanImg, v.data.asInstanceOfL[DenoiseVertexData].id, unaryFactorMaxAsg(v.data.asInstanceOfL[DenoiseVertexData].belief))
      }

      //PerformanceTimer.stop("LBP")
      //PerformanceTimer.print("LBP")
      MLOutputWriter.writeImgPgm(cleanImg, "pred.pgm")
  //  }
    /* PerformanceTimer2.summarize("BM")
   PerformanceTimer2.summarize("CD")
   PerformanceTimer2.summarize("OC")
   PerformanceTimer2.summarize("D")
   PerformanceTimer2.summarize("R") */
  }

  def constructGraph(img: Rep[Matrix[Double]], numRings: Rep[Int], sigma: Rep[Double]): Rep[Graph[MessageVertex, MessageEdge]] = {
    val g = Graph[MessageVertex, MessageEdge]()

    // Same belief for everyone
    val belief = unaryFactorUniform(numRings)

    val sigmaSq = sigma * sigma

    val vertices = Matrix[MessageVertex](img.numRows, img.numCols)

    // Set vertex potential based on image
    for (i <- 0 until img.numRows) {
      for (j <- 0 until img.numCols) {
        val pixelId = imgPixelId(img, i, j)
        var potential = Vector.zeros(numRings)

        val obs = img(i, j)

        for (pred <- 0 until numRings) {
          potential(pred) = 0.0 - ((obs - pred) * (obs - pred) / (2.0 * sigmaSq))
        }

        potential = unaryFactorNormalize(potential)

        val data = DenoiseVertexData(pixelId, belief.cloneL, potential)
        val vertex = MessageVertex(g, data)

        vertices(i)(j) = vertex
        g.addVertex(vertex)
      }
    }

    val edgeData = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))

    // Add bidirectional edges between neighboring pixels
    for (i <- 0 until img.numRows) {
      for (j <- 0 until img.numCols) {
        if(j < img.numCols - 1) {
          val edgeData = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))
          val edgeData2 = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))
        
          val edgeRight = MessageEdge(g, edgeData, edgeData2, vertices(i)(j), vertices(i)(j+1))
          g.addEdge(edgeRight, vertices(i)(j), vertices(i)(j+1))
        }

        if(i < img.numRows - 1) {
          val edgeData = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))
          val edgeData2 = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))
          val edgeDown = MessageEdge(g, edgeData, edgeData2, vertices(i)(j), vertices(i+1)(j))
          g.addEdge(edgeDown, vertices(i)(j), vertices(i+1)(j))
        }
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

  def imgUpdate(img: Rep[Matrix[Double]], id: Rep[Int], pixel: Rep[Double]) = {
    val row = id / img.numCols
    val col = id % img.numCols

    img(row, col) = pixel
  }

  def imgPaintSunset(img: Rep[Matrix[Double]], numRings: Rep[Int]) = {
    val centerR = img.numRows.asInstanceOfL[Double] / 2.0
    val centerC = img.numCols.asInstanceOfL[Double] / 2.0
    val maxRadius = Math.min(img.numRows, img.numCols).asInstanceOfL[Double] / 2.0

    for (r <- 0 until img.numRows) {
      for (c <- 0 until img.numCols) {
        val distance = Math.sqrt((r.asInstanceOfL[Double] - centerR) * (r.asInstanceOfL[Double] - centerR) + (c.asInstanceOfL[Double] - centerC) * (c.asInstanceOfL[Double] - centerC))

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
        img(r, c) = img(r, c) + randomGaussian * sigma
      }
    }
  }

  def imgSave(img: Rep[Matrix[Double]], filename: Rep[String]) = {
      MLOutputWriter.writeImgPgm(img, filename)
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
        bf(i, j) = 0.0 - lambda * Math.abs(i - j);
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
  
  def unaryFactorNormalizeM(uf: Rep[Vector[Double]]) {
    val logZ = Math.log(uf.exp.sum)
    uf mmap {_ - logZ}
  }

  // Multiply elementwise by other factor
  def unaryFactorTimes(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    a + b
  }
  
  def unaryFactorTimesM(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) {
    a += b
  }

  // Add other factor elementwise
  def unaryFactorPlus(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) = {
    (a.exp + b.exp) map {Math.log(_)}
  }

  // Divide elementwise by other factor
  def unaryFactorDivide(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    a - b
  }
  
  // Divide elementwise by other factor
  def unaryFactorDivideM(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) {
    a -= b
  }

  def unaryFactorConvolve(bf: Rep[Matrix[Double]], other: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    val indices = Vector.range(0, bf.numCols)
    val colSums = indices map {(i: Rep[Int]) => (bf.getCol(i) + other).exp.sum} map {(sum: Rep[Double]) =>if (sum == 0) Double.MinValue else sum}
    colSums map {Math.log(_)}
  }

  /**This = other * damping + this * (1-damping) */
  def unaryFactorDamp(a: Rep[Vector[Double]], b: Rep[Vector[Double]], damping: Rep[Double]): Rep[Vector[Double]] = {
    if (damping != 0) {
      (b.exp * damping + a.exp * (1.0 - damping)) map {Math.log(_)}
    }
    else {
      a
    }
  }
  
  /**This = other * damping + this * (1-damping) */
  def unaryFactorDampM(a: Rep[Vector[Double]], b: Rep[Vector[Double]], damping: Rep[Double]) {
    if (damping != 0) {
      var i = unit(0)
      while(i < a.length) {
        a(i) = Math.log(Math.exp(b(i)) * damping + Math.exp(a(i)).exp * (1.0 - damping))
        i += 1
      }
    }
  }

  /**Compute the residual between two unary factors */
  def unaryFactorResidual(a: Rep[Vector[Double]], b: Rep[Vector[Double]]): Rep[Double] = {
    (a.exp - b.exp).abs.sum / a.length
  }

  // Max assignment
  def unaryFactorMaxAsg(uf: Rep[Vector[Double]]): Rep[Int] = {
    var max_asg = unit(0)
    var max_value = uf(0)

    var asg = unit(0)
    while (asg < uf.length) {
      if (uf(asg) > max_value) {
        max_value = uf(asg)
        max_asg = asg
      }
      asg += 1
    }

    max_asg
  }

  def unaryFactorExpectation(uf: Rep[Vector[Double]]): Rep[Double] = {
    val indices = Vector.range(0, uf.length)

    (uf.exp * indices).sum / uf.exp.sum
  }
}