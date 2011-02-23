package ppl.apps.ml.lbpdenoise

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.datastruct.scala._
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication, OptiMLExp}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LBPDenoiseRunner extends OptiMLApplicationRunner with LBPDenoise

trait LBPDenoise extends OptiMLApplication {
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
    MLOutputWriter.writeImgPgm(img, "check.pgm")

    // Clean up the image and save it
    
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

    tic
    untilconverged(g) {
      v =>
        val vdata = v.data.asInstanceOfL[DenoiseVertexData]
        vdata.belief.copyFrom(0, vdata.potential)  //TODO TR: non-mutable write

        // Multiply belief by messages
        for (e <- v.edges) {  //TODO TR: non-mutable write
          val in = e.asInstanceOfL[MessageEdge].in(v).asInstanceOfL[DenoiseEdgeData]
          unaryFactorTimesM(vdata.belief, in.message)  //TODO TR: non-mutable write
        }

        // Normalize the belief
        val belief = unaryFactorNormalizeM(vdata.belief) //TODO TR: non-mutable write
        
       /* if(count % 100000 == 0) {
          print("norm")
          vdata.belief.pprint
        } */
        
        //println("mult")
        //vdata.belief.pprint

        // Send outbound messages
        for (e <- v.edges) { //TODO TR: non-mutable write (within)
          val in = e.asInstanceOfL[MessageEdge].in(v).asInstanceOfL[DenoiseEdgeData]
          val out = e.asInstanceOfL[MessageEdge].out(v).asInstanceOfL[DenoiseEdgeData]
          
          // Compute the cavity
          val cavity = unaryFactorNormalizeM(unaryFactorDivideM(vdata.belief.cloneL, in.message)) //TODO TR: non-mutable write (use mclone)

          // Convolve the cavity with the edge factor
          val msg = unaryFactorNormalizeM(unaryFactorConvolve(edgePotential, cavity))  //TODO TR: non-mutable write

          // Damp the message (MUTATE IN PLACE)
          /* unaryFactorDampM(msg, out.message, damping)
          // Compute message residual
          val residual = unaryFactorResidual(msg, out.message)
          
          // Set the message
         out.message.copyFrom(0, msg) */
         
        /* if(count % 100000 == 0) {
            print("damping")
            msg.pprint
          } */
         
          val dampMsg = unaryFactorDampM(msg, out.message, damping) //TODO TR: non-mutable write
          
         /*  if(count % 100000 == 0) {
            out.message.pprint
            dampMsg.pprint
           } */
          
          // Compute message residual
          val residual = unaryFactorResidual(dampMsg, out.message)
          
          // Set the message
          out.setMessage(dampMsg) //TODO TR: non-mutable write
          
           if(count % 100000 == 0) {
           println(count)
           println(residual)
           }
         
          // Enqueue update function on target vertex if residual is greater than bound
          if (residual > bound) {
            v.addTask(e.asInstanceOfL[MessageEdge].target(v)) //TODO TR: non-mutable write
          }
        }
      count += 1
    }
        
    toc

    // Predict the image!
    g.vertices foreach { v =>
      imgUpdate(cleanImg, v.data.asInstanceOfL[DenoiseVertexData].id, unaryFactorMaxAsg(v.data.asInstanceOfL[DenoiseVertexData].belief))   //TODO TR: non-mutable write (use mclone)
    }
    
    MLOutputWriter.writeImgPgm(cleanImg, "pred.pgm")
    println("Update functions ran: " + count)
  }

  def constructGraph(img: Rep[Matrix[Double]], numRings: Rep[Int], sigma: Rep[Double]): Rep[Graph[MessageVertex, MessageEdge]] = {
    val g = Graph[MessageVertex, MessageEdge]()

    // Same belief for everyone
    val belief = unaryFactorUniform(numRings)

    val sigmaSq = sigma * sigma

    val vertices = Matrix[MessageVertex](img.numRows, img.numCols)

    // Set vertex potential based on image
    var i = 0
    var j = 0
    while (i < img.numRows) {
      j = 0
      while (j < img.numCols) {
        val pixelId = imgPixelId(img, i, j)
        val potential = Vector.mzeros(numRings)

        val obs = img(i, j)

        var pred = 0
        while (pred < numRings) {
          potential(pred) = 0.0 - ((obs - pred) * (obs - pred) / (2.0 * sigmaSq))
          pred += 1
        }

        unaryFactorNormalizeM(potential)

        val data = DenoiseVertexData(pixelId, belief.cloneL, potential)
        val vertex = MessageVertex(g, data)

        vertices(i)(j) = vertex //TODO TR: non-mutable write (use matrix update?)
        g.addVertex(vertex)
        j += 1
      }
      i += 1
    }

    val edgeData = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))

    // Add bidirectional edges between neighboring pixels
    i = 0
    while (i < img.numRows) {
      j = 0
      while (j < img.numCols) {
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
        j += 1
      }
      i += 1
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

    var r = 0
    var c = 0
    while (r < img.numRows) {
      c = 0
      while (c < img.numCols) {
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
        c += 1
      }
      r += 1
    }
  }

  // Corrupt the image with Gaussian noise
  def imgCorrupt(img: Rep[Matrix[Double]], sigma: Rep[Double]) = {
    img mmap { _ + randomGaussian*sigma }
  }

  def imgSave(img: Rep[Matrix[Double]], filename: Rep[String]) = {
      MLOutputWriter.writeImgPgm(img, filename)
  }

  def binaryFactorSetAgreement(bf: Rep[Matrix[Double]], lambda: Rep[Double]) = {
    var i = 0
    var j = 0
    while (i < bf.numRows) {
      j = 0
      while (j < bf.numCols) {
        if (i != j)
          bf(i, j) = 0.0 - lambda
        else
          bf(i, j) = 0
        j += 1
      }
      i += 1
    }
  }

  def binaryFactorSetLaplace(bf: Rep[Matrix[Double]], lambda: Rep[Double]) = {
    var i = 0
    var j = 0
    while (i < bf.numRows) {
      j = 0
      while (j < bf.numCols) {
        bf(i, j) = 0.0 - lambda * Math.abs(i - j);
        j += 1
      }
      i += 1
    }
    // TODO: how can this mutable matrix constructor operation be expressed using parallel optiml constructs?
    /*
    // def update(IndexVector2, f: (i,j) => val): Unit
    bf(0::bf.numRows, 0::bf.numCols) = { i, j =>
      0.0 - lambda * Math.abs(i - j)
    }
    */
  }

  def unaryFactorUniform(arity: Rep[Int]) = {
    val factor = Vector.mzeros(arity)
    unaryFactorNormalizeM(factor)
    factor
  }

  def unaryFactorNormalizeM(uf: Rep[Vector[Double]]): Rep[Vector[Double]] = {
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
  def unaryFactorDivideM(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) = {
    a -= b
  }

  def unaryFactorConvolve(bf: Rep[Matrix[Double]], other: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    bf.mapRows{ (row) =>
      val csum = (row + other).exp.sum
      if(csum == 0) {
        Math.log(Double.MinValue)
      }
      else {
        Math.log(csum)
      }
    }
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
 /* def unaryFactorDampM(a: Rep[Vector[Double]], b: Rep[Vector[Double]], damping: Rep[Double]) = {
    if (damping != 0) {
      for(i <- 0 until a.length) {
        a(i) = Math.log(Math.exp(b(i)) * damping + Math.exp(a(i)).exp * (1.0 - damping))
      }
    }
    
    a
  } */
  
  /* This = other * damping + this * (1-damping) */
  def unaryFactorDampM(a: Rep[Vector[Double]], b: Rep[Vector[Double]], damping: Rep[Double]) = {
    if (damping != 0) {
      a.mzip(b){(x:Rep[Double],y:Rep[Double]) => Math.log(Math.exp(x)*(1.0-damping)+Math.exp(y)*damping)}
    }
    
    a
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
  
  def unaryFactorUniformI(arity: Rep[Int]) = {
    unaryFactorNormalizeI(Vector.zeros(arity))
  }
  
  def unaryFactorNormalizeI(uf: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    var sum = unit(0.0)
    var i = unit(0)
    while(i < uf.length) {
      sum += Math.exp(uf(i))
      i += 1
    }
    
    val logZ = Math.log(sum)
    
    i = unit(0)
    while(i < uf.length) {
      uf(i) = uf(i) - logZ
      i += 1
    }
    
    uf
  }

  def unaryFactorTimesI(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) = {
    var i = unit(0)
    while(i < a.length) {
      a(i) = a(i) + b(i)
      i += 1
    }
    
    a
  }

  // Add other factor elementwise
  def unaryFactorPlusI(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) = {
    var i = unit(0)
    while(i < a.length) {
      a(i) = Math.log(Math.exp(a(i)) + Math.exp(b(i)))
      i += 1
    }
    
    a
  }
  
  // Divide elementwise by other factor
  def unaryFactorDivideI(a: Rep[Vector[Double]], b: Rep[Vector[Double]]) = {
    var i = unit(0)
    while(i < a.length) {
      a(i) = a(i) - b(i)
      i += 1
    }
    
    a
  }

  def unaryFactorConvolveI(bf: Rep[Matrix[Double]], other: Rep[Vector[Double]]): Rep[Vector[Double]] = {
    val res = Vector.zeros(other.length)
    
    var i = unit(0)
    while(i < other.length) {
      var sum = unit(0.0)
      var j = unit(0)
      while (j < other.length) {
        sum += Math.exp(bf(i, j) + other(j))
        j += 1
      }

      // Guard against zeros
      if (sum == 0.0)
        sum = Double.MinValue

      res(i) = Math.log(sum)
      i += 1
    }
    
    res
  }
  
  /* This = other * damping + this * (1-damping) */
  def unaryFactorDampI(a: Rep[Vector[Double]], b: Rep[Vector[Double]], damping: Rep[Double]) = {
    var i = unit(0)
    while(i < a.length) {
      a(i) = Math.log(Math.exp(a(i))*(1.0-damping)+Math.exp(b(i))*damping)
      i += 1
    }
    
    a
  }
}