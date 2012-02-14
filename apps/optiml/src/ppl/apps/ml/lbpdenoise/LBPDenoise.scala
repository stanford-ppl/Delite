package ppl.apps.ml.lbpdenoise

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

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
    println("Usage: LBPDenoise <rows> <cols>")
    println("Example: LBPDenoise 100 100")
    exit(-1)
  }
    
  def loadImage(args: Rep[Array[String]], colors: Rep[Int], sigma: Rep[Int]) = {
    // rows and cols arguments
    val rows = Integer.parseInt(args(0))
    val cols = Integer.parseInt(args(1))

    // Generate image
    val img = Image[Double](rows, cols)
    imgPaintSunset(img, colors)
    MLOutputWriter.writeImgPgm(img, "src.pgm")
    imgCorrupt(img, sigma)
    MLOutputWriter.writeImgPgm(img, "noise.pgm")
    img
  }

  def main() = {
    if (args.length < 1) print_usage
  
    val colors = 5
    val damping = unit(0.1)
    val bound = 1E-15
    var rows = 100
    var cols = 100
    val sigma = 2
    val lambda = unit(10)
    var smoothing = "laplace"
    val pred_type = "map"

    val edgePotential = DenseMatrix[Double](colors, colors).mutable
    
    val img = loadImage(args, colors, sigma)
    rows = img.numRows
    cols = img.numCols

    // Clean up the image and save it
    
    // Make a copy of the image
    val cleanImg = img.mutable

    // Construct graph from image
    val g = constructGraph(cleanImg, colors, sigma)

    if (smoothing == "laplace") {
      binaryFactorSetLaplace(edgePotential, lambda)
    }
    else if (smoothing == "square") {
      binaryFactorSetAgreement(edgePotential, lambda)
    }
    
    edgePotential.pprint

    var count = 1
    
    g.freeze()

    tic()
    
    untilconverged(g) {
      v =>
        val vdata = v.data.AsInstanceOf[DenoiseVertexData]
        /*if(count % 100000 == 0) {
          print("ITER")
          vdata.belief.pprint
        } */
        
        val belief = vdata.potential.mutable
        
        /*if(count % 100000 == 0) {
          belief.pprint
        }*/

        // Multiply belief by messages
        for (e <- v.edges) {  //TODO TR: non-mutable write
          val in = e.AsInstanceOf[Edge[DenoiseVertexData,DenoiseEdgeData]].in(v)
          unaryFactorTimesM(belief, in.message)  //TODO TR: non-mutable write
          
         /* if(count % 100000 == 0) {
            print("mult ")
            in.message.pprint
            belief.pprint
          } */
        }
        
        // Normalize the belief
        unaryFactorNormalizeM(belief) //TODO TR: non-mutable write
        
        vdata.belief.copyFrom(0, belief)
        
      /* if(count % 100000 == 0) {
          print("norm")
          vdata.belief.pprint
        } */

        // Send outbound messages
        for (e <- v.edges) { //TODO TR: non-mutable write (within)
          val in = e.AsInstanceOf[Edge[DenoiseVertexData,DenoiseEdgeData]].in(v)
          val out = e.AsInstanceOf[Edge[DenoiseVertexData,DenoiseEdgeData]].out(v)
       
          val cavity = vdata.belief.mutable
          
         /* if(count % 100000 == 0) {
            print("CAVITY")
            cavity.pprint
          } */   
          
          unaryFactorDivideM(cavity, in.message)
          
          /*if(count % 100000 == 0) {
            print("div")
            in.message.pprint
            cavity.pprint
          } */  
          
          // Compute the cavity
          unaryFactorNormalizeM(cavity) //TODO TR: non-mutable write (use mclone)
          
          /*if(count % 100000 == 0) {
            cavity.pprint
          } */  
          
          // Convolve the cavity with the edge factor
          val dampMsg = unaryFactorConvolve(edgePotential, cavity).mutable
          
          /*if(count % 100000 == 0) {
            print("damp")
            dampMsg.pprint
          }   */
          
          val whatever = unaryFactorNormalizeM(dampMsg)
          
         /* if(count % 100000 == 0) {
            dampMsg.pprint
          }*/   

          // Damp the message (MUTATE IN PLACE)
          val whatever2 = unaryFactorDampM(dampMsg, out.message, damping)
          
          /*if(count % 100000 == 0) {
            print("out")
            out.message.pprint
            dampMsg.pprint
          } */  
          
          // Compute message residual
          val residual = unaryFactorResidual(dampMsg, out.message)
          
          /*if(count % 100000 == 0) {
            print("residual")
            print(residual)
          }   */
          
          // Set the message
          out.message.copyFrom(0, dampMsg) //TODO TR: non-mutable write
          
          /*if(count % 100000 == 0) {
            out.message.pprint
          } */
          
          /*if(count % 100000 == 0) {
           println(count)
           println(residual)
           } */
         
          // Enqueue update function on target vertex if residual is greater than bound
          if (residual > bound) {
            v.addTask(e.AsInstanceOf[Edge[DenoiseVertexData,DenoiseEdgeData]].target(v)) //TODO TR: non-mutable write
          }
        }
      count += 1 
    }
         
    toc()

    // Predict the image!
   g.vertices foreach { v =>
      imgUpdate(cleanImg, v.data.AsInstanceOf[DenoiseVertexData].id, unaryFactorMaxAsg(v.data.AsInstanceOf[DenoiseVertexData].belief))   //TODO TR: non-mutable write (use mclone)
    }
    
    MLOutputWriter.writeImgPgm(cleanImg, "pred.pgm")
    println("Update functions ran: " + count)
  }

  def constructGraph(img: Rep[DenseMatrix[Double]], numRings: Rep[Int], sigma: Rep[Double]): Rep[Graph[DenoiseVertexData,DenoiseEdgeData]] = {
    val g = Graph[DenoiseVertexData,DenoiseEdgeData]()

    val sigmaSq = sigma * sigma

    val vertices = DenseMatrix[Vertex[DenoiseVertexData,DenoiseEdgeData]](img.numRows, img.numCols)

    // Set vertex potential based on image
    var i = 0
    var j = 0
    
    while (i < img.numRows) {
      j = 0
      while (j < img.numCols) {
        val pixelId = imgPixelId(img, i, j)
        val potential = Vector.zeros(numRings).mutable

        val obs = img(i, j)

        var pred = 0
        while (pred < numRings) {
          potential(pred) = 0.0 - ((obs - pred) * (obs - pred) / (2.0 * sigmaSq))
          pred += 1
        }

        unaryFactorNormalizeM(potential)

        val data = DenoiseVertexData(pixelId, unaryFactorUniform(numRings), potential)
        val vertex = Vertex(g, data)

        vertices(i,j) = vertex //TODO TR: non-mutable write (use matrix update?)        
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
        
          val tmp = vertices(i)
          val edgeRight = Edge(g, edgeData, edgeData2, tmp(j), tmp(j+1))
          g.addEdge(edgeRight, tmp(j), tmp(j+1))
        }

        if(i < img.numRows - 1) {
          val edgeData = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))
          val edgeData2 = DenoiseEdgeData(unaryFactorUniform(numRings), unaryFactorUniform(numRings))
          val tmp1 = vertices(i)
          val tmp2 = vertices(i+1)
          val edgeDown = Edge(g, edgeData, edgeData2, tmp1(j), tmp2(j))
          g.addEdge(edgeDown, tmp1(j), tmp2(j))
        }
        j += 1
      }
      i += 1
    }

    g
  }

  def imgPixels(img: Rep[DenseMatrix[Double]]) = {
    img.numRows * img.numCols
  }

  def imgPixelId(img: Rep[DenseMatrix[Double]], i: Rep[Int], j: Rep[Int]) = {
    i * img.numCols + j
  }

  def imgUpdate(img: Rep[DenseMatrix[Double]], id: Rep[Int], pixel: Rep[Double]) = {
    val row = id / img.numCols
    val col = id % img.numCols

    img(row, col) = pixel
  }

  def imgPaintSunset(img: Rep[DenseMatrix[Double]], numRings: Rep[Int]) = {
    val centerR = img.numRows.AsInstanceOf[Double] / 2.0
    val centerC = img.numCols.AsInstanceOf[Double] / 2.0
    val maxRadius = min(img.numRows, img.numCols).AsInstanceOf[Double] / 2.0

    var r = 0
    var c = 0
    while (r < img.numRows) {
      c = 0
      while (c < img.numCols) {
        val distance = sqrt((r.AsInstanceOf[Double] - centerR) * (r.AsInstanceOf[Double] - centerR) + (c.AsInstanceOf[Double] - centerC) * (c.AsInstanceOf[Double] - centerC))

        // If on top of image
        if (r < img.numRows / 2) {
          // Compute ring of sunset
          val ring = floor(min(1.0, distance / maxRadius) * (numRings - 1))

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
  def imgCorrupt(img: Rep[DenseMatrix[Double]], sigma: Rep[Double]) = {
    img mmap { _ + randomGaussian*sigma }
  }

  def imgSave(img: Rep[Image[Double]], filename: Rep[String]) = {
    MLOutputWriter.writeImgPgm(img, filename)
  }

  def binaryFactorSetAgreement(bf: Rep[DenseMatrix[Double]], lambda: Rep[Double]) = {
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

  // TODO: passing lambda somehow causing a scalac internal error, even if lambda is not used anywhere
  //def binaryFactorSetLaplace(bf: Rep[DenseMatrix[Double]]) {
  def binaryFactorSetLaplace(bf: Rep[DenseMatrix[Double]], lambda: Rep[Double]) {
    var i = 0
    var j = 0
    while (i < bf.numRows) {
      j = 0
      while (j < bf.numCols) {
        bf(i, j) = 0.0 - lambda * abs(i - j);
        j += 1
      }
      i += 1
    }
    // TODO: how can this mutable matrix constructor operation be expressed using parallel optiml constructs?
    /*
    // def update(IndexVector2, f: (i,j) => val): Unit
    bf(0::bf.numRows, 0::bf.numCols) = { i, j =>
      0.0 - lambda * abs(i - j)
    }
    */
  }

  def unaryFactorUniform(arity: Rep[Int]) = {
    val factor = Vector.zeros(arity).mutable
    unaryFactorNormalizeM(factor)
  }

  def unaryFactorNormalize(uf: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    val logZ = log((uf map {exp(_)}).sum)
    uf map {_ - logZ}
  }
  
  def unaryFactorNormalizeM(uf: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    val logZ = log((uf map {exp(_)}).sum)
    uf mmap {_ - logZ}
  }

  // Multiply elementwise by other factor
  def unaryFactorTimes(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    a + b
  }
  
  def unaryFactorTimesM(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]]) {
    a += b
  }

  // Add other factor elementwise
  def unaryFactorPlus(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]]) = {
    (a.exp + b.exp) map {log(_)}
  }

  // Divide elementwise by other factor
  def unaryFactorDivide(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    a - b
  }
  
  // Divide elementwise by other factor
  def unaryFactorDivideM(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]]) = {
    a -= b
  }

  def unaryFactorConvolve(bf: Rep[DenseMatrix[Double]], other: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    bf.mapRowsToVector { (row) =>
      val csum = (row + other).exp.sum
      if(csum == 0) {
        log(Double.MinValue)
      }
      else {
        log(csum)
      }
    }
  }

  // This = other * damping + this * (1-damping)
  def unaryFactorDamp(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]], damping: Rep[Double]): Rep[DenseVector[Double]] = {
    if (damping != 0) {
      (b.exp * damping + a.exp * (1.0 - damping)) map {log(_)}
    }
    else {
      a
    }
  }
  
  def unaryFactorDampM(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]], damping: Rep[Double]) = {
/*    if (damping != 0) {
      a.mzip(b){(x:Rep[Double],y:Rep[Double]) => log(exp(x)*(1.0-damping)+exp(y)*damping)}
    }
    
    a */
    
    a.mzip(b){(x:Rep[Double],y:Rep[Double]) => log(exp(x)*(1.0-damping)+exp(y)*damping)}
  }
  
  // Compute the residual between two unary factors
  def unaryFactorResidual(a: Rep[DenseVector[Double]], b: Rep[DenseVector[Double]]): Rep[Double] = {
    ((a map {exp(_)}) - (b map {exp(_)})).abs.sum / a.length
  }

  // Max assignment
  def unaryFactorMaxAsg(uf: Rep[DenseVector[Double]]): Rep[Int] = {
    var max_asg = 0
    var max_value = uf(0)

    var asg = 0
    while (asg < uf.length) {
      if (uf(asg) > max_value) {
        max_value = uf(asg)
        max_asg = asg
      }
      asg += 1
    }

    max_asg
  }

  def unaryFactorExpectation(uf: Rep[DenseVector[Double]]): Rep[Double] = {
    //val indices = Vector.range(0, uf.length)
    val indices = (0::uf.length)

    (uf.exp * indices.toDouble).sum / uf.exp.sum
  }
  
  // def printBeliefs(v: Rep[DenseVector[Vertex]): Rep[Unit] = {
  //   for(i <- 0 until v.length) {
  //     val data = v(i).data.AsInstanceOf[DenoiseVertexData] 
  //     print(data.id + " [ ") //" " + System.identityHashCode(data.belief) + " [")
  //     
  //     for(j <- 0 until data.belief.length) {
  //       print(" " + data.belief.data(j))
  //     }
  //     
  //     println("]")
  //   }
  // }
  
}
