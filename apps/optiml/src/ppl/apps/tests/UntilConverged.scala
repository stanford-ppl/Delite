package ppl.apps.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 01/19/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object UntilConvergedRunner extends OptiMLApplicationRunner with UntilConverged

trait UntilConverged extends OptiMLApplication {
  def print_usage = {
    println("Usage: UntilConverged <rows> <cols> <print interval> <limit>")
    println("Example: UntilConverged 100 100 2500000 1000.0")
    exit(-1)
  }

  def main() = {
    if (args.length < 4) print_usage
  
    // rows and cols arguments
    val rows = Integer.parseInt(args(0))
    val cols = Integer.parseInt(args(1))
    val interval = Integer.parseInt(args(2))
    val limit = Double.parseDouble(args(3))
    val numRings = unit(5)
  
    // Construct graph
    val g = constructGraph(rows, cols, numRings)

    var count = 1
    
    g.freeze()

    tic()
    
    untilconverged(g) {
      v =>
        val vdata = v.data.AsInstanceOf[DenoiseVertexData]
        
        val belief = vdata.belief + Vector.ones(numRings)
        
        vdata.setBelief(belief)
        
        if(count % interval == 0) {
          println(count)
        }
        
        if(belief(0) < limit) {
          v.addTask(v)
        }
        
        count += 1
    }
         
    toc()

    // Predict the image!
    println("Update functions ran: " + count)
  }

  def constructGraph(rows: Rep[Int], cols: Rep[Int], numRings: Rep[Int]): Rep[Graph[DenoiseVertexData,DenoiseEdgeData]] = {
    val g = Graph[DenoiseVertexData,DenoiseEdgeData]()

    // Set vertex potential based on image
    var i = 0
    var j = 0
    
    while (i < rows) {
      j = 0
      while (j < cols) {
        val data = DenoiseVertexData(1, Vector.zeros(numRings) map {_ + 1.0 }, Vector.zeros(numRings))
        val vertex = Vertex(g, data)

        g.addVertex(vertex)
        
        j += 1
      }
      i += 1
    }

    g
  }
}
