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

object VerticesForeachRunner extends OptiMLApplicationRunner with VerticesForeach

trait VerticesForeach extends OptiMLApplication {
  def print_usage = {
    println("Usage: VerticesForeach <rows> <cols> <print interval> <times>")
    println("Example: VerticesForeach 100 100 2500000 1000")
    exit(-1)
  }

  def main() = {
    if (args.length < 4) print_usage
  
    // rows and cols arguments
    val rows = Integer.parseInt(args(0))
    val cols = Integer.parseInt(args(1))
    val interval = Integer.parseInt(args(2))
    val times = Integer.parseInt(args(3))
  
    // Construct graph
    val g = constructGraph(rows, cols, 5)
    g.freeze

    tic()

    var i = 0
    var count = 0
    while(i < times) {
      for(v <- g.vertices) {
        count += 1
        
        if(count % interval == 0) {
          println(count)
        }
      }
      i += 1
    }
         
    toc()
  }

  def constructGraph(rows: Rep[Int], cols: Rep[Int], numRings: Rep[Int]): Rep[Graph[DenoiseVertexData,DenoiseEdgeData]] = {
    val g = Graph[DenoiseVertexData,DenoiseEdgeData]()

    // Set vertex potential based on image
    var i = 0
    var j = 0
    
    while (i < rows) {
      j = 0
      while (j < cols) {
        val data = DenoiseVertexData(1, Vector.zeros(numRings) map {_ + 1}, Vector.zeros(numRings))
        val vertex = Vertex(g, data)

        g.addVertex(vertex)
        
        j += 1
      }
      i += 1
    }

    g
  }
}

