package ppl.dsl.deliszt.analysis

import collection.immutable.{TreeSet => ISSet}
import collection.mutable.{HashMap, Set => MSet}
import ppl.dsl.deliszt.datastruct.scala._

class InterferenceBuilder(val colorer: Colorer, val blockSize: Int) {
  import Stencil.StencilMap

  def trivialColoring(ms: MeshSet) : Coloring = {
    var numBlocks: Int = math.ceil(ms.size / blockSize).toInt
    val edge_idx = Array.fill[Int](numBlocks+1) {0}
    // This will OVERESTIMATE the number of edges.
    val edge_vals = new Array[Int](0)
    
    val oneColorer = new OneColorer()
    val (colors, numColors) = oneColorer.color(numBlocks, edge_idx, edge_vals)
    val nodes = (ms map { mo: Int => Mesh.internal(mo) }).toArray
    
    val elements = new Array[Int](ms.size)
    for(i <- 0 until ms.size) {
      elements(i) = Mesh.internal(ms(i))
    }
    
    // Now output them?
    new Coloring(nodes, colors, numColors, blockSize, elements)
  }
  
  def buildAndColor(ms: MeshSet, stencil: StencilMap) : Coloring = {
    val accesses = new HashMap[FieldAccess,MSet[Int]]() { override def default(key: FieldAccess) = { val mset = MSet[Int](); this(key) = mset; mset } }
    val edges = new HashMap[Int, MSet[Int]]() { override def default(key: Int) = { val mset = MSet[Int](); this(key) = mset; mset } }
    var totalEdges = 0
  
   /*  var out_access = new java.io.FileOutputStream("accesses.txt") 
    var access_stream = new java.io.PrintStream(out_access)
    
    access_stream.println("ACCESS FOR MS") */
    
    // Add edge between any that conflict on writes
    for((mo, rwset) <- stencil) {
      // access_stream.println("ELEMENT " + mo)
      for(write <- rwset.write) {
        for(access <- accesses(write)) {
         // access_stream.println("ACCESS " + Mesh.internal(access))
          if(Mesh.internal(access) != Mesh.internal(mo)) {
            if(!edges(mo).contains(access)) {
              edges(mo) += access
              edges(access) += mo
              totalEdges += 2
            }
          }
        }
        
        accesses(write) += mo
      }
    }
    
    // access_stream.close
    
    // Build a dense list of indices for the sparse list of elements in the top-level set
    val elements = new Array[Int](ms.size)
    val elementsToIndices = new HashMap[Int,Int]()
    for(i <- 0 until ms.size) {
      elements(i) = Mesh.internal(ms(i))
      elementsToIndices(ms(i)) = i
    }
    
    /* var out_edges = new java.io.FileOutputStream("edges.txt") 
    var edges_stream = new java.io.PrintStream(out_edges) 
    edges_stream.print("hello file!") 
    
    edges_stream.println("Workgroup start");
    for (i <- 0 until ms.size) {
      edges_stream.println("ELEMENT " + elements(i))
      
      for(e <- edges(i)) {
				edges_stream.println(e)
      }
    }
    edges_stream.close */
    
    // Build a interference graph between densely-numbered blocks from sparsely-numbered elements
    var numBlocks: Int = math.ceil(ms.size / blockSize).toInt
    
    val blockEdges = Array.fill[Set[Int]](numBlocks){ ISSet[Int]() }
    
    for(i <- 0 until ms.size) {
      val iblock: Int = i / blockSize
      for(connected <- edges(ms(i))) {
        val index_j = elementsToIndices(connected)
        val jblock: Int = index_j / blockSize
        blockEdges(iblock) += jblock
        // Do I need the other side?
      }
    }
    
    // Build a block to block interference graph CRS structure for coloring.
    val edge_idx = new Array[Int](numBlocks+1)
    // This will OVERESTIMATE the number of edges.
    val edge_vals = new Array[Int](totalEdges)
    
    var currEdgeV = 0
    
    for(i <- 0 until numBlocks) {
      edge_idx(i) = currEdgeV
      
      for(connected <- blockEdges(i)) {
        edge_vals(currEdgeV) = connected
        currEdgeV += 1
      }
    }
    
    edge_idx(numBlocks) = currEdgeV
    /*
    System.out.println("BLOCK BLOCK NB " + numBlocks + " NE " + totalEdges)
  
    for(i <- 0 until numBlocks) {
      System.out.println(i + ": START " + edge_idx(i) + " END " + edge_idx(i+1))
    
      for(j <- edge_idx(i) until edge_idx(i+1)) {
        System.out.println(edge_vals(j))
      }
    }
    */
    
    // Color!
    val (colors, numColors) = colorer.color(numBlocks, edge_idx, edge_vals)
    val nodes = (ms map { mo: Int => Mesh.internal(mo) }).toArray
    
    // Now output them?
    new Coloring(nodes, colors, numColors, blockSize, elements)
  }
}
