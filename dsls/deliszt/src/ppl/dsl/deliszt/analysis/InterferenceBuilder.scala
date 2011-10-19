package ppl.dsl.deliszt.analysis

import collection.mutable.{HashMap, Set => MSet}
import ppl.dsl.deliszt.datastruct.scala._

class InterferenceBuilder(val colorer: Colorer, val blockSize: Int) {
  import StencilCollector.StencilMap

  def buildAndColor[MO<:MeshObj](ms: MeshSet[MO], stencil: StencilMap) : Coloring = {
    val accesses = new HashMap[FieldAccess,MSet[MeshObj]]() { override def default(key: FieldAccess) = { val mset = MSet[MeshObj](); this(key) = mset; mset } }
    val edges = new HashMap[MeshObj, MSet[MeshObj]]() { override def default(key: MeshObj) = { val mset = MSet[MeshObj](); this(key) = mset; mset } }
    var totalEdges = 0
  
    // Add edge between any that conflict on writes
    for((mo, rwset) <- stencil) {
      for(write <- rwset.write) {
        for(access <- accesses(write)) {
          if(access.internalId != mo.internalId) {
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
    
    // Build a dense list of indices for the sparse list of elements in the top-level set
    val elements = new Array[Int](ms.size)
    val elementsToIndices = new HashMap[MeshObj,Int]()
    for(i <- 0 until ms.size) {
      elements(i) = ms(i).internalId
      elementsToIndices(ms(i)) = i
    }
    
    // Build a interference graph between densely-numbered blocks from sparsely-numbered elements
    var numBlocks: Int = math.ceil(ms.size / blockSize).toInt
    
    val blockEdges = Array.fill[MSet[Int]](numBlocks){ MSet[Int]() }
    
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
    
    // Color!
    val (colors, numColors) = colorer.color(numBlocks, edge_idx, edge_vals)
    val nodes = (ms map { mo: MO => mo.internalId }).toArray
    
    // Now output them?
    new Coloring(nodes, colors, numColors, blockSize, elements)
  }
}
