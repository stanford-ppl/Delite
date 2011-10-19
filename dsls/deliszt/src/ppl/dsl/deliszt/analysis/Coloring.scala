package ppl.dsl.deliszt.analysis

class Coloring(val nodes: Array[Int], val colors: Array[Int], val numColors: Int, val blockSize: Int, val elements: Array[Int]) {
  val numBlocks = math.ceil(nodes.length / blockSize).toInt
  
  def collect() = {
  	// Build a color -> list of elements in ordered blocks CRS structure for launching threads
    val color_idx = new Array[Int](numColors+1)
    val color_values = new Array[Int](elements.size)
    
    var currentIdx = 0

    for (color <- 0 until numColors) {
      color_idx(color) = currentIdx
      
      for (blocki <- 0 until numBlocks) {
        if (colors(blocki) == color) {
          val start_eli = blocki * blockSize
          val thisblocksize = if (start_eli + blockSize >= elements.size) (elements.size - start_eli) else blockSize
          
          for (i <- 0 until thisblocksize) {
            color_values(currentIdx) = elements(start_eli + i)
            currentIdx += 1
          }
        }
      }
    }
    
    color_idx(numColors) = currentIdx
    
    (color_idx, color_values)
  }
}