package ppl.dsl.deliszt.analysis

class Coloring(val nodes: Array[Int], val colors: Array[Int], val blockSize: Int) {
  val numBlocks = math.ceil(nodes.length / blockSize).toInt
}