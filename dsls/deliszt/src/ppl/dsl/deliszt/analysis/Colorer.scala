package ppl.dsl.deliszt.analysis

trait Colorer {
  def color(numNodes: Int, sz: Array[Int], eg: Array[Int]) : (Array[Int],Int)
}
