package ppl.dsl.deliszt.analysis

class OneColorer extends Colorer {
	def color(numNodes: Int, sz: Array[Int], eg: Array[Int]) = {
    val color_out = Array.fill[Int](numNodes)(0)
    (color_out, 1)
	}
}