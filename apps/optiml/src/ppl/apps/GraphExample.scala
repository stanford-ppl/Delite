package ppl.apps

import ppl.dsl.optiml._

object GraphExampleRunner extends OptiMLApplicationRunner with GraphExample
trait GraphExample extends OptiMLApplication { 
  
  def vertexData(x: Rep[String], n: Rep[Int]) = new Record {
    val count = n
    val name = x
  }
  type VD = Record{val count: Int; val name: String}
  
  def edgeData(x: Rep[String]) = new Record {
    val name = x
  }  
  type ED = Record{val name: String}
  
  def main() = {
    // simple diamond-shaped graph
    val g = Graph[VD,ED]()
      
    val a = Vertex(g, vertexData("a",0))
    g.addVertex(a)
    val b = Vertex(g, vertexData("b",1))
    g.addVertex(b)
    val c = Vertex(g, vertexData("c",2))
    g.addVertex(c)
    val d = Vertex(g, vertexData("d",3))
    g.addVertex(d)
    
    val ab = Edge(g, edgeData("inAB"), edgeData("outAB"), a, b)
    g.addEdge(ab,a,b)
    
    val ac = Edge(g, edgeData("inAC"), edgeData("outAC"), a, c)
    g.addEdge(ac,a,c)
    
    val bd = Edge(g, edgeData("inBD"), edgeData("outBD"), b, d)
    g.addEdge(bd,b,d)
    
    val cd = Edge(g, edgeData("inCD"), edgeData("outCD"), c, d)
    g.addEdge(cd,c,d)
    
    g.freeze()
    
    for (v <- g.vertices) {
      println("vertex " + v.data.name)
      println("  has edges: ")
      for (e <- v.edges) {
        println("    " + e.inData.name + " / " + e.outData.name)
      }
    }
  }
}
