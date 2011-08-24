import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.util.Stack
import ppl.dsl.deliszt.datastruct.scala.{Mesh, MeshLoader}

class StackSpec extends FlatSpec with ShouldMatchers {
  "The MeshLoader" should "load succesfully" in {
    val path = System.getProperty("java.library.path")
    println("Path:")
    println(path)

    val cfg = getClass.getResource("/liszt.cfg").getPath()
    println("Liszt CFG")
    println(cfg)

    try {
      MeshLoader.init(cfg)
    }
    catch {
      case e:java.lang.UnsatisfiedLinkError => {println(e); fail}
    }
  }
  
  it should "load a mesh into Mesh" in {
    Mesh.mesh should not be (null)
    Mesh.loader should not be (null)
  }
  
  it should "store itself into Mesh" in {
    Mesh.loader should not be (null)
  }
  
  it should "have a non-empty mesh" in {
    Mesh.mesh.nvertices should be > 0
    Mesh.mesh.nfaces should be > 0
    Mesh.mesh.ncells should be > 0
    Mesh.mesh.nedges should be > 0
  }
}
