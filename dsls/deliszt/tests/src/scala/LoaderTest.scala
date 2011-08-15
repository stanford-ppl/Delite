import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.util.Stack
import ppl.dsl.deliszt.datastruct.scala.MeshLoader

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
}
