package ppl.apps.ml.lbpdenoise

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.DeLisztApplication

trait LisztCode extends DeLisztApplication {
  val position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")
  var center = Vec(0.f,0.f,0.f)
  def main() = {
    for(v <- vertices(f)) {
      center += position(v)
    }
    center = center / size(vertices(f))
    ... // More Liszt code
  }
}
