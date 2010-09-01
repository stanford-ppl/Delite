
package ppl.dsl.optiml.direct

import scala.reflect.ClassManifest

trait VectorView[T] extends Vector[T] {
  def start: Int
  def stride: Int

  protected var _start: Int
  protected var _stride: Int

  def idx(n: Int) = _start + n*_stride

  def +=[A <: T](x: A) = throw new UnsupportedOperationException("operations on views not fully supported yet")

}
