package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.util.OverloadHack

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._
import ppl.delite.framework.analysis.LayoutMetadataOps

trait PPLBlockingOps extends DeliteSimpleOps with DeliteNestedOps { this: PPLApp => 

  def tile(x: Rep[Int], tileSize: Int, max: Int): Unit
  def tile(x: Rep[Int], tileSize: Option[Int], max: Int): Unit
  def tile(x: Rep[Int], tileSize: Int, max: Option[Int]): Unit
  def ? = None

  // Bunch of syntatic sugar for creating tileAssembles.
  // In general, tileAssembles are pretty annoying to write manually
  // TODO: Types A and T aren't inferred properly here - need to be specified.
  def tileAssemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: Rep[Int]*)(init: => Rep[C])(keys: List[Rep[RangeVector]] => Rep[RangeVector]*)(tile: List[Rep[RangeVector]] => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](dims.toList, init, keys.toList, tile, None)
  def tileAssemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int])(init: => Rep[C])(keys: Rep[RangeVector] => Rep[RangeVector]*)(tile: Rep[RangeVector] => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0)) }, None)
  def tileAssemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1)) }, None)

  def tileReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: Rep[Int]*)(init: => Rep[C])(keys: List[Rep[RangeVector]] => Rep[RangeVector]*)(tile: List[Rep[RangeVector]] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](dims.toList, init, keys.toList, tile, Some(rFunc))
  def tileReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int])(init: => Rep[C])(keys: Rep[RangeVector] => Rep[RangeVector]*)(tile: Rep[RangeVector] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0)) }, Some(rFunc))
  def tileReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1)) }, Some(rFunc))
  def tileReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int], d2: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1,d2), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1),rvs(2)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1),rvs(2)) }, Some(rFunc))

  def tileMReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int], d2: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1,d2), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1),rvs(2)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1),rvs(2)) }, Some(rFunc), mutable = true)
}

trait PPLBlockingOpsExp extends PPLBlockingOps with DeliteSimpleOpsExp with DeliteNestedOpsExp { this: PPLCompiler =>
  def tile(x: Rep[Int], tileSize: Int, max: Int): Unit = { freshTuna(x, tileSize).withMax(max) }
  def tile(x: Rep[Int], tileSize: Option[Int], max: Int): Unit = { val t = freshTuna(x).withMax(max); tileSize.foreach(t.withValue(_)) }
  def tile(x: Rep[Int], tileSize: Int, max: Option[Int]): Unit = { val t = freshTuna(x, tileSize); max.foreach(t.withMax(_)) }
}
