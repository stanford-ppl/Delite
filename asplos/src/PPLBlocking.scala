package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._

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

// HACK - don't want to have view of staged if-then-else here, but do want math ops
trait PPLTileAssembleExp extends DeliteNestedOpsExp 
  with MathOps with PrimitiveOps with ImplicitOps with DeliteLMSForwarderExp { this: DeliteOpsExp =>
  
  override def tile_assemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: List[Exp[Int]], init: => Rep[C], keys: List[ List[Exp[RangeVector]] => Exp[RangeVector] ], tile: List[Exp[RangeVector]] => Exp[T], reduce: Option[(Exp[T],Exp[T]) => Exp[T] ], mutable: Boolean = false)(implicit ctx: SourceContext): Exp[C] = {
    val n = dims.length

    val strides: List[Tunable] = List.tabulate(n){i => freshTuna(dims(i)) }
    val vs: List[Sym[Int]] = List.fill(n)(fresh[Int])
    val rV = if (!mutable) (fresh[T],fresh[T])  else (reflectMutableSym(fresh[T]), fresh[T])

    // Input domain RangeVectors
    def iSize(i: Int): Exp[Int] = Math.min(dims(i) - vs(i), strides(i))
    val rvs: List[Exp[RangeVector]] = List.tabulate(n){i => RangeVector(vs(i), iSize(i)) }

    // Tile/Output RangeVector functions
    val kFunc: List[Block[RangeVector]] = keys.map{key => reifyEffects(key(rvs)) }
    val func: Block[T] = reifyEffects(tile(rvs))

    // HACK: Find tile dimensions by stripping off first iteration of the kFunc for use as tile sizes
    // Contract is that the either all tiles are equally sized or the first iteration has the largest tile size
    def firstIterSize(i: Int): Exp[Int] = Math.min(dims(i), strides(i))
    val rvsIter1 = List.tabulate(n){i => RangeVector(unit(0), firstIterSize(i)) }

    val tDims: List[Exp[Int]] = keys.map{key => key(rvsIter1).length }

    // HACK: find unit dimensions by checking the given lengths of each RangeVector key result..
    val constDims: List[Int] = tDims.zipWithIndex.filter{k => k._1 match {case Const(1) => true; case _ => false }}.map{_._2}

    // HACK: Scalar reduction is done right now using a 1D array of size 1 - can't ignore all dims in this case
    val unitDims = if (constDims.length == tDims.length) constDims.drop(1) else constDims

    val buffAlloc: Block[C] = reifyEffects(init)
    val rFunc = reduce.map{rF => reifyEffects(rF(rV._1,rV._2))}

    reflectPure( TileAssemble[A,T,C](vs, rV, dims, buffAlloc, strides, kFunc, Nil, func, rFunc, tDims, unitDims) )
  }

}

trait PPLBlockingOpsExp extends PPLBlockingOps with DeliteSimpleOpsExp with PPLTileAssembleExp { this: PPLCompiler =>
  def tile(x: Rep[Int], tileSize: Int, max: Int): Unit = { freshTuna(x, tileSize).withMax(max) }
  def tile(x: Rep[Int], tileSize: Option[Int], max: Int): Unit = { val t = freshTuna(x).withMax(max); tileSize.foreach(t.withValue(_)) }
  def tile(x: Rep[Int], tileSize: Int, max: Option[Int]): Unit = { val t = freshTuna(x, tileSize); max.foreach(t.withMax(_)) }
}
