package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._

trait PPLNestedOps extends DeliteSimpleOps with DeliteNestedOps { this: PPLApp => 

  def tile(x: Rep[Int], tileSize: Int, max: Int): Unit
  def tile(x: Rep[Int], tileSize: Option[Int], max: Int): Unit
  def tile(x: Rep[Int], tileSize: Int, max: Option[Int]): Unit
  def ? = None

  def tiledReduce[A:Manifest](d0: Rep[Int])(zero: Rep[A])(tile: Rep[RangeVector] => Rep[A])(reduce: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A]
  def tiledFilterReduce[A:Manifest](d0: Rep[Int])(zero: Rep[A])(cond: Rep[RangeVector] => Rep[Boolean])(tile: Rep[RangeVector] => Rep[A])(reduce: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A]

  // Blocked forIndices is just forIndices with a non-unit stride
  def tileForIndices(d0: Rep[Int])(f: Rep[RangeVector] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
    = tile_forindices(List(d0), {rvs => f(rvs(0))})
  
  def tile_forindices(dims: List[Rep[Int]], f: List[Rep[RangeVector]] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  
  // Bunch of syntatic sugar for creating tileAssembles.
  // In general, tileAssembles are pretty annoying to write manually
  // TODO: Types A and T aren't inferred properly here - need to be specified.
  
  // Manual block sizes
  def block_assemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: List[Rep[Int]], init: => Rep[C], strides: List[Rep[Int]], keys: List[ List[Rep[RangeVector]] => Rep[RangeVector] ], tile: List[Rep[RangeVector]] => Rep[T], reduce: Option[(Rep[T],Rep[T]) => Rep[T] ], mutable: Boolean = false)(implicit ctx: SourceContext): Rep[C]  
  // Automatic block sizes
  def tile_assemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: List[Rep[Int]], init: => Rep[C], keys: List[ List[Rep[RangeVector]] => Rep[RangeVector] ], tile: List[Rep[RangeVector]] => Rep[T], reduce: Option[(Rep[T],Rep[T]) => Rep[T] ], mutable: Boolean = false)(implicit ctx: SourceContext): Rep[C]

  // Manually set block sizes
  def blockAssem[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int])(b0: Rep[Int])(init: => Rep[C])(keys: Rep[RangeVector] => Rep[RangeVector]*)(tile: Rep[RangeVector] => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = block_assemble[A,T,C](List(d0), init, List(b0), keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0)) }, None)

  def blockAssem[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int],d1: Rep[Int])(b0: Rep[Int], b1: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector],Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector],Rep[RangeVector]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = block_assemble[A,T,C](List(d0,d1), init, List(b0,b1), keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1)) }, None)

  def blockReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int])(b0: Rep[Int])(init: => Rep[C])(keys: Rep[RangeVector] => Rep[RangeVector]*)(tile: Rep[RangeVector] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = block_assemble[A,T,C](List(d0), init, List(b0), keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0)) }, Some(rFunc))

  // Auto-set block sizes
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

  // Mutable reductions
  def tileMReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int])(init: => Rep[C])(keys: Rep[RangeVector] => Rep[RangeVector]*)(tile: Rep[RangeVector] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0)) }, Some(rFunc), mutable = true)
  def tileMReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int], d2: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1,d2), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1),rvs(2)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1),rvs(2)) }, Some(rFunc), mutable = true)

  // Collect
  def collect[T:Manifest](d0: Rep[Int])(func: Rep[Int] => Rep[T])(implicit ctx: SourceContext): Rep[Array1D[T]]
    = nested_collect[T,Array1D[T]](List(d0), {is => func(is(0))})
  def collect[T:Manifest](d0: Rep[Int], d1: Rep[Int])(func: (Rep[Int],Rep[Int]) => Rep[T])(implicit ctx: SourceContext): Rep[Array2D[T]]
    = nested_collect[T,Array2D[T]](List(d0,d1), {is => func(is(0),is(1))})

  // Reduce
  def reduce[T:Manifest](d0: Rep[Int])(zero: Rep[T])(func: Rep[Int] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[T]
    = nested_reduce[T](zero, List(d0), {is => func(is(0))}, rFunc)
  def reduce[T:Manifest](d0: Rep[Int], d1: Rep[Int])(zero: Rep[T])(func: (Rep[Int],Rep[Int]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[T]
    = nested_reduce[T](zero, List(d0,d1), {is => func(is(0),is(1))}, rFunc)
  def mreduce[T:Manifest](d0: Rep[Int])(init: => Rep[T])(func: Rep[Int] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[T]
    = nested_mreduce[T](init, List(d0), {is => func(is(0))}, rFunc)
  def mreduce[T:Manifest](d0: Rep[Int], d1: Rep[Int])(init: => Rep[T])(func: (Rep[Int],Rep[Int]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[T]
    = nested_mreduce[T](init, List(d0,d1), {is => func(is(0),is(1))}, rFunc)

  // ForIndices
  def forIndices(d0: Rep[Int])(func: Rep[Int] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] 
    = nested_forIndices(List(d0), {is => func(is(0))})
  def forIndices(d0: Rep[Int],d1: Rep[Int])(func: (Rep[Int],Rep[Int]) => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
    = nested_forIndices(List(d0,d1), {is => func(is(0),is(1))})

  // Sort
  def sortIndices(length: Rep[Int])(comparator: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext) = darray_sortIndices(length, comparator)
}

// Don't want to have view of staged if-then-else here, but do want math ops
// TODO: Probably want to refactor this somehow
trait PPLTileAssembleExp extends DSLCompilerOps with DeliteNestedOpsExpOpt { this: DeliteOpsExp =>

  def tiledReduce[A:Manifest](d0: Rep[Int])(zero: Rep[A])(tile: Rep[RangeVector] => Rep[A])(reduce: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A] = {
    val blockFactor = freshTuna(d0)
    val v = fresh[Int]
    val tileSize = Math.min(d0 - v, blockFactor)
    val rv = RangeVector(v, tileSize)

    val rV = (fresh[A],fresh[A])

    val rFunc = reifyEffects(reduce(rV._1,rV._2))
    val func = reifyEffects(tile(rv))
    val zeroBlk = reifyEffects(zero)

    reflectPure( NestedReduce(List(v), rV, List(d0), Some(List(blockFactor)), func, rFunc, zeroBlk, zeroBlk, Nil, false) )
  }

  def tiledFilterReduce[A:Manifest](d0: Rep[Int])(zero: Rep[A])(cond: Rep[RangeVector] => Rep[Boolean])(tile: Rep[RangeVector] => Rep[A])(reduce: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A] = {
    val blockFactor = freshTuna(d0)
    val v = fresh[Int]
    val tileSize = Math.min(d0 - v, blockFactor)
    val rv = RangeVector(v, tileSize)

    val rV = (fresh[A],fresh[A])

    val fFunc = reifyEffects(cond(rv))
    val rFunc = reifyEffects(reduce(rV._1,rV._2))
    val func = reifyEffects(tile(rv))

    val zeroBlk = reifyEffects(zero)

    reflectPure( NestedReduce(List(v), rV, List(d0), Some(List(blockFactor)), func, rFunc, zeroBlk, zeroBlk, List(fFunc), false) )
  }

  // Blocked forIndices
  def tile_forindices(dims: List[Rep[Int]], f: List[Rep[RangeVector]] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] = {
    val n = dims.length

    val blockFactors: List[Exp[Int]] = List.tabulate(n){i => freshTuna(dims(i)) }

    val vs: List[Sym[Int]] = List.fill(n)(fresh[Int])
    def iSize(i: Int): Exp[Int] = Math.min(dims(i) - vs(i), blockFactors(i))
    val rvs: List[Exp[RangeVector]] = List.tabulate(n){i => RangeVector(vs(i), iSize(i)) }

    val func = reifyEffects(f(rvs))

    reflectEffect(NestedForeach(vs, dims, func, Some(blockFactors)), summarizeEffects(func).star andAlso Simple())
  }

  // Adds Min checks for edge cases in blocking
  // (Currently no view of Math.min in DeliteNestedOpsExp, but could use comparisons...)
  def tile_assemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: List[Exp[Int]], init: => Rep[C], keys: List[ List[Exp[RangeVector]] => Exp[RangeVector] ], tile: List[Exp[RangeVector]] => Exp[T], reduce: Option[(Exp[T],Exp[T]) => Exp[T] ], mutable: Boolean = false)(implicit ctx: SourceContext): Exp[C] = {
    common_assemble[A,T,C](dims,init,None,keys,tile,reduce,mutable) 
  }

  def block_assemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: List[Exp[Int]], init: => Rep[C], strides: List[Exp[Int]], keys: List[ List[Exp[RangeVector]] => Exp[RangeVector] ], tile: List[Exp[RangeVector]] => Exp[T], reduce: Option[(Exp[T],Exp[T]) => Exp[T] ], mutable: Boolean = false)(implicit ctx: SourceContext): Exp[C] = {
    common_assemble[A,T,C](dims,init,Some(strides),keys,tile,reduce,mutable) 
  }

  private def common_assemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: List[Exp[Int]], init: => Rep[C], strides: Option[List[Exp[Int]]], keys: List[ List[Exp[RangeVector]] => Exp[RangeVector] ], tile: List[Exp[RangeVector]] => Exp[T], reduce: Option[(Exp[T],Exp[T]) => Exp[T] ], mutable: Boolean)(implicit ctx: SourceContext): Exp[C] = {
    val n = dims.length

    val blockFactors: List[Exp[Int]] = if (strides.isDefined) strides.get else List.tabulate(n){i => freshTuna(dims(i)) }
    
    val vs: List[Sym[Int]] = List.fill(n)(fresh[Int])
    val rV = if (!mutable) (fresh[T],fresh[T]) else (reflectMutableSym(fresh[T]), fresh[T])

    // Input domain RangeVectors
    // Somewhat hacky exception here if block factor is fixed as being 1 (otherwise things further down the pipeline get messed up)
    def iSize(i: Int): Exp[Int] = blockFactors(i) match {
      case Const(1) => Const(1)
      case bf => Math.min(dims(i) - vs(i), bf)
    }
    def firstIterSize(i: Int): Exp[Int] = blockFactors(i) match {
      case Const(1) => Const(1)
      case bf => Math.min(dims(i), bf)
    }

    val rvs: List[Exp[RangeVector]] = List.tabulate(n){i => RangeVector(vs(i), iSize(i)) }

    // Tile/Output RangeVector functions
    val kFunc: List[Block[RangeVector]] = keys.map{key => reifyEffects(key(rvs)) }
    val func: Block[T] = reifyEffects(tile(rvs))

    // HACK: Find tile dimensions by stripping off first iteration of the kFunc for use as tile sizes
    // Contract is that the either all tiles are equally sized or the first iteration has the largest tile size
    val rvsIter1 = List.tabulate(n){i => RangeVector(unit(0), firstIterSize(i)) }

    val tDims: List[Exp[Int]] = keys.map{key => key(rvsIter1).length }

    // HACK: find unit dimensions by checking the given lengths of each RangeVector key result..
    val constDims: List[Int] = tDims.zipWithIndex.filter{k => k._1 match {case Const(1) => true; case _ => false }}.map{_._2}

    // HACK: Scalar reduction is done right now using a 1D array of size 1 - can't ignore all dims in this case
    val unitDims = if (constDims.length == tDims.length) constDims.drop(1) else constDims

    /*printmsg("Creating tileAssemble with unitDims: " + unitDims.mkString("(", ",", ")"))
    for (i <- 0 until tDims.length) {
      printmsg(strDef(tDims(i)))
    }*/

    val buffAlloc: Block[C] = reifyEffects(init)
    val rFunc = reduce.map{rF => reifyEffects(rF(rV._1,rV._2))}

    reflectPure( TileAssemble[A,T,C](vs, rV, dims, buffAlloc, blockFactors, kFunc, Nil, func, rFunc, tDims, unitDims) )
  }

  // Adding min to tiledFilter too
  override def tiledFilter[A:Manifest](size: Rep[Int])(func: Rep[RangeVector] => Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]] = {
    val v = fresh[Int]
    val stride = freshTuna(size)
    val rv = RangeVector(v, Math.min(size - v, stride))
    val iFuncBlk = reifyEffects(func(rv))
    reflectPure( NestedFlatMap[A,DeliteArray[A]](v, size, stride, None, None, Some(iFuncBlk)) )
  }

}

trait PPLNestedOpsExp extends PPLNestedOps with DeliteSimpleOpsExp with PPLTileAssembleExp { this: PPLCompiler =>
  def tile(x: Rep[Int], tileSize: Int, max: Int): Unit = { freshTuna(x, tileSize).withMax(max) }
  def tile(x: Rep[Int], tileSize: Option[Int], max: Int): Unit = { val t = freshTuna(x).withMax(max); tileSize.foreach(t.withValue(_)) }
  def tile(x: Rep[Int], tileSize: Int, max: Option[Int]): Unit = { val t = freshTuna(x, tileSize); max.foreach(t.withMax(_)) }
}
