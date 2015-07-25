package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

// Ops for manually creating horizontally fused, nested loops
// This goes pretty deeply into the gritty details of the compiler. That being the case, there is no Ops version here
// (Apps which use this manual fusion construct can't have a library version)
trait ManualFatLoopNestOpsExp { this: PPLOpsExp => 

  def rawBlockReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](v: Rep[Int])(tDims: List[Exp[Int]],unitDims: List[Int])(init: => Rep[C])(keys: List[Rep[RangeVector]])(tile: => Rep[T])(reduce: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Def[C] = {
    val _rV = (fresh[T],fresh[T])  // Assumed not to be mutable here

    val _kFunc: List[Block[RangeVector]] = keys.map{key => reifyEffects(key)}
    val _func: Block[T] = reifyEffects(tile)
    val _buffAlloc: Block[C] = reifyEffects(init)
    val _rFunc = reifyEffects(reduce(_rV._1,_rV._2))

    val n = _kFunc.length
    val _bS: List[Sym[RangeVector]] = List.fill(n){ fresh[RangeVector].asInstanceOf[Sym[RangeVector]] }
    val _bV: List[Sym[Int]] = List.fill(n){ fresh[Int].asInstanceOf[Sym[Int]] }
    val _tV: List[Sym[Int]] = List.fill(n){ fresh[Int].asInstanceOf[Sym[Int]] }
    val _tD: List[Sym[Int]] = List.fill(n){ fresh[Int].asInstanceOf[Sym[Int]] }
    val _buffVal: Sym[C] = reflectMutableSym(fresh[C]).asInstanceOf[Sym[C]]
    val _tileVal: Sym[T] = fresh[T].asInstanceOf[Sym[T]]
    val _partVal: Sym[T] = reflectMutableSym(fresh[T]).asInstanceOf[Sym[T]]
    val _bE: Sym[A] = fresh[A].asInstanceOf[Sym[A]]
    val _tE: Sym[A] = fresh[A].asInstanceOf[Sym[A]]

    DeliteTileElem[A,T,C](
      keys = _kFunc,
      cond = Nil,
      tile = _func,
      rV = _rV,
      rFunc = Some(_rFunc),
      buf = DeliteTileBuffer[A,T,C](
        bS = _bS,
        bV = _bV,
        tV = _tV,
        tD = _tD,
        buffVal = _buffVal,
        tileVal = _tileVal,
        partVal = _partVal,
        bE = _bE,
        tE = _tE,

        bApply = reifyEffects(dc_block_apply(_buffVal, _bV, Nil)),
        tApply = reifyEffects(dc_block_apply(_tileVal, _tV, unitDims)),
        bUpdate = reifyEffects(dc_block_update(_buffVal, _bV, _tE, Nil)),
        tUpdate = reifyEffects(dc_block_update(_partVal, _tV, _bE, unitDims)),
        allocBuff = reifyEffects(init),
        allocTile = reifyEffects(dc_alloc_block[A,T](_partVal, tDims, unitDims))
      ),
      numDynamicChunks = 0
    )
  }

  def fusedFatLoopNest2[A:Manifest,B:Manifest](d0: Rep[Int])(b0: Rep[Int])(bodies: Rep[Int] => (Def[A], Def[B]))(implicit ctx: SourceContext): (Rep[A],Rep[B]) = {
    // HACK - disable fusion if this method is called
    Config.opfusionEnabled = false

    val a = fresh[A]
    val b = fresh[B]
    val v = fresh[Int]

    val (bodyA,bodyB) = bodies(v)

    val fatLoop = SimpleFatLoopNest(List(d0),List(b0),List(v),List(bodyA,bodyB))

    createFatDefinition(List(a,b), List(bodyA,bodyB), fatLoop)
    (a,b)
  }
}
