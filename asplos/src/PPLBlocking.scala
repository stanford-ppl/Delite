package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.ScalaNestedCodegen

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.ops._
import ppl.delite.framework.analysis.LayoutMetadataOps

trait PPLBlockingOps { this: PPLApp => 
  def tileAssemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: Rep[Int]*)(init: => Rep[C])(keys: List[Rep[RangeVector]] => Rep[RangeVector]*)(tile: List[Rep[RangeVector]] => Rep[T])(implicit ctx: SourceContext): Rep[C]

  def tileAssemble2D[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector]) => Rep[T])(implicit ctx: SourceContext): Rep[C]

  def tileReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: Rep[Int]*)(init: => Rep[C])(keys: List[Rep[RangeVector]] => Rep[RangeVector]*)(tile: List[Rep[RangeVector]] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]

  def tileReduce2D[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]

  def blockSize(x: (Rep[Int], Int)): Unit
}

trait PPLBlockingOpsExp extends DeliteSimpleOpsExp { this: PPLCompiler =>
  def blockSize(x: (Rep[Int], Int)): Unit = freshTuna(x._1, x._2)

  // --- Blocking functions
  // Bunch of syntatic sugar for creating tileAssembles.
  // In general, tileAssembles are pretty annoying to write manually
  // TODO: Types A and T aren't inferred properly here - need to be specified.
  def tileAssemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: Exp[Int]*)(init: => Rep[C])(keys: List[Exp[RangeVector]] => Exp[RangeVector]*)(tile: List[Exp[RangeVector]] => Exp[T])(implicit ctx: SourceContext): Exp[C]
    = tile_assemble[A,T,C](dims.toList, init, keys.toList, tile, None)
  def tileAssemble2D[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1)) }, None)

  def tileReduce[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: Rep[Int]*)(init: => Rep[C])(keys: List[Rep[RangeVector]] => Rep[RangeVector]*)(tile: List[Rep[RangeVector]] => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](dims.toList, init, keys.toList, tile, Some(rFunc))
  def tileReduce2D[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1)) }, Some(rFunc))
  def tileReduce3D[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](d0: Rep[Int], d1: Rep[Int], d2: Rep[Int])(init: => Rep[C])(keys: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[RangeVector]*)(tile: (Rep[RangeVector], Rep[RangeVector], Rep[RangeVector]) => Rep[T])(rFunc: (Rep[T],Rep[T]) => Rep[T])(implicit ctx: SourceContext): Rep[C]
    = tile_assemble[A,T,C](List(d0,d1,d2), init, keys.toList.map{key => rvs: List[Rep[RangeVector]] => key(rvs(0),rvs(1),rvs(2)) }, {rvs: List[Rep[RangeVector]] => tile(rvs(0),rvs(1),rvs(2)) }, Some(rFunc))

  // TODO: Filtered tile assemble

  def tile_assemble[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](dims: List[Exp[Int]], init: => Rep[C], keys: List[ List[Exp[RangeVector]] => Exp[RangeVector] ], tile: List[Exp[RangeVector]] => Exp[T], reduce: Option[(Exp[T],Exp[T]) => Exp[T] ])(implicit ctx: SourceContext): Exp[C] = {
    val n = dims.length

    val strides: List[Tunable] = List.tabulate(n){i => freshTuna(dims(i)) }
    val vs: List[Sym[Int]] = List.fill(n)(fresh[Int])
    val rV = (fresh[T],fresh[T])  // Unused

    // Input domain RangeVectors
    val iSizes: List[Exp[Int]] = strides //List.tabulate(n){i => Math.min(dims(i) - vs(i), strides(i)) }
    val rvs: List[Exp[RangeVector]] = List.tabulate(n){i => RangeVector(vs(i),iSizes(i)) }

    // Tile/Output RangeVectors
    val kFunc: List[Block[RangeVector]] = keys.map{key => reifyEffects(key(rvs)) }
    val func: Block[T] = reifyEffects(tile(rvs))

    // HACK: find unit dimensions by checking the given lengths of each RangeVector key result..
    val unitDims: List[Int] = kFunc.zipWithIndex.filter{key => key._1.res match {
      case Def(RangeVectorNew(_,_,length)) => length match { case Const(1) => true; case _ => false }
      case Def(Reify(Def(RangeVectorNew(_,_,length)),_,_)) => length match { case Const(1) => true; case _ => false }
      case Def(Reflect(RangeVectorNew(_,_,length),_,_)) => length match { case Const(1) => true; case _ => false }
      case _ => false
    }}.map{_._2}

    val buffAlloc: Block[C] = reifyEffects(init)
    val rFunc = reduce.map{rF => reifyEffects(rF(rV._1,rV._2))}

    reflectPure( SimpleTileAssemble[A,T,C](vs, rV, dims, buffAlloc, strides, kFunc, Nil, func, rFunc, unitDims) )
  }
}

trait ScalaGenPPLBlocking extends ScalaNestedCodegen {
  val IR: PPLOpsExp
  import IR._

  private def emitBoundVarDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = " + rhs)
  }

  private def emitPrealloc(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: DeliteTileElem[_,_,_] => 
      stream.println("// --- Alloc output buffer")
      emitBlock(op.buf.allocBuff)
      emitValDef(op.buf.buffVal, quote(getBlockResult(op.buf.allocBuff)))

      if (op.rFunc.nonEmpty) {
        stream.println("// --- Alloc partial result tile")
        emitBlock(op.buf.allocTile)
        emitValDef(op.buf.partVal, quote(getBlockResult(op.buf.allocTile)))
      }
    case _ => // Nothing
  }
  private def emitPostLoop(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: DeliteTileElem[_,_,_] => 
      emitValDef(sym, quote(op.buf.buffVal))
    case _ => // Nothing
  }

  // FIXME: bV and tileVal are treated as vars here but are not Sym[Variable[_]] in the IR
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: BlockSlice[_,_,_] => 
      stream.println("// --- Block slice")
      for (i <- 0 until op.n) {
        emitBoundVarDef(op.bV(i), quote(op.srcOffsets(i)) )
      }
      for (j <- 0 until op.m) {
        stream.println("for (" + quote(op.vs(j)) + " <- 0 until " + quote(op.destDims(j)) + ")")
      }
      emitBlock(op.bApply)
      emitValDef(op.bE, quote(getBlockResult(op.bApply)))
      emitBlock(op.tUpdate)

      // FIXME: Shouldn't have plus explicitly emitted here?
      for (j <- 0 until op.m) {
        stream.println(quote(op.bV( op.deltaInds(j))) + " += " + quote( op.strides( op.deltaInds(j) ) ) )
        //emitAssignment(op.buf.bV(k), quote(op.buf.bV(k)) + " + 1")
        stream.println("}")
      }      

    case op: AbstractLoopNest[_] => 
      val vs = op.vs
      val sizes = op.sizes
      val strides = op.strides
      emitPrealloc(sym, op.body)
      val n = op.nestLayers
      for (i <- 0 until n) { 
        stream.println("for (" + quote(vs(i)) + " <- 0 to " + quote(sizes(i)) + " by " + quote(strides(i)) + ") {")
      }
      emitNode(sym, op.body)
      stream.println("}"*n)

      emitPostLoop(sym, op.body)

    case op: DeliteTileElem[_,_,_] => 
      if (op.cond.nonEmpty) { stream.println("if (true) { //TODO: Filter functions");  }

      stream.println("// --- Block body")
      emitBlock(op.tile)
      emitBoundVarDef(op.buf.tileVal, quote(getBlockResult(op.tile)))

      stream.println("// --- Keys ")
      for (k <- 0 until op.keys.length) {
        emitBlock(op.keys(k))
        emitValDef(op.buf.bS(k), quote(getBlockResult(op.keys(k))))
        emitValDef(op.buf.tD(k), quote(op.buf.bS(k)) + ".length")
      }

      if (op.rFunc.isDefined) {
        stream.println("// --- Accumulator copy out")
        for (k <- 0 until op.keys.length) {
          emitBoundVarDef(op.buf.bV(k), quote(op.buf.bS(k)) + ".start")
        }
        for (k <- 0 until op.keys.length) {
          stream.println("for (" + quote(op.buf.tV(k)) + " <- 0 until " + quote(op.buf.tD(k)) + ")")
        }
        emitBlock(op.buf.bApply)
        emitValDef(op.buf.bE, quote(getBlockResult(op.buf.bApply)))
        emitBlock(op.buf.tUpdate)

        // TODO: Shouldn't have plus explicitly emitted here?
        // TODO: Stride is assumed to be 1 here
        for (k <- 0 until op.keys.length) {
          stream.println(quote(op.buf.bV(k)) + " += 1")
          //emitAssignment(op.buf.bV(k), quote(op.buf.bV(k)) + " + 1")
          stream.println("}")
        }

        stream.println("// --- Reduction")
        emitValDef(op.rV._1, quote(op.buf.partVal))
        emitValDef(op.rV._2, quote(op.buf.tileVal))
        emitBlock(op.rFunc.get)
        emitAssignment(op.buf.tileVal, quote(getBlockResult(op.rFunc.get)))
      }

      stream.println("// --- Accumulator copy in")
      for (k <- 0 until op.keys.length) {
        emitAssignment(op.buf.bV(k), quote(op.buf.bS(k)) + ".start")
      }
      for (k <- 0 until op.keys.length) {
        stream.println("for (" + quote(op.buf.tV(k)) + " <- 0 until " + quote(op.buf.tD(k)) + ")")
      }
      emitBlock(op.buf.tApply)
      emitValDef(op.buf.tE, quote(getBlockResult(op.buf.tApply)))
      emitBlock(op.buf.bUpdate)

      // TODO: Shouldn't have plus explicitly emitted here?
      // TODO: Stride is assumed to be 1 here
      for (k <- 0 until op.keys.length) {
        stream.println(quote(op.buf.bV(k)) + " += 1")
        //emitAssignment(op.buf.bV(k), quote(op.buf.bV(k)) + " + 1")
        stream.println("}")
      }

      if (op.cond.nonEmpty) { stream.println("}") }

    case _ => super.emitNode(sym, rhs)
  }

}