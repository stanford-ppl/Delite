package asplos

import scala.reflect.SourceContext
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.datastructures._
import ppl.delite.framework.transform._
import ppl.delite.framework.visit._
import ppl.delite.framework.ops._

// TODO: Loop invariant symbol detection is very hacky right now, can definitely be improved upon
// Affine access detection works pretty well, but relies on loop invariant detection. Should also be
// able to account for loop-invariant but non-constant offset index patterns (e.g. a*i + b, a and b are not constants)
// Need to add more blocking patterns here - currently works on the bare minimum for the ASPLOS benchmarks
trait StripMiningExp extends DeliteVisit { self: PPLOpsExp =>
  val smDebug = false
  private def dbgmsg(x: => Any) { if (smDebug) printmsg(x) }

  var outerIndices: List[Sym[Int]] = Nil    // Bound indices in all loops except current innermost loop
  var loopIndices: List[Sym[Int]] = Nil     // All bound indices in current nested loop

  //var blockedIndices: Map[Sym[Int], Sym[Int]] = Map.empty
  var rangeVectors: Map[Sym[Int], Exp[RangeVector]] = Map.empty

  var loopIndependentSyms: List[Sym[Int]] = Nil // Hack for checking that a symbol is loop-independent

  val stripMiner = new StripMiningTransformer{val IR: self.type = self}
  //appendVisitor(stripMiner)

  var ignoreLoop = false
  def inLoop = loopIndices.nonEmpty
  def inBlockingLoop = inLoop && !ignoreLoop

  trait StripMiningTransformer extends TunnelingTransformer {
    val IR: self.type
    override lazy val name = "Strip Miner"

    // Debugging
    //override val debugMode = true
    //override val baseDebug = true
    //override val printBefore = true
    //override val printAfter = true

    def withSubstScopeList[A](extend: List[(Exp[Any],Exp[Any])])(block: => A): A = {
      withSubstScope {
        subst ++= extend
        block
      }
    }

    def dimRequiresBlocking(d: Exp[Int]) = d match {
      case Const(i) if i < 1000 => false
      case _: Tunable => false
      case d if tunableParams.contains(d) =>
        val tunable = tunableParams(d)
        (tunable.value, tunable.maxSize) match { case (v,s) => v != s; case _ => true }
      case _ => true 
    }

    // TODO: When to ignore loop during blocking? 
    // - When loop has already been blocked?
    // - When reduce loop has imperfectly nested loops in body?
    def requiresBlocking(op: DeliteOpLoopNest[_]) = {
      op.strides.forall{case Const(1) => true; case _ => false} && 
      op.sizes.exists(dimRequiresBlocking(_)) && 
      (op.body match {
        case elem: DeliteReduceElem[_] => true  // TODO: Imperfectly nested body?
        case elem: DeliteFoldElem[_,_] => false // TODO: How to block folds?
        case elem: DeliteForeachElem[_] => false // TODO: Blocking foreach

        // TODO: Probably want to try to block these eventually somehow
        case elem: DeliteHashCollectElem[_,_,_,_,_,_] => false
        case elem: DeliteHashReduceElem[_,_,_,_] => false
        case elem: DeliteHashIndexElem[_,_] => false
        case elem: DeliteTileElem[_,_,_] => false  // TODO: Affine access? Multi-level blocking?
        case _ => true
      })
    }

    // Create Some replacement for given definition node if required, None if not
    // FIXME: Assumes that the loop strides of the original loop are 1s
    override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
      case op: AbstractLoop[_] => 
        var prevIgnore = ignoreLoop
        ignoreLoop = true

        var prevOuter = outerIndices
        outerIndices = loopIndices
        loopIndices = loopIndices :+ op.v
        val newop = self_mirror(s, d)
        newop match {
          case Def(e: AbstractLoop[_]) => e.body
          case Def(Reflect(e: AbstractLoop[_], _,_)) => e.body
        }
        loopIndices = outerIndices
        outerIndices = prevOuter
        ignoreLoop = prevIgnore
        if (!inLoop && !isMutable(s)) loopIndependentSyms = loopIndependentSyms :+ s
        Some(newop)

      case op: DeliteOpLoopNest[_] if requiresBlocking(op) => 
        /*if (loopIndices.isEmpty) {
          dbgmsg("Prior to entering loop, loop-invariant symbols are:")
          loopIndependentSyms.foreach{sym => dbgmsg("\t" + strDef(sym)) }
        }*/

        val prevOuter = outerIndices
        outerIndices = loopIndices
        loopIndices = loopIndices ++ op.vs

        //val oldInds = blockedIndices
        val oldRVs = rangeVectors

        val blockingFactors: List[Tunable] = List.tabulate(op.nestLayers){i => freshTuna(f(op.sizes(i)))}
        val vs: List[Sym[Int]] = List.tabulate(op.nestLayers){i => fresh[Int]}

        def iSize(i: Int): Exp[Int] = Math.min(f(op.sizes(i)) - vs(i), blockingFactors(i))

        val rvs: List[Exp[RangeVector]] = List.tabulate(op.nestLayers){i => RangeVector(vs(i), iSize(i))}

        op.vs.zip(rvs).foreach{pair => rangeVectors += pair}
        //op.vs.zip(vs).foreach{pair => blockedIndices += pair}

        dbgmsg("Entering loop " + strDef(s))
        dbgmsg("with indices: ")
        for (i <- 0 until op.nestLayers) {
          dbgmsg("   " + op.vs(i).toString + " -> " + rvs(i).toString)
        }

        //val keys = mineLoopKeys(s, op)

        // Strip mine loop
        //val opBlk = self_mirror(s, d)
        val opBlk = mineLoop(s, op, vs, blockingFactors)        

        // Force transformation of loop bodies now (TBD: Needed here?)
        opBlk match {
          case Def(e: DeliteOpLoopNest[_]) => e.body
          case Def(Reflect(e: DeliteOpLoopNest[_], _,_)) => e.body
        }
        dbgmsg("")

        //val kFunc = mineLoopNestKeys(s, op)
        //val func = mineLoopNestBody(s, op)

        rangeVectors = oldRVs
        //blockedIndices = oldInds
        loopIndices = outerIndices
        outerIndices = prevOuter

        if (!inLoop && !isMutable(s)) loopIndependentSyms = loopIndependentSyms :+ s
        Some(opBlk)

      case op: ArrayApply[a,c] if inBlockingLoop => 
        dbgmsg("Encountered apply " + strDef(s))
        mineApply[a,c](f(op.x), op.inds)(op.mR,op.mA,ctx)
      case op: DeliteArrayApply[a] if inBlockingLoop => 
        dbgmsg("Encountered apply " + strDef(s))
        mineApply[a,DeliteArray[a]](f(op.da), List(op.i))(mtype(op.mA),mtype(darrayManifest(op.mA)),ctx)

      case op: ArraySlice[a,t,c] if inBlockingLoop => 
        dbgmsg("Encountered slice " + strDef(s))
        mineSlice[a,t,c](f(op.src),op.srcOffsets,op.srcStrides,op.destDims,op.unitDims)(op.mA,op.mR,op.mB,ctx)
      case op: BlockSlice[a,t,c] if inBlockingLoop =>
        dbgmsg("Encountered slice " + strDef(s)) 
        mineSlice[a,t,c](f(op.src),op.srcOffsets,op.srcStrides,op.destDims,op.unitDims)(op.mA,op.mT,op.mC,ctx)

      case _ => 
        // Relies on code motion to make sure symbols which are loop independent are not within a loop
        // FIXME: This may not catch all cases of mutation - should also check if sym is a var
        if (!inLoop && !isMutable(s)) 
          loopIndependentSyms = loopIndependentSyms :+ s
        
        super.transformSym(s,d)
    }

    def mineLoop(s: Sym[Any], op: DeliteOpLoopNest[_], vs: List[Sym[Int]], bfs: List[Exp[Int]])(implicit ctx: SourceContext): Exp[Any] = {
      val n = op.nestLayers
      val rvs = List.tabulate(n){i => rangeVectors(op.vs(i)) }
      val subInds = rangeVectors.toList.map{pair => pair._1 -> delite_int_plus(pair._2.start, pair._1) }

      // FIXME: Assumes that the loop strides of the original collect are 1s
      // collect(D0...DN){(i0...iN) => f(i0...iN)} 
      //  => tileAssemble(D0...DN)(B0...BN){(ii0...iiN) => (ii0...iiN)}{(ii0...iiN) =>
      //       collect(ii0.len...iiN.len){(i0...iN)} => f'(ii0...iiN, i0...iN) }
      //     }
      def mineCollect[A:Manifest,C<: DeliteCollection[A]:Manifest](elem: DeliteCollectElem[A,_,C]): Exp[C] = {
        val kFunc: List[Block[RangeVector]] = List.tabulate(n){i => reifyEffects(rvs(i)) }

        val tDims = List.tabulate(n){i => Math.min(f(op.sizes(i)), bfs(i)) }
        val constDims = tDims.zipWithIndex.filter{k => k._1 match {case Const(1) => true; case _ => false}}.map{_._2}
        val unitDims = if (constDims.length == tDims.length) constDims.drop(1) else constDims

        val rV = (fresh[C],fresh[C])  // Unused

        val buffAlloc: Block[C] = withSubstScopeList(subInds){ f(elem.buf.alloc.asInstanceOf[Block[C]]) }

        // Create smaller nested collect body
        // i -> ii.start + i (for when accesses are not simple affine)
        val innerFunc: Block[A] = withSubstScopeList(subInds){ f(elem.func) }
        val innerSizes = List.tabulate(n){i => rvs(i).length }
        
        // Tile function
        val tileCollect = NestedCollect[A,C](op.vs, innerSizes, innerFunc)
        val func: Block[C] = reifyEffects(reflectPure(tileCollect))

        val blockedCollect = TileAssemble[A,C,C](vs, rV, f(op.sizes), buffAlloc, bfs, kFunc, Nil, func, None, tDims, unitDims)
        dbgmsg("  Created new collect: "+ tileCollect)
        dbgmsg("  Created new tileAssemble: " + blockedCollect)

        reflectPure( blockedCollect )
      }

      // FIXME: Assumes immutable reduction for now
      /* reduce(D0...DN)(zero){(i0...iN) => f(i0...iN)}{(a,b) => r(a,b) }
         => reduce(D0...DN)(zero){(ii0...iiN) => 
              reduce(ii0.len...iiN.len)(zero){(i0...iN) => f'(ii0...iiN,i0...iN)}{(a,b) => r'(a,b)} }
            }{(a,b) => r'(a,b) }
        
         filterReduce(D0...DN)(zero){(i0...iN) => c(i0...iN) }{(i0...iN) => f(i0...iN) }{(a,b) => r(a,b) }
         => reduce(D0...DN)(zero){(ii0...iiN) => 
              filterReduce(ii0.len...iiN.len)(zero){(i0...iN) => 
                c'(ii0...iiN, i0...iN) 
              }{(i0...iN) => 
                f'(ii0...iiN, i0...iN) 
              }{(a,b) => r'(a,b) }
            }{(a,b) => r'(a,b) }  
      */ 
      def mineReduce[A:Manifest](elem: DeliteReduceElem[A]): Exp[A] = {
        val rV = (fresh[A],fresh[A])

        val subs = List(elem.rV._1 -> rV._1, elem.rV._2 -> rV._2)
        // Outer reduce has the same rFunc, zero, and accInit as the original reduce - but shouldn't be the same symbols!
        // TODO: Zero and init blocks may still may not be copied if all symbols in the block are CSE'd
        val rFunc = withSubstScopeList(subs){ copyBlock(elem.rFunc) }
        val initBlk = copyBlock(elem.accInit)
        val zeroBlk = copyBlock(elem.zero)

        // Smaller, nested reduce
        val innerInit: Block[A] = withSubstScopeList(subInds){ f(elem.accInit) }
        val innerZero: Block[A] = withSubstScopeList(subInds){ f(elem.zero) }
        val innerFunc: Block[A] = withSubstScopeList(subInds){ f(elem.func) }
        val innerRFunc: Block[A] = withSubstScopeList(subInds){ f(elem.rFunc) }
        val innerFFunc: List[Block[Boolean]] = withSubstScopeList(subInds){ elem.cond.map(f(_)) }
        val innerSizes = List.tabulate(n){i => rvs(i).length }

        val tileReduce = NestedReduce[A](op.vs, elem.rV, innerSizes, None, innerFunc, innerRFunc, innerInit, innerZero, innerFFunc, false)
        val func = reifyEffects(reflectPure(tileReduce))

        val blockedReduce = NestedReduce[A](vs, rV, f(op.sizes), Some(bfs), func, rFunc, initBlk, zeroBlk, Nil, false)
        dbgmsg("  Created new reduce: " + tileReduce)
        dbgmsg("  Created new blocked reduce: " + blockedReduce)

        reflectPure( blockedReduce )
      }

      op.body match {
        case elem: DeliteCollectElem[a,_,c] if elem.cond.isEmpty && elem.par == ParFlat => 
          mineCollect[a,c](elem)(elem.mA,elem.mCA)

        case elem: DeliteCollectElem[a,_,c] if elem.cond.nonEmpty && elem.iFunc.isEmpty => 
          //mineFilter[a,_,c](elem)(elem.mA,elem.mCA)
          sys.error("Blocking filter-collect is not yet implemented")

        case elem: DeliteReduceElem[a] =>
          mineReduce[a](elem)(elem.mA)
      
        case _ => sys.error("Don't know how to block " + op.body)
      }
    }
    
    // TODO: Metadata transfer
    def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = {}
  }

  private def isStatic(x: Exp[Any]): Boolean = x match {
    case x: Const[_] => true
    case x: Tunable => true
    case Def(DIntPlus(x,y)) => isStatic(x) && isStatic(y)
    case Def(DIntTimes(x,y)) => isStatic(x) && isStatic(y)
    case Def(MathMin(x, y)) if isStatic(x) || isStatic(y) => true   // Hack for boundary conditions. Range analysis here instead?
    case _ => false
  }

  // --- Simple affine access analysis
  // TODO: There's a lot more that can be done here. In particular, these multipliers and offsets don't actually need
  // to be constants - they just need to be loop invariant. 
  sealed abstract class IndexPattern
  case class AffineAccess(index: Sym[Int], mult: Int, ofs: Int) extends IndexPattern { // Simple a*i + b access
    override def toString = s"AffineAccess($mult * $index + $ofs)"
  }
  case class ConstantAccess(ofs: Int) extends IndexPattern            // Access at a constant index ( e.g. apply(0) )
  case class LoopInvariantAccess(ofs: Sym[Int]) extends IndexPattern  // Access at a loop-invariant index 
  object RandomAccess extends IndexPattern
  
  object FixedAccess { // Unapply for when we don't care if the index is statically known or not
    def unapply(p: IndexPattern): Option[Exp[Int]] = p match {
      case ConstantAccess(c) => Some(unit(c))
      case LoopInvariantAccess(x) => Some(x)
      case _ => None
    }
  }

  // Returns a multiplier and an offset, if any
  // Borrows logic from Arvind's stencil analysis
  def getIndexPattern(e: Exp[Int]): IndexPattern = e match {
    // a*i + b
    case Def(DIntPlus(Def(DIntTimes(Const(a), i: Sym[_])), Const(b))) if loopIndices.contains(i) => AffineAccess(i, a, b)
    // i*a + b
    case Def(DIntPlus(Def(DIntTimes(i: Sym[_], Const(a))), Const(b))) if loopIndices.contains(i) => AffineAccess(i, a, b)
    // b + i*a
    case Def(DIntPlus(Const(b), Def(DIntTimes(i: Sym[_], Const(a))))) if loopIndices.contains(i) => AffineAccess(i, a, b)
    // b + a*i
    case Def(DIntPlus(Const(b), Def(DIntTimes(Const(a), i: Sym[_])))) if loopIndices.contains(i) => AffineAccess(i, a, b)
    // b + i
    case Def(DIntPlus(Const(b), i: Sym[_])) if loopIndices.contains(i) => AffineAccess(i, 1, b)
    // i + b
    case Def(DIntPlus(i: Sym[_], Const(b))) if loopIndices.contains(i) => AffineAccess(i, 1, b)
    // a*i
    case Def(DIntTimes(Const(a), i: Sym[_])) if loopIndices.contains(i) => AffineAccess(i, a, 0)
    // i*a
    case Def(DIntTimes(i: Sym[_], Const(a))) if loopIndices.contains(i) => AffineAccess(i, a, 0)    
    // i
    case i: Sym[_] if loopIndices.contains(i) => AffineAccess(i, 1, 0)
    // b (loop-invariant symbol)
    case s: Sym[_] if loopIndependentSyms.contains(s) => LoopInvariantAccess(stripMiner(s).asInstanceOf[Sym[Int]])
    // b (constant)  
    case Const(b) => ConstantAccess(b)  
    // Anything else
    case _ => RandomAccess
  }

  // --- Blocking applies 
  def blockApplyIndex(p: IndexPattern): Rep[RangeVector] = p match {
    case AffineAccess(i, mult, ofs) => (rangeVectors(i) * unit(mult)) + unit(ofs)
    case ConstantAccess(ofs) => RangeVector(unit(ofs), unit(1))
    case LoopInvariantAccess(ofs) => RangeVector(ofs, unit(1))
    case RandomAccess => sys.error("Can't block random access!")
  }
  def getBlockedIndex(p: IndexPattern): Rep[Int] = p match {
    case AffineAccess(i, _, _) => i
    case ConstantAccess(_) => unit(0)
    case LoopInvariantAccess(_) => unit(0)
    case RandomAccess => sys.error("Can't block random access!")
  }

  def mineApply[A:Manifest,C<:DeliteCollection[A]:Manifest](x: Exp[C], inds: List[Exp[Int]])(implicit ctx: SourceContext): Option[Exp[A]] = {
    val patterns = inds map { getIndexPattern(_) }

    if (patterns contains RandomAccess) None 
    else if (patterns forall {case FixedAccess(_) => true; case _ => false} ) None
    else {
      dbgmsg("Found affine access of " + strDef(x))
      dbgmsg("Array type: " + manifest[C].toString + ", element type: " + manifest[A].toString)
      dbgmsg("at indices:\n   " + patterns.mkString("\n   ") )
      
      val newInds = patterns map { getBlockedIndex(_) }
      val rvs = patterns map { blockApplyIndex(_) }
      val offsets = rvs map { _.start }
      val strides = rvs map { _.stride }
      val lengths = rvs map { _.length }

      val blk = block_slice[A,C,C](x, offsets, strides, lengths, Nil)
      val elem = array_apply[A,C](blk, newInds)

      dbgmsg("Created slice/apply pair: ")
      dbgmsg(strDef(blk))
      dbgmsg(strDef(elem))
      Some(elem)
    }
  }

  // --- Blocking slices
  // NOTE: If slice is composed entirely of loop-invariant rangevectors, code motion should have removed it from the loop already
  def blockSliceVector(ofs: IndexPattern, stride: IndexPattern, length: IndexPattern): Option[(Rep[RangeVector], Int)] = (ofs, stride, length) match {
    // TODO: This is a lot of combinations for fixed slices. Can these be consolidated?
    // Should reuse for these always be 0?
    // b :: s :@: l
    case (FixedAccess(b), FixedAccess(s), FixedAccess(l)) => Some((RangeVector(b,s,l), 0))

    // a*i + b :: s :@: l
    // TOOD: Can anything intelligent be done when a > 1 and/or s > 1?
    // TODO: Can we give helpful reuse hints when the length is loop invariant but statically unknown?
    case (AffineAccess(i,a,b), ConstantAccess(s), ConstantAccess(l)) if a == 1 && s == 1 => 
      Some((rangeVectors(i) + unit(b) ++ unit(l-1), l-1)) 
    case (AffineAccess(i,a,b), ConstantAccess(s), LoopInvariantAccess(l)) if a == 1 && s == 1 => 
      Some((rangeVectors(i) + unit(b) ++ (l-1), 0)) // technically reuse is l - 1 still, but we require static reuse factors
    case _ => None
  }
  def getBlockedVector(ofs: IndexPattern, stride: IndexPattern, length: IndexPattern): Option[Rep[RangeVector]] = (ofs, stride, length) match {
    case (FixedAccess(b), FixedAccess(s), FixedAccess(l)) => Some(RangeVector(b,s,l))
    case (AffineAccess(i,a,b), ConstantAccess(s), ConstantAccess(l)) if a == 1 && s == 1 => Some(RangeVector(i,unit(1)))
    case (AffineAccess(i,a,b), ConstantAccess(s), LoopInvariantAccess(l)) if a == 1 && s == 1 => Some(RangeVector(i,unit(1)))
    case _ => None 
  }

  private def generate_slice[A:Manifest,C<:DeliteCollection[A]:Manifest](src: Exp[C], ofs: List[Exp[Int]], strides: List[Exp[Int]], dims: List[Exp[Int]], reuse: List[Int])(implicit ctx: SourceContext): Exp[C] = {
    if (dims forall { isStatic(_) } ) {
      val blk = BlockSlice[A,C,C](src, ofs, strides, dims, Nil).withReuse(reuse)
      reflectPure(blk)
    }
    else
      array_slice[A,C,C](src, ofs, strides, dims, Nil)
  }

  def mineSlice[A:Manifest,T<:DeliteCollection[A]:Manifest,C<:DeliteCollection[A]:Manifest](src: Exp[C], ofs: List[Exp[Int]], strides: List[Exp[Int]], dims: List[Exp[Int]], udims: List[Int])(implicit ctx: SourceContext): Option[Exp[T]] = {
    val offsetPatterns = ofs map { getIndexPattern(_) }
    val stridePatterns = strides map { getIndexPattern(_) }
    val lengthPatterns = dims map { getIndexPattern(_) }

    val rvsOpt = List.tabulate(ofs.length){i => blockSliceVector(offsetPatterns(i), stridePatterns(i), lengthPatterns(i)) }
    val newRvs = List.tabulate(ofs.length){i => getBlockedVector(offsetPatterns(i), stridePatterns(i), lengthPatterns(i)) }
    if (rvsOpt.contains(None) || newRvs.contains(None)) None
    else {
      dbgmsg("Found affine slice of " + strDef(src))
      dbgmsg("Array type: " + manifest[C].toString + ", tile type: " + manifest[T].toString + ", element type: " + manifest[A].toString)
      for (i <- 0 until ofs.length) {
        val offset = offsetPatterns(i)
        val stride = stridePatterns(i)
        val length = lengthPatterns(i)
        dbgmsg( s"   offset: $offset, stride: $stride, length: $length" )
      }

      val rvs = rvsOpt map { _.get._1 }
      val reuse = rvsOpt map { _.get._2 } 
      val offsets = rvs map { _.start }
      val strides = rvs map { _.stride }
      val lengths = rvs map { _.length }

      val newOffsets = newRvs map { _.get.start }
      val newStrides = newRvs map { _.get.stride }
      val newLengths = newRvs.map{ _.get.length }.zip(reuse).map{x => delite_int_plus(x._1, unit(x._2)) }

      // TODO: This won't work if C is of type Array1D or Array2D and the slice is not changed to a block slice
      // (During implementation, cannot create a view of an array which isn't a view)
      val blk = generate_slice[A,C](src, offsets, strides, lengths, reuse)
      val slc = array_slice[A,T,C](blk, newOffsets, newStrides, newLengths, udims)

      dbgmsg("Generated slice pair:")
      dbgmsg(strDef(blk))
      dbgmsg(strDef(slc))

      Some(slc)
    }
  }
}

