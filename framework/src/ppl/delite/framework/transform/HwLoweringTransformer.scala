package ppl.delite.framework.transform
import scala.virtualization.lms.common.WorklistTransformer
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteCollection, HwOpsExp}
import ppl.delite.framework.analysis.HwLoweringAnalysis
import ppl.delite.framework.datastructures.DeliteArray
import java.io.{FileWriter, PrintWriter}
import scala.util.Random
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.reflect.SourceContext

trait HwLoweringTransformer extends WorklistTransformer {
  val IR: HwOpsExp
  import IR._

  var curBlock: Block[Any] = null
  var curStm: Stm = null
  val graph = ListBuffer[HwDef[Any]]()
  val symToHwSym = Map[Sym[Any], Exp[Any]]()

  val funcBlocks = Set[Block[Any]]()
  val rfuncBlocks = Set[Block[Any]]()
  val zeroBlocks = Set[Block[Any]]()
  val accInitBlocks = Set[Block[Any]]()

  val allocBlocks = Set[Block[Any]]()
  val applyBlocks = Set[Block[Any]]()
  val updateBlocks = Set[Block[Any]]()
  val appendableBlocks = Set[Block[Any]]()
  val appendBlocks = Set[Block[Any]]()
  val setSizeBlocks = Set[Block[Any]]()
  val allocRawBlocks = Set[Block[Any]]()
  val copyRawBlocks = Set[Block[Any]]()
  val finalizerBlocks = Set[Block[Any]]()
  val hwBlocks = Set[Block[Any]]()

  val noshowBlocks = Set[Block[Any]]()
//  def noshowBlocks = Set[Block[Any]]() ++
//                      allocBlocks ++
//                      applyBlocks ++
//                      updateBlocks ++
//                      appendableBlocks ++
//                      appendBlocks ++
//                      setSizeBlocks ++
//                      allocRawBlocks ++
//                      copyRawBlocks ++
//                      finalizerBlocks

  val blkStartSync = Map[Block[Any], Sym[Any]]()
  val blkEndSync = Map[Block[Any], Sym[Any]]()
  val blkStitch = Map[Block[Any], Block[Any]]()

  var syncInfoPhase = false
  var stitchPhase = false

  val loopToMem = Map[Exp[Any], Exp[Any]]()

  private def getdef(sym: Sym[Any]) = {
    sym match {
      case Def(d) => d
      case _ => null
    }
  }

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    var res = s

//    val analyzer = new HwLoweringAnalysis { val IR: HwLoweringTransformer.this.IR.type = HwLoweringTransformer.this.IR}
//    analyzer.run(s)
    res = super.runOnce(s)
    res
  }

  private def handleZeroBlock[A:Manifest](zeroBlock: Block[A]) = {
    // For scalar accumulators, result of the block is just a constant
    // If this is the case, reflect a FF, create a block and
    // return that
    val blkres = getBlockResult(zeroBlock)
    blkres match {
      case Const(x) =>
        // Create flip-flop, create a block with that
        val ffdef = gen_ff()
        val ffNode = reflectPure(ffdef)
        Block(ffNode)
      case _ =>
        super.transformBlock(zeroBlock)
    }
  }

  override def transformBlock[A:Manifest](b: Block[A]) = {
    curBlock = b
    var res = b
//    res = super.transformBlock(b)
    if (!noshowBlocks.contains(b)) {
      if (zeroBlocks.contains(b)) {
        res = handleZeroBlock(b)
      } else {
        res = super.transformBlock(b)
      }
    }
    res
  }

  private def getStmDeps(stm: Stm): List[Sym[Any]] = {
    var ret = List[Sym[Any]]()
    stm match {
      case TP(sym, d) =>
        ret = syms(d)
      case TTP(s, d, fd) => throw new Exception("TTP not handled yet!")
    }
    ret
  }

  private def getStmSym(stm: Stm): Sym[Any] = {
    var ret: Sym[Any] = null
    stm match {
      case TP(sym, d) => ret = sym
      case TTP(s, d, fd) => throw new Exception("TTP not handled yet!")
    }
    ret
  }

  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
    super.traverseStmsInBlock(stms)
  }

  private def getHwSym(s: Exp[Any]) = {
    s match {
      case c:Const[Any] => c
      case e: Sym[Any] =>
        if (symToHwSym.contains(e)) {
          symToHwSym(e)
        } else {
          e
        }
    }
  }

  private def symsAndConsts(e: Any): List[Exp[Any]] = e match {
    case c: Exp[Any] => List(c)
    case ss: Iterable[Any] => ss.toList.flatMap(symsAndConsts(_))
    case p: Product =>
      p.productIterator.toList.flatMap(symsAndConsts(_))
    case _ => Nil
  }

  private def createSimpleLoop[A:Manifest, I<:DeliteCollection[A]:Manifest, CA<:DeliteCollection[A]:Manifest](loop: AbstractLoop[A], newbod: Block[A]) = {
    loop.body match {
      case oldbod@DeliteCollectElem(func:Block[A],cond,par,buf:DeliteBufferElem[A,I,CA],iFunc,iF,sF,eF,numDynamicChunks) =>
    SimpleLoop(
      loop.size,
      loop.v,
      DeliteCollectElem[A,I,CA](
        func = newbod,
        cond = oldbod.cond,
        par = oldbod.par,
        buf = DeliteBufferElem[A,I,CA](
          eV = oldbod.buf.eV,
          sV = this(oldbod.buf.sV).asInstanceOf[Sym[Int]],
          allocVal = oldbod.buf.allocVal,
          aV2 = oldbod.buf.aV2,
          iV = oldbod.buf.iV,
          iV2 = oldbod.buf.iV2,
          alloc = Block(fresh[I]),
          apply = oldbod.buf.apply,
          update = Block(fresh[Unit]),
          appendable = oldbod.buf.appendable,
          append = oldbod.buf.append,
          setSize = oldbod.buf.setSize,
          allocRaw = oldbod.buf.allocRaw,
          copyRaw = oldbod.buf.copyRaw,
          finalizer = buf.finalizer
        ),
        numDynamicChunks = oldbod.numDynamicChunks
      ))
      case _ =>
        sys.exit(0)
        loop
    }
  }

  def getRealBlockResult(b: Block[Any]) = {
    val reifyRes = getBlockResult(b).asInstanceOf[Sym[Any]]
    val otherRes = getBlockResultFull(b).asInstanceOf[Sym[Any]]
    val reifyDef = getdef(reifyRes)
    val otherDef = getdef(otherRes)
    println(s"getBlockResult = ($reifyRes, $reifyDef)")
    println(s"Other = ($otherRes, $otherDef)")
    println(s"$b.res = ${b.res}")
  }

  def processBodyElem(s: Sym[Any], loop: AbstractLoop[Any]): (Def[Any], Exp[Any]) = {
    val body = loop.body
    body match {
      case oldbod@DeliteCollectElem(func,cond,par,buf,iFunc,iF,sF,eF,numDynamicChunks) =>
        // Create new body block by doing the following substitution rules
        // 1. body.buf.sV -> op.size
        // 2. body.buf.eV -> body.func.output
        // 3. body.buf.allocVal -> body.allocBlk.output

        // First set up a substitution rule to propagate the
        // buffer size information, then mirror the block
        // so that the transformation takes place
        subst += (buf.sV) -> this(loop.size)
        val newallocBlk = transformBlock(buf.alloc)

        // Created the mirrored loop body,
        // then use that to join to the update block
        val mirroredFunc = transformBlock(func)
        val allocExp = this(getBlockResult(newallocBlk))
        subst += (buf.eV) -> this(getBlockResult(mirroredFunc))   // Connecting update -> func block
        subst += (buf.allocVal) -> allocExp  // Connecting update -> alloc block

        // Finally, the new loop body is the update block
        // Everything has been joined into this one
        val newfuncBlk = this(buf.update)

        if (!loop.size.isInstanceOf[Const[Int]]) {
          throw new Exception("Loop size not const")
        }

        // Create a new "hardware loop" - for lack of a better name
        val newloopnode = HwLoop(loop.size.asInstanceOf[Const[Int]], loop.v, Block(getBlockResult(newfuncBlk)))
        (newloopnode, allocExp)

      case DeliteReduceElem(func,cond,zero,accInit,rV,rFunc,stripFirst,numDynamicChunks) =>
        // Substitution rules for bounds symbols:
        // 0. val accumulator = getBlockResult(transformBlock(zero))
        // 1. rV._1 => getBlockResult(func)
        // 2. rv._2 => accumulator
        zeroBlocks += zero
        val accumBlk = transformBlock(zero)
        val accumulator = getBlockResult(accumBlk)

        // Transform the func block
        val newfuncBlk = transformBlock(func)

        subst += (rV._1) -> getBlockResult(newfuncBlk)
        subst += (rV._2) -> (accumulator)

        val newrFuncBlk = this(rFunc)
        val resFF = reflectPure(gen_ff_alias(orig = accumulator.asInstanceOf[Sym[Any]], din = getBlockResult(newrFuncBlk)))

        if (!loop.size.isInstanceOf[Const[Int]]) {
          throw new Exception("Loop size not const")
        }

        val newloopnode = HwLoop(loop.size.asInstanceOf[Const[Int]], loop.v, Block(resFF))
        (newloopnode, accumulator)

      case DeliteForeachElem(func, _) =>
        // To be done
        (HwLoop(Const(0), fresh[Int], Block(fresh[Int])), fresh[Any])

      case _ =>
        // To be done
        (HwLoop(Const(0), fresh[Int], Block(fresh[Int])), fresh[Any])
    }
  }

  private def isMemOp(s: Sym[Any], d: Def[Any]): Boolean = {
    d match {
      case Reflect(node, _,_ ) =>
        isMemOp(s, node.asInstanceOf[Def[Any]])
      case DeliteArrayNew(_,_,_) => true
      case DeliteArrayApply(_,_) => true
      case DeliteArrayUpdate(_,_,_) => true
      case _ => false
    }
  }

  private def getMemOp(d: Def[Any]): Def[Any] = {
   d match {
     case Reflect(node, _, _) =>
       getMemOp(node.asInstanceOf[Def[Any]])
      case DeliteArrayNew(length:Const[Int],_,_) =>
        gen_bram(length.x)
      case DeliteArrayApply(arr, i) =>
        println(s"Trying to getMemOp DeliteArrayApply($arr, $i)")
        println(s"loopToMem: $loopToMem")
        if (loopToMem.contains(arr)) {
          val ld = gen_hwld(loopToMem(arr), this(i))
          println(s"Replaced ld = $ld")
          ld
        } else {
          gen_hwld(this(arr), this(i))
        }
      case DeliteArrayUpdate(arr, i, x) =>
        println(s"Trying to getMemOp DeliteArrayUpdate($arr, $i, $x)")
        if (loopToMem.contains(arr)) {
          gen_hwst(loopToMem(arr), this(i), this(x))
        } else {
          gen_hwst(this(arr), this(i), this(x))
        }
      case _ =>
        throw new Exception(s"[Block $curBlock] Unknown memory operation $d")
        d
    }
  }

  override def transformStm(stm: Stm): Exp[Any] = {
    curStm = stm
    stm match {
      case TP(s, Reflect(node,summary,deps)) if node.isInstanceOf[AbstractLoop[_]] =>
        val (res, allocVal) = processBodyElem(s, node.asInstanceOf[AbstractLoop[Any]])
        val e = reflectEffect(res, mapOver(this.asInstanceOf[Transformer], summary))
        loopToMem += e -> allocVal
        e

      case TP(s,l:AbstractLoop[Any]) =>
        val (res, allocVal) = processBodyElem(s, l)
        val e = reflectEffect(res)
        loopToMem += e -> allocVal
        e

      case TP(s,d) if isMemOp(s,d) =>
        // Transform and apply existing substitution rules
        val newExp = super.transformStm(stm)

        // Use the def for mirrored node to get the equivalent hardware node
        val newDef = getdef(newExp.asInstanceOf[Sym[Any]])
        reflectPure(getMemOp(newDef))
      case TP(s,d) =>
        super.transformStm(stm)
      case TTP(s, d, fd) => throw new Exception("TTP not handled yet")
        super.transformStm(stm)
      case _ =>
        println(s"stm = $stm is not TP or TTP")
        sys.exit(-1)
        super.transformStm(stm)
    }
  }
}

trait HwLoweringTransformExp extends DeliteApplication {
  self =>
    private val t = new HwLoweringTransformer { val IR: self.type = self }
    appendVisitor(t)

    private def getdef(sym: Sym[Any]) = {
      sym match {
        case Def(d) => d
        case _ => null
      }
    }

    override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
      val mirrorVal = e match {
       case Reflect(node, summary, d) if node.isInstanceOf[HwLoop] =>
           val hwloopNode = node.asInstanceOf[HwLoop]
           reflectMirrored(Reflect(HwLoop(hwloopNode.size, hwloopNode.iter, f(hwloopNode.body)), mapOver(f,summary), f(d)))(mtype(manifest[A]), ctx)
        case HwLoop(size, iter, body) =>
          reflectPure(HwLoop(size, iter, f(body)))

        case BRAM(id, size, wordlen, banks, bankMapping, rports, wports) =>
         reflectPure(gen_bram(size, wordlen, banks, bankMapping, rports, wports))

       case FF(bitwidth, in) =>
          println(s"Trying to mirror $e")
          val newe = gen_ff(f(in), bitwidth)
          println(s"Mirrored: $newe")
         reflectPure(newe)

       case FFAlias(orig, din) =>
         reflectPure(gen_ff_alias(f(orig).asInstanceOf[Sym[Any]], f(din)))

       case Reflect(node, summary, d) if node.isInstanceOf[HwDummy] =>
           val hwdummyNode = node.asInstanceOf[HwDummy]
           val deps = hwdummyNode.deps.map(x => f(x))
           reflectMirrored(Reflect(HwDummy(deps:_*), mapOver(f,summary), f(d)))(mtype(manifest[A]), ctx)
       case Reflect(node, summary, d) if node.isInstanceOf[HwSync] =>
           val hwsyncNode = node.asInstanceOf[HwSync]
           val deps = hwsyncNode.deps.map(x => f(x))
           reflectMirrored(Reflect(HwSync(deps:_*), mapOver(f,summary), f(d)))(mtype(manifest[A]), ctx)
       case HwPlus(lhs, rhs) =>
          toAtom(HwPlus(f(lhs), f(rhs)))
       case HwMinus(lhs, rhs) =>
          toAtom(HwMinus(f(lhs), f(rhs)))
       case HwLd(mem, addr) =>
         reflectPure(gen_hwld(f(mem), f(addr).asInstanceOf[Exp[Int]]))
       case HwSt(mem, addr, value) =>
         reflectPure(gen_hwst(f(mem), f(addr).asInstanceOf[Exp[Int]], f(value)))

        case _ =>
          super.mirror(e,f)
      }
      mirrorVal.asInstanceOf[Exp[A]]
    }
}
