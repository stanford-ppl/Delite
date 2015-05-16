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

  private def getdef(sym: Sym[Any]) = {
    sym match {
      case Def(d) => d
      case _ => null
    }
  }

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    var res = s

    val analyzer = new HwLoweringAnalysis { val IR: HwLoweringTransformer.this.IR.type = HwLoweringTransformer.this.IR}
    analyzer.run(s)
//    sys.exit(0) 
    res = super.runOnce(s)
    res
  }

  override def transformBlock[A:Manifest](b: Block[A]) = {
    curBlock = b
    var res = b
    if (funcBlocks.contains(b)) {
      val sched = buildScheduleForResult(getBlockResult(b))
      println(s"Printing schedule for func block $b")
      sched.map(s => println(s))
    }
    if (!noshowBlocks.contains(b)) {
      res = super.transformBlock(b)
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

  def processBodyElem(s: Sym[Any], loop: AbstractLoop[Any]): HwLoop = {
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
        subst += (buf.eV) -> this(getBlockResult(mirroredFunc))   // Connecting update -> func block
        subst += (buf.allocVal) -> this(getBlockResult(newallocBlk))  // Connecting update -> alloc block

        // Finally, the new loop body is the update block
        // Everything has been joined into this one
        val newfuncBlk = this(buf.update)

        if (!loop.size.isInstanceOf[Const[Int]]) {
          throw new Exception("Loop size not const")
        }

        // Create a new "hardware loop" - for lack of a better name
        val newloopnode = HwLoop(loop.size.asInstanceOf[Const[Int]], loop.v, Block(getBlockResult(newfuncBlk)))
        newloopnode

      case DeliteReduceElem(func,cond,zero,accInit,rV,rFunc,stripFirst,numDynamicChunks) =>
        // To be done
        HwLoop(Const(0), fresh[Int], Block(fresh[Int]))

      case DeliteForeachElem(func, _) =>
        // To be done
        HwLoop(Const(0), fresh[Int], Block(fresh[Int]))

      case _ =>
        // To be done
        HwLoop(Const(0), fresh[Int], Block(fresh[Int]))
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
        gen_hwld(this(arr), this(i))
      case DeliteArrayUpdate(arr, i, x) =>
        gen_hwst(this(arr), this(i), this(x))
      case _ =>
        throw new Exception(s"[Block $curBlock] Unknown memory operation $d")
        d
    }
  }

  override def transformStm(stm: Stm): Exp[Any] = {
    curStm = stm
    stm match {
      case TP(s, Reflect(node,summary,deps)) if node.isInstanceOf[AbstractLoop[_]] =>
        val res = processBodyElem(s, node.asInstanceOf[AbstractLoop[Any]])
        reflectEffect(res, mapOver(this.asInstanceOf[Transformer], summary))

      case TP(s,l:AbstractLoop[Any]) =>
        val res = processBodyElem(s, l)
        reflectPure(res)

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
//           reflectMirrored(Reflect(HwLoop(hwloopNode.size, hwloopNode.iter, f(hwloopNode.body)), mapOver(f,summary), f(d)))(mtype(manifest[A]), ctx)
           reflectMirrored(Reflect(HwLoop(hwloopNode.size, hwloopNode.iter, f(hwloopNode.body)), mapOver(f,summary), f(d)))(mtype(manifest[A]), ctx)
        case HwLoop(size, iter, body) =>
          reflectPure(HwLoop(size, iter, f(body)))

        case BRAM(id, size, wordlen, banks, bankMapping, rports, wports) =>
         reflectPure(gen_bram(size, wordlen, banks, bankMapping, rports, wports))

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
