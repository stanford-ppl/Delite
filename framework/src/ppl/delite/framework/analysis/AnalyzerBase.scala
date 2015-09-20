package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.{Traversal,IterativeAnalyzer}

import scala.reflect.SourceContext
import scala.collection.mutable.HashSet

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

trait AnalyzerBase extends IterativeAnalyzer {
  val IR: DeliteOpsExp
  import IR._

  val autopropagate: Boolean = true

  override def hasConverged = !changed && !getMetadataUpdateFlag()

  /**
   * Main function for analysis.
   * By default called after metadata propagation has been completed
   */
  def processTP(lhs: Exp[Any], rhs: Def[Any])(implicit ctx: SourceContext): Unit
  def processTTP(lhs: List[Exp[Any]], mhs: List[Def[Any]], rhs: FatDef): Unit = {}

  override def processBlock[A:Manifest](block: Block[A]): Block[A] = {
    clearMetadataUpdateFlag()
    traverseBlock(block)
    (block)
  }

  override def traverseStm(stm: Stm) = {
    super.traverseStm(stm)
    analyzeStm(stm)
  }

  def analyzeStm(stm: Stm) = stm match {
    case TP(lhs, rhs) =>
      if (autopropagate) forwardPropagateTP(lhs, rhs)(mpos(lhs.pos))
      processTP(lhs, rhs)(mpos(lhs.pos))

    case TTP(lhs, mhs, rhs) =>
      // TODO: What to do for TTPs?
      processTTP(lhs, mhs, rhs)
  }

  def completed(e: Exp[Any]): Boolean = true

  def getIncompleteSyms[A:Manifest](b: Block[A]): List[Exp[Any]] = {

    class CompletenessCheck extends Traversal {
      val IR: AnalyzerBase.this.IR.type = AnalyzerBase.this.IR
      val incompleteSet = new HashSet[Exp[Any]]()

      override def traverseStm(stm: Stm) = {
        super.traverseStm(stm)
        stm match {
          case TP(sym,_) if !completed(sym) => incompleteSet += sym
          case TTP(syms,_,_) => syms foreach {sym => if (!completed(sym)) incompleteSet += sym }
        }
      }
    }

    val checker = new CompletenessCheck()
    checker.run(b)
    (checker.incompleteSet.toList)
  }

  // TODO: Where does this belong? Where should each of these be defined? In respective traits?
  def forwardPropagateTP[A](lhs: Exp[A], rhs: Def[_])(implicit ctx: SourceContext): Unit = rhs match {
    // --- Effects
    case Reify(sym, _, _) => setProps(lhs, getProps(sym))
    case Reflect(d, _, _) => forwardPropagateTP(lhs, d)

    // --- DeliteArray
    case DeliteArrayTake(da, n) => setChild(lhs, getChild(da))
    case DeliteArraySort(da) => setChild(lhs, getChild(da))
    case DeliteArrayApply(da, _) => setProps(lhs, getChild(da))
    case DeliteArrayUpdate(da, _, x) => setChild(da, tryMeet(getChild(da), getProps(x), func = MetaUpdate))
    case DeliteArrayCopy(src, _, dest, _, _) => setChild(dest, tryMeet(getChild(src), getChild(dest), func = MetaUpdate))
    case DeliteArrayUnion(da, db) => setChild(lhs, tryMeet(getChild(da), getChild(db), func = MetaUnion))
    case DeliteArrayIntersect(da, db) => setChild(lhs, tryMeet(getChild(da), getChild(db), func = MetaIntersect))

    // --- Struct Ops
    case Struct(_, elems) => elems foreach {case (index,sym) => setField(lhs, getProps(sym), index) }
    case FieldApply(struct, index) => setProps(lhs, getField(struct, index))
    case FieldUpdate(struct, index, x) =>
      val updatedField = tryMeet(getField(struct, index), getProps(x), func = MetaUpdate)
      setField(struct, updatedField, index)

    // --- Variables
    // TODO: Assignments in loops?
    // TODO: Weird to have different meet types for add/mul/sub/div...
    case ReadVar(Variable(v)) => setProps(lhs, getProps(v))
    case NewVar(init) => setProps(lhs, getProps(init))
    case Assign(Variable(v), x) => setProps(v, tryMeet(getProps(v), getProps(x), func = MetaUpdate))
    case VarPlusEquals(Variable(v), x) => setProps(v, tryMeet(getProps(v), getProps(x), func = MetaAdd))
    case VarMinusEquals(Variable(v), x) => setProps(v, tryMeet(getProps(v), getProps(x), func = MetaSub))
    case VarTimesEquals(Variable(v), x) => setProps(v, tryMeet(getProps(v), getProps(x), func = MetaMul))
    case VarDivideEquals(Variable(v), x) => setProps(v, tryMeet(getProps(v), getProps(x), func = MetaDiv))

    // --- Branches
    case op: DeliteOpCondition[_] => setProps(lhs, tryMeet(getProps(op.thenp), getProps(op.elsep), func = MetaBranch))

    // --- Delite Ops
    // TODO: Fill in the remainder of these ops
    case op: AbstractLoop[_] => op.body match {
      case r: DeliteReduceElem[_] =>
        traverseBlock(r.func)
        setProps(r.rV._1, getProps(r.func))
        setProps(r.rV._2, getProps(r.func))
        traverseBlock(r.rFunc)
        setProps(lhs, getProps(r.rFunc))

      case _ => // Nothing
    }

    // --- Nested atomic writes
    case NestedAtomicWrite(s, trace, d) =>
      var newProps: Option[SymbolProperties] = getAtomicWriteRHS(d)
      for (t <- trace.reverse) { newProps = tracerToProperties(t, newProps) }

      val updatedProps = tryMeet(newProps, getProps(s), func = MetaUpdate)
      setProps(s, updatedProps)

    case _ => // Do nothing
  }

  def tracerToProperties(t: AtomicTracer, child: Option[SymbolProperties]): Option[SymbolProperties] = t match {
    case StructTracer(index) => Some(StructProperties(PropertyMap(index,child), NoData))
    case ArrayTracer(_) => Some(ArrayProperties(child, NoData))
  }

  def getAtomicWriteRHS(d: AtomicWrite[Any])(implicit ctx: SourceContext): Option[SymbolProperties] = d match {
    case FieldUpdate(_,_,x) => getProps(x)
    case DeliteArrayUpdate(_,_,x) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteArrayCopy(src,_,_,_,_) => getProps(src)
    case _ =>
      printwarn(s"No RHS rule extraction rule given for atomic write op $d")
      None
  }

}