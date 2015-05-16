package ppl.delite.framework.analysis

import scala.collection.mutable.HashSet
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._

import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

trait AnalyzerBase extends IterativeIRVisitor {
  val IR: DeliteOpsExp with MetadataOps
  import IR._

  val autopropagate: Boolean = true     // use default metadata propagation rules (see below) 

  override def hasConverged = !changed && !getMetadataUpdateFlag()

  /**
   * Main body for analysis. By default called after metadata propagation
   * has been completed (processStructure). 
   */
  def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: SourceContext): Unit

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = { 
    clearMetadataUpdateFlag()
    traverseBlock(s)
    (s) 
  }

  /**
   * Operate on the current statement. Overriden from FatBlockTraversal
   */
  override def traverseStm(stm: Stm): Unit = {
    analyzeStm(stm)
    super.traverseStm(stm)
  }

  /** 
   * Analyze the current statement
   * TODO: May also want to add fat statements (TTP)?
   */ 
  def analyzeStm(stm: Stm): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => 
      if (autopropagate) forwardPropagate(s,d)(mpos(s.pos))
      processOp(s,d)(mpos(s.pos))
    case TP(s, d) => 
      if (autopropagate) forwardPropagate(s,d)(mpos(s.pos))
      processOp(s,d)(mpos(s.pos))
    case _ => // Nothing
  }

  ////////////////////////////////
  // Metadata completeness checks

  /** 
   * Testing completeness / presence of symbol metadata mapping
   */ 
  def completed(e: Exp[Any]): Boolean = true

  /** 
   * Traverse IR checking all encountered symbols for metadata completeness
   */
  def checkCompleteness[A](b: Block[A]): List[Exp[Any]] = {

    class CompletenessCheck extends FatBlockTraversal {
      val IR: AnalyzerBase.this.IR.type = AnalyzerBase.this.IR
      val incompleteSet = new HashSet[Exp[Any]]()

      override def traverseStm(stm: Stm): Unit = {
        stm match {
          case TP(s,_) if !completed(s) => incompleteSet += s
          case _ => // Nothing
        }
        super.traverseStm(stm)
      }
    }

    val checker = new CompletenessCheck()
    checker.traverseBlock(b)
    checker.incompleteSet.toList
  }


  /////////////////////////////////
  // Helper functions for analysis

  /**
   * Includes a whole bunch of metadata structure propagation information but 
   * no metadata instances
  */
  def forwardPropagate[A](e: Exp[A], d: Def[_])(implicit ctx: SourceContext): Unit = d match {
    case Reify(s,_,_) => setProps(e, getProps(s))

    // --- Array
    case DeliteArrayTake(lhs,n) => setChild(e, getChild(lhs))
    case DeliteArraySort(da)    => setChild(e, getChild(da))
    case DeliteArrayApply(a,_)  => setProps(e, getChild(a))
    case DeliteArrayUpdate(a,_,x) => setChild(a, attemptMeet(getChild(a), getProps(x), func = MetaUpdate))
    case DeliteArrayCopy(src,_,dest,_,_) => setChild(dest, attemptMeet(getChild(src), getChild(dest), func = MetaUpdate))
    case DeliteArrayUnion(lhs,rhs) => setChild(e, attemptMeet(getChild(lhs), getChild(rhs), func = MetaUnion))
    case DeliteArrayIntersect(lhs,rhs) => setChild(e, attemptMeet(getChild(lhs), getChild(rhs), func = MetaIntersect))


    // --- Struct ops
    case op@Struct(_,elems) =>
      elems foreach {elem => setField(e, getProps(elem._2), elem._1) }
    case op@FieldApply(struct, field) => 
      setProps(e, getField(struct, field))
    case op@FieldUpdate(struct, field, rhs) => 
      val updatedField = attemptMeet(getField(struct, field), getProps(rhs), func = MetaUpdate)
      setField(struct, updatedField, field)

    // --- Variables
    case ReadVar(Variable(v)) => 
      setProps(e, getProps(v))
    case NewVar(init) => 
      setProps(e, getProps(init))
    case Assign(Variable(v),rhs) =>  
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaUpdate))
    case VarPlusEquals(Variable(v),rhs) => 
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaAdd))
    case VarMinusEquals(Variable(v),rhs) => 
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaSub))
    case VarTimesEquals(Variable(v),rhs) => 
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaMul))
    case VarDivideEquals(Variable(v),rhs) =>
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaDiv))

    // --- Atomic Writes
    case op@NestedAtomicWrite(s,trace,d) => 
      var newProps: Option[SymbolProperties] = getAtomicWriteRhs(d)
      for (t <- trace.reverse) { newProps = tracerToProperties(t, newProps) }

      val updatedProps = attemptMeet(newProps, getProps(s), func = MetaUpdate)
      setProps(s, updatedProps)

    // --- Misc. Delite ops
    case op: DeliteOpCondition[_] => 
      setProps(e, attemptMeet(getProps(op.thenp), getProps(op.elsep), func = MetaBranch))
    case op: DeliteOpWhileLoop =>
      setProps(e, getProps(op.body))

    case _ => // Nothing
  }

  def tracerToProperties(t: AtomicTracer, child: Option[SymbolProperties]): Option[SymbolProperties] = t match {
    case StructTracer(index) => Some(StructProperties(PropertyMap(index, child), NoData))
    case ArrayTracer(_)      => Some(ArrayProperties(child, NoData))
  }

  def getAtomicWriteRhs(d: AtomicWrite[Any])(implicit ctx: SourceContext): Option[SymbolProperties] = d match {
    case FieldUpdate(_,_,rhs) => getProps(rhs)
    case DeliteArrayUpdate(_,_,x) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteArrayCopy(src,_,_,_,_) => getProps(src)
    case _ => 
      warn("No RHS rule given for atomic write op " + d)
      None
  }
}