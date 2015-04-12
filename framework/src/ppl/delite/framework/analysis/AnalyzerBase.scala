package ppl.delite.framework.analysis

import ppl.delite.framework.visit._
import ppl.delite.framework.visit.Meetable._

import scala.virtualization.lms.internal.IterativeIRVisitor

import scala.collection.mutable.{HashMap,HashSet,Stack}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

trait AnalyzerBase extends IterativeIRVisitor with MetadataTransformer {
  val IR: DeliteOpsExp with DeliteMetadata
  import IR._

  val autopropagate: Boolean = true     // use default metadata propagation rules (see below) 
  
  // TODO: Remove these after debugging is done
  var changedSyms = new HashSet[Exp[Any]]()
  override def notifyUpdate(e: Exp[Any]): Unit = { changedSyms += e; notifyChange() }

  /**
   * Main body for analysis. By default called after metadata propagation
   * has been completed (processStructure). 
   */
  def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = { 
    printlog("Beginning " + name + " pass " + (runs + 1).toString)
    traverseBlock(s)
    (s) 
  }

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
    if (regionMetadata.isEmpty) { enterRegion() }
    b
  }
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = { b }

  /**
   * Operate on the current statement. Overriden from FatBlockTraversal
   */
  override def traverseStm(stm: Stm): Unit = {
    analyzeStm(stm)(AnalysisContext(stm))
    super.traverseStm(stm)
  }

  /** 
   * Analyze the current statement
   * TODO: May also want to add fat statements (TTP)?
   */ 
  def analyzeStm(stm: Stm)(implicit ctx: AnalysisContext): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => 
      if (autopropagate) forwardPropagate(s,d)
      processOp(s,d)
    case TP(s, d) => 
      if (autopropagate) forwardPropagate(s,d)
      processOp(s,d)
    case _ => // Nothing
  }

  ////////////////////////////////
  // Metadata completeness checks

  /** 
   * Testing completeness / presence of symbol metadata mapping
   */ 
  def completed(e: Exp[Any]): Boolean = true
  def unknown(e: Exp[Any]): Boolean = !metadata.contains(e)

  /** 
   * Traverse IR checking all encountered symbols for metadata completeness
   */
  def checkCompleteness[A](b: Block[A]): List[Exp[Any]] = {

    class CompletenessCheck extends FatBlockTraversal {
      val IR: AnalyzerBase.this.IR.type = AnalyzerBase.this.IR
      val incompleteSet = new HashSet[Exp[Any]]()

      override def traverseStm(stm: Stm): Unit = {
        implicit val ctx = AnalysisContext(stm)

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

  def check(cond: Boolean, x: => Any)(implicit ctx: AnalysisContext) {
    if (!cond) {
      val op = if (ctx.defDesc == "") "" else "In " + ctx.defDesc + ", "
      printerr(quotePos(ctx.defPos) + ": " + op + x + quoteCode(ctx.defPos).map{"\n\t" + _}.getOrElse(""))
    }
  }

  /**
   * Includes a whole bunch of metadata structure propagation information but 
   * no metadata instances
  */
  def forwardPropagate[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    case Reify(s,_,_) => setProps(e, getProps(s))

    // --- Array
    case DeliteArrayTake(lhs,n) => setChild(e, getChild(lhs))
    case DeliteArraySort(da)    => setChild(e, getChild(da))
    case DeliteArrayApply(a,_)  => setProps(e, getChild(a))
    case DeliteArrayUpdate(a,_,x) =>
      ctx.setSymPos(a)
      setChild(a, attemptMeet(getChild(a), getProps(x), func = MetaUpdate))
    case DeliteArrayCopy(src,_,dest,_,_) =>
      ctx.setSymPos(dest)
      setChild(dest, attemptMeet(getChild(src), getChild(dest), func = MetaUpdate))
    case DeliteArrayUnion(lhs,rhs) =>
      setChild(e, attemptMeet(getChild(lhs), getChild(rhs), func = MetaUnion))
    case DeliteArrayIntersect(lhs,rhs) =>
      setChild(e, attemptMeet(getChild(lhs), getChild(rhs), func = MetaIntersect))


    // --- Struct ops
    case op@Struct(_,elems) => 
      //elems foreach {elem => fieldTypeTip(e, elem._2.tp, elem._1) }
      elems foreach {elem => setField(e, getProps(elem._2), elem._1) }
    case op@FieldApply(struct, field) => 
      setProps(e, getField(struct, field))
    case op@FieldUpdate(struct, field, rhs) => 
      ctx.setSymPos(struct)
      val updatedField = attemptMeet(getField(struct, field), getProps(rhs), func = MetaUpdate)
      setField(struct, updatedField, field)

    // --- Variables
    case ReadVar(Variable(v)) => 
      setProps(e, getProps(v))
    case NewVar(init) => 
      setProps(e, getProps(init))
    case Assign(Variable(v),rhs) =>  
      ctx.setSymPos(v)
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaUpdate))
    case VarPlusEquals(Variable(v),rhs) => 
      ctx.setSymPos(v)
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaAdd))
    case VarMinusEquals(Variable(v),rhs) => 
      ctx.setSymPos(v)
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaSub))
    case VarTimesEquals(Variable(v),rhs) => 
      ctx.setSymPos(v)
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaMul))
    case VarDivideEquals(Variable(v),rhs) =>
      ctx.setSymPos(v)
      setProps(v, attemptMeet(getProps(v), getProps(rhs), func = MetaDiv))

    // --- Atomic Writes
    case op@NestedAtomicWrite(s,trace,d) => 
      var newProps: Option[SymbolProperties] = getAtomicWriteRhs(d).flatMap{getProps(_)}

      for (t <- trace.reverse) { newProps = tracerToProperties(t, newProps) }
      ctx.setSymPos(s)
      val updatedProps = attemptMeet(newProps, getProps(s), func = MetaUpdate)
      setProps(s, updatedProps)
 
    // --- Delite parallel ops
    // body is a Def, not a DeliteElem...
    // TODO: Add conditional (filter) functions to metadata (like region analysis with Ranges)
    /*case op: DeliteOpMap[_,_,_] => setChild(e, getProps(op.body.func.res))
    case op: DeliteOpFlatMap[_,_,_] => setChild(e, getChild(op.body.func.res))
    case op: DeliteOpMapIndices[_,_] => setChild(e, getProps(op.body.func.res))
    case op: DeliteOpFilter[_,_,_] => setChild(e, getProps(op.body.func.res))   // actually a MapFilter
    case op: DeliteOpZipWith[_,_,_,_] => setChild(e, getProps(op.body.func.res))
    case op: DeliteOpReduce[_] => 
      setProps(op.body.rV._1, getProps(op.body.func.res))
      setProps(op.body.rV._2, getProps(op.body.func.res))
      setProps(e, getProps(op.body.rFunc.res))
    case op: DeliteOpMapReduce[_,_] => 
      setProps(op.body.rV._1, getProps(op.body.func.res))
      setProps(op.body.rV._2, getProps(op.body.func.res))
      setProps(e, getProps(op.body.rFunc.res))
    case op: DeliteOpFilterReduce[_,_] =>    // actually map-filter-reduce
      setProps(op.body.rV._1, getProps(op.body.func.res))
      setProps(op.body.rV._2, getProps(op.body.func.res))
      setProps(e, getProps(op.body.rFunc.res))
    //case op@DeliteOpFilterReduceFold ???
    //case op@DeliteOpZipWithReduceTuple ???
    //case op@DeliteOpForeachReduce ??? 
    case op: DeliteOpZipWithReduce[_,_,_] =>
      setProps(op.body.rV._1, getProps(op.body.func.res))
      setProps(op.body.rV._2, getProps(op.body.func.res))
      setProps(e, getProps(op.body.rFunc.res))*/

    // groupBy and groupByReduce...

    // --- Misc. Delite ops
    case op: DeliteOpCondition[_] => 
      setProps(e, attemptMeet(getProps(op.thenp.res), getProps(op.elsep.res), func = MetaBranch))
    case op: DeliteOpWhileLoop =>
      setProps(e, getProps(op.body.res))

    case _ => 
      //warn(quotePos(ctx.defPos) + ": Unrecognized def " + d)
      // Nothing
  }

  def tracerToProperties(t: AtomicTracer, child: Option[SymbolProperties]) = t match {
    case StructTracer(field) => StructProperties(new PropertyMap(List(field -> child)), NoData)
    case ArrayTracer(_)      => ArrayProperties(child, NoData)
  }

  def getAtomicWriteRhs(d: AtomicWrite[Any])(implicit ctx: AnalysisContext): Option[Exp[Any]] = d match {
    case FieldUpdate(_,_,rhs) => Some(rhs)
    case DeliteArrayUpdate(_,_,x) => Some(x)
    case DeliteArrayCopy(src,_,_,_,_) => Some(src)
    case _ => 
      warn("No RHS rule given for atomic write op " + d)
      None
  }
}