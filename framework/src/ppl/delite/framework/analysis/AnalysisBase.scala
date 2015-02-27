package ppl.delite.framework.analysis

import scala.language.reflectiveCalls
import scala.io.Source

import scala.collection.mutable.{HashMap,HashSet}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

import ppl.delite.framework.Util._
import ppl.delite.framework.Config

trait AnalysisBase extends FatBlockTraversal {
  val IR: DeliteOpsExp
  import IR._

  val IRMetadata: DeliteMetadata
  import IRMetadata._

  class AnalysisError(msg: String) extends Exception(msg)

  //////////////////////////////////////////////////////////////
  // Functions and fields which probably will never need to be 
  // overridden have the 'final' qualifier. Most others need to
  // be overriden/implemented

  val name: String                      // name of analysis
  val MAX_ITERS: Int = 10               // maximum number of iterations to run
  final var iter = 0                    // Current analysis iteration
  final var hadErrors: Boolean = false  // Flag for if metadata has encountered (non-fatal) errors
  def verbose = Config.debug            // analysis verbosity

  var metadata = new HashMap[Exp[Any],SymbolProperties]()

  final var changed: Boolean = false    // Flag for if any metadata has changed
  final var changedSyms = new HashSet[Exp[Any]]()
  final def notifyUpdate(e: Exp[Any]) { changedSyms += e; changed = true }

  
  case class AnalysisContext(var symPos: List[SourceContext], var defPos: List[SourceContext], var defDesc: String) {
    def setSymPos(e: Exp[Any]) { symPos = e.pos }
  }

  /**
   * Main body for analysis. By default called after structural propagation
   * has been completed (processStructure). 
   */
  def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit

  def iteration[A](b: Block[A]): Unit = {
    iter += 1
    changed = false
    changedSyms.clear()
    log("Starting iteration " + iter)
    traverseBlock(b)
  }

  /**
   * Run traversal/analysis on a given block
   */
  def run[A](b: Block[A]): Unit = {
    log("Beginning...")
    do { iteration(b) } while (changed && iter <= MAX_ITERS)
    log("Completed.")
  }    

  /** 
   * Analyze the current statement - can be overriden to circumvent
   * structural metadata propagation in processStructure or match on
   * more cases
   */ 
  def analyzeStm(stm: Stm)(implicit ctx: AnalysisContext): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => processStructure(s,d); processOp(s,d);
    case TP(s, d) => processStructure(s,d); processOp(s,d);
    case _ => // Nothing
  }

  /**
   * Operate on the current statement. Overriden from FatBlockTraversal
   */
  final override def traverseStm(stm: Stm): Unit = {
    analyzeStm(stm)(createAnalysisCtx(stm))
    super.traverseStm(stm)
  }

  final def createAnalysisCtx(stm: Stm): AnalysisContext = stm match {
    case TP(s, Reflect(d,_,_)) => AnalysisContext(s.pos, s.pos, nameDef(d))
    case TP(s,d) => AnalysisContext(s.pos, s.pos, nameDef(d))
    case _ => AnalysisContext(Nil,Nil,"")
  }

  ////////////////////////////////
  // Metadata completeness checks

  /** 
   * Traverse IR checking all encountered symbols for metadata completeness
   */
  final def checkCompleteness[A](b: Block[A]): List[Exp[Any]] = {

    class CompletenessCheck extends FatBlockTraversal {
      val IR: AnalysisBase.this.IR.type = AnalysisBase.this.IR
      val incompleteSet = new HashSet[Exp[Any]]()

      override def traverseStm(stm: Stm): Unit = {
        implicit val ctx = createAnalysisCtx(stm)

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

  /** 
   * Testing completeness / presence of symbol metadata mapping
   */ 
  final def completed(e: Exp[Any])(implicit ctx: AnalysisContext): Boolean = metadata.get(e) match {
    case Some(p) => isComplete(p)
    case None => isComplete(initExp(e))
  }
  final def unknown(e: Exp[Any]): Boolean = !metadata.contains(e)

  /////////////////////////////////
  // Helper functions for analysis

  final def strDef(e: Exp[Any]) = e match {
    case Const(z) => z.toString
    case Def(d) => e.toString + " = " + d.toString
    case e: Exp[_] => "(bound " + e.toString + ")"
  }

  final def isDataStructure[A](mA: Manifest[A]): Boolean = mA match {
    case StructType(_,_) => true
    case _ => isSubtype(mA.erasure,classOf[DeliteMultiArray[_]])
  }

  // TODO: This is directly copied from quotePos in expressions... could move this to Expressions?
  private def getPathAndLine(ctx: List[SourceContext]): List[(String,Int)] = {
    def all(cs: SourceContext): List[SourceContext] = cs.parent match {
      case None => List(cs)
      case Some(p) => cs::all(p)
    }
    ctx.map{c => val top = all(c).last; (top.fileName, top.line) }
  }

  private def quotePos(ctx: List[SourceContext]): String = getPathAndLine(ctx) match {
    case Nil => "<unknown>"
    case cs => cs.map(p => p._1 + ":" + p._2).mkString(";")
  }
  private def quoteCode(ctx: List[SourceContext]): Option[String] = {
    val pos = getPathAndLine(ctx)
    if (pos.length == 1) { 
      Some(Source.fromFile(pos.head._1).getLines().toList.apply(pos.head._2))
    }
    else None
  }

  final def log(x: String, tagged: Boolean = true) = {
    val str = (if (tagged) "[" + name + "]" else "") + x
    /* if (verbose) */  Predef.println(str) /* else () */
  }
  final def result(x: String, tagged: Boolean = true) = Predef.println((if (tagged) "[" + name + "]" else "") + x)

  final def warn(x: =>Any) { System.err.println("[\u001B[33mwarn\u001B[0m] " + name + ": " + x) }
  final def warn(cond: Boolean, x: => Any) { if (!cond) warn(x) }

  final def fatalerr(x: =>Any) { error(x); throw new AnalysisError("Fatal analysis error") }

  final def error(x: =>Any) { System.err.println("[\u001B[31merror\u001B[0m] " + name + ": " + x); hadErrors = true }
  final def check(cond: Boolean, x: => Any)(implicit ctx: AnalysisContext) {
    if (!cond) {
      val op = if (ctx.defDesc == "") "" else "In " + ctx.defDesc + ", "
      error(quotePos(ctx.defPos) + ": " + op + x + quoteCode(ctx.defPos).map{"\n\t" + _}.getOrElse("") )
    }
  }

  /**
   * Directly add symbol property metadata for symbol
   */ 
  final def setProps(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: AnalysisContext) {
    if (p.isDefined) { updateProperties(e, p.get) }
  }

  /**
   * Add metadata information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
   */
  final def setMetadata(e: Exp[Any], m: Option[Metadata])(implicit ctx: AnalysisContext): Unit = {
    val newData = initExp(e, m)
    updateProperties(e, newData)
  }
  final def setMetadata(e: Exp[Any], m: Metadata)(implicit ctx: AnalysisContext): Unit = setMetadata(e, Some(m))

  /**
   * Add child information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
   */
  final def setChild(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: AnalysisContext): Unit = {
    val newData = initExp(e, None, p)
    updateProperties(e, newData)
  } 
  final def setField(struct: Exp[Any], p: Option[SymbolProperties], field: String)(implicit ctx: AnalysisContext): Unit = {
    val newData = initExp(struct, None, p, Some(field))
    updateProperties(struct, newData)
  }

  final def getProps(e: Exp[Any]): Option[SymbolProperties] = metadata.get(e)
  final def getMetadata(e: Exp[Any], k: String): Option[Metadata] = metadata.get(e).flatMap{p => p(k)}

  /**
   * Get child information for given symbol
   */
  final def getChild(e: Exp[Any]): Option[SymbolProperties] = metadata.get(e) match {
    case Some(p: ArrayProperties) => p.child
    case Some(_) =>
      warn("Attempted to get child of non-array token")
      None
    case None => None
  }
  final def getField(struct: Exp[Any], name: String): Option[SymbolProperties] = metadata.get(struct) match {
    case Some(p: StructProperties) => p.child(name)
    case Some(_) =>
      warn("Attempted to get field " + name + " of non-Struct symbol")
      None
    case None => None
  }

  def createMetadataFromType[A](tp: Manifest[A]): List[Metadata] = Nil

  /**
   * Set metadata based on symbol type
   */ 
  final def typeTip[A](e: Exp[Any], tp: Manifest[A])(implicit ctx: AnalysisContext) {
    val newProps = initSym(tp)
    updateProperties(e, newProps)
  }

  /**
   * Set child metadata based on child type
   * TODO: Should these be private?
   */
  final def childTypeTip[A](e: Exp[Any], childType: Manifest[A])(implicit ctx: AnalysisContext) {
    val newChild = initSym(childType)
    val newProps = initExp(e, None, Some(newChild))
    updateProperties(e, newProps)
  }
  final def fieldTypeTip[A](struct: Exp[Any], fieldType: Manifest[A], field: String)(implicit ctx: AnalysisContext) {
    val newField = initSym(fieldType)
    val newProps = initExp(struct, None, Some(newField), Some(field))
    updateProperties(struct, newProps)
  }

  // TODO: Error reporting for these can definitely be better
  // (This is kind of silly right now)
  private def reportIncompatible[T:Meetable](meet: String, a: T, b: T)(implicit ctx: AnalysisContext) {
    val op = if (ctx.defDesc == "") "A" else "In " + ctx.defDesc + ", a"
    val sym = {
      val opPaths = getPathAndLine(ctx.defPos).map{_._1}
      val symPaths = getPathAndLine(ctx.symPos).map{_._1}
      if (opPaths.zip(symPaths).map{a => a._1 == a._2}.reduce{_&&_})
        "on line " + getPathAndLine(ctx.symPos).map{_._2}.mkString(";")
      else
        "at " + quotePos(ctx.symPos)
    }
    fatalerr(quotePos(ctx.defPos) + ": " + op + "ttempted to " + meet + " incompatible metadata for symbol originally defined " + sym + "\n" + 
              "LHS metadata: " + makeString(a) + "\n" +
              "RHS metadata: " + makeString(b) + "\n"
            )
  }
  final def attemptMeet[T: Meetable](a: T, b: T)(implicit ctx: AnalysisContext): T = {
    if (canMeet(a,b)) { meet(a,b) }
    else {
      reportIncompatible("meet",a,b)
      (a) // unreachable
    }
  }
  final def attemptMergeLeft[T: Meetable](orig: T, upd: T)(implicit ctx: AnalysisContext): T = {
    if (canMergeLeft(orig,upd)) { mergeLeft(orig,upd) }
    else {
      reportIncompatible("merge",orig,upd)
      (orig) // unreachable
    }
  }

  /**
   * Merge previous metadata for token and new data, notifying update if changes occurred
   * During merge, new metadata overrides pre-existing data when possible
   */ 
  private def updateProperties(e: Exp[Any], newProps: SymbolProperties)(implicit ctx: AnalysisContext) {
    if (!metadata.contains(e)) {
      metadata += e -> newProps
      notifyUpdate(e)
    }
    else {
      val prevData = metadata(e)
      val newData = attemptMergeLeft(prevData, newProps)
      metadata(e) = newData
      if (!matches(prevData, newData)) notifyUpdate(e)
    }
  }

  private def initExp(
    e: Exp[Any], 
    data: Option[Metadata] = None,
    child: Option[SymbolProperties] = None,
    field: Option[String] = None
  )(implicit ctx: AnalysisContext): SymbolProperties = initSym(e.tp, data, child, field)

  private def initSym[A](
    mA: Manifest[A], 
    data: Option[Metadata] = None,
    child: Option[SymbolProperties] = None,
    field: Option[String] = None
  )(implicit ctx: AnalysisContext): SymbolProperties = {
    val givenData = PropertyMap(data.map{m => m.name -> Some(m)}.toList)
    val typeData = PropertyMap(createMetadataFromType(mA).map{m => m.name -> Some(m)}) 
    val symData = attemptMeet(givenData, typeData)

    mA match {
      case StructType(_,elems) =>
        val typeFields = PropertyMap(elems.map{elem => elem._1 -> Some(initSym(elem._2)) })

        val symFields = if (field.isEmpty) { typeFields }
                        else {
                          val givenField = PropertyMap(List(field.get -> child))
                          attemptMeet(givenField, typeFields)
                        }
        StructProperties(symFields, symData)
      case _ =>
        if (isSubtype(mA.erasure,classOf[DeliteMultiArray[_]]) || 
            isSubtype(mA.erasure,classOf[DeliteArray[_]])) 
        {
          val typeChild = mA.typeArguments match {
            case Nil => None
            case tps => 
              warn(tps.length == 1, "Array type arguments length " + tps.length + " != 1 --- " + tps)
              Some(initSym(tps.head))
          }
          val symChild = attemptMeet(child, typeChild)

          ArrayProperties(symChild, symData)
        }
          
        else
          ScalarProperties(symData)
    }
  }

  /**
   * Includes a whole bunch of metadata structure propagation information but 
   * no metadata instances
   * TODO: Are typeTip and childTypeTip still needed here?
  */
  final def processStructure[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    case Reify(s,_,_) => 
      setProps(e, getProps(s))
    
    case DeliteMultiArraySortIndices(_,_,_) => 
      childTypeTip(e, manifest[Int])
    
    case DeliteStringSplit(_,_,_) => 
      childTypeTip(e, manifest[String])
    
    case op@DeliteMultiArrayNew(_) => 
      childTypeTip(e, op.mA)

    case op@DeliteMultiArrayView(t,_,_,_) => 
      childTypeTip(e, op.mA)         // set child type from IR node
      setChild(e, getChild(t))       // set child type from target

    case op@DeliteMultiArrayPermute(ma,_) =>
      childTypeTip(e, op.mA)
      setChild(e, getChild(ma))

    case op@DeliteMultiArrayReshape(ma,_) =>       
      childTypeTip(e, op.mA)
      setChild(e, getChild(ma))

    case op@DeliteMultiArrayApply(ma,_) =>
      typeTip(e, op.mA)
      setProps(e, getChild(ma))

    case op@DeliteMultiArrayFromFunction(dims,_) =>
      childTypeTip(e, op.mA)
      setChild(e, getProps(op.body.res))

    case op@DeliteMultiArrayMap(ma,_) =>
      childTypeTip(e, op.mB)
      setChild(e, getProps(op.body.res))

    case op@DeliteMultiArrayZipWith(ma,mb,_) =>
      childTypeTip(e, op.mT)
      setChild(e, getProps(op.body.res))

    case op@DeliteMultiArrayReduce(ma,_,_) =>
      setProps(e, getProps(op.body.res))

    case op@DeliteMultiArrayNDMap(in,_,_) =>
      childTypeTip(op.fin, op.mA)
      setChild(op.fin, getChild(in))
      childTypeTip(e, op.mB)
      setChild(e, getChild(op.body.res))

    case op@DeliteMultiArrayMapFilter(ma,_,_) =>
      childTypeTip(e, op.mB)
      setChild(e, getProps(op.mapFunc.res))

    case op@DeliteMultiArrayFlatMap(ma,_) =>
      childTypeTip(e, op.mB)
      setChild(e, getChild(op.body.res))

    case op@DeliteMultiArrayUpdate(ma,_,x) => 
      ctx.setSymPos(ma)
      val updatedChild = attemptMeet(getChild(ma), getProps(x))
      setChild(ma, updatedChild)

    // Struct ops
    case op@Struct(_,elems) => 
      // TODO: Assuming here that struct-ness of e is caught in initSym
      // Is this always the case?
      elems foreach {elem => fieldTypeTip(e, elem._2.tp, elem._1) }
      elems foreach {elem => setField(e, getProps(elem._2), elem._1) }

    case op@FieldApply(struct, field) => 
      setProps(e, getField(struct, field))

    case op@FieldUpdate(struct, field, rhs) => 
      ctx.setSymPos(struct)
      val updatedField = attemptMeet(getField(struct, field), getProps(rhs))
      setField(struct, updatedField, field)

    case op@NestedFieldUpdate(struct, fields, rhs) => 
      var newChild: SymbolProperties = StructProperties(new PropertyMap(fields.last -> getProps(rhs)), NoData)
      var i = fields.length - 1
      while (i > 0) {
        val children = new PropertyMap(fields(i) -> Some(newChild))
        newChild = StructProperties(children, NoData)
        i -= 1
      }
      
      ctx.setSymPos(struct)
      val updatedChild = attemptMeet(getField(struct, fields.head), Some(newChild))
      setField(struct, updatedChild, fields.head)

    // Misc. Delite ops
    case op:DeliteOpCondition[_] => 
      setProps(e, attemptMeet(getProps(op.thenp.res), getProps(op.elsep.res)))

    case op:DeliteOpWhileLoop =>
      setProps(e, getProps(op.body.res))


    // TODO: Anything general to be done for +=, -=, *=, and /= ?
    case ReadVar(Variable(v)) => 
      setProps(e, getChild(v))
    case NewVar(init) => 
      setProps(e, getProps(init))
    case Assign(Variable(v),rhs) =>  
      val updatedProps = attemptMeet(getProps(v), getProps(rhs))
      setProps(v, updatedProps)

    // Atomic Writes
    case op@NestedAtomicWrite(s,trace,d) => 
      var newProps: Option[SymbolProperties] = getAtomicWriteRhsProps(d)

      for (t <- trace.reverse) { t match {
        case StructTracer(field) => newProps = StructProperties(new PropertyMap(field -> newProps), NoData)
        case ArrayTracer(_) =>      newProps = ArrayProperties(newProps, NoData)
        case MultiArrayTracer(_) => newProps = ArrayProperties(newProps, NoData)
      }}

      ctx.setSymPos(s)
      val updatedProps = attemptMeet(newProps, getProps(s))
      setProps(s, updatedProps)

    case _ => 
      //warn(quotePos(ctx.defPos) + ": Unrecognized def " + d)
      // Nothing
  }

  // TODO: Add others as they're converted to AtomicWrites
  private def getAtomicWriteRhsProps(d: AtomicWrite): Option[SymbolProperties] = d match {
    //case FieldUpdate(_,_,rhs) => getProps(rhs)
    case DeliteMultiArrayUpdate(_,_,x) => getProps(x)
    case DeliteMultiArrayInsert(_,x,_) => getProps(x)
    case DeliteMultiArrayInsertAll(_,x,_,_) => getChild(x)
    case DeliteMultiArrayRemove(_,_,_,_) => None
    case _ => 
      warn("No RHS rule given for atomic write op " + d)
      None
  }

  def nameDef(d: Def[_]): String = d match {
    case Reflect(d,_,_) => nameDef(d)
    case Reify(_,_,_) => "Reify"
    case DeliteMultiArraySortIndices(_,_,_) => "sort indices"
    case DeliteStringSplit(_,_,_) => "string split"
    case DeliteMultiArrayNew(_) => "new MultiArray"
    case DeliteMultiArrayView(_,_,_,_) => "MultiArray slice"
    case DeliteMultiArrayPermute(_,_) => "MultiArray permute"
    case DeliteMultiArrayReshape(_,_) => "MultiArray reshape"    
    case DeliteMultiArrayApply(_,_) => "MultiArray apply"
    case DeliteMultiArrayFromFunction(_,_) => "MultiArray from function"
    case DeliteMultiArrayMap(_,_) => "MultiArray map"
    case DeliteMultiArrayZipWith(_,_,_) => "MultiArray zip-with"
    case DeliteMultiArrayReduce(_,_,_) => "MultiArray reduce"
    case DeliteMultiArrayForeach(_,_) => "MultiArray foreach"
    case DeliteMultiArrayNDMap(_,_,_) => "MultiArray flatmap"
    case DeliteMultiArrayMapFilter(_,_,_) => "array filter"
    case DeliteMultiArrayFlatMap(_,_) => "array flatmap"
    case DeliteMultiArrayUpdate(_,_,_) => "MultiArray update"
    case Struct(_,_) => "new struct"
    case FieldApply(_, field) => "struct field (" + field + ") apply"
    case FieldUpdate(_, field, _) => "struct field (" + field + ") update"
    case NestedFieldUpdate(_, _, _) => "struct nested field update"
    case _: DeliteOpCondition[_] => "if-then-else"
    case _: DeliteOpWhileLoop => "sequential loop"
    case _ => d.toString
  }
}