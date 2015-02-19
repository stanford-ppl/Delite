package ppl.delite.framework.analysis

import scala.language.reflectiveCalls

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

  //////////////////////////////////////////////////////////////
  // Functions and fields which probably will never need to be 
  // overridden have the 'final' qualifier. Most others need to
  // be overriden/implemented

  val name: String                      // name of analysis
  val MAX_ITERS: Int = 10               // maximum number of iterations to run
  final var iter = 0                    // Current analysis iteration
  final var hadErrors: Boolean = false  // Flag for if metadata has encountered (non-fatal) errors

  var metadata = new HashMap[Exp[Any],SymbolProperties]()

  final var changed: Boolean = false    // Flag for if any metadata has changed
  final var changedSyms = new HashSet[Exp[Any]]()
  final def notifyUpdate(e: Exp[Any]) { changedSyms += e; changed = true }

  
  case class AnalysisContext(symPos: List[SourceContext], defPos: SourceContext, defDesc: String) {
    def withSymPos(ctx: SourceContext) = new AnalysisContext(ctx, defPos, defDesc)
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
  def analyzeStm(stm: Stm): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => processStructure(s,d); processOp(s,d);
    case TP(s, d) => processStructure(s,d); processOp(s,d);
    case _ => // Nothing
  }

  /**
   * Operate on the current statement. Overriden from FatBlockTraversal
   */
  final override def traverseStm(stm: Stm): Unit = {
    analyzeStm(stm)
    super.traverseStm(stm)
  }

  ////////////////////////////////
  // Metadata completeness checks

  /** 
   * Traverse IR checking all encountered symbols for metadata completeness
   */
  final def checkCompleteness[A](b: Block[A]): List[Exp[Any]] = {
    val checker = 
      new FatBlockTraversal {
        val IR: AnalysisBase.this.IR.type = AnalysisBase.this.IR
        val incompleteSet = new HashSet[Exp[Any]]()
        
        override def traverseStm(stm: Stm): Unit = {
          stm match {
            case TP(s,_) if !completed(s) => incompleteSet += s
            case _ => // Nothing
          }
          super.traverseStm(stm)
        }
      }
    checker.traverseBlock(b)
    checker.incompleteSet.toList
  }

  /** 
   * Testing completeness / presence of symbol metadata mapping
   */ 
  final def completed(e: Exp[Any]): Boolean = metadata.get(e) match {
    case Some(p) => isComplete(p)
    case None => isComplete(initExp(e))
  }
  final def unknown(e: Exp[Any]): Boolean = !metadata.contains(e)

  /////////////////////////////////
  // Helper functions for analysis

  final def strDef(e: Exp[Any]) = e match {
    case Const(z) => z.toString
    case Def(d) => e.toString + " = " + d.toString
    case _ => "(bound " + e.toString + ")"
  }

  final def hasReflectMutable(e: Exp[Any]): Boolean = e match {
    case s: Sym[_] => isWritableSym(s)
    case _ => false
  }

  final def isDataStructure[A](mA: Manifest[A]): Boolean = mA match {
    case StructType(_,_) => true
    case _ => isSubtype(mA.erasure,classOf[DeliteMultiArray[_]])
  }

  final def verbose = Config.debug
  final def log(x: String, tagged: Boolean = true) = {
    val str = (if (tagged) "[" + name + "]" else "") + x
    /* if (verbose) */  Predef.println(str) /* else () */
  }
  final def result(x: String, tagged: Boolean = true) = Predef.println((if (tagged) "[" + name + "]" else "") + x)

  final def warn(x: =>Any) { System.err.println("[\u001B[33mwarn\u001B[0m] " + name + ": " + x) }
  final def error(x: =>Any) { System.err.println("[\u001B[31merror\u001B[0m] " + name + ": " + x); hadErrors = true }
  final def fatalerr(x: =>Any) { System.err.println("[\u001B[31merror\u001B[0m] " + name + ": " + x); System.exit(0) }

  final def check(cond: Boolean, x: => Any)(implicit ctx: AnalysisContext) {
    if (!cond)  {
      val ctx = if (lhs == null) "" else quotePos(lhs) + ": "
      error(ctx + x)
    }
  }

  /**
   * Directly add symbol property metadata for symbol
   */ 
  final def setProps(e: Exp[Any], p: Option[SymbolProperties]) {
    if (p.isDefined) { updateProperties(e, p.get) }
  }

  /**
   * Add metadata information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
   */
  final def setMetadata(e: Exp[Any], m: Option[Metadata])(implicit ctx: AnalysisContext): Unit = {
    val newData = initExp(e, new PropertyMap(m.map{m.name -> Some(m)}.toList))
    updateProperties(e, newData)
  }
  final def setMetadata(e: Exp[Any], m: Metadata)(implicit ctx: AnalysisContext): Unit = setMetadata(e, Some(m))

  /**
   * Add child information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
   */
  final def setChild(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: AnalysisContext): Unit = {
    val newData = initExp(e, NoData, p)
    updateProperties(e, newData)
  } 
  final def setField(e: Exp[Any], p: Option[SymbolProperties], name: String)(implicit ctx: AnalysisContext): Unit = {
    val newData = initExp(e, NoData, p, name)
    updateProperties(e, newData)
  }

  final def getProps(e: Exp[Any]): Option[SymbolProperties] = metadata.get(e)
  final def getMetadata(e: Exp[Any], k: String): Option[Metadata] = metadata.get(e) match { case Some(p) => p(k) case None => None}

  /**
   * Get child information for given symbol
   */
  final def getChild(e: Exp[Any]): Option[SymbolProperties] = metadata.get(e) match {
    case Some(p: ArrayProperties) => p.child
    case Some(_) =>
      warn("Attempted to get child of non-Array symbol")
      None
    case None => None
  }
  final def getField(e: Exp[Any], name: String): Option[SymbolProperties] = metadata.get(e) match {
    case Some(p: StructProperties) => p.child(name)
    case Some(_) =>
      warn("Attempted to get field " + name + " of non-Struct symbol")
      None
    case None => None
  }

  def createMetadataFromType[A](tp: Manifest[A]): List[Metadata] = Nil

  final def createPropertiesFromType[A](tp: Manifest[A]): PropertyMap[Metadata] = {
    new PropertyMap(createMetadataFromType(tp).map{m => m.name -> Some(m)})  
  }

  final def typeTip[A](e: Exp[Any], tp: Manifest[A])(implicit ctx: AnalysisContext) {
    val newProps = initSym(e, tp)
    val newData = initExp(e, newProps)
    updatedProperties(e, newData)
  }

  /**
   * Set child metadata based on child type
   * TODO: Should these be private?
   */
  final def childTypeTip[A](e: Exp[Any], childType: Manifest[A])(implicit ctx: AnalysisContext) {
    val childTpProps = createPropertiesFromType(childType)
    val newChild = initSym(childType, childTpProps)
    val newData = initExp(e, NoData, Some(newChild))
    updateProperties(e, newData)
  }
  final def fieldTypeTip[A](e: Exp[Any], fieldtype: Manifest[A], field: String)(implicit ctx: AnalysisContext) {
    val childTpProps = createPropertiesFromType(childType)
    val newChild = initSym(childType, childTpProps)
    val newData = initExp(e, NoData, Some(newChild), field)
    updateProperties(e, newData)
  }

  // TODO: This is directly copied from quotePos in expressions... could move this to Expressions?
  private def quotePos(ctx: List[SourceContext]): String = ctx match {
    case Nil => "<unknown>"
    case cs => 
      def all(cs: SourceContext): List[SourceContext] = cs.parent match {
        case None => List(cs)
        case Some(p) => cs::all(p)
      }
    cs.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
  }

  // TODO: Error reporting for these can definitely be better
  // (This is kind of silly right now)
  final def attemptMeet[T: Meetable](a: T, b: T)(implicit ctx: AnalysisContext): T = {
    if (canMeet(a,b)) { meet(a,b) }
    else {
      val op = if (ctx.opDesc == "") "A" else "In " + ctx.opDesc + ", a"
      fatalerr(ctx.opPos + ": " + op + "ttempted to meet incompatible metadata for symbol defined at " + quotePos(ctx.symPos) + "\n" +
                "Prev metadata: " + makeString(a) + "\n" + 
                "New  metadata: " + makeString(b)
              )
      (a) // unreachable
    }
  }
  final def attemptMergeLeft[T: Meetable](orig: T, upd: T)(implicit ctx: AnalysisContext): T = {
    if (canMergeLeft(orig,upd)) { mergeLeft(orig,upd) }
    else {
      val op = if (opDesc == "") "A" else "In " + ctx.opDesc + ", a"
      fatalerr(ctx.opPos + ": " + op + "ttempted to merge incompatible metadata for symbol defined at " + quotePos(ctx.symPos) + "\n" + 
                "Prev metadata: " + makeString(orig) + "\n" +
                "New  metadata: " + makeString(upd) + "\n"
              )
      (a) // unreachable
    }
  }

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

  private def initSym[A](
    mA: Manifest[A], 
    data: PropertyMap[Metadata] = NoData,
    child: Option[SymbolProperties] = None,
    field: String = ""
  )(implicit ctx: AnalysisContext): SymbolProperties = {
    val typeData = createPropertiesFromType(mA)
    val symData = attemptMeet(data, typeData)

    mA match {
      case StructType(_,elems) =>
        val fieldData = elems.map{ elem => 
            val metadata = if (field == elem._1) child else Some(initSym(elem._2))
            elem._1 -> metadata
        } 
        StructProperties(PropertyMap(fieldData), data)
      case _ =>
        if (isSubtype(mA.erasure,classOf[DeliteMultiArray[_]]))
          ArrayProperties(child, data)
        else
          ScalarProperties(data)
    }
  }

  private def initExp(
    e: Exp[Any], 
    data: PropertyMap[Metadata] = NoData,
    child: Option[SymbolProperties] = None,
    field: String = ""
  )(implicit ctx: AnalysisContext): SymbolProperties = e match { 
    case Const(_) => ScalarProperties(data)
    case _ => initSym(e.tp, data, child, field)
  }

  /**
   * Includes a whole bunch of metadata structure propagation information but 
   * no metadata instances
  */
  final def processStructure[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    case Reify(s,_,_) => 
      setProps(e, getProps(s))
    
    case DeliteMultiArraySortIndices(_,_) => 
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

    case DeliteMultiArrayApply(ma,_) =>
      setProps(e, getChild(ma))

    case op@DeliteMultiArrayFromFunction(dims,_) =>
      childTypeTip(e, op.mA)
      setChild(e, getProps(op.body.res))

    case op@DeliteMultiArrayMap(ma,_) =>
      typeTip(op.a, op.mA)
      setProps(op.a, getChild(ma))
      childTypeTip(e, op.mB)
      setChild(e, getProps(op.body.res))

    case op@DeliteMultiArrayZipWith(ma,mb,_) =>
      typeTip(op.a, op.mA)
      typeTip(op.b, op.mB)
      setProps(op.a, getChild(ma))
      setProps(op.b, getChild(mb))
      childTypeTip(e, op.mT)
      setChild(e, getProps(op.body.res))

    case op@DeliteMultiArrayReduce(ma,_,_) =>
      typeTip(op.a1, op.mA)
      typeTip(op.a2, op.mA)
      setProps(op.a1, getChild(ma))
      setProps(op.a2, getChild(ma))
      setProps(e, getProps(op.body.res))

    case op@DeliteMultiArrayForeach(ma,_) =>
      typeTip(op.a, op.mA)
      setProps(op.a, getChild(ma))

    case op@DeliteMultiArrayNDMap(in,_,_) =>
      childTypeTip(op.ma, op.mA)
      setChild(op.ma, getChild(in))
      childTypeTip(e, op.mB)
      setChild(e, getChild(op.body.res))

    case op@DeliteMultiArrayMapFilter(ma,_,_) =>
      typeTip(op.a, op.mA)
      setProps(op.a, getChild(ma))
      childTypeTip(e, op.mB)
      setChild(e, getProps(op.mapFunc.res))

    case op@DeliteMultiArrayFlatMap(ma,_) =>
      typeTip(op.a, op.mA)
      setProps(op.a, getChild(ma))
      childTypeTip(e, op.mB)
      setChild(e, getChild(op.body.res))

    case op@DeliteMultiArrayUpdate(ma,_,x) => 
      val updatedChild = attemptMeet(getChild(ma), getProps(x))(ctx.withSymPos(ma.pos))
      setChild(ma, updatedChild)

    case op@DeliteMultiArrayMutableMap(ma,_) =>
      typeTip(op.a, op.mA)
      setProps(op.a, getChild(ma))

      val updatedChild = attemptMeet(getChild(ma), getProps(op.body.res))(ctx.withSymPos(ma.pos))
      setChild(ma, updatedChild)

    case op@DeliteMultiArrayMutableZipWith(ma,mb,_) =>
      typeTip(op.a, op.mA)
      typeTip(op.b, op.mB)
      setProps(op.a, getChild(ma))
      setProps(op.b, getChild(mb))

      val updatedChild = attemptMeet(getChild(ma), getProps(op.body.res))(ctx.withSymPos(ma.pos))
      setChild(ma, updatedChild)

    // Struct ops

    case op@Struct(_,elems) => 
      // TODO: Assuming here that struct-ness of e is caught in initSym
      // Is this always the case?
      elems foreach {elem => setField(e, getProps(elem._2), elem._1) }

    case op@FieldApply(struct, field) => 
      setProps(e, getField(struct, field))

    case op@FieldUpdate(struct, field, rhs) => 
      val updatedField = attemptMeet(getField(struct, field), getProps(rhs))(ctx.withSymPos(struct.pos))
      setField(struct, updatedField, field)

    case op@NestedFieldUpdate(struct, fields, rhs) => 
      var newChild: SymbolProperties = StructProperties(new PropertyMap(fields.last -> getProps(rhs)), NoData)
      var i = fields.length - 1
      while (i > 0) {
        val children = new PropertyMap(fields(i) -> Some(newChild))
        newChild = StructProperties(children, NoData)
        i -= 1
      }
      
      val updatedChild = attemptMeet(getField(struct, fields.head), Some(newChild))(ctx.withSymPos(struct.pos))
      setField(struct, updatedChild, fields.head)

    // Misc. Delite ops
    case op:DeliteOpCondition[_] => 
      setProps(e, attemptMeet(getProps(op.thenp.res), getProps(op.elsep.res)))

    case op:DeliteOpWhileLoop =>
      setProps(e, getProps(op.body.res))

    // TODO: Vars have to be dealt with separately?

    case _ => 
      // Nothing
  }

  def nameDef(d: Def[_]): String = d match {
    case Reflect(d,_,_) => nameDef(d)
    case Reify(_,_,_) => "Reify"
    case DeliteMultiArraySortIndices(_,_) => "Sort Indices"
    case DeliteStringSplit(_,_,_) => "String Split"
    case DeliteMultiArrayNew(_) => "New MultiArray"
    case DeliteMultiArrayView(_,_,_,_) => "MultiArray Slice"
    case DeliteMultiArrayPermute(_,_) => "MultiArray Permute"
    case DeliteMultiArrayReshape(_,_) => "MultiArray Reshape"    
    case DeliteMultiArrayApply(_,_) => "MultiArray Apply"
    case DeliteMultiArrayFromFunction(_,_) => "MultiArray From Function"
    case DeliteMultiArrayMap(_,_) => "MultiArray Map"
    case DeliteMultiArrayZipWith(_,_,_) => "MultiArray Zip-With"
    case DeliteMultiArrayReduce(_,_,_) => "MultiArray Reduce"
    case DeliteMultiArrayForeach(_,_) => "MultiArray Foreach"
    case DeliteMultiArrayNDMap(_,_,_) => "MultiArray Flat Map"
    case DeliteMultiArrayMapFilter(_,_,_) => "Array Filter"
    case DeliteMultiArrayFlatMap(_,_) => "Array Flat Map"
    case DeliteMultiArrayUpdate(_,_,_) => "MultiArray Update"
    case DeliteMultiArrayMutableMap(_,_) => "MultiArray Mutable Map"
    case DeliteMultiArrayMutableZipWith(_,_,_) => "MultiArray Mutable Zip-With"
    case Struct(_,_) => "New Struct"
    case FieldApply(_, field) => "Struct Field (" + field + ") Apply"
    case FieldUpdate(_, field, _) => "Struct Field (" + field + ") Update"
    case NestedFieldUpdate(_, _, _) => "Struct Nested Field Update"
    case _: DeliteOpCondition[_] => "If-Then-Else"
    case _: DeliteOpWhileLoop => "While Loop"
    case _ => d.toString
  }
}