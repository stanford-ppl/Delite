package ppl.delite.framework.analysis

import scala.language.reflectiveCalls

import java.io._
import scala.collection.mutable.{HashMap,HashSet}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteMultiArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

trait AnalysisBase extends FatBlockTraversal with DeliteMetadata {
  val IR: DeliteOpsExp
  import IR._

  //////////////////////////////////////////////////////////////
  // Functions and fields which probably will never need to be 
  // overridden have the 'final' qualifier. Most others need to
  // be overriden/implemented

  val name: String                      // name of analysis
  val MAX_ITERS: Int = 10               // maximum number of iterations to run
  final var iter = 0                    // Current analysis iteration
  final var changed: Boolean = false    // Flag for if any metadata has changed
  final var hadErrors: Boolean = false  // Flag for if metadata has encountered (non-fatal) errors

  var metadata = new HashMap[Exp[Any],SymbolProperties]()

  def run[A](b: Block[A]): Unit = {
    result("Beginning...")

    do {
      iter += 1
      changed = false
      log("Starting iteration " + iter)
      traverseBlock(b)
    } while (changed && iter <= MAX_ITERS)

    result("Completed.")
  }    

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(s, Reflect(d,_,_)) => processStructure(s,d)
      case TP(s, d) => processStructure(s,d)
      case _ => ()
    }
    origTraversal(stm)
  }
  final def origTraversal(stm: Stm): Unit = super.traverseStm(stm)

  // will be called after processStructure completes
  def processOp[A](e: Exp[A], d: Def[_]): Unit

  
  ////////////////////////////////
  // Metadata completeness checks

  /** 
   * Test if all metadata for a symbol has been filled in 
   * Likely will want different cases for different symbol types,
   * but can override SymbolProperties version instead to test all three
   * without code duplication
  */ 
  def dataComplete(e: Exp[Any]): Boolean = metadata.get(e) match {
    case Some(a: ScalarProperties) => infoComplete(a)
    case Some(a: StructProperties) => infoComplete(a)
    case Some(a: MultiArrayProperties) => infoComplete(a)
    case None => false
  }
  def dataComplete(a: ScalarProperties): Boolean = true
  def dataComplete(a: StructProperties): Boolean = true
  def dataComplete(a: MultiArrayProperties): Boolean = true

  /** 
   * Traverse IR checking all encountered symbols for metadata completeness
  */
  final def checkCompleteness[A](b: Block[A]) = {
    val checker = new FatBlockTraversal {
        val incompleteSet = new HashSet[Exp[Any]]()
        override def traverseStm(stm: Stm): Unit = {
          stm match {
            case TP(s,_) if !isComplete(s) => incompleteSet += s
            case _ => // Nothing
          }
          super.traverseStm(stm)
        }
      }
    checker.traverseBlock(b)
    checker.incompleteSet
  }
  
  /**
   * Testing completeness on metadata containers
  */
  final def isComplete(a: Option[SymbolProperties]): Boolean = a match {
    case Some(am) => isComplete(am)
    case None => false
  }
  final def isComplete(a: SymbolProperties): Boolean = a match {
    case a: ScalarProperties => dataComplete(a)
    case a: StructProperties => 
      a.fields.map{f => isComplete(a.child(f))}.reduce{_&&_} && dataComplete(a)
    case a: MultiArrayProperties => 
      isComplete(a.child) && dataComplete(a)
  }

  /** 
   * Testing completeness / presence of symbol metadata mapping
   * Specifically ignore 
  */ 
  final def isComplete(e: Exp[Any]) = {
    if (e.tp == manifest[Unit] || e.tp == manifest[Nothing]) 
      true
    else
      isComplete(metadata.get(e))
  }
  
  final def isUnknown(e: Exp[Any]) = {
    if (e.tp == manifest[Unit] || e.tp == manifest[Nothing])
      false
    else
      !metadata.contains(e)
  }

  /////////////////////////////////
  // Helper functions for analysis

  final def strDef(e: Exp[Any]) = e match {
    case Const(z) => z.toString
    case _ => 
      // TODO: use cases instead of findDefinition
      val z = findDefinition(e.asInstanceOf[Sym[Any]])
      if (!z.isDefined) "(bound " + e.toString + ")" else e.toString + " = " + z.get.toString
  }

  final def typeTip[A](e: Exp[Any], tp: Manifest[A]) {
    //val newData = initExp(e)
    val newData = initSym(tp)
    updateProperties(e, newData)
  }

  /**
   * Directly add symbol property metadata for symbol
   * (Such as from apply nodes)
  */ 
  final def setProps(e: Exp[Any], p: Option[SymbolProperties]) {
    if (!p.isEmpty) {
      updateProperties(e, p.get)
    }
  }

  final def getProps(e: Exp[Any]): Option[SymbolProperties] = metadata.get(e)

  /**
   * Add metadata information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
  */
  final def setMetadata(e: Exp[Any], m: Option[Metadata]): Unit = {
    if (m.isDefined) setMetadata(e, m.get)
    else {
      // No new info here - just initialize expression's metadata
      val newData = initExp(e)
      updateProperties(e, newData)
    }
  }
  final def setMetadata(e: Exp[Any], m: Metadata): Unit = {
    val info = Seq(metakey(m) -> Some(m))
    val newData = initExp(e, info)
    updateProperties(e, newData)
  }
  /**
   * Add child information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
  */
  final def setChild(e: Exp[Any], p: Option[SymbolProperties]): Unit = {
    val newData = initExp(e, Nil, p)
    updateProperties(e, newData)
  } 
  final def setField(e: Exp[Any], p: Option[SymbolProperties], name: String): Unit = {
    val newData = initExp(e, Nil, p, name)
    updateProperties(e, newData)
  }
  /**
   * Set child based on child type
  */
  final def childTypeTip[A](e: Exp[Any], childType: Manifest[A], name: String = "") {
    val newData = initExp(e, Nil, initSym(childType), name)
    updateProperties(e, newData)
  }

  final def getChild(e: Exp[Any], name: String = ""): Option[SymbolProperties] = metadata.get(e) match {
    case Some(p: MultiArrayProperties) => p.child
    case Some(_) =>
      warn("Attempted to get child of non-MultiArray symbol")
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

  final def isMutable(e: Exp[Any]): Boolean = e match {
    case s: Sym[_] => isWritableSym(s)
    case _ => false
  }

  def isDataStructure[A](mA: Manifest[A]): Boolean = mA match {
    case StructType(_,_) => true
    case _ => isSubType(mA.erasure,classOf[DeliteMultiArray[_]])
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

  final def check(cond: Boolean, x: => Any, c: SourceContext = null) {
    if (!cond)  {
      val ctx = if (c == null) "" else c.fileName.split("/").last + ":" + c.line + ": "
      error(ctx + x)
    }
  }

  private def updateProperties(e: Exp[Any], newData: SymbolProperties) {
    if (!metadata.contains(e)) {
      metadata += e -> newData
      notifyUpdate()
    }
    else {
      val prevData = metadata(e)
      if (prevData canBeUpdatedWith newData)
        metadata(e) = (prevData updateWith newData)
      else {
        fatalerr("Attempted to merge incompatible metadata in info update for symbol:\n" + 
                  strDef(e) + "\nat: " + quotePos(e) + "\n" + 
                  "Prev metadata: " + prevData.makeString() + "\n" +
                  "New  metadata: " + newData.makeString() + "\n"
                )
      }

      if (!(prevData matches newData)) notifyUpdate()
    }
  }

  private def initSym[A](
    mA: Manifest[A], 
    info: Iterable[(String, Option[Metadata])] = Nil, 
    child: Option[SymbolProperties] = None,
    field: String = ""
  ): SymbolProperties = mA match {
    case StructType(_,elems) =>
      val fieldData = elems.map{
        elem => 
          val metadata = if (field == elem._1) child else Some(initSym(elem._2))
          elem._1 -> metadata
      } 
      StructProperties(fieldData, info)
    case _ =>
      if isSubType(mA.erasure,classOf[DeliteMultiArray[_]]) 
        MultiArrayProperties(child, info)
      else
        ScalarProperties(info)
  }

  private def initExp(e: Exp[Any], info: Iterable[(String, Option[Metadata])] = Nil): SymbolProperties = e match {
    case Const(_) => ScalarProperties(info)
    case _ => initSym(e.tp, info)
  }

  final def notifyUpdate() { changed = true }


  /**
   * Includes a whole bunch of metadata structure propagation information but 
   * no metadata instances
  */

  final def processStructure(e: Exp[A], d: Def[_]): Unit = {
    d match {
      case op @ DeliteMultiArraySortIndices(_,_) => 
        typeTip(e, op.mR)
        childTypeTip(e, manifest[Int])
  
      case op @ DeliteStringSplit(_,_,_) => 
        typeTip(e, op.mR)
        childTypeTip(e, manifest[String])

      case op @ DeliteMultiArrayNew(_) =>
        typeTip(e, op.mR)
        childTypeTip(e, op.mA)

      case op @ DeliteMultiArrayView(t,_,_,_) => 
        typeTip(e, op.mR)
        childTypeTip(e, op.mA)         // set child type from IR node
        setChild(e, getChild(t))       // set child type from target

      case op @ DeliteMultiArrayPermute(ma,_) =>
        typeTip(e, op.mR)
        childTypeTip(e, op.mA)
        setChild(e, getChild(ma))

      case op @ DeliteMultiArrayReshape(ma,_) =>       
        typeTip(e, op.mR)
        childTypeTip(e, op.mA)
        setChild(e, getChild(ma))

      case op @ DeliteMultiArrayApply(ma,_) =>
        typeTip(e, op.mR)
        setProps(e, getChild(ma))

      case op @ DeliteMultiArrayFromFunction(dims,_) =>
        typeTip(e, op.mR)
        childTypeTip(e, op.mA)
        setChild(e, getProps(op.body.res))

      case op @ DeliteMultiArrayMap(ma,_) =>
        typeTip(op.a, op.mA)
        setProps(op.a, getChild(ma))
        typeTip(e, op.mR)
        childTypeTip(e, op.mT)
        setChild(e, getProps(op.body.res))

      case op @ DeliteMultiArrayZipWith(ma,mb,_) =>
        typeTip(op.a, op.mA)
        typeTip(op.b, op.mB)
        setProps(op.a, getChild(ma))
        setProps(op.b, getChild(mb))

        typeTip(e, op.mR)
        childTypeTip(e, op.mT)
        setChild(e, getProps(op.body.res))

      case op @ DeliteMultiArrayReduce(ma,_,_) =>
        typeTip(op.a1, op.mA)
        typeTip(op.a2, op.mA)
        setProps(op.a1, getChild(ma))
        setProps(op.a2, getChild(ma))

        typeTip(e, op.mR)
        setProps(e, getProps(op.body.res))

      case op @ DeliteMultiArrayForeach(ma,_) =>
        typeTip(op.a, op.mA)
        updateProps(op.a, getChild(ma))

      case op @ DeliteMultiArrayNDMap(in,_,_) =>
        childTypeTip(op.ma, op.mA)
        setChild(op.ma, getChild(in))

        typeTip(e, op.mR)
        childTypeTip(e, op.mB)
        setChild(e, getChild(op.body.res))

      case op @ DeliteMultiArrayMapFilter(ma,_,_) =>
        typeTip(op.a, op.mA)
        setProps(op.a, getChild(ma))

        typeTip(e, op.mR)
        childTypeTip(e, op.mB)
        setChild(e, getProps(op.body.res))

      case op @ DeliteMultiArrayFlatMap(ma,_) =>
        typeTip(op.a, op.mA)
        setProps(op.a, getChild(ma))

        typeTip(e, op.mR)
        childTypeTip(e, op.mB)
        setChild(e, getChild(op.body.res))

      case op @ DeliteMultiArrayUpdate(ma,_,x) => 
        val updatedChild = meet(getChild(ma), getProps(x))
        setChild(ma, updatedChild)

      case op @ DeliteMultiArrayMutableMap(ma,_) =>
        typeTip(op.a, op.mA)
        setProps(op.a, getChild(ma))

        val updatedChild = meet(getChild(ma), getProps(op.body.res))
        setChild(ma, updatedChild)

      case op @ DeliteMultiArrayMutableZipWith(ma,mb,_) =>
        typeTip(op.a, op.mA)
        typeTip(op.b, op.mB)
        setProps(op.a, getChild(ma))
        setProps(op.b, getChild(mb))

        val updatedChild = meet(getChild(ma), getProps(op.body.res))
        setChild(ma, updatedChild)

      // Struct ops

      case op @ Struct(_,elems) => 
        // TODO: Assuming here that struct-ness of e is caught in initSym
        // Is this always the case?
        elems foreach {elem => setField(e, getProps(e._2), elem._1) }

      case op @ FieldApply(struct, field) => 
        setProps(e, getField(struct, field))

      case op @ FieldUpdate(struct, field, rhs) => 
        val updatedField = meet(getField(struct, field), getProps(rhs))
        setField(struct, updatedField, field)

      case op @ NestedFieldUpdate(struct, fields, rhs) => 
        var newChild = StructProperties(Seq(fields.last -> getProps(rhs)))
        var i = fields.length - 1
        while (i > 0) {
          newData = StructProperties(Seq(fields(i) -> newData))
          i -= 1
        }
        val updatedChild = meet(getField(struct, fields.first), newChild)
        setField(struct, updatedChild, fields.first)

      // Misc. Delite ops
      case DeliteIfThenElse(cond, thenp, elsep, _) => 
        setProps(e, meet(getProps(thenp), getProps(elsep)))

      case DeliteWhile(cond, body) =>
        setProps(e, getProps(body))

      // TODO: Vars have to be dealt with separately?

      case _ => 
        // Nothing
    }
    processOp(e,d)
  }
}