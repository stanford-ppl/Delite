package ppl.delite.framework.visit

import scala.reflect.SourceContext
import scala.collection.mutable.{HashMap,Stack}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.Expressions

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

import ppl.delite.framework.Util._
import Meetable._

// TODO: Find a better home for these
trait MiscMetadata {
  val IR: DeliteOpsExp
  import IR._

  def isMultiArrayTpe(x: Manifest[_]) = isSubtype(x.erasure, classOf[DeliteMultiArray[_]])

  def hasMultiArrayChild[T](tp: Manifest[T]): Boolean = tp match {
    case tp if isMultiArrayTpe(tp) => true 
    case StructType(_,elems) => elems.map{f => hasMultiArrayChild(f._2)}.fold(false){_||_}
    case tp => tp.typeArguments.map{f => hasMultiArrayChild(f)}.fold(false){_||_}
  }

  def isDataStructureType[T](tp: Manifest[T]): Boolean = tp match {
    case StructType(_,_) => true
    case tp if isDeliteArrayTpe(tp) => true
    case tp if isMultiArrayTpe(tp) => true
    case _ => false
  }

  def isMutable(e: Exp[Any]): Boolean = e match {
    case s: Sym[_] => isWritableSym(s)
    case _ => false
  }
}

trait MetadataTransformer extends MiscMetadata {
  val IR: DeliteOpsExp with DeliteMetadata
  import IR._
 
  def notifyUpdate(e: Exp[Any]): Unit

  var regionMetadata = new Stack[HashMap[Exp[Any], SymbolProperties]]()
  def metadata = regionMetadata.top

  protected object data {
    def contains(e: Exp[Any]) = (regionMetadata.indexWhere(_.contains(e)) >= 0)
    def update(e: Exp[Any], p: SymbolProperties) { if (regionMetadata.nonEmpty) metadata(e) = p }
    def get(e: Exp[Any]): Option[SymbolProperties] = {
      val i = regionMetadata.indexWhere(_.contains(e))
      if (i >= 0) regionMetadata(i).get(e) else None
    }
    def getOrElse(e: Exp[Any])(f: => SymbolProperties): SymbolProperties = get(e) match {
      case Some(p) => p
      case None => val p = f; update(e,p); (p)
    }
    def apply(e: Exp[Any]): SymbolProperties = get(e) match {
      case Some(p) => p
      case None => throw new NoSuchElementException
    }
  }

  protected def enterRegion() { regionMetadata.push(new HashMap[Exp[Any], SymbolProperties]()) }
  protected def exitRegion() { regionMetadata.pop() }
  protected def exitRegionWithPromotion() {
    val exitedRegion = regionMetadata.pop()
    regionMetadata.top ++= exitedRegion.filter{m => !regionMetadata.top.contains(m._1) } 
  }
  // TODO: should result of meet be written to location where mapping is, or top of stack?
  protected def exitRegionWithMeet(func: MeetFunc)(implicit ctx: SourceContext) {
    val exitedRegion = regionMetadata.pop()
    for ((e,p) <- exitedRegion) {
      val i = regionMetadata.indexWhere(_.contains(e))
      if (i == -1) { regionMetadata.top(e) = p }
      else         { regionMetadata.top(e) = attemptMeet(p, regionMetadata(i)(e), func) }
    }
  }
  protected def exitRegionWithOverwrite()(implicit ctx: SourceContext) { exitRegionWithMeet(func = MetaOverwrite) }

  // TODO: multiple inputs? formalize this
  final def chain(a: MetadataTransformer) { regionMetadata = a.regionMetadata }

  /////////////////////////////
  // Meet with error reporting

  final def attemptMeet[T: Meetable](a: T, b: T, func: MeetFunc)(implicit ctx: SourceContext): T = {
    if (canMeet(a,b,func)) { meet(a,b,func) }
    else {
      val inc = incompatibilities(a,b,func)
      if (!inc.isEmpty) {
        fatalerr(quotePos(ctx) + ": " + inc.head + "\n\t" + quoteCode(ctx).map{"\n\t" + _}.getOrElse("") + 
                  "LHS metadata: " + makeString(a) + "\n" +
                  "RHS metadata: " + makeString(b) + "\n")
      }
      else {
        // Old version of error reporting - remove later
        fatalerr(quotePos(ctx) + ": Attempted to meet incompatible metadata for symbol used here:\n" + 
                  "LHS metadata: " + makeString(a) + "\n" +
                  "RHS metadata: " + makeString(b) + "\n" + 
                  quoteCode(ctx).map{"\t" + _}.getOrElse("")
                )
      }
      (a) // unreachable
    }
  }

  def followTrace(p: SymbolProperties, trace: List[AtomicTracer]): SymbolProperties = {
    if (trace.isEmpty) { p }
    else (p, trace.head) match {
      case (p: StructProperties, StructTracer(field)) => followTrace(p.child(field).get, trace.tail)
      case (p: ArrayProperties, ArrayTracer(_)) => followTrace(p.child.get, trace.tail)
      case _ => sys.error("Error while following nested write trace in metadata")
    }    
  }

  /////////////////////////
  // Metadata / Property 
  // Get and Set Functions

  /**
   * Directly add symbol property metadata for symbol
   */ 
  final def setProps(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) {
    if (p.isDefined) { updateProperties(e, p.get) }
  }
  final def setProps(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    updateProperties(e, p)
  }

  /**
   * Add metadata information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
   */
  final def setMetadata(e: Exp[Any], m: Option[Metadata])(implicit ctx: SourceContext): Unit = {
    val newData = initExp(e, m)
    updateProperties(e, newData)
  }
  final def setMetadata(e: Exp[Any], m: Metadata)(implicit ctx: SourceContext): Unit = setMetadata(e, Some(m))

  final def copyMetadata(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext): Unit = {
    p.data.keys foreach {k => setMetadata(e, p(k))}
  }

  /**
   * Add child information for this symbol (possibly using meet)
   * Uses notifyUpdate() to note if symbol-metadata mapping has changed
   */
  final def setChild(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext): Unit = {
    val newData = initExp(e, None, p)
    updateProperties(e, newData)
  } 
  final def setField(struct: Exp[Any], p: Option[SymbolProperties], field: String)(implicit ctx: SourceContext): Unit = {
    val newData = initExp(struct, None, p, Some(field))
    updateProperties(struct, newData)
  }

  final def getProps(e: Exp[Any]): Option[SymbolProperties] = Some( data.getOrElse(e)(initExp(e)(mpos(e.pos))) ) 

  final def getMetadata(e: Exp[Any], k: String): Option[Metadata] = getProps(e).flatMap{_.apply(k)}

  /**
   * Get child information for given symbol
   */
  final def getChild(p: SymbolProperties): Option[SymbolProperties] = p match {
    case p: ArrayProperties => p.child
    case _ =>
      warn("This is likely a compiler bug! Attempted to get child of non-array symbol: \n" + makeString(p))
      None
  }
  final def getField(p: SymbolProperties, name: String): Option[SymbolProperties] = p match {
    case p: StructProperties => p.child(name)
    case _ => 
      warn("This is likely a compiler bug! Attempted to get field " + name + " of non-Struct symbol \n" + makeString(p))
      None
  }

  final def getChild(e: Exp[Any]): Option[SymbolProperties] = getProps(e) match {
    case Some(p) => getChild(p)
    case None => None
  }
  final def getField(struct: Exp[Any], name: String): Option[SymbolProperties] = getProps(struct) match {
    case Some(p) => getField(p, name)
    case None => None
  }

  // --- Shortcuts for properties, manifests
  // Get symbol properties for data field 
  final def mdat(x: Exp[Any]): SymbolProperties = mdat(props(x))
  final def mdat(x: SymbolProperties): SymbolProperties = x match {
    case (s: StructProperties) => s.child("data").get
    case (a: ArrayProperties) => a.child.get
    case _ => sys.error("Symbol properties " + x + " has no data field")
  }

  // These should only be used when child is guaranteed to be defined
  final def child(p: SymbolProperties): SymbolProperties = getChild(p).get
  final def child(p: SymbolProperties, index: String) = getField(p, index).get
  final def props(e: Exp[Any]): SymbolProperties = getProps(e).get
  final def child(e: Exp[Any]): SymbolProperties = getChild(e).get
  final def child(e: Exp[Any], index: String) = getField(e, index).get

  /**
   * Merge previous metadata for token and new data, notifying update if changes occurred
   * During merge, new metadata overrides pre-existing data when possible
   */ 
  private def updateProperties(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    if (!data.contains(e)) {
      data(e) = p
      notifyUpdate(e)
    }
    else {
      val prevProps = data(e)
      try {
        val newProps = attemptMeet(prevProps, p, func = MetaOverwrite)
        data(e) = newProps
        if (!matches(prevProps, newProps)) notifyUpdate(e)
      } catch { case e: Throwable =>
        printmsg("Unable to meet existing symbol property: \n" + makeString(prevProps) + "\n and new property: \n" + makeString(p))
        sys.error("Metadata compiler error!")
      }
    }
  }

  //////////////////////////////////
  // Symbol property initialization

  def defaultTypeMetadata[A](tp: Manifest[A]): List[Metadata] = Nil

  private def initExp(
    e: Exp[Any], 
    data: Option[Metadata] = None,
    child: Option[SymbolProperties] = None,
    field: Option[String] = None
  )(implicit ctx: SourceContext): SymbolProperties = initSym(e.tp, data, child, field)

  private def initSym[A](
    mA: Manifest[A], 
    data: Option[Metadata] = None,
    child: Option[SymbolProperties] = None,
    field: Option[String] = None
  )(implicit ctx: SourceContext): SymbolProperties = {
    val givenData = PropertyMap(data.map{m => m.name -> Some(m)}.toList)
    val typeData = PropertyMap(defaultTypeMetadata(mA).map{m => m.name -> Some(m)}) 
    val symData = attemptMeet(givenData, typeData, func = MetaTypeInit)

    mA match {
      case StructType(_,elems) =>
        val typeFields = PropertyMap(elems.map{elem => elem._1 -> Some(initSym(elem._2)) })

        val symFields = if (field.isEmpty) { typeFields }
                        else {
                          val givenField = PropertyMap(List(field.get -> child))
                          attemptMeet(givenField, typeFields, func = MetaTypeInit)
                        }

        StructProperties(symFields, symData)

      case tp if isDeliteArrayTpe(tp) || isMultiArrayTpe(tp) =>
        val typeChild = mA.typeArguments match {
          case Nil => None
          case tps => 
            warn(tps.length == 1, "Array type arguments length " + tps.length + " != 1 --- " + tps)
            Some(initSym(tps.head))
        }
        val symChild = attemptMeet(child, typeChild, func = MetaTypeInit)
        ArrayProperties(symChild, symData)

      case _ =>
        ScalarProperties(symData)
    }
  }

}