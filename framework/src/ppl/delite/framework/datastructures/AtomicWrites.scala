package ppl.delite.framework.datastructures

import scala.reflect.SourceContext

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.GenericFatCodegen

import ppl.delite.framework.ops.DeliteOpsExp

trait AtomicWriteOps extends Base { /* Nothing? */ }

/**
 * Purpose of these ops is to allow redirection of general nested mutations 
 * This way, DSL authors should be able to use mutable data structures normally without
 * creating mutable aliases in the effects system. Secondary goal is to limit the number
 * of codegen-able IR nodes as much as possible using the 
 * 
 * (More generalized version of Kevin's nested mutations work in DeliteArray)
 *
 * Note that these accesses aren't 'atomic' in the concurrency sense, just in the 
 * code generation sense
 *
 * TODO: Does this belong in datastructures?
 */
trait AtomicWriteOpsExp extends AtomicWriteOps with EffectExp {
  this: DeliteOpsExp with DeliteStructsExp with DeliteArrayOpsExp =>

  //////////////
  // "Tracers"
  
  /**
   * Nested Mutation tracer
   * Used to track a single field/apply lookup in a nested update
   * Instances should include a reference to the symbol being accessed
   * and any information normally included in the extract method for that symbol
   */ 
  trait AtomicTracer extends Product

  // Similar to FieldApply
  case class StructTracer(field: String) extends AtomicTracer
  // Similar to DeliteArrayApply
  case class ArrayTracer(i: Exp[Int]) extends AtomicTracer

  // Tracer mirroring
  def mirrorTrace(t: AtomicTracer, f: Transformer)(implicit pos: SourceContext): AtomicTracer = t match {
    case ArrayTracer(i) => ArrayTracer(f(i))
    case _ => t
  }

  // TODO: Do we ever need to note explicitly that ReadVar was encountered?
  def recurseLookup[T:Manifest](sym: Exp[Any], trace: List[AtomicTracer]): (Exp[Any],List[AtomicTracer]) = sym match {
    case Def(Field(struct,field)) => recurseLookup(struct, StructTracer(field) +: trace)
    case Def(Reflect(Field(struct,field),_,_)) => recurseLookup(struct, StructTracer(field) +: trace)
    case Def(ReadVar(Variable(e))) => recurseLookup(e, trace)
    case Def(Reflect(ReadVar(Variable(e)),_,_)) => recurseLookup(e, trace)
    case Def(DeliteArrayApply(array, i)) => recurseLookup(array, ArrayTracer(i) +: trace)
    case Def(Reflect(DeliteArrayApply(array, i),_,_)) => recurseLookup(array, ArrayTracer(i) +: trace)
    case _ => (sym,trace)
  }

  ///////////////// 
  // Atomic Writes

  /** 
   * Abstract atomic write node.
   */
  abstract class AtomicWrite extends Def[Unit] { 
    var isNested: Boolean = false 
    def asNested: AtomicWrite = {isNested = true; this} // TODO: should be contravariant?
    /* 
     * Symbols which should always be externally visible, even with nesting
     * e.g.:
     * DeliteArrayApply(array, i) => need to see dependencies (i, array)
     * NestedAtomicWrite(struct, field, DeliteArrayApply(array, i)) => need (struct, i)
     * TODO: Better name for this?
     */
    def externalFields: List[Any]
  }
  abstract class AtomicWriteWithManifest[A:Manifest] extends AtomicWrite { val mA = manifest[A] } 

  // mirroring
  def mirrorNestedAtomic[A:Manifest](d: AtomicWrite, f: Transformer)(implicit ctx: SourceContext): AtomicWrite = d match {
    case _ => sys.error("No mirror atomic rule found for " + d)
  }

  // Version of reflectEffect used for Atomic Writes
  def reflectAtomicWrite(sym: Exp[Any])(d: AtomicWrite)(implicit ctx: SourceContext): Exp[Unit] = {
    val (outerSym, trace) = recurseLookup(sym, Nil)
    val outerDef = if (trace.isEmpty) { d } else { NestedAtomicWrite(outerSym, trace, d.asNested) } 
    reflectWrite(outerSym)(outerDef)
  }

  ////////////////////
  // Nested Mutations

  /**
   * Nested Mutation IR node
   * Includes a list of tracers giving the series of extractions done to get to the 
   * structure being updated. The first element in the tracer list should contain the
   * outer data structure being written to
   */
  case class NestedAtomicWrite(sym: Exp[Any], trace: List[AtomicTracer], d: AtomicWrite) extends Def[Unit] {
    private lazy val deps = trace.flatMap{t => t.productIterator.toList} ::: List(sym, d)
    override def productIterator = deps.iterator
    override def productElement(n: Int) = deps(n) 
    override def productArity = deps.length
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case op: AtomicWrite if op.isNested => sys.error("Shouldn't be mirroring a nested write!")
    case Reflect(NestedAtomicWrite(s,t,d), u, es) => 
      reflectMirrored(Reflect(NestedAtomicWrite(f(s),t.map{r => mirrorTrace(r,f)}, mirrorNestedAtomic(d,f)), mapOver(f,u), f(es)))(mtype(manifest[Unit]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  // These are A LOT like overriding the product iterator for case classes, but 
  // we only want to use the "overriden" version in dependencies/effects checking
  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(blocks(_))
    case _ => super.blocks(e)
  }

  // dependencies (Expressions)
  override def syms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(syms(_))
    case _ => super.syms(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(boundSyms(_))
    case _ => super.boundSyms(e)
  }
  override def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(tunnelSyms(_))
    case _ => super.tunnelSyms(e)
  }
  override def effectSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(effectSyms(_))
    case _ => super.effectSyms(e)
  }
  override def softSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(softSyms(_))
    case _ => super.softSyms(e)
  }
  override def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(rsyms(_)(f))
    case _ => super.rsyms(e)(f)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(symsFreq(_))
    case _ => super.symsFreq(e)
  }

  // effects
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(readSyms(_))
    case _ => super.readSyms(e)
  }
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(aliasSyms(_))
    case _ => super.aliasSyms(e)
  }
  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(containSyms(_))
    case _ => super.containSyms(e)
  }
  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(extractSyms(_))
    case _ => super.extractSyms(e)
  }
  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(copySyms(_))
    case _ => super.extractSyms(e)
  }

}

// Codegen for AtomicWrite operations
// Not target specific! All other codegens for AtomicWrites should extend this
trait BaseGenAtomicOps extends GenericFatCodegen {
  val IR: DeliteOpsExp with AtomicWriteOpsExp
  import IR._

  def quote(t: AtomicTracer): String = t match {
    case StructTracer(f) => "." + f                    // [struct].field
    case ArrayTracer(i) => "(" + quote(i) + ".toInt)"  // [array](i.toInt)
    case _ => sys.error("No codegen rule defined for atomic trace " + t)
  }
  def quote(trace: List[AtomicTracer]): String = trace.map{t => quote(t)}.mkString("")

  /**
   * Emit rules for nested AtomicWrite nodes
   * Must be filled in by codegen-able atomic write nodes!
   * @param sym   - output symbol for the result of this write operation (Unit)
   * @param d     - the atomic write IR node
   * @param trace - optional string representing codegenned write target
   * (if trace is None, codegen should use d's write target instead)
   */
  def emitAtomicWrite(sym: Sym[Any], d: AtomicWrite, trace: Option[String]) = d match {
    // e.g.:
    // case DeliteArrayUpdate(a,i,x) => 
    //  emitValDef(sym, trace.getOrElse(quote(a)) + "(" + quote(i) + ".toInt) = " + quote(x))
    case _ => sys.error("No emit rule defined for atomic write op " + d)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: AtomicWrite => 
      emitAtomicWrite(sym, op, None)
    case op@NestedAtomicWrite(s, trace, d) => 
      emitAtomicWrite(sym, d, Some(quote(s) + quote(trace)))
    case _ => super.emitNode(sym, rhs)
  }
}
