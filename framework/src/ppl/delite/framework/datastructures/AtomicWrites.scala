package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

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
 * TODO: Does this belong in datastructures? Seems very LMS-y
 */
trait AtomicWriteOpsExp extends AtomicWriteOps with DeliteStructsExp with Effects {
  
  //////////////
  // "Tracers"
  
  /**
   * Nested Mutation tracer
   * Used to track a single field/apply lookup in a nested update
   * Instances should include a reference to the symbol being accessed
   * and any information normally included in the extract method for that symbol
   */ 
  abstract class AtomicTracer {
    // Dependencies - include the data structure symbol only if 'top' is true
    def deps(top: Boolean): List[Any]
    // Pointer to data structure being accessed
    def ptr: Any
  }
  case class StructTracer(struct: Exp[Any], field: String) extends AtomicTracer {
    def deps(top: Boolean) = if (top) List(struct) else Nil
    def ptr = struct
  }
  case class VarTracer(v: Variable[Any]) extends AtomicTracer {
    def deps(top: Boolean) = if (top) List(v) else Nil
    def ptr = v
  }
  case class ArrayTracer(array: Exp[Any], i: Exp[Int]) extends AtomicTracer {
    def deps(top: Boolean) = if (top) List(a,i) else Nil
    def ptr = a
  }

  // Tracer mirroring
  // Data structure symbol should only be mirrored if this tracer is at the top
  def mirrorTrace(t: AtomicTracer, f: Transformer, top: Boolean)(implicit pos: SourceContext): AtomicTracer = t match {
    case StructTracer(struct,field) => if (top) StructTracer(f(struct),field) else StructTracer(struct,field)
    case VarTracer(v) => if (top) StructTracer(f(v)) else VarTracer(v)
    case ArrayTracer(a,i) => if (top) StructTracer(f(a),f(i)) else ArrayTracer(a,f(i))
    case _ => sys.error("Don't know how to mirror atomic tracer " + t)
  }

  def recurseLookup[T:Manifest](target: Exp[Any], trace: List[AtomicTracer]): (Exp[Any],List[AtomicTracer]) = target match {
    case Def(Field(struct,field)) => recurseLookup(struct, StructTracer(struct,field) +: trace)
    case Def(Reflect(Field(struct,field),_,_)) => recurseLookup(struct, StructTracer(struct,field) +: trace)
    case Def(ReadVar(v)) => recurseLookup(v.e, VarTracer(v) +: trace)
    case Def(Reflect(ReadVar(v),_,_)) => recurseLookup(v.e, VarTracer(v) +: trace)
    case Def(DeliteArrayApply(array, i)) => recurseLookup(array, ArrayTracer(array,i) +: trace)
    case Def(Reflect(DeliteArrayApply(array, i),_,_)) => recurseLookup(array, ArrayTracer(array,i) +: trace)
    case _ => (target,trace)
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

  // mirroring
  override def mirrorNestedAtomic[A:Manifest](d: AtomicWrite, f: Transformer)(implicit pos: SourceContext): AtomicWrite = d match {
    case _ => sys.error("No mirror atomic rule found for " + d)
  }

  // Version of reflectEffect used for Atomic Writes
  def reflectAtomicWrite(target: Exp[Any])(d: AtomicWrite)(implicit pos: SourceContext): Exp[Unit] = {
    val (outerTarget, trace) = recurseLookup(target, Nil)
    val outerDef = if (trace.isEmpty) { d } else { NestedAtomicWrite(trace, d.asNested) } 
    reflectWrite(outerTarget)(outerDef)
  }

  ////////////////////
  // Nested Mutations

  /**
   * Nested Mutation IR node
   * Includes a list of tracers giving the series of extractions done to get to the 
   * structure being updated. The first element in the tracer list should contain the
   * outer data structure being written to
   */
  case class NestedAtomicWrite(t: List[AtomicTracer], d: AtomicWrite) extends Def[Unit] {
    private val deps = t.zipWithIndex.flatMap{i => i._1.deps(i._2 == 0)} ::: List(d)
    def getTop: Any = t(0).ptr // Either a Var or an Exp...
    override def productIterator = deps.iterator
    override def productElement(n: Int) = deps(n) 
    override def productArity = deps.length
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case op: AtomicWrite if op.isNested => sys.error("Shouldn't be mirroring a nested write!")
    case Reflect(NestedAtomicWrite(e,t,d), u, es) => 
      reflectMirrored(Reflect(NestedAtomicWrite(f(e), t.zipWithIndex.map{r => mirrorTrace(r._1,f,r._2 == 0)}),mirrorNestedAtomic(d,f)), mapOver(f,u), f(es)))(mtype(manifest[Unit]), ctx)
    case _ => super.mirror(e,t)
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
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(rsyms(_))
    case _ => super.rsyms(e)
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
  override def containsSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite if op.isNested => op.externalFields.flatMap(containsSyms(_))
    case _ => super.containsSyms(e)
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

  def quote(t: AtomicTracer) = t match {
    case StructTrace(field) => "." + field
    case VarTrace(_) => ""
    case ArrayTrace(i) => "(" + quote(i) + ".toInt)" 
    case _ => sys.error("No stringify method defined for atomic trace " + rhs)
  }
  def quote(t: List[AtomicTracer]): String = t.map{quote(_)}.mkString("")

  /**
   * Emit rules for nested AtomicWrite nodes
   * Must be filled in by codegen-able atomic write nodes!
   * e.g. struct.field(0) = 1
   * @param sym   - output symbol for the result of this write operation (Unit)
   * @param outer - parent symbol that the write effect is on (e.g. 'struct' in above example)
   * @param trace - list of atomic write tracers (see above) 
   * @param d     - the atomic write IR node (has no corresponding Unit symbol)
   */
  def emitNestedAtomic(sym: Sym[Any], outer: Exp[Any], trace: List[AtomicTracer], d: AtomicWrite) = d match {
    case _ => sys.error("No emit rule defined for atomic write op " + d)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op@NestedAtomicWrite(target, trace, d) => 
      emitAtomicWrite(sym, target, trace, d)
    case _ => super.emitNode(sym, rhs)
  }
}
