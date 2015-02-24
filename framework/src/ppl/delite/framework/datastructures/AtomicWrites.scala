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

  /** 
   * Abstract atomic write def. Used in the effects system as a coditional extenstion to
   * the general 'Product' dependencies rule
   */
  abstract class AtomicWrite extends Def[Unit] { 
    var isNested: Boolean = false 
    def asNested: AtomicWrite = {isNested = true; this} // TODO: should be contravariant?

    /* 
     * Symbols which should always be externally visible, even with nesting
     * e.g.:
     * DeliteArrayApply(array, i) => need to see dependencies (i, array)
     * StructAtomicWrite(struct, field, DeliteArrayApply(array, i)) => need (struct, i)
     * TODO: Better name for this?
     */
    def externalFields: List[Any]
  }

  // TODO: should the constructors for these be private to this trait?
  // Shouldn't create these outside of recurseWrite..
  // Considered overriding Product methods instead, but then the descendants can't
  // be case classes...

  abstract class NestedAtomicWrite(d: AtomicWrite) extends AtomicWrite

  case class StructAtomicWrite(struct: Exp[Any], field: String, override val d: AtomicWrite) extends NestedAtomicWrite(d) {
    def externalFields = List(field, d)
  }
  case class VarAtomicWrite(v: Var[Any], override val d: AtomicWrite) extends NestedAtomicWrite(d) {
    def externalFields = List(d)
  }
  case class ArrayAtomicWrite(array: Exp[DeliteArray[Any]], i: Exp[Int], override val d: AtomicWrite) extends NestedAtomicWrite(d) {
    def externalFields = List(i, d)
  }

  def reflectAtomicWrite(target: Exp[Any])(d: AtomicWrite)(implicit pos: SourceContext): Exp[Unit] = {
    val (outerTarget, outerDef) = recurseLookup(target, d)
    reflectWrite(outerTarget)(outerDef)
  }

  def recurseLookup[T:Manifest](target: Exp[Any], d: AtomicWrite): (Exp[Any],AtomicWrite) = target match {
    case Def(Field(struct,field)) => recurseLookup(struct, StructAtomicWrite(struct, field, d.asNested))
    case Def(Reflect(Field(struct,field),_,_)) => recurseLookup(struct, StructAtomicWrite(struct, field, d.asNested))
    case Def(ReadVar(v)) => recurseLookup(v.e, VarAtomicWrite(v, d.asNested))
    case Def(Reflect(ReadVar(v),_,_)) => recurseLookup(v.e, VarAtomicWrite(v, d.asNested))
    case Def(DeliteArrayApply(array, i)) => recurseLookup(array, ArrayAtomicWrite(array, i, d.asNested))
    case Def(Reflect(DeliteArrayApply(array, i),_,_)) => recurseLookup(array, ArrayAtomicWrite(array, i, d.asNested))
    case _ => (target,d)
  }

  // mirroring
  // Base.scala takes care of Reflect() and Reify() cases
  // TODO: do we need non-Reflect cases for mirror?
  override def mirrorDef[A:Manifest](e: Def[A], t: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case StructAtomicWrite(s,f,d) => StructAtomicWrite(t(s),f,mirrorDef(d,t))
    case VarAtomicWrite(v,d) => VarAtomicWrite(t(v),mirrorDef(d,t))
    case ArrayAtomicWrite(a,i,d) => ArrayAtomicWrite(t(a),t(i),mirrorDef(d))
    case _ => super.mirrorDef(e,t)
  }
  override def mirror[A:Manifest](e: Def[A], t: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case op: AtomicWrite if op.isNested => sys.error("Shouldn't be mirroring a nested write!")
    case Reflect(StructAtomicWrite(s,f,d), u, es) => reflectMirrored(Reflect(StructAtomicWrite(t(s),f,mirrorDef(d,t)), mapOver(t,u), t(es)))(mtype(manifest[Unit]), ctx)
    case Reflect(VarAtomicWrite(v,d), u, es) => reflectMirrored(Reflect(VarAtomicWrite(t(v),mirrorDef(d,t)), mapOver(t,u), t(es)))(mtype(manifest[Unit]), ctx)
    case Reflect(ArrayAtomicWrite(a,i,d), u, es) => reflectMirrored(Reflect(ArrayAtomicWrite(t(a),t(i),mirrorDef(d,t)), mapOver(t,u), t(es)))(mtype(manifest[Unit]), ctx)
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

trait ScalaGenAtomicOps extends ScalaGenDeliteArrayOps with ScalaGenDeliteStruct {
  val IR: DeliteArrayFatExp with DeliteOpsExp 
  import IR._

  def genNestedAtomicWriteString(sym: Sym[Any], rhs: AtomicWrite) = rhs match {
    case StructAtomicWrite(_,field,d) => "." + field + genNestedAtomicWriteString(d)
    case VarAtomicWrite(_,d) => genNestedAtomicWriteString(d)
    case ArrayAtomicWrite(_,i,d) => "(" + quote(i) + ".toInt)" + genNestedAtomicWriteString(d)
    case _ => sys.error("No stringify method defined for atomic write def " + rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op@StructAtomicWrite(struct,field,d) => 
      emitValDef(sym, quote(struct) + "." + field + genNestedAtomicWriteString(d))
    case op@VarAtomicWrite(Variable(a),d) =>
      emitValDef(sym, quote(a) + genNestedAtomicWriteString(d))
    case op@ArrayAtomicWrite(a,i,d) =>
      emitValDef(sym, quote(a) + "(" + quote(i) + ".toInt)" + genNestedAtomicWriteString(d))

    case _ => super.emitNode(sym, rhs)
  }
}

// TODO: CUDA, C, etc. 
*/
