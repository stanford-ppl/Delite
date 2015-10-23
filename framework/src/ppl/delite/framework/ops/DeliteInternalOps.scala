/**
 * DeliteInternalOps.scala
 *
 * This file contains all code-generated, non-Delite op definitions required by Delite.
 * Delite should have no dependencies on LMS common other than what is in this file.
 * The idea is to eventually deprecate our use of LMS common ops inside Delite and Delite DSLs. 
 * All IR nodes Delite needs should be defined here, and all DSL nodes should be defined 
 * in a Forge specification.
 * 
 * We include a forwarding trait for LMS common ops, but any DSL that defines their own
 * front-end API that overlap with ops here must ensure that they are forwarded appropriately.
 * Forge handles this automatically for Forge DSLs.
 */

package ppl.delite.framework.ops

import org.scala_lang.virtualized.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericCodegen, GenericFatCodegen, GenerationFailedException, CCodegen}
import scala.virtualization.lms.util.OverloadHack

import ppl.delite.framework.datastructures._

/**
 * A forwarder for LMS common ops to the Delite back-end. Any DSL that includes LMS common ops
 * should also include this forwarder trait.
 */
trait DeliteLMSForwarderExp extends BooleanOpsExp with EqualExpBridge with PrimitiveOpsExp with OrderingOpsExp with ObjectOpsExp with DeliteInternalOpsExp {
  this: DeliteOpsExp with DeliteStructsExp => 

  override def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = delite_int_plus(lhs,rhs)
  override def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = delite_int_minus(lhs,rhs)
  override def int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = delite_int_times(lhs,rhs)
  override def object_unsafe_immutable[A:Manifest](lhs: Exp[A])(implicit pos: SourceContext): Exp[A] = delite_unsafe_immutable(lhs)
  override def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext): Exp[Boolean] = delite_boolean_negate(lhs)
  override def equals[A:Manifest,B:Manifest](lhs: Exp[A], rhs: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = delite_equals(lhs,rhs)
  override def notequals[A:Manifest,B:Manifest](lhs: Exp[A], rhs: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = delite_notequals(lhs,rhs)  
}


/**** There is intentionally no DeliteInternalOps (syntax / base trait) ****/


/**
 * Any DSL operations that overlap with this set of ops must forward their implementation to the
 * version here (e.g. by including the trait above if using LMS, or by using Forge), or Delite
 * analyses may fail.
 */
trait DeliteAnalysesOps extends Base {
  def delite_int_plus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int] 
  def delite_int_minus(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def delite_int_times(lhs: Rep[Int], rhs: Rep[Int])(implicit pos: SourceContext): Rep[Int]
  def delite_unsafe_immutable[A:Manifest](lhs: Rep[A])(implicit pos: SourceContext): Rep[A]
  def delite_boolean_negate(lhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def delite_equals[A:Manifest,B:Manifest](lhs: Rep[A], rhs: Rep[B])(implicit pos: SourceContext): Rep[Boolean]
  def delite_notequals[A:Manifest,B:Manifest](lhs: Rep[A], rhs: Rep[B])(implicit pos: SourceContext): Rep[Boolean] 
}

/**
 * This is the self-contained Delite implementation for its internal operations. In addition to the methods
 * from DeliteAnalysesOps, it includes primitive operations that DSLs do not necessarily need to forward to,
 * but Delite needs to use internally (e.g. in DeliteArray). We separate this into an isolated trait, even
 * though it results in copied implementations from LMS common ops, in order to avoid accidental inclusion
 * of LMS common traits in DSLs.
 *
 * Any LMS trait declared here IS pulled in to all DSLs, as is as any LMS trait that is explicitly pulled in by
 * other Delite classes. We may want to write a small static checker to keep better track of this.
 */
trait DeliteInternalOpsExpBase extends DeliteAnalysesOps 
  with BaseFatExp with EffectExp with VariablesExp with LoopsFatExp with IfThenElseFatExp with WhileExp with StaticDataExp with FunctionBlocksExp {

  this: DeliteOpsExp with DeliteStructsExp => 
  
  // These are the IR nodes that Delite requires internally, or for analyses.
  // They are redefinitions of a small subset of LMS common nodes. The originals should be considered deprecated for our purposes.
  case class DBooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class DEqual[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B]) extends Def[Boolean]
  case class DNotEqual[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B]) extends Def[Boolean]
  case class DIntPlus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class DIntMinus(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class DIntTimes(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class DIntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class DIntMod(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]

  abstract class DefMN[T:Ordering:Manifest,A] extends Def[A] {
    def mev = manifest[T]
    def aev = implicitly[Ordering[T]]
  }
  case class DLessThan[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class DGreaterThan[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]

  case class DUnsafeImmutable[A:Manifest](o: Exp[A]) extends Def[A] { val m = manifest[A]  }

  def delite_boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext): Exp[Boolean] = DBooleanNegate(lhs)
  def delite_equals[A:Manifest,B:Manifest](lhs: Exp[A], rhs: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = DEqual(lhs, rhs)
  def delite_notequals[A:Manifest,B:Manifest](lhs: Exp[A], rhs: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = DNotEqual(lhs, rhs)
  def delite_int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = DIntPlus(lhs, rhs)
  def delite_int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = DIntMinus(lhs, rhs)
  def delite_int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = DIntTimes(lhs, rhs)
  def delite_int_divide(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = DIntDivide(lhs, rhs)
  def delite_int_mod(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = DIntMod(lhs, rhs)
  def delite_less_than[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Boolean] = DLessThan(lhs, rhs)  
  def delite_greater_than[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[Boolean] = DGreaterThan(lhs, rhs)  
  def delite_unsafe_immutable[A:Manifest](lhs: Exp[A])(implicit pos: SourceContext) = lhs match {
    // INVESTIGATE: there was an issue where Const(0).unsafeImmutable == Const(0.0). How is this possible? CSE with primitive widening?
    case c@Const(x) => c
    case s: Sym[_] if !isWritableSym(s) => lhs
    case _ => DUnsafeImmutable(lhs)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case DBooleanNegate(x) => delite_boolean_negate(f(x))
    case DEqual(a, b) => delite_equals(f(a),f(b))
    case DNotEqual(a, b) => delite_notequals(f(a),f(b))
    case DIntPlus(x,y) => delite_int_plus(f(x),f(y))
    case DIntMinus(x,y) => delite_int_minus(f(x),f(y))
    case DIntTimes(x,y) => delite_int_times(f(x),f(y))
    case DIntDivide(x,y) => delite_int_divide(f(x),f(y))
    case DIntMod(x,y) => delite_int_mod(f(x),f(y))
    case e@DLessThan(a,b) => delite_less_than(f(a),f(b))(e.aev,e.mev,pos)
    case e@DGreaterThan(a,b) => delite_greater_than(f(a),f(b))(e.aev,e.mev,pos)
    case e@DUnsafeImmutable(a) => delite_unsafe_immutable(f(a))(mtype(e.m),pos)

    case Reflect(DBooleanNegate(x), u, es) => reflectMirrored(Reflect(DBooleanNegate(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DIntPlus(x,y), u, es) => reflectMirrored(Reflect(DIntPlus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DIntMinus(x,y), u, es) => reflectMirrored(Reflect(DIntMinus(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DIntTimes(x,y), u, es) => reflectMirrored(Reflect(DIntTimes(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DIntDivide(x,y), u, es) => reflectMirrored(Reflect(DIntDivide(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(DIntMod(x,y), u, es) => reflectMirrored(Reflect(DIntMod(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@DLessThan(a,b), u, es) => reflectMirrored(Reflect(DLessThan(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@DGreaterThan(a,b), u, es) => reflectMirrored(Reflect(DGreaterThan(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@DUnsafeImmutable(a), u, es) => reflectMirrored(Reflect(DUnsafeImmutable(f(a))(mtype(e.m)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case DEqual(a, b) => DEqual(f(a),f(b))
    case DNotEqual(a, b) => DNotEqual(f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]


  ///////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DUnsafeImmutable(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DUnsafeImmutable(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DUnsafeImmutable(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DUnsafeImmutable(a) => syms(a)
    case _ => super.copySyms(e)
  }
}

trait DeliteInternalOpsExp extends DeliteInternalOpsExpBase {

  this: DeliteOpsExp with DeliteStructsExp => 
  
  override def delite_equals[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = {
    if (a == b) Const(true) 
    else (a,b) match {
      case (Const(a),Const(b)) => Const(a == b)
      case _ => super.delite_equals(a,b)
    }
  }

  override def delite_notequals[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B])(implicit pos: SourceContext): Exp[Boolean] = {
    if (a == b) Const(false)
    else (a,b) match {
      case (Const(a),Const(b)) => Const(a != b)
      case _ => super.delite_notequals(a,b)
    }
  }

  override def delite_int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = (lhs, rhs) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => DIntPlus(lhs,rhs)
  }

  override def delite_int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = (lhs, rhs) match {
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x-y)
    case _ => DIntMinus(lhs, rhs)
  }

  override def delite_int_times(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = (lhs, rhs) match {
    case (l@Const(0), r) => l
    case (l, r@Const(0)) => r
    case (Const(1), r) => r
    case (l, Const(1)) => l
    case (Const(x), Const(y)) => Const(x*y)
    case _ => DIntTimes(lhs, rhs)
  }

  // -- Struct delite_unsafe_immutable can't be in DeliteStructs.scala because of illegal cyclic inheritance

  def delite_imm_field(struct: Exp[Any], name: String, f: Exp[Any])(implicit pos: SourceContext): Exp[Any] = {
    if (f.tp.erasure.getSimpleName == "Variable") {
      field(struct,name)(mtype(f.tp.typeArguments(0)),pos)
    }
    else {
      delite_unsafe_immutable(f)(mtype(f.tp),pos)
    }
  }

  // don't let unsafeImmutable hide struct-ness
  override def delite_unsafe_immutable[A:Manifest](lhs: Exp[A])(implicit pos: SourceContext) = lhs match {
    case Def(Struct(tag,elems)) => struct[A](tag, elems.map(t => (t._1, delite_imm_field(lhs, t._1, t._2))))
    case Def(d@Reflect(Struct(tag, elems), u, es)) => struct[A](tag, elems.map(t => (t._1, delite_imm_field(lhs, t._1, t._2))))
    case _ => super.delite_unsafe_immutable(lhs)
  }

  // -- end Struct unsafeImmutable rewrite
}

trait ScalaGenDeliteInternalOps extends ScalaGenBase {
  val IR: DeliteOpsExp with DeliteInternalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DBooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case DEqual(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
    case DNotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
    case DIntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case DIntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case DIntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case DIntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case DIntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case DLessThan(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
    case DGreaterThan(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
    case DUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")
    case _ => super.emitNode(sym,rhs)
  }
}

trait CLikeGenDeliteInternalOps extends CLikeGenBase {
  val IR: DeliteOpsExp with DeliteInternalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DBooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case DEqual(a,b) => emitValDef(sym, quote(a) + " == " + quote(b))
    case DNotEqual(a,b) => emitValDef(sym, quote(a) + " != " + quote(b))
    case DIntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case DIntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case DIntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case DIntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case DIntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case DLessThan(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
    case DGreaterThan(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
    case DUnsafeImmutable(x) => emitValDef(sym, quote(x) + "; // unsafe immutable")
    case _ => super.emitNode(sym,rhs)
  }  
}

trait CudaGenDeliteInternalOps extends CudaGenBase with CLikeGenDeliteInternalOps
trait OpenCLGenDeliteInternalOps extends OpenCLGenBase with CLikeGenDeliteInternalOps

trait CGenDeliteInternalOps extends CGenBase with CLikeGenDeliteInternalOps {
  val IR: DeliteOpsExp with DeliteInternalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case DEqual(a,b) if(remap(a.tp) == "string" && remap(b.tp) == "string") =>
        emitValDef(sym, quote(a) + ".compare(" + quote(b) + ") == 0")
      case DEqual(a,b) if (!isPrimitiveType(a.tp) && !isPrimitiveType(b.tp) && (remap(a.tp) == remap(b.tp))) =>
        emitValDef(sym, quote(a) + "->equals(" + quote(b) + ")")
      case DNotEqual(a,b) if(remap(a.tp) == "string" && remap(b.tp) == "string") =>
        emitValDef(sym, quote(a) + ".compare(" + quote(b) + ") != 0")
      case DNotEqual(a,b) if (!isPrimitiveType(a.tp) && !isPrimitiveType(b.tp) && (remap(a.tp) == remap(b.tp))) =>
        emitValDef(sym, "!" + quote(a) + "->equals(" + quote(b) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

