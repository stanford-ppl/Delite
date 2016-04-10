/*package ppl.delite.framework.transform

import scala.reflect.SourceContext
import scala.collection.immutable

import scala.virtualization.lms.internal._

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._

import ppl.delite.framework.analysis._
import ppl.delite.framework.codegen.delite.overrides._

trait MultiArrayWrapExp extends DeliteMultiArrayOpsExp with RankMetadataOps { this: DeliteOpsExp =>
  case class MultiArrayBuffify[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends Fig2[T,DeliteMultiArray[T]]
  case class MultiArrayViewify[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends Fig2[T,DeliteMultiArray[T]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@MultiArrayBuffify(ma) => reflectPure(MultiArrayBuffify(f(ma))(e.mA))(mtype(manifest[A]), pos)
    case e@MultiArrayViewify(ma) => reflectPure(MultiArrayViewify(f(ma))(e.mA))(mtype(manifest[A]), pos)
    case Reflect(e@MultiArrayBuffify(ma), u, es) => reflectMirrored(Reflect(MultiArrayBuffify(f(ma))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MultiArrayViewify(ma), u, es) => reflectMirrored(Reflect(MultiArrayViewify(f(ma))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait MultiArrayWrapTransformer extends TunnelingTransformer {
  val IR: MultiArrayWrapExp with DeliteOpsExp
  import IR._
  override val name = "MultiArray Wrapper"

  def buffify[T:Manifest](x: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[DeliteMultiArray[T]] = {
    if (!isPhysBuffer(x)) {
      val x2 = reflectPure(MultiArrayBuffify(x))
      setProps(x2, getProps(x))
      setMetadata(x2, MBuffer(PhysType))
      (x2)
    }
    else x
  }

  def viewify[T:Manifest](x: Exp[DeliteMultiArray[T]])(implicit ctx: SourceContext): Exp[DeliteMultiArray[T]] = {
    if (!isPhysView(x)) {
      val x2 = reflectPure(MultiArrayViewify(x))
      setProps(x2, getProps(x))
      setMetadata(x2, MView(PhysType))
      (x2)
    }
    else x
  }

  // TODO: Check alignment further down? (e.g. Array of Buffers aliased w/ Array of Arrays)
  // TODO: Change prints to debug/logging statements
  // Top level mismatch is easy, just wrap with View or Buffer
  // But what about levels further down? Implicitly generate a map? Just give an error?
  def wrapMultiArray[T:Manifest](e: Exp[DeliteMultiArray[T]], out: SymbolProperties)(implicit ctx: SourceContext): Option[Exp[DeliteMultiArray[T]]] = {
    if (isPhysBuffer(out) && !isPhysBuffer(e) && isPhysView(out) && !isPhysView(e)) {
      println(s"Adding buffer and view conversion to symbol $e")
      Some(viewify(buffify(e)))
    }
    else if (isPhysBuffer(out) && !isPhysBuffer(e)) {
      println(s"Adding buffer conversion to symbol $e")
      Some(buffify(e))
    }
    else if (isPhysView(out) && !isPhysView(e)) {
      println(s"Adding view conversion to symbol $e")
      Some(viewify(e))
    }
    else None
  }
  def wrap(e: Exp[Any], out: SymbolProperties)(implicit ctx: SourceContext): Option[Exp[DeliteMultiArray[Any]]] = {
    // Hack - turn a Exp[Any] into a Exp[DeliteMultiArray[T]]
    // Needed to preserve inner type of MultiArray (otherwise get a bunch of MultiArray[Any])
    def dataTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp.typeArguments(0).asInstanceOf[Manifest[B]]
    def unerase[A](e: Exp[Any], tp: Manifest[A]): Exp[DeliteMultiArray[A]] = e.asInstanceOf[Exp[DeliteMultiArray[A]]]

    if (isMultiArrayType(e.tp)) {
      wrapMultiArray(unerase(e, dataTp(e.tp)), out)
    }
    else None
  }

  def mirrorWith[A](extend: (SymbolProperties, Exp[Any])*)(s: Sym[A], d: Def[A]) = {
    val subs = extend.flatMap{case (p, e) => wrap(e, p).map{e2 => e -> e2} }
    if (subs.isEmpty) None
    else withSubstScope(subs:_*) { Some(self_mirror(s, d)) }
  }

  override def transform(s: Sym[Any], d: Def[Any])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
    // --- Var aliasing
    case Assign(Variable(v),rhs) => mirrorWith(props(v) -> rhs)(s, d)
    case VarPlusEquals(Variable(v), rhs) => mirrorWith(props(v) -> rhs)(s, d)
    case VarMinusEquals(Variable(v), rhs) => mirrorWith(props(v) -> rhs)(s, d)
    case VarTimesEquals(Variable(v), rhs) => mirrorWith(props(v) -> rhs)(s, d)
    case VarDivideEquals(Variable(v), rhs) => mirrorWith(props(v) -> rhs)(s, d)

    // --- Atomic Writes
    case NestedAtomicWrite(s, trace, d) => substituteAtomicWrite(s, d, trace)

    // --- Conditional branch aliasing
    case op: DeliteOpCondition[_] =>
      val t = getBlockResult(op.thenp)
      val e = getBlockResult(op.elsep)
      mirrorWith(props(s) -> t, props(s) -> e)(s, d)

    case op: AbstractLoop[_] => op.body match {
      case r: DeliteReduceElem[_] =>
        val map  = getBlockResult(r.func)
        val red  = getBlockResult(r.rFunc)
        val zero = getBlockResult(r.zero)
        val init = getBlockResult(r.accInit)
        val p = meet(ReduceAlias, props(map), props(red), props(zero), props(init))

        val s2 = mirrorWith(p -> map, p -> red, p -> zero, p -> init)(s, d)
        val res = s2.getOrElse(s)
        setMetadata(res, getBuffer(p)); setMetadata(res, getView(p))
        res match { case Def(op2: AbstractLoop[_]) => op2.body match {
          case r2: DeliteReduceElem[_] =>
            setMetadata(r2.rV._1, getBuffer(p)); setMetadata(r2.rV._1, getView(p))
            setMetadata(r2.rV._2, getBuffer(p)); setMetadata(r2.rV._2, getView(p))
        }}
        (s2)

      case _ => None
    }

    case op@DeliteMultiArrayFold(ma,_,zero) =>
      val red = getBlockResult(op.body)
      val p = meet(ReduceAlias, props(red), child(ma), props(zero))
      val s2 = mirrorWith(p -> red, p -> zero)(s, d)

      val res = s2.getOrElse(s)
      setMetadata(res, getBuffer(p));  setMetadata(res, getView(p))
      res match { case Def(op: DeliteMultiArrayFold[_]) =>
        setMetadata(op.rV._1, getBuffer(p)); setMetadata(op.rV._1, getView(p))
        setMetadata(op.rV._2, getBuffer(p)); setMetadata(op.rV._2, getView(p))
      }
      (s2)

    case op@DeliteMultiArrayReduce(ma,_,zero) =>
      val red = getBlockResult(op.body)
      val p = meet(ReduceAlias, props(red), child(ma), props(zero))
      val s2 = mirrorWith(p -> red, p -> zero)(s, d)

      val res = s2.getOrElse(s)
      setMetadata(res, getBuffer(p)); setMetadata(res, getView(p))
      res match { case Def(op: DeliteMultiArrayReduce[_]) =>
        setMetadata(op.rV._1, getBuffer(p)); setMetadata(op.rV._1, getView(p))
        setMetadata(op.rV._2, getBuffer(p)); setMetadata(op.rV._2, getView(p))
      }
      (s2)

    case d: AtomicWrite[_] => substituteAtomicWrite(s, d, Nil) // technically not right... "s" is in update sym position

    case _ => super.transformTP(s,d)
  }

  def substituteAtomicWrite(s: Exp[Any], d: Def[Any], trace: List[AtomicTracer])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
    // --- Updates
    case e@DeliteMultiArrayUpdate(ma,i,x) =>
      val lhs = if (trace.isEmpty) child(ma) else followTrace(props(s), trace).asInstanceOf[ArrayProperties].child.get
      wrap(x, lhs).map{x2 => dmultia_update(ma, i, x2)(e.mA, ctx) }

    case FieldUpdate(struct,index,x) =>
      val lhs = if (trace.isEmpty) child(struct,index) else followTrace(props(s), trace).asInstanceOf[StructProperties].child(index).get
      wrap(x, lhs).map{x2 => field_update(struct, index, x2)(x2.tp) }

    // --- Buffers
    case e@DeliteMultiArrayInsert(ma,x,i) =>
      val lhs = if (trace.isEmpty) child(ma) else followTrace(props(s), trace).asInstanceOf[ArrayProperties].child.get
      wrap(x, lhs).map{x2 => dmultia_insert(ma, x2.as1D, i)(e.mA, ctx) }

    // TODO: How to wrap here? Is it needed?
    case DeliteMultiArrayInsertAll(ma,x,a,i) => None

    case _ => None
  }

  // TODO: Move this elsewhere?
  def followTrace(p: SymbolProperties, trace: List[AtomicTracer]): SymbolProperties = {
    if (trace.isEmpty) { p }
    else (p, trace.head) match {
      case (p: StructProperties, StructTracer(field)) => followTrace(p.child(field).get, trace.tail)
      case (p: ArrayProperties, ArrayTracer(_)) => followTrace(p.child.get, trace.tail)
      case _ => sys.error("Error while following nested write trace in metadata")
    }
  }
}*/
