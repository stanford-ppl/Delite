package ppl.delite.framework.transform

import scala.reflect.SourceContext
import scala.collection.immutable

import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._

import ppl.delite.framework.analysis._
import ppl.delite.framework.codegen.delite.overrides._

trait MultiArrayWrapExp extends DeliteMultiArrayOpsExp with RankMetadataOps { this: DeliteOpsExp =>
  // Abstract def family identifier
  private implicit val fc = AbstractFamily("multiarray")
  case class MultiArrayBuffify[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends AbstractDefWithManifest[T,DeliteMultiArray[T]]
  case class MultiArrayViewify[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends AbstractDefWithManifest[T,DeliteMultiArray[T]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@MultiArrayBuffify(ma) => reflectPure(MultiArrayBuffify(f(ma))(e.mA))(mtype(manifest[A]), pos)
    case e@MultiArrayViewify(ma) => reflectPure(MultiArrayViewify(f(ma))(e.mA))(mtype(manifest[A]), pos)
    case Reflect(e@MultiArrayBuffify(ma), u, es) => reflectMirrored(Reflect(MultiArrayBuffify(f(ma))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MultiArrayViewify(ma), u, es) => reflectMirrored(Reflect(MultiArrayViewify(f(ma))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait MultiArrayWrapTransformer extends TransformerBase {
  val IR: MultiArrayWrapExp with DeliteOpsExp
  import IR._
  override val name = "MultiArray Wrapper"
  override val printAfter = true

  // TODO: This can be removed later
  def wrapBlocksWith[A](extend: (Exp[Any], Exp[Any])*)(block: => A): A = {
    //val tmp = innerLoopBody
    //innerLoopBody = true
    val res = withSubstScope {
      subst ++= extend
      block
    }
    //innerLoopBody = tmp
    (res)
  }


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

  // TODO: Check alignment further down (e.g. Array of Buffers vs. Array of Arrays)
  // Top level mismatch is easy, just wrap with View or Buffer
  // But what about levels further down? Implicitly generate a map??
  def wrapMultiArray[T:Manifest](e: Exp[DeliteMultiArray[T]], out: SymbolProperties)(implicit ctx: SourceContext): Option[Exp[DeliteMultiArray[T]]] = {
    if (isPhysBuffer(out) && !isPhysBuffer(e) && isPhysView(out) && !isPhysView(e)) {
      Some(viewify(buffify(e)))
    }
    else if (isPhysBuffer(out) && !isPhysBuffer(e)) { Some(buffify(e)) }
    else if (isPhysView(out) && !isPhysView(e)) { Some(viewify(e)) }
    else None
  }
  def wrap(e: Exp[Any], out: SymbolProperties)(implicit ctx: SourceContext): Option[Exp[DeliteMultiArray[Any]]] = {
    // Hack - turn a Exp[Any] into a Exp[DeliteMultiArray[T]]
    // Needed to preserve inner type of MultiArray (otherwise get a bunch of MultiArray[Any])
    def dataTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp.typeArguments(0).asInstanceOf[Manifest[B]]
    //def unerase[A](e: Exp[Any], tp: Manifest[A]): Exp[DeliteMultiArray[A]] = e.asInstanceOf[Exp[DeliteMultiArray[A]]]

    if (isMultiArrayTpe(e.tp)) {
      val ma = e.asInstanceOf[Exp[DeliteMultiArray[Any]]]
      wrapMultiArray(ma, out)(dataTp(e.tp), ctx)
    }
    else None
  }

  /*def wrap(b: Block[A], out: SymbolProperties)(implicit ctx: SourceContext): Option[Exp[DeliteMultiArray[Any]]] = {
    val res = getBlockResult(b)
    wrap(res, out).map{res2 => s2 }
  }*/

  override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
    // --- var aliasing
    // TODO: Should be written more like a mirror call rather than a direct mirror?
    case Reflect(Assign(Variable(v),rhs),_,_) =>
      wrap(rhs, props(v)).map{r2 => var_assign(Variable(v), r2) }
    case Reflect(VarPlusEquals(Variable(v), rhs),_,_) =>
      wrap(rhs, props(v)).map{r2 => var_plusequals(Variable(v), r2) }
    case Reflect(VarMinusEquals(Variable(v), rhs),_,_) => 
      wrap(rhs, props(v)).map{r2 => var_minusequals(Variable(v), r2) }
    case Reflect(VarTimesEquals(Variable(v), rhs),_,_) => 
      wrap(rhs, props(v)).map{r2 => var_timesequals(Variable(v), r2) }
    case Reflect(VarDivideEquals(Variable(v), rhs),_,_) => 
      wrap(rhs, props(v)).map{r2 => var_divideequals(Variable(v), r2) }

    // --- Atomic Writes
    case Reflect(NestedAtomicWrite(s, trace, d),_,_) => substituteAtomicWrite(s, d, trace)

    // --- conditional branch aliasing
    case op: DeliteOpCondition[_] => 
      val t1 = getBlockResult(op.thenp)
      val e1 = getBlockResult(op.elsep)
      val then2 = wrap(t1, props(s))
      val else2 = wrap(e1, props(s))
      (then2, else2) match {
        case (Some(t2), Some(e2)) => wrapBlocksWith(t1 -> t2, e1 -> e2) { Some(self_mirror(s, d)) }
        case (Some(t2), None) => wrapBlocksWith(t1 -> t2) { Some(self_mirror(s, d)) }
        case (None, Some(e2)) => wrapBlocksWith(e1 -> e2) { Some(self_mirror(s, d)) }
        case (None, None) => None
      }

    case op: AbstractLoop[_] => op.body match {
      case r: DeliteReduceElem[_] => 
        val map  = getBlockResult(r.func)
        val red  = getBlockResult(r.rFunc)
        val zero = getBlockResult(r.zero)
        val init = getBlockResult(r.accInit)
        val p = meetAll(MetaReduce)(props(map), props(red), props(zero), props(init))
        val m2 = wrap(map, p)
        val r2 = wrap(red, p)
        val z2 = wrap(zero, p)
        val i2 = wrap(init, p)
        wrapBlocksWith() {
          m2.foreach{m2 => subst += map -> m2}
          r2.foreach{r2 => subst += red -> r2}
          z2.foreach{z2 => subst += zero -> z2}
          i2.foreach{i2 => subst += init -> i2}
          val e = self_mirror(s, d)
          setMetadata(e, getBuffer(p)); setMetadata(e, getView(p))
          setMetadata(r.rV._1, getBuffer(p)); setMetadata(r.rV._1, getView(p))
          setMetadata(r.rV._2, getBuffer(p)); setMetadata(r.rV._2, getView(p))

          //println("Created reduce with props " + makeString(props(e)))
          /*e match { case Def(op: AbstractLoop[_]) => op.body match {
            case r: DeliteReduceElem[_] => 
              println("Func: " + strDef(r.func.res))
              println("Reduce: " + strDef(r.rFunc.res))
              println("Zero: " + strDef(r.zero.res))
              println("Init: " + strDef(r.accInit.res))
            case _ => // Nothing
          }}*/

          Some(e)
        }

      case _ => None
    }
 
    case op@DeliteMultiArrayFold(ma,_,zero) =>
      val res = getBlockResult(op.body)
      val r2 = wrap(res, child(ma))
      val z2 = wrap(zero, child(ma))
      if (r2.isDefined || z2.isDefined) {
        wrapBlocksWith() {
          r2.foreach{r2 => subst += res -> r2}
          z2.foreach{z2 => subst += zero -> z2}
          val e = self_mirror(s, d)
          setMetadata(e, getBuffer(child(ma)) )
          setMetadata(e, getView(child(ma)) )
          Some(e)
        }
      } else None
    case op@DeliteMultiArrayReduce(ma,_,zero) => 
      val res = getBlockResult(op.body)
      val r2 = wrap(res, child(ma))
      val z2 = wrap(zero, child(ma))
      if (r2.isDefined || z2.isDefined) {
        wrapBlocksWith() {
          r2.foreach{r2 => subst += res -> r2}
          z2.foreach{z2 => subst += zero -> z2}
          val e = self_mirror(s, d)
          setMetadata(e, getBuffer(child(ma)) )
          setMetadata(e, getView(child(ma)) )
          Some(e)
        }
      } else None
    
    case op@DeliteMultiArrayFilterReduce(ma,z,_,_,_) => 
      // TODO: Clean this up - rather odd right now
      // Zero and reduce body must both be views (at least physically)
      // They may also need to be buffers if the input multiarray is a buffer 
      val res = getBlockResult(op.redFunc)
      val r2 = wrap(res, props(ma) withData MView(PhysType))
      val z2 = wrap(z, props(ma) withData MView(PhysType))
      if (r2.isDefined || z2.isDefined) {
        wrapBlocksWith() {
          r2.foreach{r2 => subst += res -> r2}
          z2.foreach{z2 => subst += z -> z2}
          val e = self_mirror(s, d)
          e match { case Def(op@DeliteMultiArrayFilterReduce(_,_,_,_,_)) => setProps(e, props(op.redFunc)) }
          Some(e)
        }
      } else None

    // TODO: Clean this up
    case Reflect(d: AtomicWrite[_], _, _) => substituteAtomicWrite(s, d, Nil) // technically not right... "s" is in update sym position
    case Reflect(d, u, es) => transformSym(s, d).map{e => 
      transferMetadata(e, s, d)
      e match {
        case Def(Reify(_,_,_)) => e 

        case Def(Reflect(d2, u2, es2)) => 
          val out = reflectMirrored(Reflect(d2, mapOver(f,u) andAlso u2, (f(es) ++ es2).distinct))(mtype(e.tp), ctx)
          setProps(out, getProps(e))
            
          // Also need to scrub intermediate Reflect node, otherwise its in the context list
          if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
          (out)
        case Def(d2) =>
          val out = reflectMirrored(Reflect(d2, mapOver(f,u), f(es)))(mtype(e.tp), ctx)
          setProps(out, getProps(e))

          // Scrub intermediate def, otherwise its in the context list
          if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
          (out)
      }
    }

    case _ => None
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
 
  override def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = {
    val p = props(orig)
    p.data.keys foreach {k => 
      if (k != "isView" && k != "isBuffer")
        setMetadata(sub, p(k))
    }
  }
}