package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.analysis._
import ppl.delite.framework.DeliteApplication

trait MultiArrayWrapExp extends DeliteApplication with DeliteMultiArrayOpsExp { 

  case class MultiArrayBuffify[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class MultiArrayViewify[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@MultiArrayBuffify(ma) => MultiArrayBuffify(f(ma))(e.mA)
    case e@MultiArrayViewify(ma) => MultiArrayViewify(f(ma))(e.mA)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait MultiArrayWrapTransformer extends TransformerBase with MultiArrayHelperStageTwo {
  val IR: MultiArrayWrapExp with RankMetadata
  import IR._
  override val name = "MultiArray Wrapper"
  override val debugMode = true

  def buffify[T:Manifest](x: Exp[DeliteMultiArray[T]])(implicit ctx: AnalysisContext): Exp[DeliteMultiArray[T]] = {
    if (!isPhysBuffer(x)) {
      val x2 = reflectPure(MultiArrayBuffify(x))
      setProps(x2, getProps(x))
      setMetadata(x2, MBuffer(PhysType))
      (x2)
    }
    else x
  }

  def viewify[T:Manifest](x: Exp[DeliteMultiArray[T]])(implicit ctx: AnalysisContext): Exp[DeliteMultiArray[T]] = {
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
  def wrap(e: Exp[Any], out: SymbolProperties)(implicit ctx: AnalysisContext): Option[Exp[DeliteMultiArray[Any]]] = {
    if (isMultiArrayType(e.tp)) {
      if (isPhysBuffer(out) && !isPhysBuffer(e) && isPhysView(out) && !isPhysView(e)) {
        Some(viewify(buffify(e.asInstanceOf[Exp[DeliteMultiArray[Any]]])))
      }
      else if (isPhysBuffer(out) && !isPhysBuffer(e)) { Some(buffify(e.asInstanceOf[Exp[DeliteMultiArray[Any]]])) }
      else if (isPhysView(out) && !isPhysView(e)) { Some(viewify(e.asInstanceOf[Exp[DeliteMultiArray[Any]]])) }
      else None
    }
    else None
  }

  override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: AnalysisContext): Option[Exp[Any]] = d match {
    // --- var aliasing
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
    case Reflect(NestedAtomicWrite(s, trace, d),_,_) => 
      substituteAtomicWrite(s, d, trace)

    // --- conditional branch aliasing
    case DeliteIfThenElse(cond, thenp, elsep, flat) => 
      val then2 = wrap(thenp.res, props(s))
      val else2 = wrap(elsep.res, props(s))
      (then2, else2) match {
        case (Some(t2), Some(e2)) => Some(delite_ifThenElse(cond, t2, e2, flat, true))
        case (Some(t2), None) => Some(delite_ifThenElse(cond, t2, elsep.res, flat, true))
        case (None, Some(e2)) => Some(delite_ifThenElse(cond, thenp.res, e2, flat, true))
        case (None, None) => None
      }

    case Reflect(d, _, _) => substituteAtomicWrite(s, d, Nil)
    case _ => None
  }

  def substituteAtomicWrite(s: Exp[Any], d: Def[Any], trace: List[AtomicTracer])(implicit ctx: AnalysisContext): Option[Exp[Any]] = d match {
    // --- Updates
    case e@DeliteMultiArrayUpdate(ma,i,x) => 
      val lhs = if (trace.isEmpty) child(ma) else followTrace(props(s), trace).asInstanceOf[ArrayProperties].child.get
      wrap(x, lhs).map{x2 => dmultia_update(ma, i, x2)(e.mA, mpos(ctx.defPos)) }
      
    case FieldUpdate(struct,index,x) => 
      val lhs = if (trace.isEmpty) child(struct,index) else followTrace(props(s), trace).asInstanceOf[StructProperties].child(index).get
      wrap(x, lhs).map{x2 => field_update(struct, index, x2)(x2.tp) }

    // --- Buffers
    case e@DeliteMultiArrayInsert(ma,x,i) => 
      val lhs = if (trace.isEmpty) child(ma) else followTrace(props(s), trace).asInstanceOf[ArrayProperties].child.get
      wrap(x, lhs).map{x2 => dmultia_insert(ma, x2.as1D, i)(e.mA, mpos(ctx.defPos)) }
      
    // TODO: How to wrap here?
    case DeliteMultiArrayInsertAll(ma,x,a,i) => None

    case _ => None
  }
 
  override def transferMetadata(orig: Exp[Any], e2: Exp[Any], d: Def[Any])(implicit ctx: AnalysisContext): Unit = {
    copyMetadata(e2, props(orig))
  }
}