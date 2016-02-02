package ppl.delite.framework.transform

import scala.reflect.SourceContext
import scala.collection.immutable
import scala.virtualization.lms.common.{ForwardTransformer, EffectExp}
import scala.virtualization.lms.internal.{FatExpressions, IterativeTraversal}

// Modified version of WorklistTransformer
trait TransformerBase extends ForwardTransformer with IterativeTraversal { self =>
  val IR: EffectExp with FatExpressions
  import IR._

  var nextSubst = immutable.Map.empty[Exp[Any], Exp[Any]]
  def register(x: Exp[Any])(y: Exp[Any]): Unit = {
    if (nextSubst.contains(x))
      printdbg(s"Discarding, already have replacement for $x")
    else {
      printdbg(s"Register replacement for $x")
      nextSubst += (x -> y)
      subst += (x -> y)
    }
  }

  override def processBlock[A:Manifest](xs: Block[A]): Block[A] = {
    subst = subst ++ nextSubst
    nextSubst = Map.empty
    transformBlock(xs)
  }

  protected def f = self.asInstanceOf[Transformer]

  override def hasConverged = runs > 0 && nextSubst.isEmpty

  override def traverseStm(stm: Stm) = stm match {
    case TP(lhs, rhs) if apply(lhs) == lhs =>
      val replace = transformTP(lhs, rhs)(mpos(lhs.pos)) match {
        case Some(replace) =>
          transferMetadata(replace, lhs)(rhs)
          replace
        case None => self_mirror(lhs, rhs)
      }

    case TP(lhs, rhs) =>
      // This is actually fine in certain contexts, don't necessarily need a warning for it.
      printwarn(s"Transformer $name already has a substitution $lhs -> ${apply(lhs)} when encountering statement $stm")

    case TTP(syms, mhs, rhs) if syms.forall{s => apply(s) == s} =>
      transformTTP(syms, mhs, rhs)(mpos(syms.head.pos)) match {
        case Some(replace) =>
          syms.zip(replace).foreach{case(s,s2) =>
            assert(!subst.contains(s) || subst(s) == s2)
            if (s != s2) subst += (s -> s2)
          }
        case None => // TODO: Mirroring fat statements?
      }

    case TTP(lhs, mhs, rhs) =>
      printwarn(s"Transformer $name already has substitution for symbols in statement $stm")

    case _ => super.traverseStm(stm)
  }

  // Note: Shouldn't be calling transformStm from TransformerBase (slightly modified transformer design)
  override def transformStm(stm: Stm): Exp[Any] = throw new Exception("New transformer design - should not be calling transformStm here")

  // For user-friendliness? Is this safe?
  implicit def getSome(x: Exp[Any]): Option[Exp[Any]] = Some(x)

  def transformTP[A](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]]
  def transformTTP(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef)(implicit ctx: SourceContext): Option[List[Exp[Any]]] = None

  def transferMetadata(dest: Exp[Any], src: Exp[Any])(node: Def[Any]) {
    setProps(dest, meet(MetaOverwrite, props(dest), props(src)) )
  }
}


trait TunnelingTransformer extends TransformerBase {
  import IR._

  // TODO: Is this always correct? What happens when we transform something with an effect to something without one?
  // Does that ever happen in practice? Can always special case...
  override def transformTP[A](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = rhs match {
    case Reflect(d, u, es) =>
      transformTP(lhs, d) match {
        case None => None
        case Some(e: Sym[_]) =>
          transferMetadata(e, lhs)(d)

          e match {
            case Def(Reify(_,_,_)) => Some(e)
            case Def(Reflect(d2, u2, es2)) =>
              val out = reflectMirrored(Reflect(d2, mapOver(f,u) andAlso u2, (f(es) ++ es2).distinct))(mtype(e.tp), ctx)
              setProps(out, getProps(e))

              if (out != e) scrubSym(e)
              Some(out)

            case Def(d2) =>
              val out = reflectMirrored(Reflect(d2, mapOver(f,u), f(es)))(mtype(e.tp), ctx)
              setProps(out, getProps(e))

              if (out != e) scrubSym(e)
              Some(out)
          }
        case e => e
      }
    case _ => None
  }
}