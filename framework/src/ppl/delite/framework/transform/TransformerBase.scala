package ppl.delite.framework.transform

import scala.reflect.{RefinedManifest,SourceContext}
import scala.collection.immutable
import scala.virtualization.lms.common.{ForwardTransformer, EffectExp, Record}
import scala.virtualization.lms.internal.{FatExpressions, IterativeTraversal, Transforming}

import ppl.delite.framework.datastructures._

// Useful operations for transforming
trait DeliteTransforming extends Transforming {
  this: DeliteStructsExp with DeliteArrayOpsExp =>

  // Get inner element type for given data structure type
  def dataTp[A,B:Manifest](x: Exp[A]): Manifest[B] = dataTp(x.tp)
  def dataTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp match {
    // Check if type is DeliteArray first to avoid issues with AOS - SOA in StructType unapplies
    case t if isDeliteArrayType(t) => t.typeArguments(0).asInstanceOf[Manifest[B]]
    case StructType(_,elems) => elems.find(_._1 == "data").getOrElse(
      throw new RuntimeException("Can't find data field for " + tp)
    )._2.typeArguments(0).asInstanceOf[Manifest[B]]
    case t if !t.typeArguments.isEmpty => t.typeArguments(0).asInstanceOf[Manifest[B]]
    case _ => sys.error("Cannot get data type of " + tp + " - type has no type arguments")
  }

  def fieldTp[A,B:Manifest](x: Exp[A], index: String): Manifest[B] = fieldTp(x.tp, index)
  def fieldTp[A,B:Manifest](tp: Manifest[A], index: String): Manifest[B] = tp match {
    case StructType(_,elems) => elems.find(_._1 == index).getOrElse{
      throw new RuntimeException("Can't find field " + index +" for " + tp)
    }._2.typeArguments(0).asInstanceOf[Manifest[B]]
    case _ => sys.error("Cannot get field type of " + tp + " - not a struct type")
  }

  // --- Type transformation
  /**
   * Transform saved manifests to match transformed IR
   * @param tp - element type manifest, to be transformed
   * @param p  - symbol properties matching type
   */
  def ttype[A,B:Manifest](tp: Manifest[A], p: SymbolProperties): Manifest[B] = (tp,p) match {
    case (StructType(_,elems), s: StructProperties) =>
      new RefinedManifest[Record] {
        def runtimeClass = classOf[Record]
        val fields = elems.map{f => f._1 -> ttype(f._2, s.child(f._1).get) }
      }.asInstanceOf[Manifest[B]]
    case (tp, a: ArrayProperties) if isDeliteArrayType(tp) =>
      val inner = ttype(tp.typeArguments(0), a.child.get)
      darrayManifest(inner).asInstanceOf[Manifest[B]]
    case (tp, a: ScalarProperties) => tp.asInstanceOf[Manifest[B]]
    case _ => sys.error("Don't know how to transform type " + tp + " with associated properties " + p)
  }
}



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