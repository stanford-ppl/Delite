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

  // Get symbol properties for data field
  def mdat(b: Block[Any]): SymbolProperties = mdat(b.res)
  def mdat(e: Exp[Any]): SymbolProperties = mdat(props(e))
  def mdat(p: SymbolProperties): SymbolProperties = p match {
    case (s: StructProperties) => s.child("data").get
    case (a: ArrayProperties) => a.child.get
    case _ => sys.error("Symbol properties " + makeString(p) + " has no data field")
  }
}

trait SinglePassTransformer extends ForwardTransformer {
  import IR._

  def f = this.asInstanceOf[Transformer]

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) if apply(sym) == sym =>
      val replace = transformStm(stm)
      assert(!subst.contains(sym) || subst(sym) == replace)
      if (sym != replace) {
        register(sym -> replace)  // record substitution only if result is different
      }

    case TP(sym, rhs) =>
      if (recursive.contains(sym))  // O(n) since recursive is a list!
        transformStm(stm)
      else
        // TODO: What to do when sym already has replacement? Bail out? Look up def and then transform???
        printerr("warning: transformer already has a substitution " + sym + "->" + apply(sym) + " when encountering stm " + stm)
  }

  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(lhs,rhs) => transform(lhs,rhs)(mtype(lhs.tp),mpos(lhs.pos)) match {
      case Some(e) => e
      case _ => self_mirror(lhs, rhs)
    }
  }

  def transform[A:Manifest](lhs: Sym[A], rhs: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = None

  protected def mirrorExp[A](e: Exp[A]) = e match {
    case Def(d) => self_mirror(e.asInstanceOf[Sym[A]], d.asInstanceOf[Def[A]])
    case _ => e
  }
}

// Adds a rule for what to do if a node already has a substitution rule when it is reached in traversal
// Given that the IR is hierarchical, this case suggests an outer parent set up a transformation for a child block, then called
// for a traversal over that block to put the transformation into effect. In this case, the expected behavior is to take the
// current substitution as if it was the original node and perform mirroring/transformation as usual.
trait MultiPassTransformer extends SinglePassTransformer {
  import IR._

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) if apply(sym) == sym =>
      //debug(s"Encountered $sym = $rhs")
      val replace = transform(sym, rhs)(mtype(sym.tp),mpos(sym.pos)).getOrElse(self_mirror(sym,rhs))
      assert(!subst.contains(sym) || subst(sym) == replace, s"when transforming $sym, subst already contains $sym => $replace")
      if (sym != replace) {
        register(sym -> replace) // record substitution only if result is different
      }

      //debug(s"  replacing with $replace")

    // Someone else has already mirrored/transformed us!
    // Assumed case: Some higher scope has a block which includes us, and they've already gone through and
    // mirrored some or all of the nodes in that block before traversing the block
    // The correct thing to do here is mirror the previously transformed node, then scrub the intermediate node from
    // the IR def and context lists so it doesn't appear in any effects lists.
    case TP(sym, rhs) =>
      val sym2 = apply(sym)
      val replace = mirrorExp(sym2)
      if (replace != sym2 && sym != sym2) scrubSym(sym2.asInstanceOf[Sym[Any]])
      if (sym != replace) {
        register(sym -> replace) // Change substitution from sym -> sym2 to sym -> replace
      }
  }

  // Should be used within some outer reifyBlock scope
  def inlineBlock[A:Manifest](blk: Block[A]): Exp[A] = {
    traverseBlock(blk)
    apply(getBlockResult(blk))
  }
}
