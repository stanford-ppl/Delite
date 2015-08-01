package ppl.delite.framework.transform

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._

import ppl.delite.framework.ops.DeliteOpsExp

import scala.collection.immutable
import scala.reflect.SourceContext

// Quick and dirty method for dumping stack trace to stdout
/*import java.io.{StringWriter, PrintWriter}
  try { throw new Exception("wrap subTP") }
    catch { case e: Throwable => 
      val sw = new StringWriter()
      e.printStackTrace(new PrintWriter(sw))
      printmsg(sw.toString())
    }*/

// TODO: Unify this with ForwardTransformer - there's a lot of duplication here
trait TransformerBase extends AbstractSubstTransformer with IterativeIRVisitor { self =>
  val IR: DeliteOpsExp with MetadataOps
  import IR._
  // substitutions which should carry over to the next iteration
  var nextSubst = immutable.Map.empty[Exp[Any], Exp[Any]]

  // TODO: Probably want to refactor this
  protected def f = self.asInstanceOf[Transformer]

  override def hasConverged = runs > 0 && nextSubst.isEmpty
  override def hasContext = true

  // Update innerScope to reflect changes/additions to this block
  // This allows multiple transformations of the same block in a single pass
  // Important because a pattern like:
  // val block2 = transformBlock(block)
  // val block3 = transformBlock(block2)
  // will silently eat the Reify of block2 without these additions to innerScope
  // Also needed(?) to create new blocks in the IR during transform?
  // TODO: This should not be here! This should be in LMS scheduling!
  def withInnerScopeAdditions[A](block: => Block[A]): Block[A] = {
    val prevDefs = globalDefs
    val tblock = block
    val newDefs = globalDefs filterNot { prevDefs contains _ }
    if (innerScope ne null)
      innerScope = innerScope ::: newDefs
    //else
    //  innerScope = newDefs  // Is this correct/needed? innerScope is null at top?
    (tblock)
  }

  override def reflectBlock[A](block: Block[A]): Exp[A] = withSubstScope {
    traverseBlock(block)
    apply(getBlockResult(block))
  }
  def transformBlock[A:Manifest](block: Block[A]): Block[A] = {
    withInnerScopeAdditions{ reifyEffects{ reflectBlock(block) } }
  }

  override def apply[A:Manifest](xs: Block[A]): Block[A] = transformBlock(xs)

  override def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
    case Some(y) => 
      // HACK: Mirror tunable parameter mappings
      if (tunableParams.contains(x) && !tunableParams.contains(y))
        tunableParams += (y -> tunableParams(x))
      
      y.asInstanceOf[Exp[A]] 
    case _ => x
  }

  def register[A](x: Exp[A])(y: Exp[A]): Unit = {
    if (nextSubst.contains(x))
      printdbg("discarding, already have a replacement for " + x)
    else {
      printdbg("register replacement for " + x)
      nextSubst += (x -> y)
      subst += (x -> y)
    }
  }

  /**
   * Remove an intermediate (dead) sym from local def table, global def table,
   * and context symbol list. Needed to keep intermediate steps from causing 
   * code duplication by getting into reflect/reify node symbol lists
   * TODO: Does NOT remove from symbol table - should it?
   * TODO: This is rather hacky - is there API for this kind of thing?
   */
  def scrubSym(sym: Sym[Any]) = {
    def scrubIntermediateSym(stms: List[Stm]) = stms filterNot {
      case TP(lhs,rhs) => (lhs == sym)
      case _ => false
    }
    localDefs = scrubIntermediateSym(localDefs)
    globalDefs = scrubIntermediateSym(globalDefs)
    context = context filterNot {s => s == sym}
  }

  override def traverseStm(stm: Stm): Unit = transformStm(stm)

  def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(s, d) if (apply(s) == s) => 
      implicit val ctx: SourceContext = mpos(s.pos)

      printDebug("Transforming: " + strDef(s))
      val sub = transformSym(s,d) match {
        case Some(s2) => 
          transferMetadata(s2, s, d)
          (s2)
        case None => 
          self_mirror(s,d)
      }
      if (subst.contains(s) && subst(s) != sub)
        printmsg("error: already have substitution for " + strDef(s) + ":\n\t" + strDef(subst(s)))
        
      printDebug("Created: " + strDef(sub))

      assert(!subst.contains(s) || subst(s) == sub)
      if (s != sub) { subst += s -> sub }

    case TTP(syms, m, d) if syms.forall{s => apply(s) == s} => 
      implicit val ctx: SourceContext = mpos(syms(0).pos)

      val syms2 = transformFatDef(syms, d) match {
        case Some(syms2) => syms2
        case None => self_fat_mirror(syms, d)
      }
      syms.zip(syms2).foreach{s => 
        assert(!subst.contains(s._1) || subst(s._1) == s._2)
        if (s._1 != s._2) subst += (s._1 -> s._2)
      }
      
    case _ => 
      printDebug(s"Statement $stm already had substition rule. Doing nothing.")
      cwarn("Already have substitution for symbols in statement " + stm)
  }

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    if (debugMode) {
      printmsg("--------------------------------------------------------")
      printmsg(name + ": Starting iteration " + runs)
      printmsg("--------------------------------------------------------")
    }
    subst = subst ++ nextSubst
    nextSubst = Map.empty
    transformBlock(b)
  }

  def transformFatDef[A](syms: List[Exp[Any]], d: FatDef)(implicit ctx: SourceContext): Option[List[Exp[Any]]] = None

  // Create Some replacement for given definition node if required, None if not
  def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]]
  // Transfer metadata from original symbol to transformed symbol (can also be done during trasnformation)
  def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit

  def self_fat_mirror(syms: List[Sym[Any]], rhs: FatDef)(implicit ctx: SourceContext): List[Exp[Any]] = {
    mirror(syms, rhs, self.asInstanceOf[Transformer])
  }

  // TODO: Will have to move this change to generic transformer framework eventually
  def self_mirror[A](sym: Sym[A], rhs : Def[A])(implicit ctx: SourceContext): Exp[A] = {
    try {
      val s2 = mirror(rhs, self.asInstanceOf[Transformer])(mtype(sym.tp),ctx) // cast needed why?
      setProps(s2, getProps(sym))
      (s2)
    } catch { //hack -- should not catch errors
      case e if e.toString contains "don't know how to mirror" => 
        printerr(e.getMessage)
        sym
      case e: Throwable => 
        printerr("exception during mirroring of "+rhs+": "+ e)
        e.printStackTrace; 
        sym            
    }
  }
}

// Ignores all Reflects on defs in transformSym (effectively looks like it uses EatReflect everywhere)
// Alternative name idea: HungryTransformer (since it eats a lot... haha)
trait TunnelingTransformer extends TransformerBase {
  import IR._

  override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]] = d match {
    case Reflect(d, u, es) => 
      transformSym(s, d) match {
        case None => None
        case Some(e) => 
          transferMetadata(e, s, d)

          e match {
            case Def(Reify(_,_,_)) => Some(e)
            
            case Def(Reflect(d2,u2,es2)) => 
              val out = reflectMirrored(Reflect(d2, mapOver(f,u) andAlso u2, (f(es) ++ es2).distinct))(mtype(e.tp), ctx)
              setProps(out, getProps(e))

              if (out != e) scrubSym(e.asInstanceOf[Sym[Any]])
              Some(out)

            case Def(d2) => 
              val out = reflectMirrored(Reflect(d2, mapOver(f,u), f(es)))(mtype(e.tp), ctx)
              setProps(out, getProps(e))

              if (out != e) scrubSym(e.asInstanceOf[Sym[Any]])
              Some(out)

            case e => Some(e)
        }
      }
    // TODO: This is probably not needed
    //case Reify(x,u,es) => Some(reflectPure(Reify(f(x),mapOver(f,u),f(es)))(f(x).tp,ctx))
    case _ => None
  }
}
