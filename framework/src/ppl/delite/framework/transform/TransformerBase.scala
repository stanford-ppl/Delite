package ppl.delite.framework.transform

import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._
import ppl.delite.framework.visit._
import ppl.delite.framework.visit.Meetable._
import ppl.delite.framework.ops.DeliteOpsExp

import scala.collection.immutable

// Quick and dirty method for dumping stack trace to stdout
/*
  import java.io.{StringWriter, PrintWriter}

  try { throw new Exception("wrap subTP") }
  catch { case e: Throwable => 
    val sw = new StringWriter()
    e.printStackTrace(new PrintWriter(sw))
    printmsg(sw.toString())
  }
*/

/** 
 * Made separate from WorklistTransformer for now since my concept of curSubst
 * and nextSubst are a little different from what WorklistTransformer does
 */  
trait TransformerBase extends AbstractSubstTransformer with IterativeIRVisitor with MetadataTransformer { self =>
  val IR: DeliteOpsExp with DeliteMetadata
  import IR._
  // substitutions which should carry over to the next iteration
  var nextSubst = immutable.Map.empty[Exp[Any], Exp[Any]]

  override def hasConverged = runs > 0 && nextSubst.isEmpty
  override def hasContext = true
  override def notifyUpdate(e: Exp[Any]): Unit = { notifyChange() }

  // Removed withSubstScope to preserve subst across iterations
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    traverseBlock(block)
    apply(getBlockResult(block))
  }
  def transformBlock[A:Manifest](block: Block[A]): Block[A] = {
    reifyEffects{ reflectBlock(block) }
  }

  override def apply[A:Manifest](xs: Block[A]): Block[A] = transformBlock(xs)

  override def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
    case Some(y) => y.asInstanceOf[Exp[A]] 
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

  // Transform blocks prior to transforming current 
  override def traverseStm(stm: Stm): Unit = { transformStm(stm) }

  def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(s, d) if (apply(s) == s) => 
      
      //printmsg("Context prior to block traversal: " + context)
      //super.traverseStm(stm)
      //printmsg("Context after block traversal: " + context)

      //printmsg(blocks(d).map{b => "\t" + strDef(b.res)}.mkString("\n"))
      val sub = transformSym(s,d)(AnalysisContext(stm)) match {
        case Some(s2) => 
          transferMetadata(s, s2, d)(AnalysisContext(stm))
          (s2)
        case None => 
          self_mirror(s,d)
      }
      assert(!subst.contains(s) || subst(s) == sub)
      if (s != sub) { subst += s -> sub }

      //printmsg("\n" + name + " at statement: " + s + " -> " + sub)
      //printmsg("Context after transform: " + context)

    case TP(s,d) => 
      //printmsg("\n" + strDef(s)) 
      //printmsg("<>" + strDef(apply(s)))
      // Ignore statements that already have mappings
    case _ => //Nothing
  }

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    if (debugMode) {
      printmsg("")
      printmsg("------------------------------------------")
      printmsg(name + ": Starting iteration " + runs)
      printmsg("------------------------------------------")
    }
    subst = subst ++ nextSubst // TODO: is this needed?
    nextSubst = Map.empty
    transformBlock(b)
  }

  // Create Some replacement for given definition node if required, None if not
  def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: AnalysisContext): Option[Exp[Any]]
  // Transfer metadata from original symbol to transformed symbol (can also be done during trasnformation)
  def transferMetadata(orig: Exp[Any], e2: Exp[Any], d: Def[Any])(implicit ctx: AnalysisContext): Unit

  // TODO: Will have to move this change to generic transformer framework eventually
  def self_mirror[A](sym: Sym[A], rhs : Def[A]): Exp[A] = {
    try {
      val s2 = mirror(rhs, self.asInstanceOf[Transformer])(mtype(sym.tp),mpos(sym.pos)) // cast needed why?
      setProps(s2, getProps(sym))(AnalysisContext(sym.pos, sym.pos, ""))
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
