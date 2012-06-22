package ppl.delite.framework.codegen.delite.overrides

import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.{WhileExp}
import scala.virtualization.lms.common.{ScalaGenEffect, CudaGenEffect, OpenCLGenEffect, CGenEffect}
import scala.virtualization.lms.internal.{GenericNestedCodegen}
import scala.reflect.SourceContext
import java.io.PrintWriter
import scala.reflect.SourceContext

trait DeliteWhileExp extends WhileExp with DeliteOpsExp {

  this: DeliteOpsExp =>

  // there is a lot of duplication between DeliteWhile and While in lms -- do we really need a separate class here?

  case class DeliteWhile(cond: Block[Boolean], body: Block[Unit]) extends DeliteOpWhileLoop

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] = {
    //val c = reifyEffects(cond)
    //val a = reifyEffects(body)
    // TODO: reflectEffect(new While(c, a) with DeliteOpWhile))
    //reflectEffect(DeliteWhile(c, a))
    val c = reifyEffects(cond)
    c.res match {
      case Const(true)  => // print warning?
      case Const(false)  => return
      case _ => // pass
    }

    val a = reifyEffects(body)
    val ce = summarizeEffects(c)
    val ae = summarizeEffects(a)
    reflectEffect(DeliteWhile(c, a), ce andThen ((ae andThen ce).star))
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DeliteWhile(c, b) => syms(c):::syms(b) // wouldn't need to override...
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteWhile(c, b) => effectSyms(c):::effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case DeliteWhile(c, b) => freqHot(c) ++ freqHot(b)
    case _ => super.symsFreq(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case Reflect(e@DeliteWhile(cond, block), u, es) => {
      if (f.hasContext) 
        __whileDo(f.reflectBlock(cond), f.reflectBlock(block))
      else
        reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteWhile(f(cond),f(block)), mapOver(f,u), f(es)))(mtype(manifest[A]))      
      }
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

}

trait DeliteBaseGenWhile extends GenericNestedCodegen {
  val IR: DeliteWhileExp
  import IR._

}

trait DeliteScalaGenWhile extends ScalaGenEffect with DeliteBaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteWhile(c,b) =>
      //val save = deliteKernel
      //deliteKernel = false
      // wrapping while loops in methods appears to slow things down 
      // stream.println("def " + quote(sym) + "_while = ")
      stream.println("val " + quote(sym) + " = while ({")
      emitBlock(c)
      stream.print(quote(getBlockResult(c)))
      stream.println("}) {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
      // stream.println("val " + quote(sym) + " = " + quote(sym) + "_while")
      //deliteKernel = save

    case _ => super.emitNode(sym, rhs)
  }
}

trait DeliteCudaGenWhile extends CudaGenEffect with DeliteBaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case DeliteWhile(c,b) =>
          emitBlock(c)
          stream.println(addTab() + remap(getBlockResult(c).tp) + " " + quote(sym) + "_cond = " + quote(getBlockResult(c)) + ";")
          stream.print(addTab() + "while (")
          stream.print(quote(sym) + "_cond")
          stream.println(") {")
          tabWidth += 1
          emitBlock(b)
          emitBlock(c)
          stream.println(addTab() + quote(sym) + "_cond = " + quote(getBlockResult(c)) + ";")
          tabWidth -= 1
          //stream.println(quote(getBlockResult(b)))   //TODO: Is this needed?
          stream.println(addTab() + "}")
        case _ => super.emitNode(sym, rhs)
      }
    }
}


trait DeliteOpenCLGenWhile extends OpenCLGenEffect with DeliteBaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case DeliteWhile(c,b) =>
          emitBlock(c)
          stream.println(addTab() + remap(getBlockResult(c).tp) + " " + quote(sym) + "_cond = " + quote(getBlockResult(c)) + ";")
          stream.print(addTab() + "while (")
          stream.print(quote(sym) + "_cond")
          stream.println(") {")
          tabWidth += 1
          emitBlock(b)
          emitBlock(c)
          stream.println(addTab() + quote(sym) + "_cond = " + quote(getBlockResult(c)) + ";")
          tabWidth -= 1
          //stream.println(quote(getBlockResult(b)))   //TODO: Is this needed?
          stream.println(addTab() + "}")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait DeliteCGenWhile extends CGenEffect with DeliteBaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case DeliteWhile(c,b) =>
        // calculate condition
        emitBlock(c)
        stream.println("bool cond_%s = %s;".format(quote(sym),quote(getBlockResult(c))))
        // Emit while loop
        stream.print("while (cond_%s) {".format(quote(sym)))
        emitBlock(b)
        stream.println("}")
      case _ => super.emitNode(sym, rhs)
    }
  }
}
