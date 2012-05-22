/*
package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.RangeOpsExp
import scala.virtualization.lms.common.{ScalaGenEffect, CudaGenEffect, CGenEffect, OpenCLGenEffect}
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen}

trait DeliteRangeOpsExp extends RangeOpsExp with DeliteOpsExp {
  this: DeliteOpsExp =>

  // there is a lot of code duplication between DeliteWhile and While in lms -- do we really need a separate class?

  case class DeliteRangeForEach(start: Exp[Int], end: Exp[Int], index: Sym[Int], body: Exp[Unit])
    extends DeliteOpIndexedLoop

  override def range_foreach(r: Exp[Range], block: Exp[Int] => Exp[Unit]) : Exp[Unit] = {
    val i = fresh[Int]
    //reflectEffect(RangeForeach(r, i, reifyEffects(block(i))))
    val (start,end) = r match {
      case Def(Until(start,end)) => (start,end)
      case _ => throw new Exception("unexpected symbol in RangeForeach")
    }
    reflectEffect(DeliteRangeForEach(start, end, i, reifyEffects(block(i)))) //TODO: finer grained effects
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DeliteRangeForEach(start, end, i, body) => syms(start):::syms(end):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteRangeForEach(start, end, i, body) => i::effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case DeliteRangeForEach(start, end, i, body) => freqNormal(start) ++ freqNormal(end) ++ freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait DeliteBaseGenRangeOps extends GenericNestedCodegen {
  val IR: DeliteRangeOpsExp
  import IR._

}

trait DeliteScalaGenRange extends ScalaGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteRangeForEach(start, end, i, body) => {
      val save = deliteKernel
      deliteKernel = false
      stream.println("var " + quote(i) + " : Int = " + quote(start))
      stream.println("val " + quote(sym) + " = " + "while (" + quote(i) + " < " + quote(end) + ") {")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println(quote(i) + " = " + quote(i) + " + 1")
      stream.println("}")
      deliteKernel = save
    }

    case _ => super.emitNode(sym, rhs)
  }
}

trait DeliteCudaGenRange extends CudaGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // TODO: What if the range is not continuous integer set?
    case DeliteRangeForEach(start, end, i, body) => {
      stream.println(addTab()+"for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      tabWidth += 1
      emitBlock(body)
      tabWidth -= 1
      stream.println(addTab() + "}")
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait DeliteOpenCLGenRange extends OpenCLGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // TODO: What if the range is not continuous integer set?
    case DeliteRangeForEach(start, end, i, body) => {
      stream.println(addTab()+"for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      tabWidth += 1
      emitBlock(body)
      tabWidth -= 1
      stream.println(addTab() + "}")
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait DeliteCGenRange extends CGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DeliteRangeForEach(start, end, i, body) =>
      stream.println("for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
*/