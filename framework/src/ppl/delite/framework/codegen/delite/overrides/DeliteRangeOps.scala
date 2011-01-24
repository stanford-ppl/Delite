package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.RangeOpsExp
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.internal._

trait DeliteRangeOpsExp extends RangeOpsExp {
  this: DeliteOpsExp =>

  case class DeliteRangeForEach(start: Exp[Int], end: Exp[Int], i: Exp[Int], body: Exp[Unit])
    extends DeliteOpIndexedLoop(start, end, i, body)

  override def range_foreach(r: Exp[Range], block: Exp[Int] => Exp[Unit]) : Exp[Unit] = {
    val i = fresh[Int]
    //reflectEffect(RangeForeach(r, i, reifyEffects(block(i))))
    val (start,end) = r match {
      case Def(Until(start,end)) => (start,end)
      case _ => throw new Exception("unexpected symbol in RangeForeach")
    }
    reflectEffect(DeliteRangeForEach(start, end, i, reifyEffects(block(i))))
  }
}

trait DeliteBaseGenRangeOps extends GenericNestedCodegen {
  val IR: DeliteRangeOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DeliteRangeForEach(start, end, i, body) if shallow => syms(start) ::: syms(end) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case DeliteRangeForEach(start, end, i, body) => getFreeVarBlock(body,List(i.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }
}

trait DeliteScalaGenRange extends ScalaGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case DeliteRangeForEach(start, end, i, body) => {
      stream.println("var " + quote(i) + " : Int = " + quote(start))
      stream.println("val " + quote(sym) + " = " + "while (" + quote(i) + " < " + quote(end) + ") {")
      nestedEmission = true
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println(quote(i) + " = " + quote(i) + " + 1")
      stream.println("}")
    }

    case _ => super.emitNode(sym, rhs)
  }
}

trait DeliteCudaGenRange extends CudaGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
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

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case DeliteRangeForEach(start, end, i, body) =>
      stream.println("for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
