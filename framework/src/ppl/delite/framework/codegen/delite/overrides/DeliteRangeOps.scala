package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.RangeOpsExp
import scala.virtualization.lms.common.{ScalaGenEffect, CudaGenEffect, CGenEffect}
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen}

trait DeliteRangeOpsExp extends RangeOpsExp {
  this: DeliteOpsExp =>

  case class DeliteRangeForEach(start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Exp[Unit])
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
    case DeliteRangeForEach(start, end, i, body) => syms(start):::syms(end):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case DeliteRangeForEach(start, end, i, body) => i::effectSyms(body)
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = rhs match {
    case DeliteRangeForEach(start, end, i, body) => getFreeVarBlock(body,List(i.asInstanceOf[Sym[Any]]))
    case _ => super.getFreeVarNode(rhs)
  }
}

trait DeliteScalaGenRange extends ScalaGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DeliteRangeForEach(start, end, i, body) => {
      stream.println("var " + quote(i) + " : Int = " + quote(start))
      stream.println("val " + quote(sym) + " = " + "while (" + quote(i) + " < " + quote(end) + ") {")
      //nestedEmission = true TODO?
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // TODO: What if the range is not continuous integer set?
    case DeliteRangeForEach(start, end, i, body) => {
        //var freeVars = buildScheduleForResult(body).filter(scope.contains(_)).map(_.sym)
        val freeVars = getFreeVarBlock(body,List(i.asInstanceOf[Sym[Any]]))

        // Add the variables of range to the free variable list if necessary
        var paramList = freeVars
        //val Until(startIdx,endIdx) = findDefinition(r.asInstanceOf[Sym[Range]]).map(_.rhs).get.asInstanceOf[Until]
        if(start.isInstanceOf[Sym[Any]]) paramList = start.asInstanceOf[Sym[Any]] :: paramList
        if(end.isInstanceOf[Sym[Any]]) paramList = end.asInstanceOf[Sym[Any]] :: paramList
        paramList = paramList.distinct
        val paramListStr = paramList.map(ele=>remap(ele.Type) + " " + quote(ele)).mkString(", ")

        if(parallelFor) {
          //stream.println("__global__ gpuKernel_%s(%s) {".format(quote(sym),paramListStr))
          tabWidth += 1
          stream.println(addTab()+"int %s = blockIdx.x*blockDim.x + threadIdx.x;".format(quote(i)))
          //stream.println(addTab() + "%s = %s + %s;".format(quote(i), quote(i), quote(startIdx)))
          //stream.println(addTab()+"if(%s < %s) {".format(quote(i), quote(endIdx)))
          stream.println(addTab() + "%s = %s + %s;".format(quote(i), quote(i), quote(start)))
          stream.println(addTab()+"if(%s < %s) {".format(quote(i), quote(end)))
          tabWidth += 1
          // No parallelism in the inner block
          parallelFor = true
          emitBlock(body)
          parallelFor = false
          tabWidth -= 1
          stream.println(addTab()+"}")
          tabWidth -= 1
          stream.println(addTab()+"}")
        }
        else {
          stream.println(addTab()+"for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
          tabWidth += 1
          emitBlock(body)
          tabWidth -= 1
          stream.println(addTab() + "}")
        }
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait DeliteCGenRange extends CGenEffect with DeliteBaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DeliteRangeForEach(start, end, i, body) =>
      stream.println("for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
