package ppl.delite.framework.codegen.restage

import java.io.PrintWriter
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

import ppl.delite.framework.codegen.Target
import ppl.delite.framework.datastructures._
import ppl.delite.framework.{DeliteRestageOps,DeliteRestageOpsExp}
import ppl.delite.framework.ops.DeliteOpsExp

trait TargetRestage extends Target {
  import IR._

  val name = "Restage"
}

trait RestageCodegen extends ScalaCodegen with Config {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "scala"

  override def toString = "restage"

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val staticData = getFreeDataBlock(body)
    
    println("--RestageCodegen emitSource")
    
    withStream(out) {
      emitBlock(body)
      stream.println("val scope_0 = " + quote(getBlockResult(body))) // TODO: need a scopeId
    }    
    
    staticData
  }  
}

trait RestageFatCodegen extends GenericFatCodegen with RestageCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
  

}

// for now just lumping all the Delite restage generators together in one place..
trait DeliteCodeGenRestage extends RestageFatCodegen with ScalaGenDeliteArrayOps {
  val IR: Expressions with Effects with FatExpressions with DeliteRestageOpsExp with DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PreviousStageData(n) => emitValDef(sym, "scope_0") // TODO
    case a@DeliteArrayNew(n) => emitValDef(sym, "DeliteArray[" + remap(a.mA) + "](" + quote(n) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
