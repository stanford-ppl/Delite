package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.Effects
import collection.mutable.ListBuffer
import java.io.PrintWriter


/**
 * This trait encodes a target for code generation, the target can have multiple code generators registered
 */
trait Target {

  val intermediate: BaseExp with Effects
  import intermediate._

  val name: String

  val generators : ListBuffer[CodeGenerator{val intermediate: Target.this.intermediate.type}]

  def addGenerator(c: CodeGenerator{val intermediate: Target.this.intermediate.type}) {
    generators += c
  }

  def emitSource[A,B](args: Exp[A], f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit
  
  def emitTargetNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) {
    var gen = generators.head
    var rest = generators.tail
    var success = false
    var finished = false
    var excStr = ""
    while(!success && !finished) {
      try{
        gen.emitNode(sym, rhs)
        success = true
      } catch {
        case e => {
          excStr = e.getMessage
          if(rest.isEmpty == false) {
            gen = rest.head
            rest = rest.tail
          } else finished = true
        }
      }
    }
    if(!success) throw new RuntimeException("[" + name + "]Generation Failed: " + excStr)
  }

  def getTargetSyms(e: Any, shallow: Boolean) : Option[List[Sym[Any]]] = {
    //println("calling target syms on " + e)
    var i = 0
    while (i < generators.length){
      val syms = generators(i).syms2(e, shallow)
      if (!syms.isEmpty) {
        //println("found overridden dependency")
        return syms
      }
      i += 1
    }
    None
  }

}