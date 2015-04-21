package ppl.delite.framework.ops

import ppl.delite.framework.Config
import scala.virtualization.lms.common._


trait ScalaGenStaticDataDelite extends ScalaGenStaticData {
  val IR: StaticDataExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StaticData(x) =>
      // TODO: should call super if we're generating a single big scala file
      emitValDef(sym, "ppl.delite.runtime.graph.ops.Arguments.staticData["+remap(sym.tp)+"](\""+quote(sym)+"\") // static data: " + x)
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenDeliteOps extends ScalaGenLoopsFat with ScalaGenStaticDataDelite with GenericGenDeliteOps with ScalaGenDeliteInternalOps { // not sure where to mix in ScalaGenStaticData
  import IR._

  def quotearg(x: Sym[Any]) = quote(x) + ": " + quotetp(x)
  def quotetp(x: Sym[Any]) = remap(x.tp)

  def methodCall(name:String, inputs: List[String] = Nil): String = {
    unalteredMethodCall(name, resourceInfoSym::inputs)
  }

  def emitMethodCall(name:String, inputs: List[String]) {
    stream.println(methodCall(name, inputs))
  }

  def emitMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit) {
    stream.println("// begin emitMethod")
    stream.println(unalteredMethodHeader(name, outputType, ((resourceInfoSym,resourceInfoType))::inputs) + " = {")
    body
    stream.println("}\n")
    stream.println("// end emitMethod")
  }

  def unalteredMethodHeader(name:String, outputType: String, inputs:List[(String,String)]): String = {
    "def " + name + "(" + inputs.map(i => i._1 + ":" + i._2).mkString(",") + "): " +  outputType
  }

  override def unalteredMethodCall(name:String, inputs: List[String] = Nil): String = {
    inputs match {
      case Nil => name
      case _ => name + "(" + inputs.mkString(",") + ")"
    }
  }

  override def emitUnalteredMethodCall(name:String, inputs: List[String]) {
    stream.println(unalteredMethodCall(name, inputs))
  }

  override def emitUnalteredMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit) {
    stream.println(unalteredMethodHeader(name, outputType, inputs) + " = {")
    body
    stream.println("}\n")
  }


  def createInstance(typeName:String, args: List[String] = Nil): String = {
    "new " + typeName + args.mkString("(",",",")")
  }

  def fieldAccess(className: String, varName: String): String = {
    if (className == "") varName
    else className + "." + varName
  }

  def releaseRef(varName: String) {
    stream.println(varName + " = null")
  }

  def emitReturn(rhs: String) = stream.print(rhs)

  def emitFieldDecl(name: String, tpe: String) {
    emitVarDef(name, tpe, "_")
  }

  def emitClass(name: String)(body: => Unit) {
    stream.println("final class " + name + " {")
    body
    stream.println("}")
  }

  def emitObject(name: String)(body: => Unit) {
    stream.println("object " + name + " {")
    body
    stream.println("}")
  }

  def emitValDef(name: String, tpe: String, init: String) {
    stream.println("val " + name + ": " + tpe + " = " + init)
  }

  def emitVarDef(name: String, tpe: String, init: String) {
    stream.println("var " + name + ": " + tpe + " = " + init)
  }

  def emitAssignment(name: String, tpe: String, rhs: String) = emitAssignment(name, rhs)

  def emitAssignment(lhs: String, rhs: String) {
    stream.println(lhs + " = " + rhs)
  }

  def emitAbstractFatLoopHeader(className: String, actType: String) {
    if (Config.debugCodegen) {
      println("[codegen] emitAbstractFatLoopHeader")
    }
    stream.println("val " + className + " = new generated.scala.DeliteOpMultiLoop[" + actType + "] {"/*}*/)
  }

  def emitAbstractFatLoopFooter() {
    if (Config.debugCodegen) {
      println("[codegen] emitAbstractFatLoopFooter")
    }
    stream.println("}")
  }

  def castInt32(name: String) = name + ".toInt"

  def refNotEq: String = "ne"
  def nullRef: String = "null"

  def arrayType(argType: String): String = "Array[" + argType + "]"
  def arrayApply(arr: String, idx: String): String = arr + "(" + idx + ")"
  def newArray(argType: String, size: String): String = createInstance(arrayType(argType),List(size))
  def hashmapType(argType: String): String = "generated.scala.container.HashMapImpl[" + argType + "]"
  def typeCast(sym: String, to: String): String = "(" + sym + ").asInstanceOf[" + to + "]"
  def withBlock(name: String)(block: => Unit): Unit = {
    emitValDef(name, remap(manifest[Unit]), "{")
    block
    stream.println("}")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] => {
      if (Config.debugCodegen) {
        println("[codegen] emitNode::DeliteOpSingleTask");
      }
      //printlog("EMIT single "+s)
      // always wrap single tasks in methods to reduce JIT compilation unit size
      val b = s.block
      stream.println("def " + quote(sym) + "_block = { ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
      stream.println("val " + quote(sym) + " = " + quote(sym) + "_block")
    }
    case op: AbstractLoop[_] =>
      if (Config.debugCodegen) {
        println("[codegen] emitNode::AbstractLoop");
      }
      // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      // if inline, wrap thin loops in methods to reduce JIT compilation unit size
      if (!deliteKernel) {
        stream.println("def " + quote(sym) + "_thin = {")
      }
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))
      if (!deliteKernel) {
        stream.println(quote(sym))
        stream.println("}")
        stream.println("val " + quote(sym) + " = " + quote(sym) + "_thin")
      }
    case _ => super.emitNode(sym,rhs)
  }

}
