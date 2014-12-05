package ppl.delite.framework.ops

import scala.virtualization.lms.common._


trait CGenDeliteOps extends CGenLoopsFat with GenericGenDeliteOps with CGenDeliteInternalOps {

  import IR._

  override def addRef(tpe: String): String = {
    if (tpe startsWith "activation") " *"
    else super.addRef(tpe)
  }

  def quotearg(x: Sym[Any]) = quotetp(x) + " " + quote(x)
  def quotetp(x: Sym[Any]) = remap(x.tp)

  def methodCall(name: String, inputs: List[String] = Nil): String = {
    unalteredMethodCall(name, (resourceInfoSym::inputs))
  }

  def emitMethodCall(name:String, inputs: List[String]) {
    stream.println(methodCall(name, inputs) + ";")
  }

  def emitMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit) {
    stream.println(unalteredMethodHeader(name, outputType, ((resourceInfoSym,resourceInfoType))::inputs) + " {")
    body
    stream.println("}\n")
  }

  def unalteredMethodHeader(name: String, outputType: String, inputs:List[(String,String)]): String = {
    remapWithRef(outputType) + name + "(" + inputs.map(i => remapWithRef(i._2) + i._1).mkString(",") + ")"
  }

  override def unalteredMethodCall(name: String, inputs: List[String] = Nil): String = {
    name + "(" + inputs.mkString(",") + ")"
  }

  override def emitUnalteredMethodCall(name: String, inputs: List[String]) {
    stream.println(unalteredMethodCall(name, inputs) + ";")
  }

  override def emitUnalteredMethod(name:String, outputType: String, inputs:List[(String,String)])(body: => Unit) {
    stream.println(unalteredMethodHeader(name, outputType, inputs) + " {")
    body
    stream.println("}\n")
  }


  def createInstance(typeName:String, args: List[String] = Nil): String = {
    "new " + typeName + args.mkString("(",",",");")
  }

  def fieldAccess(className: String, varName: String): String = {
    if (className == "") varName
    else className + "->" + varName
  }

  def releaseRef(varName: String) {
    //TODO: Change this to decrement the reference count?
    //stream.println("free(" + varName + ");")
    //stream.println(varName + " = NULL;")
    //stream.println(varName + ".reset();")
  }

  def emitReturn(rhs: String) = {
    stream.print("return " + rhs + ";")
  }

  def emitFieldDecl(name: String, tpe: String) {
    tpe match {
      case "void" => //
      case _ =>
        stream.println(remapWithRef(tpe) + name + ";")
    }
  }

  def emitClass(name: String)(body: => Unit) {
    stream.println("#ifndef __" + name + "__")
    stream.println("#define __" + name + "__")
    stream.println("#include \"" + deviceTarget + "helperFuncs.h\"")
    stream.println("#include \"Config.h\"")
    stream.println("class " + name + " {")
    stream.println("public:")
    body
    stream.println("};")
    stream.println("#endif")
  }

  def emitObject(name: String)(body: => Unit) {

  }

  def emitValDef(name: String, tpe: String, init: String) {
    emitVarDef(name, tpe, init)
  }

  def emitVarDef(name: String, tpe: String, init: String) {
    tpe match {
      case "void" => //
      case _ if tpe.contains("*") && isPrimitiveType(tpe.replaceAll("\\*","").trim) => // primitive type pointer (not cppDeliteArray)
        stream.println(tpe + " " + name + " = " + init + ";")
      case _ =>
        stream.println(remapWithRef(tpe) + " " + name + " = " + init + ";")
    }
  }

  def emitAssignment(name: String, tpe: String, rhs: String) {
    tpe match {
      case "void" => //
      case _ => emitAssignment(name, rhs)
    }
  }

  def emitAssignment(lhs: String, rhs: String) {
    stream.println(lhs + " = " + rhs + ";")
  }

  def emitAbstractFatLoopHeader(className: String, actType: String) {
    stream.println("#ifndef __" + kernelName + "__")
    stream.println("#define __" + kernelName + "__")
    stream.println("class " + kernelName + "{")
    stream.println("public:")
    emitFieldsAndConstructor()
  }

  def emitAbstractFatLoopFooter() {
    stream.println("};")
    stream.println("#endif")
  }

  def castInt32(name: String) = name

  def refNotEq: String = "!="
  def nullRef: String = "NULL"

  def arrayType(argType: String): String = argType + "*"
  def arrayApply(arr: String, idx: String): String = arr + "[" + idx + "]"
  def newArray(argType: String, size: String): String = "new " + remapWithRef(argType) + "[" + size + "]"
  def hashmapType(argType: String): String = "cppHashMap<" + remapWithRef(argType) + ">"
  def typeCast(sym: String, to: String): String = "(" + to + ")(" + sym + ")"
  def withBlock(name: String)(block: => Unit): Unit = {
    stream.println("//block:" + name)
    stream.println("{")
    block
    stream.println("}")
  }

  private def emitFieldsAndConstructor() {
    val fields = if (cppMemMgr == "refcnt") kernelInputVals.map(i => remapWithRef(i.tp) + quote(i)) ++ kernelInputVars.map(i => wrapSharedPtr(deviceTarget.toString + "Ref" + unwrapSharedPtr(remap(i.tp))) + " " + quote(i))
                 else kernelInputVals.map(i => remapWithRef(i.tp) + quote(i)) ++ kernelInputVars.map(i => remapWithRef(deviceTarget.toString + "Ref" + remap(i.tp)) + quote(i))
    val constructorInputs = if (cppMemMgr == "refcnt") kernelInputVals.map(i => remapWithRef(i.tp) + " _" + quote(i)) ++ kernelInputVars.map(i => wrapSharedPtr(deviceTarget.toString + "Ref" + unwrapSharedPtr(remap(i.tp))) + " _" + quote(i))
                            else kernelInputVals.map(i => remapWithRef(i.tp) + " _" + quote(i)) ++ kernelInputVars.map(i => remapWithRef(deviceTarget.toString + "Ref" + remap(i.tp)) + " _" + quote(i))

    //print fields
    stream.println(fields.map(_ + ";\n").mkString(""))

    //print constructor
    stream.println(kernelName + "(" + constructorInputs.mkString(",") + ") {")
    stream.print((kernelInputVals++kernelInputVars).map(i => quote(i) + " = _" + quote(i) + ";\n").mkString(""))
    stream.println("}")
    stream.println
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] =>
      //printlog("EMIT single "+s)
      // always wrap single tasks in methods to reduce JIT compilation unit size
      val b = s.block
      emitBlock(b)
      if (!isVoidType(sym.tp))
        stream.println(remap(sym.tp) + addRef(sym.tp) + quote(sym) + " = " + quote(getBlockResult(b)) + ";")

    case op: AbstractLoop[_] =>
      // TODO: we'd like to always have fat loops but currently they are not allowed to have effects
      stream.println("// a *thin* loop follows: " + quote(sym))
      emitFatNode(List(sym), SimpleFatLoop(op.size, op.v, List(op.body)))

    case _ => super.emitNode(sym,rhs)
  }

  /*
  // Prevent C++ kernel generation for HashElems. Not supported yet.
  override def emitKernelMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = ""){
    if (ps.length > 0)
      throw new GenerationFailedException("CGen: DeliteHashElems are not yet supported for C++ target.")
  }
  override def emitInlineMultiHashInit(op: AbstractFatLoop, ps: List[(Sym[Any], DeliteHashElem[_,_])], prefixSym: String = "") {
    if (ps.length > 0)
      throw new GenerationFailedException("CGen: DeliteHashElems are not yet supported for C++ target.")
  }
  */

}
