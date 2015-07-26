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
    stream.println("return " + rhs + ";")
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
    //stream.println("#include \"" + deviceTarget + "helperFuncs.h\"")
    //stream.println("#include \"Config.h\"")
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

  def emitAbstractFatLoopHeader(syms: List[Sym[Any]], rhs: AbstractFatLoop) {
    val kernelName = getKernelName(syms)
    stream.println("class " + kernelName+"_class" + "{")
    stream.println("public:")
    emitFieldsAndConstructor(kernelName)
  }

  def emitAbstractFatLoopFooter(syms: List[Sym[Any]], rhs: AbstractFatLoop) {
    val kernelName = getKernelName(syms)
    val actType = getActType(kernelName)
    emitFieldDecl("allocVal", actType)
    emitFieldDecl("syncVal", syncType(actType))
    stream.println("static void* launchFunc(void* arg) {")
      val tpe = s"""std::pair<$resourceInfoType*,void*>*"""
      stream.println(tpe+" in = ("+tpe+")arg;")
      stream.println(s"""${kernelName}_class *closure = (${kernelName}_class *)in->second;""")
      stream.println("closure->main_par(in->first, closure->allocVal, closure->syncVal);")
    stream.println("}")
    stream.println("};")
    val inputs = (inputVals(rhs)++inputVars(rhs)).map(quote).mkString(",")
    stream.println(kernelName+"_class *"+kernelName+"_closure = new "+kernelName+"_class("+inputs+");")
  }

  def emitHeapMark() = {
    stream.println("DeliteHeapMark("+resourceInfoSym+"->threadId);")
  }

  def emitHeapReset(result: List[String]) = {
    stream.println("DeliteHeapReset("+resourceInfoSym+"->threadId);")
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

  def syncType(actType: String) = "MultiLoopSync<"+actType+"*>"

  def emitWorkLaunch(kernelName: String, rSym: String, allocSym: String, syncSym: String): Unit = {
    emitAssignment(fieldAccess(kernelName+"_closure","allocVal"), "alloc")
    emitAssignment(fieldAccess(kernelName+"_closure","syncVal"), "sync")
    stream.println(s"""submitWork($rSym->threadId, ${kernelName}_closure->launchFunc, (void*)(new std::pair<$resourceInfoType*,void*>($rSym,${kernelName}_closure)));""")
  }

  private def emitFieldsAndConstructor(kernelName: String) {
    val fields = if (cppMemMgr == "refcnt") kernelInputVals.map(i => remapWithRef(i.tp) + quote(i)) ++ kernelInputVars.map(i => wrapSharedPtr(deviceTarget.toString + "Ref" + unwrapSharedPtr(remap(i.tp))) + " " + quote(i))
                 else kernelInputVals.map(i => remapWithRef(i.tp) + quote(i)) ++ kernelInputVars.map(i => remapWithRef(deviceTarget.toString + "Ref" + remap(i.tp)) + quote(i))
    val constructorInputs = if (cppMemMgr == "refcnt") kernelInputVals.map(i => remapWithRef(i.tp) + " _" + quote(i)) ++ kernelInputVars.map(i => wrapSharedPtr(deviceTarget.toString + "Ref" + unwrapSharedPtr(remap(i.tp))) + " _" + quote(i))
                            else kernelInputVals.map(i => remapWithRef(i.tp) + " _" + quote(i)) ++ kernelInputVars.map(i => remapWithRef(deviceTarget.toString + "Ref" + remap(i.tp)) + " _" + quote(i))

    //print fields
    stream.println(fields.map(_ + ";\n").mkString(""))

    //print constructor
    stream.println(kernelName + "_class(" + constructorInputs.mkString(",") + ") {")
    stream.print((kernelInputVals++kernelInputVars).map(i => quote(i) + " = _" + quote(i) + ";\n").mkString(""))
    stream.println("}")
    stream.println
  }

  override def emitAbstractFatLoopKernelExtra(op: AbstractFatLoop, symList: List[Sym[Any]]): Unit = {
    withStream(actRecordStream){ super.emitAbstractFatLoopKernelExtra(op, symList) }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case s:DeliteOpSingleTask[_] =>
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

  override def emitStartMultiLoopTimerForSlave(name: String) = {
    stream.println("if (tid != 0) {")
    stream.println("DeliteCppTimerStart(resourceInfo->threadId,\"" + name + "\");")
    stream.println("}")
  }

  override def emitStopMultiLoopTimerForSlave(name: String) = {
    stream.println("if (tid != 0) {")
    stream.println("DeliteCppTimerStopMultiLoop(resourceInfo->threadId,\"" + name + "\");")
    stream.println("}")
  }

  override def emitStartPCM() = {
    stream.println("CoreCounterState before = getCoreCounterState(resourceInfo->threadId);")
  }
  
  override def emitStopPCM(sourceContext: String) = {
    stream.println("CoreCounterState after = getCoreCounterState(resourceInfo->threadId);")
    stream.println("DeliteUpdateMemoryAccessStats( resourceInfo->threadId, " + "\"" + sourceContext + "\" , getPCMStats( before, after ));")
  }
}
