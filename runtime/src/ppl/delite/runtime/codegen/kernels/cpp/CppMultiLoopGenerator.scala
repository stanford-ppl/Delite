package ppl.delite.runtime.codegen.kernels.cpp

import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.kernels.{MultiLoop_SMP_Array_Header_Generator, MultiLoop_SMP_Array_Generator}
import ppl.delite.runtime.codegen.{CppExecutableGenerator, CppCompile, CppResourceInfoGenerator}
import ppl.delite.runtime.graph.targets.Targets
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config

object CppMultiLoopGenerator {
  def makeChunks(op: OP_MultiLoop, numChunks: Int, graph: DeliteTaskGraph) = {
    for (idx <- 0 until numChunks) yield {
      val chunk = if (idx == 0) op else op.chunk(idx)
      (new CppMultiLoopGenerator(chunk, op, idx, numChunks, graph)).makeChunk()
      chunk
    }
  }
}

class CppMultiLoopGenerator(val op: OP_MultiLoop, val master: OP_MultiLoop, val chunkIdx: Int, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Generator with CppResourceInfoGenerator {

  protected val headerObject = "head"
  protected val closure = "head->closure"

  protected def addSource(source: String, name: String) = CppCompile.addSource(source, name)

  protected def writeHeader() {
    out.append("#include \""+CppMultiLoopHeaderGenerator.className(master) + ".h\"\n")
    out.append("#include \"DeliteCppProfiler.h\"\n")
    CppMultiLoopHeaderGenerator.headerList += kernelSignature + ";\n"
  }

  protected def writeFooter(){ }

  protected def writeKernelHeader() {
    out.append(kernelSignature)
    out.append(" {\n")
  }

  protected def kernelSignature = {
    op.outputType(Targets.Cpp) + "* " + kernelName + "(" +
    resourceInfoType + " &" + resourceInfoSym + "," +
    op.inputType(Targets.Cpp, op.getInputs.head._2) + "* " + headerObject + ")"
  }

  protected def writeKernelFooter() {
    out.append("}\n")
  }

  protected def returnResult(result: String) {
    out.append("return "+result+";\n")
  }

  protected def release(name: String, cond: Option[String] = None) {
    if (Config.cppMemMgr == "refcnt") {
      cond match {
        case Some(c) => out.append("if(" + c + ") delete " + name + ";\n")
        case None => out.append("delete " + name + ";\n")
      }
    }
  }

  protected def dynamicScheduler(outputSym: String) : String = {
    //used to be calculate range
    out.append("int64_t startOffset = "+closure+"->loopStart;\n")
    out.append("int64_t size = "+closure+"->loopSize;\n")
    out.append("int64_t start = startOffset + size*"+chunkIdx+"/"+numChunks+";\n")
    out.append("int64_t end = startOffset + size*"+(chunkIdx+1)+"/"+numChunks+";\n")
    processRange(outputSym,"start","end")
    "acc"
  }

  protected def dynamicCombine(acc: String) = {
    out.append("")
  }
  
  protected def dynamicPostCombine(acc: String) = {
    if (chunkIdx != 0) {
      postCombine(acc, get("B", chunkIdx-1)) //linear chain combine
    }
    if (chunkIdx == numChunks-1) {
      postProcInit(acc) //single-threaded
    }

    if (numChunks > 1) set("B", chunkIdx, acc) // kick off next in chain
    if (chunkIdx != numChunks-1) get("B", numChunks-1) // wait for last one
    postProcess(acc) //parallel again

    // release activation records except the master chunk
    if (chunkIdx != 0) release(acc)
  }
  
  protected def allocateOutput(): String = {
    out.append(master.outputType(Targets.Cpp)+"* out = "+headerObject+"->out;\n")
    "out"
  }

  protected def processRange(outputSym: String, start: String, end: String) = {
    out.append(master.outputType(Targets.Cpp)+"* acc = "+closure+"->processRange("+resourceInfoSym+","+outputSym+","+start+","+end+");\n")
    "acc"
  }

  protected def combine(acc: String, neighbor: String) {
    out.append(closure+"->combine("+resourceInfoSym+","+acc+", "+neighbor+");\n")
  }

  protected def postProcess(acc: String) {
    out.append(closure+"->postProcess("+resourceInfoSym+","+acc+");\n")
  }

  protected def postProcInit(acc: String) {
    out.append(closure+"->postProcInit("+resourceInfoSym+","+acc+");\n")
  }

  protected def postCombine(acc: String, neighbor: String) {
    out.append(closure+"->postCombine("+resourceInfoSym+","+acc+", "+neighbor+");\n")
  }

  protected def finalize(acc: String) {
    out.append(closure+"->finalize("+resourceInfoSym+","+acc+");\n")
  }

  protected def set(syncObject: String, idx: Int, value: String) {
    out.append(headerObject+"->set"+syncObject+idx+"("+value+");\n")
  }

  protected def get(syncObject: String, idx: Int) = {
    out.append(master.outputType(Targets.Cpp)+"* neighbor"+syncObject+idx+" = "+headerObject+"->get"+syncObject+idx+"();\n")
    "neighbor"+syncObject+idx
  }

  protected def beginProfile() {
    val chunkName = master.id + "_" + chunkIdx
    out.append("DeliteCppTimerStart(" + chunkIdx + ",\""+chunkName+"\");\n")
  }

  protected def endProfile() {
    val chunkName = master.id + "_" + chunkIdx
    out.append("DeliteCppTimerStop(" + chunkIdx + ",\""+chunkName+"\");\n")
  }

  protected def kernelName = {
    "MultiLoop_" + master.id + "_Chunk_" + chunkIdx
  }

}


class CppMultiLoopHeaderGenerator(val op: OP_MultiLoop, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Header_Generator with CppResourceInfoGenerator {

  //NOTE: Header is just a class, so put into header list instead of source list
  protected def addSource(source: String, name: String) = CppCompile.addHeader(source, name)

  protected def isPrimitiveType(scalaType: String) = scalaType match {
    case "java.lang.String" => true
    case _ => Targets.isPrimitiveType(scalaType)
  }

  protected def writeHeader() {
    writeClassHeader()
    writeInstance()
  }

  protected def writeFooter() {
    initSync()
    writeDestructor()
    out.append("};\n")
    out.append("#endif\n")

    val stream = new StringBuilder
    writeKernelFunction(stream)
    CppCompile.addSource(stream.toString, kernelName)
    CppMultiLoopHeaderGenerator.headerList += "#include \"" + className + ".h\"\n"
    CppMultiLoopHeaderGenerator.headerList += kernelSignature + ";\n"
  }

  protected def writeClassHeader() {
    out.append("#include <pthread.h>\n")
    out.append("#include \""+op.id+".cpp\"\n")
    out.append("#ifndef HEADER_"+op.id+"\n")
    out.append("#define HEADER_"+op.id+"\n")
    out.append("class ")
    out.append(className)
    out.append(" {\n")
    out.append("public: \n")
  }

  def addRef(name: String) = {
    if (isPrimitiveType(op.inputType(name)) || Config.cppMemMgr=="refcnt") " "
    else " * "
  }

  protected def kernelSignature = {
    className + "* " + kernelName + ((resourceInfoType+" &"+resourceInfoSym)+:op.getInputs.map(in => op.inputType(Targets.Cpp, in._2) + addRef(in._2) + in._2)).mkString("(", ", ", ")")
  }

  protected def writeKernelFunction(stream: StringBuilder) {
    stream.append("#include \"" + className + ".h\"\n")

    stream.append(kernelSignature)
    stream.append(" {\n")
    stream.append("return new " + /*CppMultiLoopHeaderGenerator.className(master) + "::" + */ className)
    stream.append((resourceInfoSym+:op.getInputs.map(_._2)).mkString("(",", ",");\n"))
    stream.append("}\n")
  }

  protected def writeInstance() {
    out.append(op.function + "* closure;\n")
    out.append(op.outputType(Targets.Cpp) + "* out;\n")

    out.append(className)
    out.append("(")
    out.append(resourceInfoType)
    out.append(" &")
    out.append(resourceInfoSym)
    var inIdx = 0
    for ((input, name) <- op.getInputs) {
      out.append(", ")
      out.append(op.inputType(Targets.Cpp, name))
      out.append(addRef(name))
      out.append(" in")
      out.append(inIdx)
      inIdx += 1
    }
    out.append(") {\n")

    out.append("closure = new ")
    out.append(op.function)
    out.append("(")
    for (i <- 0 until inIdx) {
      if (i > 0) out.append(", ")
      out.append("in")
      out.append(i)
    }
    out.append(");\n")

    out.append("closure->loopStart = 0;\n")
    out.append("closure->loopSize = closure->size(" + resourceInfoSym + ");\n")

    out.append("out = closure->alloc(" + resourceInfoSym + ");\n")
    out.append("initSync();\n")
    out.append("}\n")
  }

  protected val syncList = new ArrayBuffer[String]

  //TODO: fill in
  protected def writeSynchronizedOffset(){
    out.append("")
  }
  protected def writeSync(key: String) {
    syncList += key //need a way to initialize these fields in C++
    val outputType = op.outputType(Targets.Cpp)

    out.append("pthread_mutex_t lock"+key+";\n")
    out.append("pthread_cond_t cond"+key+";\n")

    out.append("bool notReady"+key+";\n")
    out.append(outputType+ "* _result"+key+";\n")

    out.append(outputType+"* get"+key+"(){\n")
    out.append("pthread_mutex_lock(&lock"+key+");\n")
    out.append("while(notReady"+key+") {\n")
    out.append("pthread_cond_wait(&cond"+key+", &lock"+key+");\n")
    out.append("}\n")
    out.append("pthread_mutex_unlock(&lock"+key+");\n")
    out.append("return _result"+key+";\n")
    out.append("}\n")

    out.append("void set"+key+"("+outputType+"* result) {\n")
    out.append("pthread_mutex_lock(&lock"+key+");\n")
    out.append("_result"+key + " = result;\n")
    out.append("notReady"+key+" = false;\n")
    out.append("pthread_cond_broadcast(&cond"+key+");\n")
    out.append("pthread_mutex_unlock(&lock"+key+");\n")
    out.append("}\n")
  }
  protected def dynamicWriteSync() {
    out.append("")
  }
  protected def initSync() {
    out.append("void initSync() {\n")
    for (key <- syncList) {
      out.append("pthread_mutex_init(&lock"+key+", NULL);\n")
      out.append("pthread_cond_init(&cond"+key+", NULL);\n")
      out.append("notReady"+key+ " = true;\n")
    }
    out.append("}\n")
  }

  protected def writeDestructor() {
    out.append("~" + className + "() {\n")
    out.append("delete closure;\n")
    out.append("//out will be released by the caller\n")
    out.append("}\n")
  }

  protected def className = CppMultiLoopHeaderGenerator.className(op)

  protected def kernelName = "kernel_" + className

}

object CppMultiLoopHeaderGenerator {
  def className(op: OP_MultiLoop) = "MultiLoopHeader_" + op.id

  private[kernels] val headerList = new ArrayBuffer[String]
  headerList += "#include \"" + Targets.Cpp + "helperFuncs.h\"\n"

  def headerFile = "multiLoopHeaders"
  def createHeaderFile() = {
    CppCompile.addHeader(headerList.mkString(""),headerFile)
  }

  def clear() { headerList.clear }

}
