package ppl.delite.runtime.codegen.kernels.cpp

import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.kernels.{MultiLoop_SMP_Array_Header_Generator, MultiLoop_SMP_Array_Generator}
import ppl.delite.runtime.codegen.{CppExecutableGenerator, CppCompile}
import ppl.delite.runtime.graph.targets.Targets
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.DeliteTaskGraph

object CppMultiLoopGenerator {
  def makeChunks(op: OP_MultiLoop, numChunks: Int, kernelPath: String) = {
    for (idx <- 0 until numChunks) yield {
      val chunk = if (idx == 0) op else op.chunk(idx)
      (new CppMultiLoopGenerator(chunk, op, idx, numChunks, kernelPath)).makeChunk()
      chunk
    }
  }
}

class CppMultiLoopGenerator(val op: OP_MultiLoop, val master: OP_MultiLoop, val chunkIdx: Int, val numChunks: Int, val kernelPath: String) extends MultiLoop_SMP_Array_Generator {

  protected val headerObject = "head"
  protected val closure = "head->closure"

  protected def addSource(source: String, name: String) = CppCompile.addSource(source, name)

  protected def writeHeader() {
    out.append("#include \""+CppMultiLoopHeaderGenerator.className(master) + ".cpp\"\n")
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
      op.inputType(Targets.Cpp, op.getInputs.head._2) + "* " + headerObject + ")"
  }

  protected def writeKernelFooter() {
    out.append("}\n")
  }

  protected def returnResult(result: String) {
    out.append("return "+result+";\n")
  }

  protected def calculateRange(): (String,String) = {
    out.append("int64_t startOffset = "+closure+"->loopStart;\n")
    out.append("int64_t size = "+closure+"->loopSize;\n")
    out.append("int64_t start = startOffset + size*"+chunkIdx+"/"+numChunks+";\n")
    out.append("int64_t end = startOffset + size*"+(chunkIdx+1)+"/"+numChunks+";\n")
    ("start","end")
  }

  protected def allocateOutput(): String = {
    out.append(master.outputType(Targets.Cpp)+"* out = "+headerObject+"->out;\n")
    "out"
  }

  protected def processRange(outputSym: String, start: String, end: String) = {
    out.append(master.outputType(Targets.Cpp)+"* acc = "+closure+"->processRange("+outputSym+","+start+","+end+");\n")
    "acc"
  }

  protected def combine(acc: String, neighbor: String) {
    out.append(closure+"->combine("+acc+", "+neighbor+");\n")
  }

  protected def postProcess(acc: String) {
    out.append(closure+"->postProcess("+acc+");\n")
  }

  protected def postProcInit(acc: String) {
    out.append(closure+"->postProcInit("+acc+");\n")
  }

  protected def postCombine(acc: String, neighbor: String) {
    out.append(closure+"->postCombine("+acc+", "+neighbor+");\n")
  }

  protected def finalize(acc: String) {
    out.append(closure+"->finalize("+acc+");\n")
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


class CppMultiLoopHeaderGenerator(val op: OP_MultiLoop, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Header_Generator {

  protected def addSource(source: String, name: String) = CppCompile.addSource(source, name)

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
    out.append("};\n")
    out.append("#endif\n")

    val stream = new StringBuilder
    writeKernelFunction(stream)
    addSource(stream.toString, kernelName)
    CppMultiLoopHeaderGenerator.headerList += "#include \"" + className + ".cpp\"\n"
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

  protected def kernelSignature = {
    def ref(name: String) = if(!isPrimitiveType(op.inputType(name))) "* " else " "
    className + "* " + kernelName + op.getInputs.map(in => op.inputType(Targets.Cpp, in._2) + ref(in._2) + in._2).mkString("(", ", ", ")")
  }

  protected def writeKernelFunction(stream: StringBuilder) {
    stream.append("#include \"" + className + ".cpp\"\n")

    stream.append(kernelSignature)
    stream.append(" {\n")
    stream.append("return new " + /*CppMultiLoopHeaderGenerator.className(master) + "::" + */ className)
    stream.append(op.getInputs.map(_._2).mkString("(",", ",");\n"))
    stream.append("}\n")
  }

  protected def writeInstance() {
    out.append(op.function + "* closure;\n")
    out.append(op.outputType(Targets.Cpp) + "* out;\n")

    out.append(className)
    out.append("(")
    var inIdx = 0
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(op.inputType(Targets.Cpp, name))
      if (!isPrimitiveType(op.inputType(name))) out.append("*")
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
    out.append("closure->loopSize = closure->size();\n")

    out.append("out = closure->alloc();\n")
    out.append("initSync();\n")
    out.append("}\n")
  }

  protected val syncList = new ArrayBuffer[String]

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

  protected def initSync() {
    out.append("void initSync() {\n")
    for (key <- syncList) {
      out.append("pthread_mutex_init(&lock"+key+", NULL);\n")
      out.append("pthread_cond_init(&cond"+key+", NULL);\n")
      out.append("notReady"+key+ " = true;\n")
    }
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
