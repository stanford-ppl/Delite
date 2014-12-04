package ppl.delite.runtime.codegen.kernels.cpp

import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.kernels.{MultiLoop_SMP_Array_Header_Generator, MultiLoop_SMP_Array_Generator}
import ppl.delite.runtime.codegen.{CppExecutableGenerator, CppCompile, CppResourceInfo}
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

class CppMultiLoopGenerator(val op: OP_MultiLoop, val master: OP_MultiLoop, val chunkIdx: Int, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Generator with CppResourceInfo {

  protected val headerObject = "head"
  protected val closure = "head->closure"
  protected val allocSym = "head->out"

  protected def addSource(source: String, name: String) = CppCompile.addSource(source, name)

  protected def writeHeader() {
    out.append("#include \""+CppMultiLoopHeaderGenerator.className(master) + ".h\"\n")
    out.append("#include \"DeliteCppProfiler.h\"\n")
    CppMultiLoopHeaderGenerator.headerList += kernelSignature + ";\n"
  }

  protected def writeFooter(){ }

  protected def writeKernelHeader() {
    out.append(kernelSignature + "{\n")
    out.append(s"int tid = $resourceInfoSym->threadId;\n")
    out.append(s"int numThreads = $resourceInfoSym->numThreads;\n")
  }

  protected def kernelSignature = {
    s"${op.outputType(Targets.Cpp)}* $kernelName($resourceInfoType *$resourceInfoSym, ${op.getInputs.head._1.outputType}* $headerObject)"
  }

  protected def writeKernelFooter() {
    out.append("}\n")
  }

  protected def processLocal() = {
    val outputType = master.outputType(Targets.Cpp)+"*"
    out.append(s"""//process local
  size_t dIdx = tid;
  size_t numChunks = $headerObject->numChunks;
  size_t startOffset = $closure->loopStart;
  size_t size = $closure->loopSize;
  while(dIdx < numChunks) {
    size_t start = startOffset + size*dIdx/numChunks;
    size_t end = startOffset + size*(dIdx+1)/numChunks;
    $outputType act = $closure->processRange($resourceInfoSym, $allocSym, start, end);
    $headerObject->set(dIdx,act);
    dIdx = $headerObject->getNextChunkIdx();
  }
  size_t localStart = tid*numChunks/numThreads;
  size_t localEnd = (tid+1)*numChunks/numThreads;
  $outputType act = $headerObject->get(localStart);
  \n""")
  }

  protected def combineLocal() = {
    out.append(s"""//combine local
  for (size_t i = localStart+1; i<localEnd; i++) {
    $closure->combine($resourceInfoSym, act, $headerObject->get(i));
  }
  \n""")
  }

  protected def combineRemote() = {
    out.append(s"""//combine remote
  int half = tid;
  int step = 1;
  while ((half % 2 == 0) && (tid + step < numThreads)) {
    int neighborIdx = tid + step;
    $closure->combine($resourceInfoSym, act, $headerObject->getC(neighborIdx));
    half /= 2;
    step *= 2;
  }
  $headerObject->setC(tid, act);
  \n""")
  }

  protected def postCombine() {
    val outputType = master.outputType(Targets.Cpp)+"*"
    out.append(s"""//post combine
  if (tid != 0) $closure->postCombine($resourceInfoSym, act, $headerObject->getP(tid-1));
  $outputType currentAct = act;
  for(size_t j=localStart+1; j<localEnd; j++) {
    $outputType rhsAct = $headerObject->get(j);
    $closure->postCombine($resourceInfoSym, rhsAct, currentAct);
    currentAct = rhsAct;
  }
  if (tid == numThreads-1) $closure->postProcInit($resourceInfoSym, currentAct);
  $headerObject->setP(tid, currentAct);
  \n""")
  }

  protected def postProcess() {
    out.append(s"""//post process
  for(size_t k=localStart; k<localEnd; k++) {
    $closure->postProcess($resourceInfoSym, $headerObject->get(k));
  }
  \n""")
  }

  protected def finalizer() {
    out.append(s"if (tid == 0) $closure->finalize($resourceInfoSym, act);\n")
  }

  protected def returnResult() {
    out.append("return act;\n")
  }

  protected def barrier() {
    out.append(s"delite_barrier(numThreads);\n")
  }

  protected def release(name: String, cond: Option[String] = None) {
    if (Config.cppMemMgr == "refcnt") {
      cond match {
        case Some(c) => out.append("if(" + c + ") delete " + name + ";\n")
        case None => out.append("delete " + name + ";\n")
      }
    }
  }

  protected def beginProfile() {
    val chunkName = master.id + "_" + chunkIdx
    out.append("DeliteCppTimerStart(tid,\""+master.id+"\");\n")
  }

  protected def endProfile(isMaster: Boolean) {
    val chunkName = master.id + "_" + chunkIdx
    val timeStr = "DeliteCppTimerStop(tid,\""+master.id+"\");\n"
    if (isMaster) out.append("if (tid == 0) "+timeStr) else out.append("if (tid != 0) "+timeStr)
  }

  protected def kernelName = "MultiLoop_" + master.id

}


class CppMultiLoopHeaderGenerator(val op: OP_MultiLoop, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Header_Generator with CppResourceInfo {

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
    out.append("#include <math.h>\n")
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
    className + "* " + kernelName + ((resourceInfoType+" *"+resourceInfoSym)+:op.getInputs.map(in => op.inputType(Targets.Cpp, in._2) + addRef(in._2) + in._2)).mkString("(", ", ", ")")
  }

  protected def writeKernelFunction(stream: StringBuilder) {
    stream.append("#include \"multiLoopHeaders.h\"\n")

    if (Config.scheduler == "dynamic") {
      writeThreadLaunchFunction(stream)
    }

    stream.append(kernelSignature)
    stream.append(" {\n")
    stream.append(className + "* header = new " + className)
    stream.append((resourceInfoSym+:op.getInputs.map(_._2)).mkString("(",", ",");\n"))
    if (Config.scheduler == "dynamic") {
      writeThreadLaunch(stream)
    }
    stream.append("return header;\n")
    stream.append("}\n")
  }

  protected def writeInstance() {
    out.append(op.function + "* closure;\n")
    out.append(op.outputType(Targets.Cpp) + "* out;\n")

    val typedArgs = ((resourceInfoType+" *"+resourceInfoSym)+:op.getInputs.map(in => in._1.outputType(Targets.Cpp,in._2)+addRef(in._2)+" "+in._2)).mkString(", ")
    val unTypedArgs = (/*resourceInfoSym+:*/op.getInputs.map(_._2)).mkString(", ")
    out.append(s"""$className($typedArgs) {
  closure = new ${op.function}($unTypedArgs);
  closure->loopStart = 0;
  closure->loopSize = closure->size($resourceInfoSym);
  numThreads = $resourceInfoSym->numThreads;
  out = closure->alloc($resourceInfoSym);
  initSync();
}
\n""")
  }

  protected val syncList = new ArrayBuffer[String]

  protected def writeActSync(key: String) {
    val outputType = op.outputType(Targets.Cpp)+"*"
    syncList += key //need a way to initialize these fields in C++
    out.append(s"""//$key sync
  $outputType* results$key;
  pthread_mutex_t* lock$key;
  pthread_cond_t* cond$key;
  bool* notReady$key;

  $outputType get$key(size_t i) { 
    pthread_mutex_lock(&lock$key[i]);
    while (notReady$key[i]) { 
      pthread_cond_wait(&cond$key[i], &lock$key[i]);
    }
    pthread_mutex_unlock(&lock$key[i]);
    return results$key[i];
  }
  void set$key(size_t i, $outputType res) { 
    pthread_mutex_lock(&lock$key[i]);
    results$key[i] = res;
    notReady$key[i] = false;
    pthread_cond_broadcast(&cond$key[i]);
    pthread_mutex_unlock(&lock$key[i]);
  }
  \n""")
  }

  protected def writeScheduler() {
    out.append(s"""//scheduler
  size_t numThreads;
  size_t numChunks;
  size_t offset;
  pthread_mutex_t offset_lock;
  size_t getNextChunkIdx() {
    size_t idx;
    pthread_mutex_lock(&offset_lock);
    idx = offset;
    offset += 1;
    pthread_mutex_unlock(&offset_lock);
    return idx;
  }
  \n""")
  }

  protected def writeThreadLaunchFunction(stream: StringBuilder) {
    stream.append(s"""
  void* launchFunc_${op.id}(void* arg) {
    std::pair<$resourceInfoType*, $className*>* in = (std::pair<$resourceInfoType*, $className*>*)arg;
    MultiLoop_${op.id}(in->first, in->second);
  }
  \n""")
  }

  protected def writeThreadLaunch(stream: StringBuilder) {
    stream.append(s"""//thread launch
  for (int i=1; i<resourceInfo->numThreads; i++) {
    void* arg = (void*)(new std::pair<$resourceInfoType*, $className*>(&resourceInfos[i], header));
    submitWork(i, launchFunc_${op.id}, arg);
  }
  """)
  }

  protected def defaultChunks() = {
    if(op.numDynamicChunks == "-1")
      "(size_t)(closure->loopSize/(log10((double)closure->loopSize)*(500.0/numThreads)))"
    else
      op.numDynamicChunks
  }

  protected def initSync() {
    val outputType = op.outputType(Targets.Cpp)+"*"
    out.append(s"""  void initSync() {
    size_t defaultChunks = ${defaultChunks};
    if (numThreads == 1 || defaultChunks <= numThreads || closure->loopSize < defaultChunks) numChunks = numThreads; else numChunks = defaultChunks;
    offset = numThreads;
    pthread_mutex_init(&offset_lock, NULL);
    \n""")

    for (key <- syncList) {
      val size = if (key == "") "numChunks" else "numThreads"
      out.append(s"""//$key sync
    results$key = new $outputType[$size];
    lock$key = new pthread_mutex_t[$size];
    cond$key = new pthread_cond_t[$size];
    notReady$key = new bool[$size];
    for(size_t i=0; i<$size; i++) {
      pthread_mutex_init(&lock$key[i], NULL);
      pthread_cond_init(&cond$key[i], NULL);
      notReady$key[i] = true;
    }
  """)
    }
    out.append("}\n")
  }

  protected def writeDestructor() {
    out.append("~" + className + "() {\n")
    out.append("delete closure;\n")
    for (key <- syncList) {
      out.append(s"delete results$key;\n")
      out.append(s"delete lock$key;\n")
      out.append(s"delete cond$key;\n")
      out.append(s"delete notReady$key;\n")
    }
    //out will be released by the caller
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
