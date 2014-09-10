package ppl.delite.runtime.codegen

import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph._
import ppl.delite.runtime.scheduler.{OpList, PartialSchedule}
import ppl.delite.runtime.{Config,Delite}
import ppl.delite.runtime.graph.targets.{OS, Targets}
import collection.mutable.ArrayBuffer
import sync._
import ppl.delite.runtime.graph.DeliteTaskGraph

trait CudaExecutableGenerator extends ExecutableGenerator with JNIFuncs{

  def deviceTarget = Targets.Cuda
  def hostTarget = Targets.getHostTarget(deviceTarget)

  protected val getterList: ArrayBuffer[String] = ArrayBuffer()
  protected val available: ArrayBuffer[(DeliteOP,String)] = ArrayBuffer()
  //protected val aliasTable: AliasTable[(DeliteOP,String)] = new AliasTable[(DeliteOP, String)]

  protected def writeDataFrees(op: DeliteOP)/*(implicit aliases: AliasTable[(DeliteOP,String)])*/ {
    var count = 0
    val freeItem = "freeItem_" + op.id

    def writeFreeInit() {
      out.append("FreeItem ")
      out.append(freeItem)
      out.append(";\n")
      out.append(freeItem)
      out.append(".keys = new std::list< std::pair<void*,bool> >();\n")
    }

    def writeFree(sym: String, isPrim: Boolean) {
      if (count == 0) writeFreeInit()
      out.append("std::pair<void*,bool> ")
      out.append(getSymDevice(op,sym))
      out.append("_pair(")
      out.append(getSymDevice(op,sym))
      out.append(",")
      out.append(isPrim) //Do not free this ptr using free() : primitive type pointers points to device memory
      out.append(");\n")
      out.append(freeItem)
      out.append(".keys->push_back(")
      out.append(getSymDevice(op,sym))
      out.append("_pair);\n")
    }

    def opFreeable(op: DeliteOP, sym: String) = {
      //TODO: Better to make OP_Condition extending OP_Executable?
      (op.isInstanceOf[OP_Executable] || op.isInstanceOf[OP_Condition]) /*&& available.contains(op,sym)*/ && op.outputType(sym)!="Unit"
    }

    //free outputs
    //TODO: Handle alias for Condition op output
    for (name <- op.getOutputs if(opFreeable(op,name))) {
      if (op.getConsumers.filter(c => c.getInputs.contains((op,name)) && c.scheduledResource == op.scheduledResource).isEmpty) {
        writeFree(name,isPrimitiveType(op.outputType(name)))
        count += 1
      }
    }

    //free inputs
    for ((in,name) <- op.getInputs if(opFreeable(in,name))) {
      var free = true
      if (isPrimitiveType(in.outputType(name)) && (in.scheduledResource!=op.scheduledResource)) free = false
      //for ((i,n) <- aliases.get(in,name); c <- i.getConsumers.filter(c => c.getInputs.contains(i,n) && c.scheduledResource == op.scheduledResource)) {
      for (c <- in.getConsumers.filter(c => c.getInputs.contains(in,name) && c.scheduledResource == op.scheduledResource)) {
        if (!available.map(_._1).contains(c)) free = false
      }
      if (free) {
        writeFree(name,isPrimitiveType(in.outputType(name)))
        count += 1
      }
    }

    if (count > 0) {
      //sync on kernel stream (if copied back guaranteed to have completed, so don't need sync on d2h stream)
      out.append(freeItem)
      out.append(".event = addHostEvent(kernelStream);\n")
      out.append("freeList->push(")
      out.append(freeItem)
      out.append(");\n")
    }
  }

  protected def addSource(source: String) {
    // Add extern header files for generated kernels at walk-time
    val externs = CudaCompile.headers.map(s => "#include \"" + s + "\"\n").mkString("")
    CudaCompile.addSource(externs+source, executableName)
  }

  protected def writeHeader() {
    out.append("#include <stdio.h>\n")
    out.append("#include <stdlib.h>\n")
    out.append("#include <string.h>\n")
    out.append("#include <jni.h>\n")
    out.append("#include <map>\n")
    out.append("#include <cuda_runtime.h>\n") //Cuda runtime api
    out.append("#include \"cublas.h\"\n") //cublas library
    out.append("#include \"DeliteCuda.h\"\n") //Delite-Cuda interface for DSL
    out.append("#include \"cudaSyncObjects.h\"\n")
    out.append("#include \"" + Targets.Cuda + "helperFuncs.h\"\n")
    out.append("extern JNIEnv* env" + location + ";\n")
    out.append("extern cudaStream_t kernelStream;\n")
    out.append("extern cudaStream_t h2dStream;\n")
    out.append("extern cudaStream_t d2hStream;\n")
  }

  protected def writeMethodHeader() {
    out.append("cudaStream_t kernelStream;\n")
    out.append("cudaStream_t h2dStream;\n")
    out.append("cudaStream_t d2hStream;\n")
    out.append("JNIEnv* env" + location + ";\n")
    val function = "JNIEXPORT void JNICALL Java_" + executableName + "_00024_host" + executableName + "(JNIEnv* jnienv, jobject object)"
    out.append("extern \"C\" ") //necessary because of JNI
    out.append(function)
    out.append(";\n")
    out.append(function)
    out.append(" {\n")

    out.append("env" + location + " = jnienv;\n")
    out.append("cudaStreamCreate(&kernelStream);\n")
    out.append("cudaStreamCreate(&h2dStream);\n")
    out.append("cudaStreamCreate(&d2hStream);\n")
    out.append("cublasSetKernelStream(kernelStream);\n")  // set cublas to use the kernel stream
    out.append("hostInit();\n") // try to remove the overhead of the first call to hostInit (internally calls cudaHostAlloc)
    out.append("tempCudaMemInit(" + Config.tempCudaMemRate + ");\n")  // Allocate temporary device memory used for multiloops
    out.append("cudaDeviceSetLimit(cudaLimitMallocHeapSize, cudaHeapSize);\n") // Allocate heap device memory
    out.append("cudaDeviceSetCacheConfig(cudaFuncCachePreferL1);\n") // 48KB L1 and 16KB Shared mem
    out.append("std::map<char*,void*>* outputMap = new std::map<char*,void*>();\n")
    val locations = Range(0,Config.numThreads+Config.numCpp+Config.numCuda+Config.numOpenCL).toSet
    writeJNIInitializer(locations)
  }

  protected def writeJNIInitializer(locations: Set[Int]) {
    for (i <- locations) {
      out.append("jclass cls")
      out.append(i)
      out.append(" = env")
      out.append(location)
      out.append("->FindClass(\"")
      out.append("Sync_" + executableName(i))
      out.append("\");\n")
    }
    //add a reference to the singleton of scala.runtime.BoxedUnit for use everywhere required
    out.append("jclass clsBU = env" + location + "->FindClass(\"scala/runtime/BoxedUnit\");\n")
    out.append("jobject boxedUnit = env" + location + "->GetStaticObjectField(clsBU, env" + location + "->GetStaticFieldID(clsBU, \"UNIT\", \"Lscala/runtime/BoxedUnit;\"));\n")
  }

  protected def writeMethodFooter() {
    out.append("tempCudaMemFree();\n")
    out.append("cudaHostMemFree();\n")
    out.append("DeliteCudaCheckGC();\n")
    out.append("DeliteCudaCheckError();\n")
    out.append("}\n")
  }

  protected def writeFooter() { }

  //TODO: can/should this be factored out? need some kind of factory for each target
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case c: OP_Condition => {
      val codegen = new CudaConditionGenerator(c, location, graph)
      codegen.makeExecutable()
      CudaCompile.addHeader(codegen.generateMethodSignature + ";\nextern bool " + c.id.split('_').head + "_cond;\n", codegen.executableName(location))
    }
   case w: OP_While => {
      val codegen = new CudaWhileGenerator(w, location, graph)
      codegen.makeExecutable()
      CudaCompile.addHeader(codegen.generateMethodSignature + ";\n", codegen.executableName(location))
    }
    case err => println("Cannot generate op" + op.id) //sys.error("Unrecognized OP type: " + err.getClass.getSimpleName)
  }

  private def deref(o: DeliteOP, s: String):String = {
    if (!isPrimitiveType(o.outputType(s))) "*"
    else ""
  }

  protected def writeFunctionCall(op: DeliteOP) {
    if (op.task == null) return //dummy op

    out.append("addEvent(h2dStream, kernelStream);\n")

    for (i <- op.getInputs)
      available += i
    for (o <- op.getOutputs if op.outputType(o)!="Unit")
      available += Pair(op,o)

    if (Config.profile)
      out.append("DeliteCudaTic(\"" + op.id + "\");\n")

    op match {
      case _:OP_Single =>
        //TODO: enable singletask GPU kernels that has an output
        assert(op.getOutputs.filter(o=>op.outputType(o)!="Unit").isEmpty)
        out.append(op.task)
        out.append("<<<dim3(1,1,1),dim3(1,1,1),0,kernelStream>>>")
        val args = op.getInputs.map(i => deref(i._1,i._2) + getSymDevice(i._1,i._2))
        out.append(args.mkString("(",",",");\n"))
      case op:OP_MultiLoop =>
        for (name <- op.getOutputs if(op.outputType(name)!="Unit")) {
          out.append(op.outputType(Targets.Cuda, name))
          out.append(" *" + getSymDevice(op,name) + ";\n")
        }
        out.append(op.task) //kernel name
        
        val size = if(op.sizeIsConst) op.size else getSymDevice(op,op.size)
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => op.outputType(Targets.Cuda,o._2)!="void").map(o => "&"+getSymDevice(op,o._2)).toList ++ op.getInputs.map(i=>getSymDevice(i._1,i._2)) :+ size
        
        out.append(args.mkString("(",",",");\n"))
      case _:OP_Nested =>
        for (name <- op.getOutputs if(op.outputType(name)!="Unit")) {
          out.append(op.outputType(Targets.Cuda, name))
          if (!isPrimitiveType(op.outputType(name))) out.append('*')
          out.append(" " + getSymDevice(op,name) + " = ")
        }
        out.append(op.task) //kernel name
        //val args = op.getInputs.map(i=>getSymDevice(i._1,i._2))
        out.append("(" + inputArgs(op) + ");\n")
      case _:OP_External =>
        assert(op.getOutputs.size == 1) //TODO: what does libCall support?
        writeOutputAlloc(op)
        out.append(op.task) //kernel name
        out.append('(')
        out.append((op.getOutputs.toList++op.getInputs.map(i => i._2)).map(getSymDevice(op,_)).mkString(","))
        //out.append(",kernelStream") //TODO: what other libraries besides cuBlas do we use? how do we use the stream only with supported libraries?
        out.append(");\n")
    }

    if (Config.profile)
      out.append("DeliteCudaToc(\"" + op.id + "\");\n")
    
    out.append("addEvent(kernelStream, d2hStream);\n")
    //writeDataFrees(op)
  }

  protected def writeOutputAlloc(op: DeliteOP) {
    for ((odata,osym) <- op.getGPUMetadata(Targets.Cuda).outputs if op.outputType(Targets.Cuda,osym)!="void") {// if !isPrimitiveType(op.outputType(osym))) {
      out.append(op.outputType(Targets.Cuda, osym))
      out.append(" *")
      out.append(getSymDevice(op,osym))
      out.append(";\n")
      out.append("alloc_" + osym)
      out.append('(')
      out.append((odata.getInputs("alloc").map(i => getSymDevice(op,i)):+("&"+getSymDevice(op,osym))).mkString(","))
      out.append(");\n")
      out.append("cudaMemoryMap->insert(std::pair<void*,std::list<void*>*>(")
      out.append(getSymDevice(op,osym))
      out.append(",lastAlloc));\n")
      out.append("lastAlloc = new std::list<void*>();\n")
    }
  }


  //TODO: Remove using getSymCPU
  protected def getSymCPU(name: String): String = getSymRemote(null, name)

  protected def getSymRemote(op: DeliteOP, name: String): String = {
    "xC"+name
  }

  protected def getSymHost(op: DeliteOP, name: String): String = {
    "xH"+name
  }

  protected def getSymDevice(op: DeliteOP, name: String): String = {
    "xG"+name
  }

  protected def writeSyncObject() {  }

  protected def isPrimitiveType(scalaType: String) = Targets.isPrimitiveType(scalaType)

  // List of non-local symbols (among inputs) that require SendUpdate or ReceiveUpdate in current / nested schedule scope
  // TODO: When we schedule host kernels on the same execution plan with device kernels,
  //       need to separate updates only for the host from the updates for the remote (which includes the updates for the host)
  protected def updateOps(op: OP_Nested): Seq[String] = {
    val updates = op.nestedGraphs.flatMap { g =>
      g.schedule(location).toArray flatMap {
        case SendUpdate(sym, _, _) => List(sym)
        case ReceiveUpdate(sender, _) => List(sender.sym)
        case nested: OP_Nested => updateOps(nested) 
        case _ => Nil
      }
    }
    op.getInputs.map(_._2).filter(updates.contains(_))
  }

  protected def inputArgs(op: DeliteOP): String = {
    op match {
      case op: OP_Nested => 
        op.getInputs.map(i => 
          if (updateOps(op).contains(i._2)) getSymDevice(i._1,i._2) + "," + getSymHost(i._1,i._2) + "," + getSymRemote(i._1,i._2) 
          else getSymDevice(i._1,i._2)
        ).mkString(",")
      case _ => 
        op.getInputs.map(i => getSymDevice(i._1,i._2)).mkString(",")
    }
  }

}

class CudaMainExecutableGenerator(val location: Int, val graph: DeliteTaskGraph)
  extends CudaExecutableGenerator with CudaSyncGenerator {

  def executableName(location: Int) = "Executable" + location

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value) = {
    target match {
      case Targets.Scala => new ScalaMainExecutableGenerator(location, graph) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      //case Targets.Cpp => new CppMainExecutableGenerator(location, graph) with CudaSyncObjectGenerator {
      //  protected val sync = syncs
      //  override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      //}
      case _ => throw new RuntimeException("Unknown Host type " + target.toString)
    }
  }
}

//TODO: Some location symbols are hard-coded. Change them.
class CudaDynamicExecutableGenerator(val location: Int, val graph: DeliteTaskGraph) extends CudaExecutableGenerator with CudaSyncGenerator {
  def executableName(location: Int) = "Executable" + location

  var syncLocation: Int = location

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], host: Targets.Value) = {
    host match {
      case Targets.Scala => new ScalaMainExecutableGenerator(syncLocation, graph) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(syncLocation)
        override def consumerSet(sender: Send) = {  if(location == 0) scala.collection.mutable.HashSet(1) else scala.collection.mutable.HashSet(0) }
      }
      //case Targets.Cpp => new CppMainExecutableGenerator(location, graph) with CudaSyncObjectGenerator {
      //  protected val sync = syncs
      //  override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      //}
      case _ => throw new RuntimeException("Unknown Host type " + host.toString)
    }
  }

  override protected def addKernelCalls(resource: OpList) {
    val graph = Delite.loadDeliteDEG(Delite.inputArgs(0))
    
    initializeBlock()
    // Set all the ops to be scheduled on the resource 0
    for (op <- graph.totalOps) op.scheduledResource = 0

    for (op <- graph.totalOps if op.supportsTarget(Targets.Cuda)) {
      writeFunctionCall(op) 
    }
    finalizeBlock()
  }

  private val localSyncObjects = new ArrayBuffer[Send]
  private val remoteSyncObjects = new ArrayBuffer[Send]
  private def addLocalSync(s: SendData) { if(!localSyncObjects.exists(_.asInstanceOf[SendData].sym == s.sym)) localSyncObjects += s }
  private def addRemoteSync(s: SendData) { if(!remoteSyncObjects.exists(_.asInstanceOf[SendData].sym == s.sym)) remoteSyncObjects += s }

  override protected def makeNestedFunction(op: DeliteOP) = op match {
    case s: Send => localSyncObjects += s
    case _ => //
  }

  override protected def writeSyncObject() {
    syncObjectGenerator(localSyncObjects, Targets.Scala).makeSyncObjects
    syncLocation = 0
    syncObjectGenerator(remoteSyncObjects, Targets.Scala).makeSyncObjects
  }

  override protected def writeFunctionCall(op: DeliteOP) {
    if (op.task == null) return //dummy op

    op match {
      case op:OP_MultiLoop =>
        out.append("else if(strcmp(task,\"" + op.id + "\") == 0) {\n")
        out.append("addEvent(h2dStream, kernelStream);\n")
        writeFreeInit(op)
        for (name <- op.getOutputs if(op.outputType(name)!="Unit")) {
          out.append(op.outputType(Targets.Cuda, name))
          out.append(" *" + getSymDevice(op,name) + ";\n")
          writeFreeOutput(op, name, isPrimitiveType(op.outputType(name)))
        }
        out.append("jobject inputcopyArr = env" + location + "->CallObjectMethod(objTask,env" + location + "->GetMethodID(clsTask,\"inputCopy\",\"()[Z\"));\n")
        out.append("jboolean *inputcopyJava = (jboolean *)env" + location + "->GetPrimitiveArrayCritical((jbooleanArray)inputcopyArr,0);\n")
        out.append("bool *inputcopy = (bool *)malloc(sizeof(bool)*" + op.getInputs.size + ");\n")
        out.append("memcpy(inputcopy,inputcopyJava,sizeof(bool)*" + op.getInputs.size + ");\n")
        out.append("env" + location + "->ReleasePrimitiveArrayCritical((jbooleanArray)inputcopyArr,inputcopyJava,0);\n")

        out.append("int inputIdx = 0;\n")
        for ((in,name) <- op.getInputs.toArray) {
          writeGetter(in, name, op, false)
          out.append("inputIdx += 1;\n")
        }
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => op.outputType(Targets.Cuda,o._2)!="void").map(o => "&"+getSymDevice(op,o._2)).toList ++ op.getInputs.map(i=>getSymDevice(i._1,i._2)) ++ List("size")
        
        out.append("jint start = env" + location + "->CallIntMethod(objTask,env" + location + "->GetMethodID(clsTask,\"start\",\"()I\"));\n")
        out.append("jint size = env" + location + "->CallIntMethod(objTask,env" + location + "->GetMethodID(clsTask,\"size\",\"()I\"));\n")
        val size = if(op.sizeIsConst) op.size else getSymDevice(op,op.size)
        out.append("if(size==-1) size = " + size + " - start;\n")
        
        //out.append(op.task) //kernel name
        out.append("MultiLoop_GPU_Array_" + op.id)
        out.append(args.mkString("(",",",");\n"))
        out.append("addEvent(kernelStream, d2hStream);\n")
        
        val blockingCall = op.needsCombine || op.needsPostProcess || op.getOutputs.map(op.outputType(_)).exists(t => !t.startsWith("ppl.delite.runtime.data.DeliteArray"))
        if(blockingCall) {
          for (name <- op.getOutputs if(op.outputType(name)!="Unit")) {
            writeSetter(op, name, false)
          }
          writeDataFreesRegister(op)
          out.append("}\n")
        }
        else {
          var offset = 0
          for (name <- op.getOutputs) { 
            op.outputType(name) match {
              case da if da.startsWith("ppl.delite.runtime.data.DeliteArrayObject") => throw new RuntimeException("Cannot output object delitearray type!")
              case da if da.startsWith("ppl.delite.runtime.data.DeliteArray") =>
                out.append("outputMap->insert(std::pair<char*,void*>(\"" + getSymDevice(op,name) + "\","+getSymDevice(op,name)+"));\n")
                out.append("jintArray arr" + name + " = env" + location + "->NewIntArray(1);\n")
                out.append("jint *dataPtr" + name + " = (jint *)env" + location + "->GetPrimitiveArrayCritical((jintArray)arr" + name + ",0);\n")
                out.append("dataPtr" + name + "[0] = " + getSymDevice(op,name) + "->length;\n")
                out.append("env" + location + "->ReleasePrimitiveArrayCritical((jintArray)arr" + name + ", dataPtr" + name + ", 0);\n") 
                out.append("jstring id" + name + " = env" + location + "->NewStringUTF(\"" + name + "_" + offset + "\");\n")
                out.append("jclass cls" + name + " = env" + location + "->FindClass(\"" + da.replaceAll("\\.","/").replaceAll("DeliteArray","RemoteDeliteArray") + "\");\n")
                out.append("jmethodID mid" + name + " = env" + location + "->GetMethodID(cls"+name+",\"<init>\",\"(Ljava/lang/String;[I)V\");\n")
                out.append("jobject obj" + name + " = env" + location + "->NewObject(cls"+name+",mid"+name+",id"+name+",arr"+name+");\n")
                out.append("env%1$s->CallStaticVoidMethod(cls%1$s,env%1$s->GetStaticMethodID(cls%1$s,\"set_%2$s\",\"(%3$s)V\"),%4$s);\n".format(location,getSym(op,name),getJNIArgType(op.outputType(name)),"obj"+name))
              case _ => throw new RuntimeException("Cannot output non-delitearray types!")
            }
            //offset += 1
          }
          writeDataFreesRegister(op)
          out.append("}\n")
          //Print out the getters
          for (name <- op.getOutputs) { 
            op.outputType(name) match {
              case da if da.startsWith("ppl.delite.runtime.data.DeliteArrayObject") => throw new RuntimeException("Cannot output object delitearray type!")
              case da if da.startsWith("ppl.delite.runtime.data.DeliteArray") =>
                out.append("else if(strcmp(task,\"get_" + name + "\") == 0) {\n")
                out.append(op.outputType(Targets.Cuda, name) + " *" + getSymDevice(op,name) + " = (" + op.outputType(Targets.Cuda,name) + "*)outputMap->find(\""+getSymDevice(op,name)+"\")->second;\n")
                writeSetter(op, name, false)
                out.append("}\n")
              case _ => throw new RuntimeException("Cannot output non-delitearray types!")
            }
          }
        }
        
      case _ =>
    }
  }

  override protected def initializeBlock() {
    out.append("bool terminate = false;\n")
    out.append("while(!terminate) {\n")
    out.append("jobject objTask = env" + location + "->CallStaticObjectMethod(clsMesosExecutor,env" + location + "->GetStaticMethodID(clsMesosExecutor,\"getTask\",\"(I)Lppl/delite/runtime/DeliteMesosExecutor$Task;\"),"+location+");\n")
    out.append("jclass clsTask = env" + location + "->FindClass(\"ppl/delite/runtime/DeliteMesosExecutor$Task\");\n")
    out.append("jobject taskName = env" + location + "->CallObjectMethod(objTask,env" + location + "->GetMethodID(clsTask,\"name\",\"()Ljava/lang/String;\"),"+location+");\n")
    out.append("const char *task = env" + location + "->GetStringUTFChars((jstring)taskName,NULL);\n")
    out.append("if(strcmp(task,\"TERMINATE\") == 0) {\n")
    out.append("terminate = true;\n")
    out.append("}\n")
  }

  override protected def finalizeBlock() {
    out.append("}\n")
  }
  
  override protected def writeMethodHeader() { 
    super.writeMethodHeader()
    out.append("jclass clsMesosExecutor = env" + location + "->FindClass(\"ppl/delite/runtime/DeliteMesosExecutor\");\n")
  }

  private def writeGetter(dep: DeliteOP, sym: String, to: DeliteOP, view: Boolean) {
    addRemoteSync(SendData(sym,dep,1))
    out.append(getJNIType(dep.outputType(sym)))
    out.append(' ')
    out.append(getSymCPU(sym))
    out.append(" = ")
    out.append("env")
    out.append(location)
    out.append("->CallStatic")
    out.append(getJNIFuncType(dep.outputType(sym)))
    out.append("Method(cls")
    out.append(dep.scheduledResource)
    out.append(",env")
    out.append(location)
    out.append("->GetStaticMethodID(cls")
    out.append(dep.scheduledResource)
    out.append(",\"get")
    out.append(location)
    out.append('_')
    out.append(getSym(dep,sym))
    out.append("\",\"()")
    out.append(getJNIOutputType(dep.outputType(Targets.Scala,sym)))
    out.append("\"));\n")
    val ref = if (isPrimitiveType(dep.outputType(sym))) "" else "*"
    val devType = dep.outputType(Targets.Cuda, sym)
    if (view) {
      out.append("Host%s %s%s = recvViewCPPfromJVM_%s(env%s,%s);\n".format(devType,ref,getSymHost(dep,sym),mangledName(devType),location,getSymCPU(sym)))
      out.append("%s %s%s = sendCuda_%s(%s);\n".format(devType,ref,getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym)))
    }
    else if(isPrimitiveType(dep.outputType(sym))) {
      out.append("%s %s = (%s)%s;\n".format(devType,getSymHost(dep,sym),devType,getSymCPU(sym)))
      out.append("%s %s = (%s)%s;\n".format(devType,getSymDevice(dep,sym),devType,getSymHost(dep,sym)))
    }
    else {
      out.append("%s %s%s;\n".format(devType,ref,getSymDevice(dep,sym)))
      out.append("if(inputcopy[inputIdx]) {\n")
      writeFreeInput(to, sym, isPrimitiveType(dep.outputType(sym)))
      out.append("Host%s %s%s = recvCPPfromJVM_%s(env%s,%s);\n".format(devType,ref,getSymHost(dep,sym),mangledName(devType),location,getSymCPU(sym)))
      // Use transpose copy
      dep.stencilOrElse(sym)(Empty) match { 
        case Interval(start,stride,length) => out.append("%s = sendCudaTrans_%s(%s,%s);\n".format(getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym),getSymDevice(dep,length.trim)))
        case _ => out.append("%s = sendCuda_%s(%s);\n".format(getSymDevice(dep,sym),mangledName(devType),getSymHost(dep,sym)))
      }
      out.append("outputMap->insert(std::pair<char*,void*>(\"%s\",%s));\n".format(getSymDevice(dep,sym),getSymDevice(dep,sym)))
      out.append("}\n")
      out.append("else {\n")
      out.append("%s = (%s%s)(outputMap->find(\"%s\")->second);\n".format(getSymDevice(dep,sym),devType,ref,getSymDevice(dep,sym)))
      out.append("}\n")
      out.append("cudaMemoryMap->insert(std::pair<void*,std::list<void*>*>(")
      out.append(getSymDevice(dep,sym))
      out.append(",lastAlloc));\n")
      out.append("lastAlloc = new std::list<void*>();\n")
    }
  }

  private def writeSetter(op: DeliteOP, sym: String, view: Boolean) {
    addLocalSync(SendData(sym,op,0))
    val devType = op.outputType(Targets.Cuda, sym)
    if (view) {
      out.append("Host%s %s = recvCuda_%s(%s);\n".format(devType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s *%s = sendViewCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(devType),location,getSymHost(op,sym)))
    }
    else if(isPrimitiveType(op.outputType(sym))) {
      out.append("%s %s = recvCuda_%s(%s);\n".format(getCPrimitiveType(op.outputType(sym)),getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s %s = (%s)%s;\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),getJNIType(op.outputType(sym)),getSymHost(op,sym)))
    }
    else if(devType.startsWith("DeliteArray<")) {
      devType match { //TODO: Fix this for nested object types
        case "DeliteArray< bool >" | "DeliteArray< char >" | "DeliteArray< CHAR >" | "DeliteArray< short >" | "DeliteArray< int >" | "DeiteArray< long >" | "DeliteArray< float >" | "DeliteArray< double >" => 
          out.append("Host%s *%s = recvCuda_%s(%s);\n".format(devType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
        case _ => //DeliteArrayObject Type
          out.append("HostDeliteArray< Host%s  *%s = recvCuda_%s(%s);\n".format(devType.drop(13),getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      }
      out.append("%s %s = sendCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(devType),location,getSymHost(op,sym)))
    }
    else {
      out.append("Host%s *%s = recvCuda_%s(%s);\n".format(devType,getSymHost(op,sym),mangledName(devType),getSymDevice(op,sym)))
      out.append("%s %s = sendCPPtoJVM_%s(env%s,%s);\n".format(getJNIType(op.outputType(sym)),getSymCPU(sym),mangledName(devType),location,getSymHost(op,sym)))
    }

    out.append("env")
    out.append(location)
    out.append("->CallStaticVoidMethod(cls")
    out.append(location)
    out.append(",env")
    out.append(location)
    out.append("->GetStaticMethodID(cls")
    out.append(location)
    out.append(",\"set_")
    out.append(getSym(op,sym))
    out.append("\",\"(")
    out.append(getJNIArgType(op.outputType(sym)))
    out.append(")V\"),")
    out.append(getSymCPU(sym))
    out.append(");\n")
  }

  private def freeItem(op: DeliteOP) = "freeItem_" + op.id

  private def writeFreeInit(op: DeliteOP) {
    out.append("FreeItem ")
    out.append(freeItem(op))
    out.append(";\n")
    out.append(freeItem(op))
    out.append(".keys = new std::list< std::pair<void*,bool> >();\n")
  }

  private def writeFreeInput(op: DeliteOP, sym: String, isPrim: Boolean = false) {
    out.append("if(outputMap->find(\"" + getSymDevice(op,sym) + "\") != outputMap->end()) {\n")
    out.append("std::pair<void*,bool> ")
    out.append(getSymDevice(op,sym))
    out.append("_pair(")
    out.append("outputMap->find(\"" + getSymDevice(op,sym) + "\")->second")
    out.append(",")
    out.append(isPrim) //Do not free this ptr using free() : primitive type pointers points to device memory
    out.append(");\n")
    out.append(freeItem(op))
    out.append(".keys->push_back(")
    out.append(getSymDevice(op,sym))
    out.append("_pair);\n")
    out.append("}\n")
  }
  
  private def writeFreeOutput(op: DeliteOP, sym: String, isPrim: Boolean = false) {
    out.append("if(outputMap->find(\"" + getSymDevice(op,sym) + "\") != outputMap->end()) {\n")
    out.append("std::pair<void*,bool> ")
    out.append(getSymDevice(op,sym))
    out.append("_pair(")
    out.append("outputMap->find(\"" + getSymDevice(op,sym) + "\")->second")
    out.append(",")
    out.append(isPrim) //Do not free this ptr using free() : primitive type pointers points to device memory
    out.append(");\n")
    out.append(freeItem(op))
    out.append(".keys->push_back(")
    out.append(getSymDevice(op,sym))
    out.append("_pair);\n")
    out.append("}\n")
  }

  private def writeDataFreesRegister(op: DeliteOP) {
    val freeItem = "freeItem_" + op.id
    out.append(freeItem)
    out.append(".event = addHostEvent(kernelStream);\n")
    out.append("freeList->push(")
    out.append(freeItem)
    out.append(");\n")
  }

}

object CudaExecutableGenerator {

  val syncObjects = ArrayBuffer[String]()
  syncObjects += "#include <pthread.h>\n"
  syncObjects += "#include \"" + Targets.Cuda + "helperFuncs.h\"\n"

  def makeExecutables(schedule: PartialSchedule, graph: DeliteTaskGraph) {
    for (sch <- schedule if sch.size > 0) {
      val location = sch.peek.scheduledResource
      if(Config.clusterMode == 2) 
        new CudaDynamicExecutableGenerator(location, graph).makeExecutable(sch) // native execution plan
      else 
        new CudaMainExecutableGenerator(location, graph).makeExecutable(sch) // native execution plan
      new ScalaNativeExecutableGenerator(location, graph).makeExecutable(sch) // JNI launcher scala source
    }
    // Register header file for the Cpp sync objects
    CudaCompile.addHeader(syncObjects.mkString(""),"cudaSyncObjects")
  }

  def clear() { 
    syncObjects.clear 
  }
}
