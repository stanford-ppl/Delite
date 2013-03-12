package ppl.delite.runtime.codegen


import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.scheduler.{OpList, PartialSchedule}
import ppl.delite.runtime.Config
import ppl.delite.runtime.codegen.hosts.Hosts
import ppl.delite.runtime.graph.targets.{OS, Targets}
import collection.mutable.ArrayBuffer
import sync._
import ppl.delite.runtime.graph.DeliteTaskGraph

trait CudaExecutableGenerator extends ExecutableGenerator {

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
      out.append(".keys = new list< pair<void*,bool> >();\n")
    }

    def writeFree(sym: String, isPrim: Boolean) {
      if (count == 0) writeFreeInit()
      out.append("pair<void*,bool> ")
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
    out.append("#include <jni.h>\n")
    out.append("#include <cuda_runtime.h>\n") //Cuda runtime api
    out.append("#include \"cublas.h\"\n") //cublas library
    out.append("#include \"DeliteCuda.h\"\n") //Delite-Cuda interface for DSL
    out.append("#include \"cudaSyncObjects.h\"\n")
    out.append("#include \"helperFuncs.h\"\n")
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
    out.append("}\n")
  }

  protected def writeFooter() { }

  //TODO: can/should this be factored out? need some kind of factory for each target
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case c: OP_Condition => {
      val codegen = new CudaConditionGenerator(c, location, kernelPath)
      codegen.makeExecutable()
      CudaCompile.addHeader(codegen.generateMethodSignature + ";\n", codegen.executableName(location))
    }
   case w: OP_While => {
      val codegen = new CudaWhileGenerator(w, location, kernelPath)
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
        val args = op.getInputs.map(i=>getSymDevice(i._1,i._2))
        out.append(args.mkString("(",",",");\n"))
      /*
      case _:OP_External =>
        assert(op.getOutputs.size == 1) //TODO: what does libCall support?
        out.append(op.task) //kernel name
        out.append('(')
        for (name <- op.getOutputs) {
          if (op.outputType(name) != "Unit") out.append("*" + getSymDevice(op,name) + ",")
        }
        //writeInputs(op) //then all op inputs
        //out.append(",kernelStream") //TODO: what other libraries besides cuBlas do we use? how do we use the stream only with supported libraries?
        out.append(");\n")
      */
    }
    out.append("addEvent(kernelStream, d2hStream);\n")
    //writeDataFrees(op)
  }

  protected def getSymCPU(name: String): String = {
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

}

class CudaMainExecutableGenerator(val location: Int, val kernelPath: String)
  extends CudaExecutableGenerator with CudaSyncGenerator {

  def executableName(location: Int) = "Executable" + location

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], host: Hosts.Value) = {
    host match {
      case Hosts.Scala => new ScalaMainExecutableGenerator(location, kernelPath) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      //case Hosts.Cpp => new CppMainExecutableGenerator(location, kernelPath) with CudaSyncObjectGenerator {
      //  protected val sync = syncs
      //  override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      //}
      case _ => throw new RuntimeException("Unknown Host type " + host.toString)
    }
  }
}

object CudaExecutableGenerator {

  val syncObjects = ArrayBuffer[String]()
  syncObjects += "#include <pthread.h>\n"
  syncObjects += "#include \"helperFuncs.h\"\n"

  var typesMap = Map[Targets.Value, Map[String,String]]()

  //TODO: Remove this not to use global structure for type information
  def collectInputTypesMap(graph: DeliteTaskGraph) {
    for (resource <- graph.schedule; op <- resource) {
      if (op.getInputTypesMap != null)
        typesMap = DeliteTaskGraph.combineTypesMap(List(op.getInputTypesMap,typesMap))
      if (op.getOutputTypesMap != null)
        typesMap = DeliteTaskGraph.combineTypesMap(List(op.getOutputTypesMap,typesMap))

      if (op.isInstanceOf[OP_Nested]) {
        for (subgraph <- op.asInstanceOf[OP_Nested].nestedGraphs) {
          collectInputTypesMap(subgraph)
        }
      }
    }
  }

  def makeExecutables(schedule: PartialSchedule, kernelPath: String) {
    for (sch <- schedule if sch.size > 0) {
      val location = sch.peek.scheduledResource
      new CudaMainExecutableGenerator(location, kernelPath).makeExecutable(sch) // native execution plan
      new ScalaNativeExecutableGenerator(location, kernelPath).makeExecutable() // JNI launcher scala source
    }
    // Register header file for the Cpp sync objects
    CudaCompile.addHeader(syncObjects.mkString(""),"cudaSyncObjects")
  }

}
