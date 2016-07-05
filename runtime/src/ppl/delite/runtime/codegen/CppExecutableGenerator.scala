package ppl.delite.runtime.codegen

import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.{OS, Targets}
import collection.mutable.ArrayBuffer
import sync._
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.scheduler.{OpHelper, OpList, PartialSchedule}

object CppResourceInfo {
  def resourceInfoType = "resourceInfo_t"
  def resourceInfoSym = "resourceInfo"
}

trait CppExecutableGenerator extends ExecutableGenerator {
  import CppResourceInfo._

  // To get a non-conflicting index for a variable name used to temporarily store jobject
  private var index = 0
  protected def getSyncVarIdx = {
    index += 1
    index
  }

  protected def addSource(source: String) {
    // Add extern header files for generated kernels at walk-time
    val externs = CppCompile.headers.map(s => "#include \"" + s + "\"\n").mkString("")
    CppCompile.addSource(externs+source, executableName)
  }

  protected[codegen] def writeHeader() {
    out.append("#include <stdio.h>\n")
    out.append("#include <stdlib.h>\n")
    if (!Config.noJVM) out.append("#include <jni.h>\n")
    out.append("#include \"cppSyncObjects.h\"\n")
    out.append("#include \"" + Targets.Cpp + "helperFuncs.h\"\n")
    out.append("#include \""+CppMultiLoopHeaderGenerator.headerFile+".h\"\n")
    if (!Config.noJVM) out.append("extern JNIEnv* env" + location + ";\n")
  }

  private def scheduledLocations() = {
    graph.schedule.resources.filterNot(_.isEmpty).map(_.resourceID).toSet
  }

  private def activeCppLocations() = {
    scheduledLocations.filter(l => Targets.getHostTarget(Targets.getByLocation(l)) == Targets.Cpp).size
  }

  protected[codegen] def writeMethodHeader() {
    declareGlobals()

    if (!Config.noJVM) writeJNIMethodHeader()
    else writeStandaloneMethodHeader()
    
    initializeGlobals()

    if (!Config.noJVM) writeJNIInitializer(scheduledLocations)    
  }

  protected def writeStandaloneMethodHeader() {
    val appName = if (graph.appName == "") "DeliteApplication" else graph.appName
    val args = graph.ops.filter(_.isInstanceOf[Arguments]).map(_.asInstanceOf[Arguments]).toSeq.sortBy(_.argIdx)
    val argNames = args.map("in"+_.argIdx)
    val argTypes = args.map { a =>
      val tp = a.outputType(Targets.Cpp)
      // Adding of the '*' should be done in delitec, not delite
//      if (!isPrimitiveType(a.outputType)) tp + " *" else tp
      tp
    }
    val eop = graph.result._1.asInstanceOf[EOP]
    val res = if (!isPrimitiveType(eop.outputType) && eop.outputType!="Unit") eop.outputType(Targets.Cpp)+" *" else eop.outputType(Targets.Cpp)
    out.append(res+" "+appName+"(int numThreads, ")
    out.append(argNames.zip(argTypes).map(a => a._2+" "+a._1).mkString(", ")+"){\n")
  }

  protected[codegen] def declareGlobals() {
    if (!Config.noJVM) out.append("JNIEnv* env" + location + ";\n")
  }

  protected[codegen] def initializeGlobals() {
    if (!Config.noJVM) {
      out.append("env" + location + " = jnienv;\n")
      out.append("JNIEnv *env = jnienv;\n")
    }

    out.append(s"initializeAll(${Targets.getRelativeLocation(location)}, numThreads, ${activeCppLocations}, ${Config.numThreads}, ${Config.cppHeapSize}ULL);\n")
    out.append(resourceInfoType + " " + resourceInfoSym + "_stack = resourceInfos["+Targets.getRelativeLocation(location)+"];\n")
    out.append(resourceInfoType + "* " + resourceInfoSym + " = &" + resourceInfoSym + "_stack;\n")
  }

  protected def writeJNIMethodHeader() {
    val function = "JNIEXPORT void JNICALL Java_" + executableName + "_00024_host" + executableName + "(JNIEnv* jnienv, jobject object, jint numThreads)"
    out.append("extern \"C\" ") //necessary because of JNI
    out.append(function)
    out.append(";\n")
    out.append(function)
    out.append(" {\n")
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
    if (!locations.isEmpty) {
      out.append("jclass clsBU = env" + location + "->FindClass(\"scala/runtime/BoxedUnit\");\n")
      out.append("jobject boxedUnit = env" + location + "->GetStaticObjectField(clsBU, env" + location + "->GetStaticFieldID(clsBU, \"UNIT\", \"Lscala/runtime/BoxedUnit;\"));\n")
    }
  }

  /**
   * Add entry method to primary executable
   */
  def writeMain() {
    if (Config.noJVM && location == 0) {
      out.append(s"""
int main(int argc, char *argv[]) {
  cppDeliteArraystring *args = new cppDeliteArraystring(argc-1);
  for (int i=1; i<argc; i++) {
    args->update(i-1, *(new string(argv[i])));
  }
  int numThreads = 1;
  char *env_threads = getenv("DELITE_NUM_THREADS");
  if (env_threads != NULL) {
    numThreads = atoi(env_threads);
  } else {
    fprintf(stderr, "[WARNING]: DELITE_NUM_THREADS undefined, defaulting to 1\\n");
  }
  fprintf(stderr, "Executing with %d thread(s)\\n", numThreads);
  DeliteApplication(numThreads, args);
  return 0;
}
""")
    }
  }

  protected[codegen] def writeMethodFooter() {
    val env = if (Config.numMaxJ > 0 && Config.noJVM) "0" else s"env$location"
    out.append(s"clearAll(numThreads, ${activeCppLocations}, ${Config.numThreads}, $env);\n")
    out.append("}\n")
  }

  protected[codegen] def writeFooter() {
    writeMain()
  }

  //TODO: can/should this be factored out? need some kind of factory for each target
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case c: OP_Condition => {
      val codegen = new CppConditionGenerator(c, location, graph)
      codegen.makeExecutable()
      CppCompile.addHeader(codegen.generateMethodSignature + ";\nextern bool " + c.id.split('_').head + "_cond;\n", codegen.executableName(location))
    }
   case w: OP_While => {
      val codegen = new CppWhileGenerator(w, location, graph)
      codegen.makeExecutable()
      CppCompile.addHeader(codegen.generateMethodSignature + ";\n", codegen.executableName(location))
    }
    case err => println("Cannot generate op" + op.id) //sys.error("Unrecognized OP type: " + err.getClass.getSimpleName)
  }

  protected[codegen] def writeFunctionCall(op: DeliteOP) {
    def returnsResult = op.outputType(op.getOutputs.head) == op.outputType
    def resultName = if (returnsResult) getSymHost(op, op.getOutputs.head) else getOpSym(op)

    if (op.task == null) return //dummy op

    if (Config.profile) {
      out.append("DeliteCppTimerStart(resourceInfo->threadId, \""+op.id+"\");\n")
    }

    if (op.outputType(Targets.Cpp) != "void") {
      out.append(op.outputType(Targets.Cpp))
      out.append(' ')
      // Adding of the '*' should be done in delitec, not delite
//      if (!isPrimitiveType(op.outputType) && !op.outputType(Targets.Cpp).startsWith("std::shared_ptr") && !op.outputType(Targets.Cpp).contains("*")) out.append('*')
      out.append(resultName)
      out.append(" = ")
    }

    out.append(op.task) //kernel name
    op match {
      case _: Arguments => out.append(";\n") //no function to call
      case _ => out.append((resourceInfoSym+:op.getInputs.map(i=>getSymHost(i._1,i._2))).mkString("(",",",");\n"))
    }

    if (Config.profile) {
      if (op.isInstanceOf[OP_MultiLoop]) {
        out.append("DeliteCppTimerStopMultiLoop(resourceInfo->threadId, \""+op.id+"\");\n")
      } else {
        out.append("DeliteCppTimerStop(resourceInfo->threadId, \""+op.id+"\");\n")
      }
    }

    if (!returnsResult) {
      for (name <- op.getOutputs if(op.outputType(Targets.Cpp,name)!="void")) {
        out.append(op.outputType(Targets.Cpp, name))
//        out.append(op.outputType(Targets.Cpp, name) + addRef(op.outputType(name)))
        out.append(" " + getSymHost(op,name) + " = " + resultName + "->" + name + ";\n")
      }
      // Delete activation record and multiloop header
      assert(op.isInstanceOf[OP_MultiLoop])
      if (Config.cppMemMgr == "refcnt") {
        val (multiloop_h_op,multiloop_h_sym) = op.getInputs.head
        out.append("delete " + resultName + ";\n")
      }
    }
  }

  protected def getSymCPU(name: String): String = {
    "xC"+name
  }

  protected def getSymHost(op: DeliteOP, name: String): String = {
    "xH"+name
  }

  override protected[codegen] def writeSyncObject() {  }

  protected def isPrimitiveType(scalaType: String) = scalaType match {
    case "java.lang.String" => true
    case _ => Targets.isPrimitiveType(scalaType)
  }

  private def addRef(): String = if (Config.cppMemMgr == "refcnt") " " else " *"
  protected def addRef(scalaType: String): String = if (isPrimitiveType(scalaType)) " " else addRef()
}

class CppMainExecutableGenerator(val location: Int, val graph: DeliteTaskGraph)
  extends CppExecutableGenerator with CppSyncGenerator {

  def executableName(location: Int) = "Executable" + location

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value) = {
    target match {
      case Targets.Scala => new ScalaMainExecutableGenerator(location, graph) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Targets.Cpp => new CppMainExecutableGenerator(location, graph) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + target.toString)
    }
  }
}

object CppExecutableGenerator {

  val syncObjects = ArrayBuffer[String]()
  syncObjects += "#include <pthread.h>\n"
  syncObjects += "#include \"" + Targets.Cpp + "helperFuncs.h\"\n"
  syncObjects += "#include \""+CppMultiLoopHeaderGenerator.headerFile+".h\"\n"

  def makeExecutables(schedule: PartialSchedule, graph: DeliteTaskGraph) {
    for (sch <- schedule if sch.size > 0) {
      val location = sch.peek.scheduledResource
      new CppMainExecutableGenerator(location, graph).makeExecutable(sch) // native execution plan
      new ScalaNativeExecutableGenerator(location, graph).makeExecutable(sch) // JNI launcher scala source
    }
    // Register header file for the Cpp sync objects
    val str = new StringBuilder
    str.append("#include <pthread.h>\n")
    str.append("#include \"cpphelperFuncs.h\"\n")
    str.append("#include \"multiLoopHeaders.h\"\n")
    str.append(syncObjects.mkString(""))
    CppCompile.addHeader(str.toString,"cppSyncObjects")
    CppMultiLoopHeaderGenerator.createHeaderFile()
  }

  def clear() {
    syncObjects.clear()
  }

}

class ScalaNativeExecutableGenerator(override val location: Int, override val graph: DeliteTaskGraph) extends ScalaMainExecutableGenerator(location, graph) {

  override def makeExecutable(ops: OpList) {
    opList = ops
    writeHeader()
    writeNativeLoad()
    writeMethodHeader()
    writeNativeCall()
    writeMethodFooter()
    writeFooter()
    addSource(out.toString)
  }

  private def writeNativeLoad() {
    out.append("@native def host" + executableName(location) + "(numThreads: Int): Unit\n")
    out.append("System.load(\"")
    Compilers(OpHelper.scheduledTarget(location)) match {
      case c: CCompile => out.append(c.executableName)
      case _ => sys.error("NativeExecutable must be compiled by a CCompiler: " + location)
    }
    out.append("\")\n")
  }

  private def writeNativeCall() {
    val target = Targets.getByLocation(location)
    val numThreads = target match {
      case Targets.Cpp => "Config.numCpp"
      case Targets.Cuda => "Config.numCuda"
      case Targets.OpenCL => "Config.numOpenCL"
      case _ => throw new RuntimeException("Unsupported native target: " + target)
    }
    out.append(s"host${executableName(location)}(${numThreads});\n")
  }
}
