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

trait CppResourceInfoGenerator {
  protected def resourceInfoType = "resourceInfo_t"
  protected def resourceInfoSym = "resourceInfo"
}

trait CppExecutableGenerator extends ExecutableGenerator with CppResourceInfoGenerator {

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

  protected def writeHeader() {
    out.append("#include <stdio.h>\n")
    out.append("#include <stdlib.h>\n")
    out.append("#include <jni.h>\n")
    out.append("#include \"DeliteCppProfiler.h\"\n")
    out.append("#include \"cppSyncObjects.h\"\n")
    out.append("#include \"" + Targets.Cpp + "helperFuncs.h\"\n")
    out.append("#include \""+CppMultiLoopHeaderGenerator.headerFile+".h\"\n")
    out.append("extern JNIEnv* env" + location + ";\n")
  }

  protected def writeMethodHeader() {
    out.append("JNIEnv* env" + location + ";\n")
    val function = "JNIEXPORT void JNICALL Java_" + executableName + "_00024_host" + executableName + "(JNIEnv* jnienv, jobject object)"
    out.append("extern \"C\" ") //necessary because of JNI
    out.append(function)
    out.append(";\n")
    out.append(function)
    out.append(" {\n")
    out.append("env" + location + " = jnienv;\n")
    out.append(resourceInfoType + " " + resourceInfoSym + ";\n")
    out.append(resourceInfoSym + ".thread_id = " + Targets.getRelativeLocation(location) + ";\n")
    if (Config.profile)
      out.append("InitDeliteCppTimer(" + Targets.getRelativeLocation(location) + ");\n")
    val locations = opList.siblings.filterNot(_.isEmpty).map(_.resourceID).toSet
    val cppLocations = locations.filter(l => Targets.getByLocation(l) == Targets.Cpp)
    val numActiveCpps = cppLocations.size
    val initializerIdx = Targets.getRelativeLocation(cppLocations.min)
    out.append("DeliteHeapInit(" + Targets.getRelativeLocation(location) + "," + Config.numCpp + "," + numActiveCpps + "," + initializerIdx + "," + Config.cppHeapSize + "ULL);\n")
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
    val locations = opList.siblings.filterNot(_.isEmpty).map(_.resourceID).toSet
    val cppLocations = locations.filter(l => Targets.getByLocation(l) == Targets.Cpp)
    val numActiveCpps = cppLocations.size
    val finalizerIdx = Targets.getRelativeLocation(cppLocations.min)
    if (Config.profile) 
      out.append("DeliteCppTimerDump(" + Targets.getRelativeLocation(location) + "," + location + ",env" + location + ");\n")
    out.append("DeliteHeapClear(" + Targets.getRelativeLocation(location) + "," + Config.numCpp + "," + numActiveCpps + "," + finalizerIdx + ");\n")
    out.append("}\n")
  }

  protected def writeFooter() { }

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

  protected def writeFunctionCall(op: DeliteOP) {
    def returnsResult = op.outputType(op.getOutputs.head) == op.outputType
    def resultName = if (returnsResult) getSymHost(op, op.getOutputs.head) else getOpSym(op)

    if (op.task == null) return //dummy op

    if (Config.profile) {
      if (!op.isInstanceOf[OP_MultiLoop]) {
        out.append("DeliteCppTimerStart(" + Targets.getRelativeLocation(location) + ",\""+op.id+"\");\n")
      }
    }

    if (op.outputType(Targets.Cpp) != "void") {
      out.append(op.outputType(Targets.Cpp))
      out.append(' ')
      if (!isPrimitiveType(op.outputType) && !op.outputType(Targets.Cpp).startsWith("std::shared_ptr")) out.append('*')
      out.append(resultName)
      out.append(" = ")
    }

    out.append(op.task) //kernel name
    op match {
      case _:Arguments => 
        assert(Arguments.args.length == 1 && Arguments.args(0).isInstanceOf[Array[String]], "ERROR: Custom input arguments are not currently suppored with Cpp target")
        val args = Arguments.args(0).asInstanceOf[Array[String]]
        if(args.length > 0)
          out.append("(" + args.length + args.map("\""+_+"\"").mkString(",",",",");\n"))
        else
          out.append("(" + args.length + ");\n") 
      case _ => out.append((resourceInfoSym+:op.getInputs.map(i=>getSymHost(i._1,i._2))).mkString("(",",",");\n"))
    }
   
    if (Config.profile) {
      if (!op.isInstanceOf[OP_MultiLoop]) {
        out.append("DeliteCppTimerStop(" + Targets.getRelativeLocation(location) + ",\""+op.id+"\");\n")
      }
    }

    if (!returnsResult) {
      for (name <- op.getOutputs if(op.outputType(Targets.Cpp,name)!="void")) {
        out.append(op.outputType(Targets.Cpp, name) + addRef(op.outputType(name)))
        out.append(" " + getSymHost(op,name) + " = " + resultName + "->" + name + ";\n")
      }
      // Delete activation record and multiloop header
      assert(op.isInstanceOf[OP_MultiLoop] && op.getInputs.size==1)
      if (Config.cppMemMgr == "refcnt") {
        val (multiloop_h_op,multiloop_h_sym) = op.getInputs.head
        out.append("delete " + resultName + ";\n")
        out.append("delete " + getSymHost(multiloop_h_op,multiloop_h_sym) + ";\n")
      }
    }
  }

  protected def getSymCPU(name: String): String = {
    "xC"+name
  }

  protected def getSymHost(op: DeliteOP, name: String): String = {
    "xH"+name
  }

  protected def writeSyncObject() {  }

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
    syncObjects.clear 
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
    val degName = ppl.delite.runtime.Delite.inputArgs(0).split('.')
    val appName = degName(degName.length-2)
    val tgt = OpHelper.scheduledTarget(location)
    out.append("@native def host" + executableName(location) + ": Unit\n")
    out.append("System.load(\"\"\"")
    out.append(Compilers(tgt).binCacheHome)
    out.append(tgt)
    out.append("Host" + appName + ".")
    out.append(OS.libExt)
    out.append("\"\"\")\n")
  }

  private def writeNativeCall() {
    out.append("host")
    out.append(executableName(location))
    out.append('\n')
  }
}
