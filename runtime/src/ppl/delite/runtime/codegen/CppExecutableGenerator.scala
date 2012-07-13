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

trait CppExecutableGenerator extends ExecutableGenerator {

  protected def addSource(source: String) {
    // Add extern header files for generated kernels at walk-time
    val externs = CppCompile.headers.map(s => "#include \"" + s + "\"\n").mkString("")
    CppCompile.addSource(externs+source, executableName)
  }

  protected def writeHeader() {
    out.append("#include <jni.h>\n") //jni
    out.append("#include \"cppSyncObjects.h\"\n")
    out.append("#include \"cppHeader.hpp\"\n")
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

  protected def writeFooter() {
    addAccessor()
  }

  //TODO: can/should this be factored out? need some kind of factory for each target
  //TODO: why is multiloop codegen handled differently?
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case c: OP_Condition => {
      val codegen = new CppConditionGenerator(c, location, kernelPath)
      codegen.makeExecutable()
      CppCompile.addHeader(codegen.generateMethodSignature + ";\n", codegen.executableName(location))
    }
   case w: OP_While => {
      val codegen = new CppWhileGenerator(w, location, kernelPath)
      codegen.makeExecutable()
      CppCompile.addHeader(codegen.generateMethodSignature + ";\n", codegen.executableName(location))
    }
    //case v: OP_Variant => new CVariantGenerator(v, location, kernelPath).makeExecutable()
    case err => println("Cannot generate op" + op.id) //sys.error("Unrecognized OP type: " + err.getClass.getSimpleName)
  }

  protected def writeFunctionCall(op: DeliteOP) {
    def returnsResult = op.outputType(op.getOutputs.head) == op.outputType
    def resultName = if (returnsResult) getSymHost(op, op.getOutputs.head) else getOpSym(op)

    if (op.task == null) return //dummy op
    if (op.outputType != "Unit") {
      out.append(op.outputType(Targets.Cpp))
      out.append(' ')
      if (!isPrimitiveType(op.outputType)) out.append('*')
      out.append(resultName)
      out.append(" = ")
    }
    out.append(op.task) //kernel name
    out.append(op.getInputs.map(i=>getSymHost(i._1,i._2)).mkString("(",",",");\n"))

    if (!returnsResult) {
      for (name <- op.getOutputs) {
        out.append(op.outputType(Targets.Cpp, name))
        if (!isPrimitiveType(op.outputType(name))) out.append('*')
        out.append(" " + getSymHost(op,name) + " = " + resultName + "->" + name + ";\n")
      }
    }
  }

  protected def getSymCPU(name: String): String = {
    "xC"+name
  }

  protected def getSymHost(op: DeliteOP, name: String): String = {
    "xH"+name
  }

  protected def addAccessor() {  }

  protected def writeSyncObject() {  }

  protected def isPrimitiveType(scalaType: String) = CppExecutableGenerator.isPrimitiveType(scalaType)

}

class CppMainExecutableGenerator(val location: Int, val kernelPath: String)
  extends CppExecutableGenerator with CppSyncGenerator {

  def executableName(location: Int) = "Executable" + location

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], host: Hosts.Value) = {
    host match {
      case Hosts.Scala => new ScalaMainExecutableGenerator(location, kernelPath) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Hosts.Cpp => new CppMainExecutableGenerator(location, kernelPath) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + host.toString)
    }
  }
}

object CppExecutableGenerator {

  val syncObjects = ArrayBuffer[String]()
  syncObjects += "#include <pthread.h>\n"
  syncObjects += "#include \"cppHeader.hpp\"\n"
  syncObjects += "#include \""+CppMultiLoopHeaderGenerator.headerFile+".h\"\n"


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
    for (i <- 0 until schedule.numResources) {
      new CppMainExecutableGenerator(Config.numThreads+i, kernelPath).makeExecutable(schedule(i)) // native execution plan
      new ScalaNativeExecutableGenerator(Config.numThreads+i, kernelPath).makeExecutable          // JNI launcher scala source
    }
    // Register header file for the Cpp sync objects
    CppCompile.addHeader(syncObjects.mkString(""),"cppSyncObjects")
    CppMultiLoopHeaderGenerator.createHeaderFile()
  }

  def isPrimitiveType(scalaType: String): Boolean = scalaType match {
    case "Unit" => true
    case "Int" => true
    case "Long" => true
    case "Float" => true
    case "Double" => true
    case "Boolean" => true
    case "Short" => true
    case "Char" => true
    case "Byte" => true
    //case r if r.startsWith("generated.scala.Ref[") => isPrimitiveType(r.slice(20,r.length-1))
    case _ => false
  }

}

class ScalaNativeExecutableGenerator(override val location: Int, override val kernelPath: String) extends ScalaMainExecutableGenerator(location, kernelPath) {

  def makeExecutable() {
    writeHeader()
    writeNativeLoad()
    writeMethodHeader()
    writeNativeCall()
    writeMethodFooter()
    writeFooter()
    addSource(out.toString)
  }

  private def writeNativeLoad() {
    out.append("@native def host" + executableName(location) + ": Unit\n")
    out.append("System.load(\"\"\"")
    out.append(CppCompile.binCacheHome)
    out.append(CppCompile.target)
    out.append("Host.")
    out.append(OS.libExt)
    out.append("\"\"\")\n")
  }

  private def writeNativeCall() {
    out.append("host")
    out.append(executableName(location))
    out.append('\n')
  }
}
