package ppl.delite.runtime.codegen

import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph._
import ppl.delite.runtime.scheduler.{OpList, PartialSchedule}
import ppl.delite.runtime.{Config,Delite}
import ppl.delite.runtime.graph.targets.{OS, Targets}
import collection.mutable.{ArrayBuffer, HashSet}
import sync._
import ppl.delite.runtime.graph.DeliteTaskGraph

/**
 * MaxJ code generator logic to generate function calls
 */
trait MaxJExecutableGenerator extends ExecutableGenerator {
  import CppResourceInfo._

  def deviceTarget = Targets.MaxJ
  def hostTarget = Targets.getHostTarget(deviceTarget)

  protected val getterList: ArrayBuffer[String] = ArrayBuffer()
  protected val available: ArrayBuffer[(DeliteOP,String)] = ArrayBuffer()

  /**
   * Code generator to generate non-MaxJ host code.
   * MaxJ and host (Cpp) schedules are merged prior to code generation;
   * see "Compilers::compileSchedule()".
   * Nodes scheduled on the host (Cpp) will be generated using hostGenerator.
   */
  protected val hostGenerator: CppExecutableGenerator

  /**
   * Add a source to the companion compilation object (MaxJCompile)
   */
  protected def addSource(source: String) {
    MaxJCompile.addSource(source, executableName)
  }

  protected[codegen] def writeHeader() {
    hostGenerator.writeHeader()
    out.append("#include <stdint.h>\n")
    out.append("#include <sys/time.h>\n")
    out.append("#include <Maxfiles.h>\n")
    out.append("#include <MaxSLiCInterface.h>\n")
  }

  protected[codegen] def declareGlobals() {
  }

  protected[codegen] def initializeGlobals() {
  }

  protected def writeMethodHeader() {
    out.append("// MaxJ writeMethodHeader();\n")
    hostGenerator.writeMethodHeader()
    declareGlobals()
    initializeGlobals()
    out.append(s"""// Load max file from disk -> DRAM\n""")
    out.append(s"""max_file_t *maxfile = Top_init();\n""")
    out.append(s"""// Configure the FPGA\n""")
    out.append(s"""max_engine_t *engine = max_load(maxfile, "local:*");\n""")
    out.append(s"""int burstSize = max_get_burst_size(maxfile, NULL);\n""")
    out.append(s"""printf("Burst size for architecture: %d bytes\n", burstSize);\n""")
  }

  protected def writeMethodFooter() {
    out.append("// MaxJ writeMethodFooter();\n")
    out.append(s"""max_unload(engine);\n""")
    hostGenerator.writeMethodFooter()  // <-- This generates the main method
  }

  protected def writeFooter() { }

  /**
   * [COMMENT TODO] What does this method do?
   * MaxJ can only schedule SingleTasks with opName "Accel"
   * Doing nothing in this method now. However, it looks like
   * the 'makeNestedFunction' method in the sync generator is
   * responsible for sync node codegen. Some clarity is required
   *
   */
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case _ =>
      out.append(s"""// MaxJExecutableGenerator::makeNestedFunction($op): Cannot generate\n""")
      println(s"""[MaxJExecutableGenerator::makeNestedFunction() Cannot generate '$op')""")
  }

  private def deref(o: DeliteOP, s: String): String = {
    o.opName match {
      case "ArgIn" => "*"
      case _ => ""
    }
  }

  protected[codegen] def writeFunctionCall(op: DeliteOP) {
    if (op.scheduledOn(Targets.Cpp) && !op.isInstanceOf[OP_Nested]) {
      hostGenerator.writeFunctionCall(op)
    } else {
      if (op.task == null) return //dummy op

      out.append(s"""// MaxJ writeFunctionCall($op) {\n""")
      out.append(s"""// Inputs($op) = ${op.getInputs.map {op => (op._1.id, op._1.irnode, op._1.opName)}}\n""")
      for (i <- op.getInputs)
        available += i
      for (o <- op.getOutputs if op.outputType(o)!="Unit")
        available += Pair(op,o)

      op match {
        case _:OP_Single =>
          assert(op.irnode == "Hwblock")
          assert(op.getOutputs.filter(o=>op.outputType(o)!="Unit").isEmpty)
          out.append(s"""Top_actions_t runAct;\n""")
          op.getInputs.filter { t => t._1.irnode == "Reg_new" }.map { t =>
            val inop = t._1
            val prefix = inop.opName match {
              case "ArgIn" => "param"
              case "ArgOut" => "outscalar_TopKernel"
            }
            out.append(s"""runAct.${prefix}_${inop.id} = ${deref(t._1,t._2) + getSymDevice(t._1,t._2)};\n""")
          }
          out.append(s"""gettimeofday(&t1, 0);\n""")
          out.append(s"""${op.task}(engine, &runAct);\n""")
          out.append(s"""gettimeofday(&t2, 0);\n""")
          out.append(s"""double elapsed = (t2.tv_sec-t1.tv_sec)*1000000 + t2.tv_usec-t1.tv_usec;\n""")
          out.append(s"""fprintf(stderr, "Kernel done, elapsed time = %lf\n", elapsed/1000000);\n""")
        case _ =>
          sys.error(s"""ERROR: Unsupported op '$op' for MaxJ writeFunctionCall()""")
      }
      out.append(s"""// MaxJ writeFunctionCall($op) }\n""")
    }
  }

  protected def writeOutputAlloc(op: DeliteOP) {
    out.append(s"""// MaxJ writeOutputAlloc($op)\n""")
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
    "xF"+name
  }

  override protected[codegen] def writeSyncObject() {  }

  protected def isPrimitiveType(scalaType: String) = Targets.isPrimitiveType(scalaType)

  protected def generateInputArgs(op: DeliteOP): String = {
    op match {
      case _ =>
        op.getInputs.map(i => getSymDevice(i._1,i._2)).mkString(",")
    }
  }
}

/**
 * MaxJ code generator class that includes function call and sync node
 * generation. This class is instantiated by callers that need to generate
 * MaxJ.
 * @param location: Resource ID assigned to the MaxJ target
 * @param graph: Parsed DEG with mutable schedule information
 */
class MaxJMainExecutableGenerator(val location: Int, val graph: DeliteTaskGraph)
  extends MaxJExecutableGenerator with MaxJSyncGenerator {

  protected val hostGenerator: CppExecutableGenerator = new CppMainExecutableGenerator(Targets.resourceIDs(Targets.Cpp).head, graph)
  hostGenerator.out = out

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

object MaxJExecutableGenerator {
  val syncObjects = ArrayBuffer[String]()

  def makeExecutables(schedule: PartialSchedule, graph: DeliteTaskGraph) {
    for (sch <- schedule if sch.size > 0) {
      val location = sch.resourceID
      new MaxJMainExecutableGenerator(location, graph).makeExecutable(sch) // native execution plan
    }
  }

  def clear() {
    syncObjects.clear
  }
}
