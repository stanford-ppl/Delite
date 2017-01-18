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
import java.io.{File, PrintWriter}

/**
 * Chisel code generator logic to generate function calls
 */
trait ChiselExecutableGenerator extends ExecutableGenerator {
  import CppResourceInfo._

  def deviceTarget = Targets.Chisel
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
   * Add a source to the companion compilation object (ChiselCompile)
   */
  protected def addSource(source: String) {
    ChiselCompile.addSource(source, executableName)
  }

  protected[codegen] def writeHeader() {
    hostGenerator.writeHeader()
    out.append("#include \"interface.h\"\n")
    out.append("#include <stdint.h>\n")
    out.append("#include <sys/time.h>\n")
    out.append(s"""#include <iostream>
#include <fstream>
#include <string> 
#include <sstream> 
#include <signal.h>
#include <sys/wait.h>
#include <pwd.h>

void Top_run( Interface_t *args )
{
  // int numInputs = sizeof(args->ArgIns) / sizeof(args->ArgIns[0]);
  // std::string argString = "";
  // for (int i=0; i < numInputs; i++) {
  //   argString += " ";
  //   std::ostringstream ss;
  //   ss << *args->ArgIns[i];
  //   argString += ss.str();
  // }

  // TODO: Figure out how to get Makefile to compile verilator in when we make bitstream-cpu
  // TODO: Figure out why verilator runs 1000x slower when launched from syscall
  // // std::string cmdStr = "sbt \"test:run-main app.Launcher TopModule " + argString + "\"";
  // // const char * cmd = cmdStr.c_str();
  // std::string cmdStr = "test:run-main app.Launcher TopModule " + argString;
  // char * cmd = (char*)cmdStr.c_str();
  // char *argv[] = { "sbt", cmd, 0 };

  // // Make fork with timeout
  // int timeout_time=40;
  // pid_t intermediate_pid = fork();
  // if (intermediate_pid == 0) {
  //   pid_t worker_pid = fork();
  //   if (worker_pid == 0) {
  //     execvp(argv[0], argv);
  //     printf("Simulation success!\\n");
  //       _exit(0);
  //   }

  //   pid_t timeout_pid = fork();
  //   if (timeout_pid == 0) {
  //       sleep(timeout_time);
  //       printf("============================\\n");
  //       printf("ERROR: Simulation timeout!!\\n");
  //       printf("============================\\n");
  //       _exit(0);
  //   }

  //   pid_t exited_pid = wait(NULL);
  //   if (exited_pid == worker_pid) {
  //       kill(timeout_pid, SIGKILL);
  //   } else {
  //       kill(worker_pid, SIGKILL); // Or something less violent if you prefer
  //   }
  //   wait(NULL); // Collect the other process
  //   _exit(0); // Or some more informative status
  // }
  // waitpid(intermediate_pid, 0, 0);

  // system(cmd);

  uid_t uid = geteuid();
  struct passwd *pw = getpwuid (uid);
  std::ostringstream stringStream;
  stringStream << "/tmp/chisel_test_result_" << pw->pw_name;
  std::string fname = stringStream.str();
  std::ifstream result_file;
  result_file.open( fname.c_str() );
  int32_t result;
  uint64_t cycles;
  result_file >> result >> cycles;
  *args->ArgOuts[0] = result;
  *args->cycles = cycles;

}
""")

    // out.append(s"""max_engine_t *engine = 0;""")
  }

  protected[codegen] def declareGlobals() {
  }

  protected[codegen] def initializeGlobals() {
  }

  protected def writeMethodHeader() {
    out.append("// Chisel writeMethodHeader();\n")
    hostGenerator.writeMethodHeader()
    declareGlobals()
    initializeGlobals()
    // out.append(s"""// Load max file from disk -> DRAM\n""")
    // out.append(s"""max_file_t *maxfile = Top_init();\n""")
    out.append(s"""// Configure the FPGA\n""")
    // out.append(s"""engine = max_load(maxfile, "local:*");\n""")
    // out.append(s"""int burstSize = max_get_burst_size(maxfile, NULL);\n""")
    // out.append(s"""printf("Burst size for architecture: %d bytes\\n", burstSize);\n""")
  }

  protected def writeMethodFooter() {
    out.append("// Chisel writeMethodFooter();\n")
    // out.append(s"""max_unload(engine);\n""")
    hostGenerator.writeMethodFooter()
  }

  protected def writeFooter() {
    hostGenerator.writeFooter()
  }

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
      out.append(s"""// ChiselExecutableGenerator::makeNestedFunction($op): Cannot generate\n""")
      println(s"""[ChiselExecutableGenerator::makeNestedFunction() Cannot generate '$op')""")
  }

  /**
   * Delite assumes that any type that isn't a primitive type should be
   * a pointer type. Hence, all registers are seen as pointers.
   * For ArgIns, we want to dereference the value of the pointer and
   * do pass-by-value
   */
  private def deref(o: DeliteOP, s: String): String = {
    o.irnode match {
      case "Argin_new" => "*"
      case _ => ""
    }
  }

  protected[codegen] def writeFunctionCall(op: DeliteOP) {
    if (op.scheduledOn(Targets.Cpp) && !op.isInstanceOf[OP_Nested]) {
      hostGenerator.writeFunctionCall(op)
    } else {
      if (op.task == null) return //dummy op

      out.append(s"""// Chisel writeFunctionCall($op) {\n""")
      out.append(s"""// Inputs($op) = ${op.getInputs.map {op => (op._1.id, op._1.irnode, op._1.opName)}}\n""")
      for (i <- op.getInputs)
        available += i
      for (o <- op.getOutputs if op.outputType(o)!="Unit")
        available += Pair(op,o)

      op match {
        case _:OP_Single =>
          assert(op.irnode == "Hwblock")
          assert(op.getOutputs.filter(o=>op.outputType(o)!="Unit").isEmpty)
          out.append(s"""struct timeval t1, t2;\n""")
          out.append(s"""uint64_t Top_cycles = 0;\n""")
          out.append(s"""Interface_t interface;\n""")
          val interfaceOuts = op.getInputs.filter { t =>
            t._1.irnode == "Argout_new"
          }.map { t =>
            val inop = t._1
            val typecastStr = inop.irnode match {
              //legacy thing from maxj
              case "Argout_new" => inop.outputType(hostTarget) match {
                case s if s.contains("int") => "(int32_t*)"//"(uint64_t*)"
                case s if s.contains("float") => ""//"(double*)"
                case _ => ""
              }
              case _ => ""
            }
            s"""$typecastStr ${deref(t._1,t._2) + getSymHost(t._1,t._2)}"""
          }
          val interfaceIns = op.getInputs.filter { t =>
            t._1.irnode == "Argin_new"
          }.map { t =>
            val inop = t._1
            // s"""${deref(t._1,t._2) + getSymHost(t._1,t._2)}"""
            s"""${getSymHost(t._1,t._2)}"""
          }
          val interfaceMem = List() // TODO: Register mem interfaces too
          out.append(s"""interface.cycles = &Top_cycles;\n""")
          interfaceOuts.zipWithIndex.foreach{ case(a,i) => out.append(s"""interface.ArgOuts[$i] = $a;\n""")}
          interfaceIns.zipWithIndex.foreach{ case(a,i) => out.append(s"""interface.ArgIns[$i] = $a;\n""")}
          out.append(s"""gettimeofday(&t1, 0);\n""")
          out.append(s"""Top_run(&interface); // ${op.task}(engine, &interface);\n""")
          out.append(s"""gettimeofday(&t2, 0);\n""")
          out.append(s"""double elapsed = (t2.tv_sec-t1.tv_sec)*1000000 + t2.tv_usec-t1.tv_usec;\n""")
          out.append(s"""fprintf(stderr, "Kernel done, test run time = %lf\\n", elapsed/1000000);\n""")
          out.append(s"""fprintf(stderr, "Kernel done, hw cycles = %lu \\n", Top_cycles);\n""")
        case _ =>
          sys.error(s"""ERROR: Unsupported op '$op' for Chisel writeFunctionCall()""")
      }
      out.append(s"""// Chisel writeFunctionCall($op) }\n""")
    }
  }

  protected def writeOutputAlloc(op: DeliteOP) {
    out.append(s"""// Chisel writeOutputAlloc($op)\n""")
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
class ChiselMainExecutableGenerator(val location: Int, val graph: DeliteTaskGraph)
  extends ChiselExecutableGenerator with ChiselSyncGenerator {

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

object ChiselExecutableGenerator {
  val syncObjects = ArrayBuffer[String]()

  def makeExecutables(schedule: PartialSchedule, graph: DeliteTaskGraph) {
    for (sch <- schedule if sch.size > 0) {
      val location = sch.resourceID
      new ChiselMainExecutableGenerator(location, graph).makeExecutable(sch) // native execution plan
    }
  }

  def clear() {
    syncObjects.clear
  }
}
