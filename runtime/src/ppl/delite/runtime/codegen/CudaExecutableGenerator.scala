package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.{OPData, Targets}
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.codegen.hosts.Hosts
import ppl.delite.runtime.scheduler.PartialSchedule

trait CudaExecutableGenerator extends GPUExecutableGenerator {

  protected def target = Targets.Cuda

  override protected def writeHeader() {
    out.append("#include <cuda_runtime.h>\n") //Cuda runtime api
    out.append("#include \"cublas.h\"\n") //cublas library
    out.append("#include \"DeliteCuda.cu\"\n") //Delite-Cuda interface for DSL
    out.append("#include \"cudaProfiler.cu\"\n")
    out.append("#include \"helperFuncs.cu\"\n") //framework helper functions //TODO: belongs in sync?
    out.append(CudaCompile.headers.map(s => "#include \"" + s + "\"\n").mkString("")) //runtime-generated headers

    super.writeHeader()
    writeGlobals()
  }

  protected def writeGlobals() {
    out.append("cudaStream_t kernelStream;\n")
    out.append("cudaStream_t h2dStream;\n")
    out.append("cudaStream_t d2hStream;\n")
  }

  override protected def writeMethodHeader() {
    super.writeMethodHeader()
    writeGlobalsInitializer()
  }

  protected def writeGlobalsInitializer() { //TODO: no one calls me!!!
    out.append("cudaStreamCreate(&kernelStream);\n")
    out.append("cudaStreamCreate(&h2dStream);\n")
    out.append("cudaStreamCreate(&d2hStream);\n")
    out.append("cublasSetKernelStream(kernelStream);\n")  // set cublas to use the kernel stream
    out.append("hostInit();\n") // try to remove the overhead of the first call to hostInit (internally calls cudaHostAlloc)
  }

  protected def writeKernelCall(op: DeliteOP) {
    if (op.task == null) return //dummy op
    out.append(op.task) //kernel name
    val dims = op.getGPUMetadata(target)
    out.append("<<<") //kernel dimensions

    //grid dimensions
    out.append("dim3(" + dims.dimSizeX.func + "(")
    writeInputList(op, dims.dimSizeX)
    out.append(")," + dims.dimSizeY.func + "(")
    writeInputList(op, dims.dimSizeY)
    out.append("),1),")

    //block dimensions
    out.append("dim3(" + dims.blockSizeX.func + "(")
    writeInputList(op, dims.blockSizeX)
    out.append(")," + dims.blockSizeY.func + "(")
    writeInputList(op, dims.blockSizeY)
    out.append(")," + dims.blockSizeZ.func + "(")
    writeInputList(op, dims.blockSizeZ)
    out.append(")),")

    //dynamic shared memory (unused)
    out.append("0,")

    //stream
    out.append("kernelStream")
    out.append(">>>")

    out.append('(')
    var first = true
    for ((data,name) <- (op.getGPUMetadata(target).outputs) if data.resultType!="void") {
      if(!first) out.append(',')
      out.append('*')
      out.append(getSymGPU(name)) //first kernel inputs are OP outputs
      first = false
    }
    if (!first && (op.getInputs.length > 0 || op.getGPUMetadata(target).temps.length > 0)) out.append(',')
    writeInputs(op) //then all op inputs
    writeTemps(op) //then all op temporaries
    out.append(");\n")
  }

  override protected def addSource(source: String) { //TODO: override C++ compile
    CudaCompile.addSource(source, executableName)
  }

}

class CudaMainExecutableGenerator(val location: Int, val kernelPath: String) extends CudaExecutableGenerator {

  def executableName(location: Int) = "Executable" + location

  //TODO: FIXME!!!
  def writeMemoryAdd(sym: String) { }
  def syncObjectGenerator(syncs: ArrayBuffer[Send], host: Hosts.Value) = null
}

object CudaExecutableGenerator {
  def makeExecutable(schedule: PartialSchedule, kernelPath: String) {
    if (schedule(0).size == 0) return //empty schedule, generate nothing
    val location = schedule(0).peek.scheduledResource
    new CudaMainExecutableGenerator(location, kernelPath).makeExecutable(schedule(0))
  }
}
