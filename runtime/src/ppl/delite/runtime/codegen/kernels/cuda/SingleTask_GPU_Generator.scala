package ppl.delite.runtime.codegen.kernels.cuda

//, CudaGPUExecutableGenerator}
import ppl.delite.runtime.graph.targets.Targets
import java.io.{File, FileReader, BufferedReader}
import ppl.delite.runtime.graph.ops.DeliteOP
import java.lang.StringBuilder
import ppl.delite.runtime.codegen.{CudaExecutableGenerator, CudaCompile}

object SingleTask_GPU_Generator { //extends CudaGPUExecutableGenerator {

  def executableName = error("no executable name for singletask")

  private def kernelName(op: DeliteOP): String = op.task

  private def isPrimitiveType(scalaType: String) = Targets.isPrimitiveType(scalaType)

  def apply(op: DeliteOP) {
    val src = makeKernel(op)
    val header = makeHeader(op) + ";"
    CudaCompile.addHeader(header, kernelName(op))
    CudaCompile.addSource(src, kernelName(op))
  }

  private def makeKernel(op: DeliteOP): String = {
    val out = new StringBuilder
    val sep = File.separator

    out.append(makeHeader(op))
    out.append(" {\n")

    writePrimitiveRemap(op, out)

    val xfs = new BufferedReader(new FileReader(CudaCompile.sourceCacheHome + sep + "kernels" + sep + op.id  + "." + CudaCompile.ext))
    var line = xfs.readLine()
    while(line != null) {
      out.append(line)
      line = xfs.readLine()
    }
    xfs.close()
    out.append("}\n")

    out.toString
  }
  private def makeHeader(op: DeliteOP): String = {
    val out = new StringBuilder

    out.append("#include <cuda.h>\n")
    out.append("#include \"" + Targets.Cuda + "helperFuncs.h\"\n")

    out.append("__global__ void ")
    out.append(op.task)
    out.append('(')
    writeOutputParams(op, out)
    writeInputParams(op, out)
    out.append(')')

    out.toString
  }

  private def writePrimitiveRemap(op: DeliteOP, out: StringBuilder) {
    for ((in, sym) <- op.getInputs if isPrimitiveType(in.outputType(sym))) {
      if(in.scheduledResource==op.scheduledResource) {
        out.append(in.outputType(Targets.Cuda, sym))
        out.append(' ')
        out.append(sym)
        out.append('=')
        out.append('*')
        out.append(sym)
        out.append("_ptr;")
        out.append("\n")
      }
    }
  }

  private def writeOutputParams(op: DeliteOP, out: StringBuilder) {
    out.append(op.getGPUMetadata(Targets.Cuda).outputs.filter(o => op.outputType(Targets.Cuda,o._2)!="void").map(o => op.outputType(Targets.Cuda, o._2) + "* " + o._2).mkString(","))
    if ((op.getGPUMetadata(Targets.Cuda).outputs.filter(o => op.outputType(Targets.Cuda,o._2)!="void").size>0) && (op.getInputs.size>0)) out.append(", ")
  }

  private def writeInputParams(op: DeliteOP, out:StringBuilder) {
    var first = true

    for ((in, sym) <- op.getInputs) {
      if (!isPrimitiveType(in.outputType(sym))) {
        if (!first) out.append(", ")
        first = false
        out.append(op.inputType(Targets.Cuda,sym))
        out.append(" " + sym)
      }
      else {
        if (!first) out.append(", ")
        first = false
        if(in.scheduledResource==op.scheduledResource) {
          out.append(in.outputType(Targets.Cuda, sym))
          out.append(" *" + sym + "_ptr")
        }
        else {
          out.append(in.outputType(Targets.Cuda, sym))
          out.append(" " + sym)
        }
      }
    }
  }
}