package ppl.delite.runtime.codegen.kernels.cuda

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.{CudaGPUExecutableGenerator, CudaCompile, CudaMainGenerator}
import tools.nsc.io._
import ppl.delite.runtime.graph.ops.{OP_Executable, DeliteOP, OP_MultiLoop}

/*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

/**
 * @author Kevin J. Brown
 */

object MultiLoop_GPU_Array_Generator extends CudaGPUExecutableGenerator {

  def executableName = error("MultiLoop is not a stand-alone executable")

  //TODO: expand to multiple chunks (multiple GPUs)
  def makeChunk(op: OP_MultiLoop): OP_MultiLoop = {
    val src = makeKernel(op)
    val header = makeHeader(op)
    //CudaMainGenerator.addFunction(src)
    CudaCompile.addHeader(header, kernelName(op))
    CudaCompile.addSource(src, kernelName(op))
    op
  }

  private def makeHeader(op: OP_MultiLoop) = {
    val out = new StringBuilder
    writeLauncherHeader(out, op)
    out.append(";\n")
    out.toString
  }

  private def makeKernel(op: OP_MultiLoop) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op)
    writeFileHeader(out, op)
    makeKernels(out, op)
    writeKernelLauncher(out, op)
    out.toString
  }

  private def updateOP(op: OP_MultiLoop) {
    op.setKernelName(kernelName(op))
  }

  private def kernelName(op: OP_MultiLoop) = {
    "MultiLoop_GPU_Array_" + op.id
  }

  private def writeFileHeader(out: StringBuilder, op: OP_MultiLoop) {
    out.append("#include <cuda.h>\n")
    out.append("#include <cudpp.h>\n")
    out.append("#include \"" + op.id + ".cu\"\n")
    out.append("extern cudaStream_t kernelStream;\n")
    out.append("extern CUDPPHandle cudppHandle;\n")
  }

  private def writeLauncherHeader(out: StringBuilder, op: OP_MultiLoop) {
    out.append("void ")
    out.append(kernelName(op))
    def deref(op: DeliteOP, tp: String) = if (isPrimitiveType(op.outputType(tp))) "" else "*"
    out.append(op.getOutputs.map(o => op.outputType(Targets.Cuda, o) + "* " + o).mkString("(",", ",""))
    if (op.getOutputs.size > 0) out.append(", ")
    writeInputs(out,op,true)
    out.append(")\n")
    //out.append(op.getGPUMetadata(Targets.Cuda).temps.map(i => i._1.resultType + "* " + i._2).mkString(", ",", ",")\n"))
  }

  private def writeFooter(out: StringBuilder) {
    out.append("}\n")
  }

  private def makeKernels(out: StringBuilder, op: OP_MultiLoop) {
    //copyFunctionOutSymbolMap(op)

    //TODO: Fix DEG
    //if (op.needsCombine)
    //  error("ERROR: reductions not yet supported on GPU")

    if (op.needsPostProcess) {
      //makeTemps(out, op)
      writeConditionKernel(out, op)
      writeMapLikeKernel(out, op, true)
    }
    else {
      writeMapLikeKernel(out, op, false)
    }
  }

  private def writeKernelLauncher(out: StringBuilder, op: OP_MultiLoop) {
    writeLauncherHeader(out, op)
    out.append("{\n")
    writeSize(out, op)

    if (op.needsPostProcess) {
      makeTemps(out, op)
      writeKernelCall(out, op, "Cond")
      writeScanKernel(out, op)
      //writeCopyBackKernel(out, op)
      //writeOutAllocs(op, out)
      writeKernelCall(out, op, "Map")
    }
    else {
      //writeOutAllocs(op, out)
      writeKernelCall(out, op, "Map")
    }

    writeFooter(out)
  }

  private def writeKernelCall(out: StringBuilder, op: OP_MultiLoop, id: String) {
    out.append(kernelName(op))
    out.append(id)
    out.append("<<<") //kernel dimensions
    //grid dimensions
    out.append("dim3")
    out.append('(')
    out.append(dimSize)
    out.append(",1,1),") //Y & Z dims = 1
    //block dimensions
    out.append("dim3")
    out.append('(')
    out.append(blockSize)
    out.append(",1,1),0,")
    //stream
    out.append("kernelStream")
    out.append(">>>")

    def dimSize = "(1 +((" + op.size + "-1)/" + blockSize + "))"
    def blockSize = "1024"
    def deref(op: DeliteOP, tp: String) = if (isPrimitiveType(op.outputType(tp))) "" else "*"

    out.append('(')
    id match {
      case "Cond" =>
        out.append(op.getGPUMetadata(Targets.Cuda).outputs.filter(_._1.hasCond).map(o => "bitmap_" + o._2 + ", *" + o._2).mkString("",", ",""))
        out.append(op.getInputs.map(i => deref(i._1,i._2) + i._2).mkString(", ",", ",""))
        //out.append(op.getOutputs.filter(o => outSymbolMap(o)._3).map(o => "*" + o + "_bitmap").mkString(", ",", ",""))
      case "Map" =>
        out.append(op.getGPUMetadata(Targets.Cuda).outputs.filter(_._1.hasCond).map(o => "bitmap_" + o._2 + ", scanmap_" + o._2 + ", *" + o._2).mkString("",", ",""))
        //out.append(op.getOutputs.map(o => "*" + outSymbolMap(o)._1).mkString(", "))
        out.append(op.getInputs.map(i => deref(i._1,i._2) + i._2).mkString(", ",", ",""))
        //out.append(op.getOutputs.filter(o => outSymbolMap(o)._3).map(o => "*" + o + "_bitmap").mkString(", ",", ",""))
        //out.append(op.getOutputs.filter(o => outSymbolMap(o)._3).map(o => "*" + o + "_scanmap").mkString(", ",", ",""))
      case _ => error(id + " is not a known kernel type")
    }
    out.append(");\n")
  }

  private var outSymbolMap = Map[String,(String,String,Boolean)]()

  private def writeKernelHeader(out: StringBuilder, op: OP_MultiLoop, id: String) {
    out.append("__global__ void ")
    out.append(kernelName(op))
    out.append(id)
    out.append('(')

    id match {
      case "Cond" =>
        out.append(op.getGPUMetadata(Targets.Cuda).outputs.filter(_._1.hasCond).map(o => "unsigned int *" + "bitmap_" + o._2 + "," + op.outputType(Targets.Cuda,o._2) + " " + o._2).mkString("",", ",""))

        if (op.getInputs.nonEmpty) out.append(",")
        writeInputs(out,op,false)
      case "Map" =>
        out.append(op.getGPUMetadata(Targets.Cuda).outputs.filter(_._1.hasCond).map(o => "unsigned int * bitmap_" + o._2 + ", unsigned int * scanmap_" + o._2 + ", " + op.outputType(Targets.Cuda,o._2) + " " + o._2).mkString("",", ",""))
        //out.append(op.getOutputs.map(o => outSymbolMap(o)._2 + " " + outSymbolMap(o)._1).mkString("",", ",", "))
        if (op.getInputs.nonEmpty) out.append(",")
        writeInputs(out,op,false)
        //out.append(op.getOutputs.filter(o => outSymbolMap(o)._3).map(o => "unsigned int* " + o + "_bitmap").mkString(", ",", ",""))
        //out.append(op.getOutputs.filter(o => outSymbolMap(o)._3).map(o => "unsigned int* " + o + "_scanmap").mkString(", ",", ",""))
      case _ => error(id + " is not a known kernel type")
    }

    out.append(") {\n")
    out.append("int idxX = blockIdx.x * blockDim.x + threadIdx.x;\n")
    out.append("if (idxX < " + op.size + ") {\n")
  }

  private def writeKernelFooter(out: StringBuilder) {
    out.append("}\n}\n") //end if, end kernel
  }

  private def parseFile(op: OP_MultiLoop, marker: String, process: String => Unit) {
    val file = File(Path(CudaCompile.sourceCacheHome + "kernels" + File.separator + op.id + "." + CudaCompile.ext))
    val c = file.chars()
    val endMarker = "#END"
    var copy = false
    for (line <- c.getLines) {
      if (line == endMarker) copy = false
      if (copy) process(line)
      if (line == marker) copy = true
    }
    c.close()
  }

  private def copyFunction(out: StringBuilder, op: OP_MultiLoop, marker: String) {
    val process: String => Unit = line => {
      out.append(line)
      out.append('\n')
    }
    parseFile(op, marker, process)
  }

  private def copyFunctionOutSymbolMap(op: OP_MultiLoop) {
    val marker = "#MAPPING"
    val process: String => Unit = line => {
      val str = line.split(":")
      outSymbolMap = outSymbolMap + (str(0)->(str(1),str(2),str(3)=="true"))
    }
    parseFile(op, marker, process)
  }

  private def writeMapLikeKernel(out: StringBuilder, op: OP_MultiLoop, needsPostProcess: Boolean) {
    writeKernelHeader(out, op, "Map")
    out.append("//map kernel code\n")
    for((odata,osym) <- op.getGPUMetadata(Targets.Cuda).outputs) {
      if (odata.hasCond) {
        out.append(odata.loopFuncOutputType + " collect_" + osym + " = dev_collect_" + osym + "(" + odata.loopFuncInputs.mkString("",",",",") + "idxX);\n")
        out.append("if(bitmap_" + osym + "[idxX] == 1) " + osym + ".dcUpdate(scanmap_" + osym + "[idxX], collect_" + osym + ");\n")
      }
      else {
        out.append(odata.loopFuncOutputType + " collect_" + osym + " = dev_collect_" + osym + "(" + odata.loopFuncInputs.mkString("",",",",") + "idxX);\n")
        out.append(osym + ".dcUpdate(scanmap_" + osym + "[idxX]], collect_" + osym + ");\n")
      }
    }

    //copyFunction(out, op, "#MAP")
    writeKernelFooter(out)
  }

  private def writeConditionKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "Cond")
    out.append("//condition kernel code\n")
    for((odata,osym) <- op.getGPUMetadata(Targets.Cuda).outputs if (odata.hasCond)) {
      out.append("bool cond_" + osym + " = dev_cond_" + osym + "(" + odata.loopCondInputs.mkString("",",",",") + "idxX);\n")
      out.append("if(cond_" + osym + " == 1) bitmap_" + osym + "[idxX] = 1;\n")
      out.append("else bitmap_" + osym + "[idxX] = 0;\n")
    }
    //copyFunction(out, op, "#COND")
    writeKernelFooter(out)
  }

  private def writeSize(out: StringBuilder, op: OP_MultiLoop) {
    copyFunction(out, op, "#SIZE")
  }

  /*
  private def writeOutAllocs(op: OP_MultiLoop, out: StringBuilder) {
    for ((data,name) <- op.getGPUMetadata(target).outputs) {
      //Parse the file
      copyFunction(out,op,"#ALLOC_" + name)
      writeMemoryAdd(name, out)
    }
  }
  */

  private def writeCopyBackKernel(out: StringBuilder, op: OP_MultiLoop) {
    for (output <- op.getOutputs if (outSymbolMap(output)._3)) {
      out.append("int *%s_size_ptr;\n".format(output))
      out.append("DeliteCudaMallocHost((void**)&%s_size_ptr,sizeof(int));\n".format(output))
      out.append("DeliteCudaMemcpyDtoHAsync((void*)%s_size_ptr,*%s_scanmap+%s_size-1,sizeof(int));\n".format(output,output,op.id))
      out.append("int %s_size = *(%s_size_ptr);\n".format(output,output))
    }
  }

  private def writeScanKernel(out: StringBuilder, op: OP_MultiLoop) {
    //exclusive scan
    out.append("CUDPPConfiguration config;\n")
    out.append("config.algorithm = CUDPP_SCAN;\n")
    out.append("config.op = CUDPP_ADD;\n")
    out.append("config.datatype = CUDPP_UINT;\n")
    out.append("config.options = CUDPP_OPTION_FORWARD | CUDPP_OPTION_EXCLUSIVE;\n")
    out.append("CUDPPHandle plan;\n")
    out.append("cudppPlan(cudppHandle, &plan, config, " + op.size + ", 1, 0);\n")
    for ((odata,osym) <- op.getGPUMetadata(Targets.Cuda).outputs if (odata.hasCond)) {
      out.append("cudppScan(plan, " + "scanmap_" + osym + ", bitmap_" + osym + ", " + op.size + ");\n")
    }
    out.append("cudppDestroyPlan(plan);\n")
  }

  private def makeTemps(out: StringBuilder, op: OP_MultiLoop) {
    allocateMaps(out, op) //TODO: read per-output tag in DEG to find out how many bitmaps are needed
    //addOpTemps(op)
  }

  private def allocateMaps(out: StringBuilder, op: OP_MultiLoop) {
    for (name <- List("bitmap_", "scanmap_")) {
      for ((odata,osym) <- op.getGPUMetadata(Targets.Cuda).outputs if(odata.hasCond)) {
        //out.append("unsigned int * " + name + osym + " = (unsigned int *) malloc(sizeof(unsigned int *));\n")
        out.append("unsigned int * " + name + osym + ";\n")
        out.append("DeliteCudaMalloc((void**)&" + name + osym + ", " + op.size + "*sizeof(unsigned int));\n")
      }
    }
  }

  private def writeInputs(out: StringBuilder, op: OP_MultiLoop, reference: Boolean) {
    var first = true
    val metadata = op.getGPUMetadata(target)

    for ((in, sym) <- op.getInputs) {
      if (metadata.inputs.contains((in,sym))) {
        if (!first) out.append(", ")
        first = false
        out.append(metadata.inputs((in,sym)).resultType)
        if (reference) out.append("*")
        out.append(" " + sym)
      }
      else if (isPrimitiveType(in.outputType(sym))) {
        if (!first) out.append(", ")
        first = false
        out.append(getCPrimitiveType(in.outputType(sym)))
        out.append(" " + sym)
      }
    }
  }

  private def addOpTemps(op: OP_MultiLoop) {
    for (name <- List("_bitmap", "_scanmap")) {
      for (output <- op.getOutputs if (outSymbolMap(output)._3)) {
        val temp = op.getGPUMetadata(Targets.Cuda).newTemp(output + name)
        temp.func = output + name
        temp.resultType = "unsigned int*"
        temp.inputs = op.getInputs.toList
      }
    }
  }

  override def getSymGPU(name: String) = name

}
