package ppl.delite.runtime.codegen.kernels.cuda

import ppl.delite.runtime.codegen.{CudaGPUExecutableGenerator, CudaCompile, CudaMainGenerator}
import tools.nsc.io._
import ppl.delite.runtime.graph.ops.{OP_Executable, DeliteOP, OP_MultiLoop}
import ppl.delite.runtime.graph.targets.{OPData, Targets}
import collection.mutable.ArrayBuffer

object MultiLoop_GPU_Array_Generator extends CudaGPUExecutableGenerator {

  def executableName = error("MultiLoop is not a stand-alone executable")

  private def needsReduction(op: OP_MultiLoop): Boolean = {
    if (reductionList(op).nonEmpty) true
    else false
  }
  private def needsReductionTuple(op: OP_MultiLoop): Boolean = {
    if (reductionTupleList(op).nonEmpty) true
    else false
  }
  private def needsCondition(op: OP_MultiLoop): Boolean = {
    if (conditionList(op).nonEmpty) true
    else false
  }

  private def reductionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="REDUCE")
  }
  private def reductionTupleList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="REDUCE_TUPLE")
  }
  private def conditionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.hasCond)
  }

  private def needDeref(op:OP_MultiLoop, in: DeliteOP, sym:String): Boolean = {
    //println("in: " + in.id + "," + in.scheduledResource)
    //println("op: " + op.id + "," + op.scheduledResource)
    isPrimitiveType(in.outputType(sym)) && (in.scheduledResource==op.scheduledResource)
  }

  private def addDeref(out: StringBuilder, op: OP_MultiLoop) = {
    for ((in,sym) <- op.getInputs) if (needDeref(op, in, sym)) {
      out.append(getCPrimitiveType(in.outputType(sym)))
      out.append(' ')
      out.append(sym)
      out.append(" = *")
      out.append(sym)
      out.append("_ptr;\n")
    }
  }

  //TODO: expand to multiple chunks (multiple GPUs)
  def makeChunk(op: OP_MultiLoop): OP_MultiLoop = {
    updateOP(op)
    val src = makeKernel(op)
    val header = makeHeader(op)
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
    out.append("#include <assert.h>\n")
    out.append("#include <thrust/scan.h>\n")
    out.append("#include \"helperFuncs.h\"\n")
    out.append("#include \"" + op.id + ".cu\"\n")
    out.append("extern cudaStream_t kernelStream;\n")
  }

  private def writeLauncherHeader(out: StringBuilder, op: OP_MultiLoop) {
    out.append("void ")
    out.append(kernelName(op))
    out.append(op.getOutputs.filter(o => op.outputType(Targets.Cuda,o)!="void").map(o => op.outputType(Targets.Cuda, o) + "** " + o).mkString("(",", ",""))
    if ((op.getOutputs.filter(o => op.outputType(Targets.Cuda,o)!="void").size>0) && (op.getInputs.size>0)) out.append(", ")
    writeInputs(out,op,true)
    out.append(")\n")
  }

  private def writeFooter(out: StringBuilder) {
    out.append("}\n")
  }

  private def makeKernels(out: StringBuilder, op: OP_MultiLoop) {
    if(needsCondition(op)) writeConditionKernel(out, op)
    writeMapReduceKernel(out, op)
    if(needsReduction(op)) writeReduceKernel(out,op)
    if(needsReductionTuple(op)) writeReduceTupleKernel(out,op)
  }

  private def writeKernelLauncher(out: StringBuilder, op: OP_MultiLoop) {
    writeLauncherHeader(out, op)
    out.append("{\n")

    makeTemps(out, op)
    if(needsCondition(op)) {
      writeKernelCall(out, op, "Cond")
      writeScanKernel(out, op)
      writeCopyBackKernel(out, op)
    }

    writeOutputAllocs(op, out)
    writeKernelCall(out, op, "MapReduce")

    if(needsReduction(op)) {
      writeKernelCall(out, op, "Reduce")
    }
    if(needsReductionTuple(op)) {
      writeKernelCall(out, op, "ReduceTuple")
    }
    writeCopyBackReduceKernel(out, op)

    writeFooter(out)
  }

  private def writeKernelCall(out: StringBuilder, op: OP_MultiLoop, id: String) {
    def dimSize = "(1 +((" + op.size + "-1)/" + blockSize + "))"
    def blockSize = "1024"
    def deref(op: DeliteOP, tp: String) = if (isPrimitiveType(op.outputType(tp))) "" else "*"
    def cudaLaunch(dimConfig: String): String = {
      val str = new StringBuilder
      str.append(kernelName(op))
      str.append(id)
      str.append("<<<") //kernel dimensions
      str.append("dim3")
      str.append('(')
      str.append(dimConfig)
      str.append(",dim3")
      str.append('(')
      str.append(blockSize)
      str.append(",1,1),0,")
      str.append("kernelStream")
      str.append(">>>")
      str.toString
    }

    id match {
      case "Cond" =>
        out.append("if(" + dimSize +" > 65536) { printf(\"Grid size for GPU is too large!\\n\"); assert(false); }\n")
        out.append(cudaLaunch(dimSize + ",1,1)"))
        //val args = op.getOutputs.filter(o => !isPrimitiveType(op.outputType(o))).map(o => "**" + o).toList ++ conditionList(op).map(o => "bitmap_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else ""))
        val args = conditionList(op).map(o => "bitmap_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else ""))
        out.append(args.mkString("(",",",");\n"))
      case "MapReduce" =>
        out.append("if(" + dimSize +" > 65536) { printf(\"Grid size for GPU is too large!\\n\"); assert(false); }\n")
        out.append(cudaLaunch(dimSize + ",1,1)"))
        val args = op.getOutputs.filter(o => !isPrimitiveType(op.outputType(o))).map(o => "**" + o).toList ++ conditionList(op).map(o => "bitmap_" + o._2 + ", scanmap_" + o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => "temp_1_" + o._2 + ", temp_1_" + o._2 + "_2, temp_2_" + o._2 + ", temp_2_" + o._2 + "_2") ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else ""))
        out.append(args.mkString("(",",",");\n"))
      case "Reduce" | "ReduceTuple" =>
        out.append("int num_blocks_" + id + " = " + dimSize + ";\n")
        out.append("while(num_blocks_" + id + " != 1) {\n")
        out.append(cudaLaunch("1+(num_blocks_" + id + "-1)/1024,1,1)"))
        val args = op.getOutputs.filter(o => !isPrimitiveType(op.outputType(o))).map(o => "**" + o).toList ++ conditionList(op).map(o => "bitmap_" + o._2 + ", scanmap_" + o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => "temp_1_" + o._2 + ", temp_1_" + o._2 + "_2, temp_2_" + o._2 + ", temp_2_" + o._2 + "_2") ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ List("num_blocks_"+id)
        out.append(args.mkString("(",",",");\n"))
        out.append("num_blocks_" + id + " = 1 + (num_blocks_" + id + "-1)/1024;\n")
        for((odata,osym) <- reductionList(op)) {
          out.append(odata.loopFuncOutputType + " *temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append(odata.loopFuncOutputType + " *temp_1_" + osym + "_t = temp_1_" + osym + ";\n")
          out.append("temp_1_" + osym + " = temp_1_" + osym + "_2;\n")
          out.append("temp_1_" + osym + "_2 = temp_1_" + osym + "_t;\n")
          out.append(odata.loopFuncOutputType_2 + " *temp_2_" + osym + "_t = temp_2_" + osym + ";\n")
          out.append("temp_2_" + osym + " = temp_2_" + osym + "_2;\n")
          out.append("temp_2_" + osym + "_2 = temp_2_" + osym + "_t;\n")
        }
        out.append("}\n")

      case _ => error(id + " is not a known kernel type")
    }
  }

  private def writeKernelHeader(out: StringBuilder, op: OP_MultiLoop, id: String) {
    out.append("__global__ void ")
    out.append(kernelName(op))
    out.append(id)
    out.append('(')

    id match {
      case "Cond" =>
        //val params = op.getOutputs.filter(o => !isPrimitiveType(op.outputType(o))).map(o => op.outputType(Targets.Cuda, o) + " " + o) ++ conditionList(op).map(o => "unsigned int * bitmap_" + o._2)
        val params = conditionList(op).map(o => "unsigned int * bitmap_" + o._2)
        out.append(params.mkString(","))
        if (params.nonEmpty && op.getInputs.nonEmpty) out.append(',')
        writeInputs(out,op,false)
      case "MapReduce" | "Reduce" | "ReduceTuple" =>
        val params = op.getOutputs.filter(o => !isPrimitiveType(op.outputType(o))).map(o => op.outputType(Targets.Cuda, o) + " " + o) ++ conditionList(op).map(o => "unsigned int * bitmap_" + o._2 + ", unsigned int * scanmap_" + o._2) ++ reductionList(op).map(o => o._1.loopFuncOutputType + " *temp_" + o._2 + "," + o._1.loopFuncOutputType + " *temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => o._1.loopFuncOutputType + " *temp_1_" + o._2 + "," + o._1.loopFuncOutputType + " *temp_1_" + o._2 + "_2," + o._1.loopFuncOutputType_2 + " *temp_2_" + o._2 + "," + o._1.loopFuncOutputType_2 + " *temp_2_" + o._2 + "_2")
        out.append(params.mkString(","))
        if (params.nonEmpty && op.getInputs.nonEmpty) out.append(',')
        writeInputs(out,op,false)
        if((id=="Reduce") || (id=="ReduceTuple")) {
          out.append(", int size")
        }
      case _ => error(id + " is not a known kernel type")
    }

    out.append(") {\n")
    out.append("int idxX = blockIdx.x * blockDim.x + threadIdx.x;\n")
    addDeref(out, op)
    allocateSharedMem(out, op)
  }

  /* Allocate shared memory for reduction operation */
  private def allocateSharedMem(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op))
      out.append("__shared__ " + odata.loopFuncOutputType + " smem_" + osym + "[1024];\n")
    for((odata,osym) <- reductionTupleList(op)) {
      out.append("__shared__ " + odata.loopFuncOutputType + " smem_1_" + osym + "[1024];\n")
      out.append("__shared__ " + odata.loopFuncOutputType_2 + " smem_2_" + osym + "[1024];\n")
    }
  }

  private def writeKernelFooter(out: StringBuilder) {
    out.append("}\n") //end if, end kernel
  }

  private def funcNameSuffix(op: OP_MultiLoop, sym: String):String = {
    op.id + "_" + sym
  }

  private def writeConditionKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "Cond")
    out.append("if (idxX < " + op.size + ") {\n")
    for((odata,osym) <- conditionList(op)) {
      out.append("bool cond_" + osym + " = dev_cond_" + funcNameSuffix(op,osym) + "(" + (odata.loopCondInputs:+"idxX").mkString(",") + ");\n")
      out.append("if(cond_" + osym + " == 1) bitmap_" + osym + "[idxX] = 1;\n")
      out.append("else bitmap_" + osym + "[idxX] = 0;\n")
    }
    out.append("}\n")
    writeKernelFooter(out)
  }

  private def writeMapReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "MapReduce")
    for((odata,osym) <- op.getGPUMetadata(Targets.Cuda).outputs) {
      odata.loopType match {
        case "COLLECT" =>
          if (odata.hasCond) out.append("if (idxX<" + op.size + " && bitmap_" + osym + "[idxX]==1) {\n")
          else out.append("if (idxX < " + op.size + ") {\n")
          out.append(odata.loopFuncOutputType + " collect_" + osym + " = dev_collect_" + funcNameSuffix(op,osym) + "(" + (odata.loopFuncInputs:+"idxX").mkString(",") + ");\n")
          if(odata.hasCond) out.append(osym + ".dcUpdate(scanmap_" + osym + "[idxX], collect_" + osym + ");\n")
          else out.append(osym + ".dcUpdate(idxX, collect_" + osym + ");\n")
          out.append("}\n")
        case "FOREACH" =>
          out.append("if (idxX < " + op.size + ") {\n")
          out.append("dev_foreach_" + funcNameSuffix(op,osym) + "(" + odata.loopFuncInputs.mkString("",",",",") + "idxX);\n")
          out.append("}\n")
        case "REDUCE" =>
          if (odata.hasCond) out.append("smem_" + osym + "[threadIdx.x] = ((idxX<" + op.size + ") && (bitmap_" + osym + "[idxX]==1)) ? dev_collect_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") +  ": dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
          else out.append("smem_" + osym + "[threadIdx.x] = (idxX < " + op.size + ") ? dev_collect_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") +  ": dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
          out.append("__syncthreads();\n")
          out.append("for(unsigned int s=1; s<blockDim.x; s*=2) {\n")
          out.append("if((idxX%(2*s))==0) { \n")
          out.append("smem_" + osym + "[threadIdx.x] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+s]","idxX")).mkString("(",",",");\n"))
          out.append("}\n")
          out.append("__syncthreads();\n")
          out.append("}\n")
          out.append("if(threadIdx.x==0) temp_" + osym + "[blockIdx.x] = smem_" + osym + "[0];\n")
        case "REDUCE_TUPLE" =>
          if (odata.hasCond) {
            out.append("smem_1_" + osym + "[threadIdx.x] = ((idxX<" + op.size + ") && (bitmap_" + osym + "[idxX]==1)) ? dev_collect_1_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") + " : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
            out.append("smem_2_" + osym + "[threadIdx.x] = ((idxX<" + op.size + ") && (bitmap_" + osym + "[idxX]==1)) ? dev_collect_2_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs_2:+"idxX").mkString("(",",",")") + " : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
          }
          else {
            out.append("smem_1_" + osym + "[threadIdx.x] = (idxX < " + op.size + ") ? dev_collect_1_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") + " : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
            out.append("smem_2_" + osym + "[threadIdx.x] = (idxX < " + op.size + ") ? dev_collect_2_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs_2:+"idxX").mkString("(",",",")") + " : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
          }
          out.append("if(idxX<" + op.size + ") smem_1_" + osym + "[threadIdx.x] = dev_reduce_seq_1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs ++ List("dev_zero_1_"+funcNameSuffix(op,osym)+odata.loopZeroInputs.mkString("(",",",")"),"dev_zero_2_"+funcNameSuffix(op,osym)+odata.loopZeroInputs_2.mkString("(",",",")"),"smem_1_"+osym+"[threadIdx.x]","smem_2_"+osym+"[threadIdx.x]","idxX")).mkString("(",",",");\n"))
          out.append("if(idxX<" + op.size + ") smem_2_" + osym + "[threadIdx.x] = dev_reduce_seq_2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs_2 ++ List("dev_zero_1_"+funcNameSuffix(op,osym)+odata.loopZeroInputs.mkString("(",",",")"),"dev_zero_2_"+funcNameSuffix(op,osym)+odata.loopZeroInputs_2.mkString("(",",",")"),"smem_1_"+osym+"[threadIdx.x]","smem_2_"+osym+"[threadIdx.x]","idxX")).mkString("(",",",");\n"))
          out.append("__syncthreads();\n")
          out.append("for(unsigned int s=1; s<blockDim.x; s*=2) {\n")
          out.append("if((idxX%(2*s))==0) { \n")
          out.append("smem_1_" + osym + "[threadIdx.x] = dev_reduce_par_1_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs ++ List("smem_1_"+osym+"[threadIdx.x]","smem_2_"+osym+"[threadIdx.x]","smem_1_"+osym+"[threadIdx.x+s]","smem_2_"+osym+"[threadIdx.x+s]","idxX")).mkString("(",",",");\n"))
          out.append("smem_2_" + osym + "[threadIdx.x] = dev_reduce_par_2_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs_2 ++ List("smem_1_"+osym+"[threadIdx.x]","smem_2_"+osym+"[threadIdx.x]","smem_1_"+osym+"[threadIdx.x+s]","smem_2_"+osym+"[threadIdx.x+s]","idxX")).mkString("(",",",");\n"))
          out.append("}\n")
          out.append("__syncthreads();\n")
          out.append("}\n")
          out.append("if(threadIdx.x==0) {\n")
          out.append("temp_1_" + osym + "[blockIdx.x] = smem_1_" + osym + "[0];\n")
          out.append("temp_2_" + osym + "[blockIdx.x] = smem_2_" + osym + "[0];\n")
          out.append("}\n")
      }
    }
    writeKernelFooter(out)
  }

  //TODO: Use more efficient reduction algorithm
  private def writeReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "Reduce")
    for((odata,osym) <- reductionList(op)) {
      if (odata.hasCond) out.append("smem_" + osym + "[threadIdx.x] = ((idxX<size) && (bitmap_" + osym + "[idxX]==1)) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      else out.append("smem_" + osym + "[threadIdx.x] = (idxX < size) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      out.append("__syncthreads();\n")
      out.append("for(unsigned int s=1; s<blockDim.x; s*=2) {\n")
      out.append("if((idxX%(2*s))==0) { \n")
      out.append("smem_" + osym + "[threadIdx.x] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+s]","idxX")).mkString("(",",",");\n"))
      out.append("}\n")
      out.append("__syncthreads();\n")
      out.append("}\n")
      out.append("if(threadIdx.x==0) temp_" + osym + "_2[blockIdx.x] = smem_" + osym + "[0];\n")
    }
    writeKernelFooter(out)
  }

  private def writeReduceTupleKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "ReduceTuple")
    for((odata,osym) <- reductionTupleList(op)) {
      if (odata.hasCond) {
        out.append("smem_1_" + osym + "[threadIdx.x] = ((idxX<size) && (bitmap_" + osym + "[idxX]==1)) ? temp_1_" + osym + "[idxX] : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
        out.append("smem_2_" + osym + "[threadIdx.x] = ((idxX<size) && (bitmap_" + osym + "[idxX]==1)) ? temp_2_" + osym + "[idxX] : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
      }
      else {
        out.append("smem_1_" + osym + "[threadIdx.x] = (idxX < size) ? temp_1_" + osym + "[idxX] : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
        out.append("smem_2_" + osym + "[threadIdx.x] = (idxX < size) ? temp_2_" + osym + "[idxX] : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
      }
      out.append("__syncthreads();\n")
      out.append("for(unsigned int s=1; s<blockDim.x; s*=2) {\n")
      out.append("if((idxX%(2*s))==0) { \n")
      out.append("smem_1_" + osym + "[threadIdx.x] = dev_reduce_par_1_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs ++ List("smem_1_"+osym+"[threadIdx.x]","smem_2_"+osym+"[threadIdx.x]","smem_1_"+osym+"[threadIdx.x+s]","smem_2_"+osym+"[threadIdx.x+s]","idxX")).mkString("(",",",");\n"))
      out.append("smem_2_" + osym + "[threadIdx.x] = dev_reduce_par_2_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs_2 ++ List("smem_1_"+osym+"[threadIdx.x]","smem_2_"+osym+"[threadIdx.x]","smem_1_"+osym+"[threadIdx.x+s]","smem_2_"+osym+"[threadIdx.x+s]","idxX")).mkString("(",",",");\n"))
      out.append("}\n")
      out.append("__syncthreads();\n")
      out.append("}\n")
      out.append("if(threadIdx.x==0) {\n")
      out.append("temp_1_" + osym + "_2[blockIdx.x] = smem_1_" + osym + "[0];\n")
      out.append("temp_2_" + osym + "_2[blockIdx.x] = smem_2_" + osym + "[0];\n")
      out.append("}\n")
    }
    writeKernelFooter(out)
  }

  override protected def writeOutputAllocs(op: DeliteOP, out: StringBuilder) {
      for ((odata,osym) <- op.getGPUMetadata(target).outputs if odata.resultType!="void") {// if !isPrimitiveType(op.outputType(osym))) {
        out.append("*" + osym)
        out.append(" = ")
        out.append(odata.func)
        out.append('(')
        writeInputList(op, odata, out)
        if(odata.inputs.nonEmpty && odata.loopType=="COLLECT" && odata.hasCond) out.append(',')
        if(odata.loopType=="COLLECT" && odata.hasCond) out.append("*" + osym + "_size_ptr")
        out.append(");\n")
        out.append("cudaMemoryMap->insert(pair<void*,list<void*>*>(")
        out.append("*" + osym)
        out.append(",lastAlloc));\n")
        out.append("lastAlloc = new list<void*>();\n")
      }
  }

  private def writeCopyBackKernel(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- conditionList(op)) {
      out.append("unsigned int *%s_size_ptr;\n".format(osym))
      out.append("DeliteCudaMallocHost((void**)&%s_size_ptr,2*sizeof(unsigned int));\n".format(osym))
      out.append("DeliteCudaMemcpyDtoHAsync((void*)%s_size_ptr,scanmap_%s+%s-1,sizeof(unsigned int));\n".format(osym,osym,op.size))
      out.append("DeliteCudaMemcpyDtoHAsync((void*)(%s_size_ptr+1),bitmap_%s+%s-1,sizeof(unsigned int));\n".format(osym,osym,op.size))
      out.append("*%s_size_ptr = *%s_size_ptr + *(%s_size_ptr+1);\n".format(osym,osym,osym))
    }
  }

  private def writeCopyBackReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op)) {
      out.append("DeliteCudaMemcpyDtoDAsync((void*)(*%s),temp_%s,sizeof(%s));\n".format(osym,osym,odata.loopFuncOutputType))
    }
    for((odata,osym) <- reductionTupleList(op)) {
      out.append("DeliteCudaMemcpyDtoDAsync((void*)(*%s),temp_1_%s,sizeof(%s));\n".format(osym,osym,odata.loopFuncOutputType))
    }
  }

  private def writeScanKernel(out: StringBuilder, op: OP_MultiLoop) {
    //exclusive scan
    for ((odata,osym) <- conditionList(op)) {
      out.append("thrust::device_ptr<unsigned int> bitmap_" + osym + "_thrust(bitmap_" + osym + ");\n")
      out.append("thrust::device_ptr<unsigned int> scanmap_" + osym + "_thrust(scanmap_" + osym + ");\n")
      out.append("thrust::exclusive_scan(bitmap_" + osym + "_thrust, bitmap_" + osym + "_thrust+" + op.size + ", scanmap_" + osym + "_thrust);\n")
    }
  }

  // Allocate & Register temporary memory for filter and reduction operations
  private def makeTemps(out: StringBuilder, op: OP_MultiLoop) {
    allocateMaps(out, op)
    allocateTemps(out, op)
    //addOpTemps(op)
  }

  // Allocate bitmap and scanmap for filter operations
  private def allocateMaps(out: StringBuilder, op: OP_MultiLoop) {
    for (name <- List("bitmap_", "scanmap_")) {
      for ((odata,osym) <- conditionList(op)) {
        out.append("unsigned int * " + name + osym + ";\n")
        out.append("DeliteCudaMalloc((void**)&" + name + osym + ", " + op.size + "*sizeof(unsigned int));\n")
      }
    }
  }

  // Allocate temporary outputs for reduction operations
  private def allocateTemps(out: StringBuilder, op: OP_MultiLoop) {
    for ((odata,osym) <- reductionList(op)) {
      out.append(odata.loopFuncOutputType + " *temp_" + osym + ";\n")
      out.append("DeliteCudaMalloc((void**)&temp_" + osym + ", " + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append(odata.loopFuncOutputType + " *temp_" + osym + "_2;\n")
      out.append("DeliteCudaMalloc((void**)&temp_" + osym + "_2, " + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
    }
    for ((odata,osym) <- reductionTupleList(op)) {
      out.append(odata.loopFuncOutputType + " *temp_1_" + osym + ";\n")
      out.append("DeliteCudaMalloc((void**)&temp_1_" + osym + ", " + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append(odata.loopFuncOutputType + " *temp_1_" + osym + "_2;\n")
      out.append("DeliteCudaMalloc((void**)&temp_1_" + osym + "_2, " + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append(odata.loopFuncOutputType_2 + " *temp_2_" + osym + ";\n")
      out.append("DeliteCudaMalloc((void**)&temp_2_" + osym + ", " + op.size + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
      out.append(odata.loopFuncOutputType_2 + " *temp_2_" + osym + "_2;\n")
      out.append("DeliteCudaMalloc((void**)&temp_2_" + osym + "_2, " + op.size + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
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
        if(needDeref(op,in,sym)) {
          out.append(getCPrimitiveType(in.outputType(sym)))
          out.append(" *" + sym + "_ptr")
        }
        else {
          out.append(getCPrimitiveType(in.outputType(sym)))
          out.append(" " + sym)
        }
      }
    }
  }

  override def getSymGPU(name: String) = name

}
