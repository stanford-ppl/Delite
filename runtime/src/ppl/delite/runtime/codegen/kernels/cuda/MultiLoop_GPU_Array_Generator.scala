package ppl.delite.runtime.codegen.kernels.cuda

import tools.nsc.io._
import ppl.delite.runtime.graph.ops.{OP_Executable, DeliteOP, OP_MultiLoop}
import ppl.delite.runtime.graph.targets.{OPData, Targets, TempAlloc}
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.codegen.sync.JNIFuncs
import ppl.delite.runtime.codegen.{CppCompile, CppExecutableGenerator, CudaExecutableGenerator, CudaCompile}

// TODO: Optimizations
// 1. For Reduce/TupleReduce/HashReduce, remove unnecessary scan operation (only used for Collection type).
//   For those cases the condition kernel does not have to be called separately: Just do in place with HashReduce.
//   This will remove the memory allocations for bitmap and related memory accesses.
// 2. Combine all condition checks and reduction templates for each symbol into a single one within a MultiLoop kernel.

object MultiLoop_GPU_Array_Generator extends JNIFuncs {

  val target = Targets.Cuda

  private def isPrimitiveType(scalaType: String) = Targets.isPrimitiveType(scalaType)

  private def needsReduction(op: OP_MultiLoop): Boolean = {
    if (reductionList(op).nonEmpty) true
    else false
  }
  private def needsReductionTuple(op: OP_MultiLoop): Boolean = {
    if (reductionTupleList(op).nonEmpty) true
    else false
  }
  private def needsHashReduction(op: OP_MultiLoop): Boolean = {
    if (hashReductionList(op).nonEmpty) true
    else false
  }
  private def needsCondition(op: OP_MultiLoop): Boolean = {
    if (conditionList(op).nonEmpty) true
    else false
  }
  private def needsCondition(op: OP_MultiLoop, sym: String) = {
    val output = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._2==sym)
    if (output.length != 1) throw new RuntimeException("incorrect output symbol info!")
    if (output(0)._1.hasCond) true
    else false
  }

  private def collectList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="COLLECT")
  }
  private def foreachList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="FOREACH")
  }
  private def reductionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="REDUCE")
  }
  private def reductionSpecList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="REDUCE_SPEC")
  }
  private def reductionTupleList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="REDUCE_TUPLE")
  }
  private def hashReductionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.loopType=="HASH_REDUCE")
  }
  // Separate condition kernel only needed for COLLECT with condition (i.e., need scan phase later)
  private def conditionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.hasCond && o._1.loopType=="COLLECT")
    //op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.hasCond)
  }

  private def tempAllocs(op: OP_MultiLoop): List[TempAlloc] = {
    op.getGPUMetadata(Targets.Cuda).temps
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
    out.append("#include \"helperFuncs.h\"\n")
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
    out.append("#include <stdlib.h>\n")
    out.append("#include <cuda.h>\n")
    out.append("#include <assert.h>\n")
    out.append("#include <thrust/scan.h>\n")
    out.append("#include <thrust/device_vector.h>\n")
    out.append("#include <thrust/generate.h>\n")
    out.append("#include <thrust/sort.h>\n")
    out.append("#include <thrust/copy.h>\n")
    out.append("#include \"helperFuncs.h\"\n")
    out.append("#define MAX_GROUP 4\n")  //TODO: This will get removed by generelizing the GPU HashReduce
    if(tempAllocs(op).size==0) out.append("#define TEMP_" + op.id + "\n")
    else out.append("#define TEMP_" + op.id + " " + tempAllocs(op).map(t => t.tp + " *" + t.sym).mkString(",") + ",\n")
    out.append("#include \"" + op.id + ".cu\"\n")
    out.append("extern cudaStream_t kernelStream;\n")
  }

  private def writeLauncherHeader(out: StringBuilder, op: OP_MultiLoop) {
    out.append("void ")
    out.append(kernelName(op))
    out.append(op.getGPUMetadata(Targets.Cuda).outputs.filter(o => op.outputType(Targets.Cuda,o._2)!="void").map(o => op.outputType(Targets.Cuda, o._2) + "** " + o._2).mkString("(",", ",""))
    if ((op.getGPUMetadata(Targets.Cuda).outputs.filter(o => op.outputType(Targets.Cuda,o._2)!="void").size>0) && (op.getInputs.size>0)) out.append(", ")
    writeInputs(out,op,true)
    out.append(", int size")
    out.append(")\n")
  }

  private def writeFooter(out: StringBuilder) {
    out.append("}\n")
  }

  private def makeKernels(out: StringBuilder, op: OP_MultiLoop) {
    writeProcessKernel(out, op) 
    if(op.needsCombine) {
      writeCombineKernel(out,op)
      writeReduceSpecKernel1(out,op)
      writeReduceSpecKernel2(out,op)
      writeCopyBackReduceKernel(out, op)
    }
    if(needsHashReduction(op))
      writeHashReduceKernel(out,op)
  }

  private def writeKernelLauncher(out: StringBuilder, op: OP_MultiLoop) {
    writeLauncherHeader(out, op)
    out.append("{\n")

    // Initialize temporary memory for this kernel
    out.append("tempCudaMemReset();\n")
    out.append("int loopIdx = 0;\n")
    makeTemps(out, op)
    //if(needsCondition(op)) {
    //  writeKernelCall(out, op, "Cond")
    //  writeScanKernel(out, op)
    //  writeCopyBackKernel(out, op)
    //}

    writeOutputAllocs(op, out)
    
    writeKernelCall(out, op, "Process")

    writeHashReducePreKernelCall(out, op)

    if(op.needsCombine) { 
      writeKernelCall(out, op, "Combine")
    }
    if(needsHashReduction(op)) {
      writeKernelCall(out, op, "HashReduce")
      //writeKernelCall(out, op, "HashReduce1")
      //writeKernelCall(out, op, "HashReduce2")
    } 
    writeCopyBackReduceKernelCall(out, op)
  
    for ((odata,osym) <- reductionSpecList(op)) {
      out.append("loopIdx = 0;\n")
      out.append(odata.loopFuncOutputType + " result_" +  osym + ";\n")
      out.append("addEvent(kernelStream, d2hStream);\n")
      out.append("DeliteCudaMemcpyDtoHAsync((void*)&result_"+osym+",(void*)tempIn_"+osym+",sizeof("+odata.loopFuncOutputType+"));\n")
      out.append("*" + osym + " = result_" + osym + ".dc_alloc();\n")
      out.append("while(loopIdx < result_" + osym + ".dc_size()) {\n")
      writeKernelCall(out, op, "ReduceSpecKernel1")
      writeKernelCall(out, op, "ReduceSpecKernel2")
      out.append("loopIdx += 1;\n")
      out.append("}\n")
    }

    writeFooter(out)
  }

  private def blockSizeConfig(op: OP_MultiLoop): String = if(op.needsCombine) "256" else "512"

  private def opSize = "size"

  private def writeKernelCall(out: StringBuilder, op: OP_MultiLoop, id: String) {
    val blockSize = blockSizeConfig(op) 

    def dimSize(size: String) = if(id=="HashReduce1" || id=="HashReduce2") "1 +((" + size + "-1)/(" + blockSize + "/MAX_GROUP))"
                                else if(id=="HashReduce") size
                                else if(op.needsCombine) "1 +((" + size + "-1)/" + blockSize + "/2)"
                                else "1 +((" + size + "-1)/" + blockSize + ")"
    
    def deref(op: DeliteOP, tp: String) = if (isPrimitiveType(op.outputType(tp))) "" else "*"
    def cudaLaunch(dimConfig: String): String = {
      val str = new StringBuilder
      str.append(kernelName(op))
      str.append(id)
      str.append("<<<dim3(") //kernel dimensions
      str.append(dimConfig)
      str.append(",1,1),dim3(")
      str.append(blockSize)
      str.append(",1,1),0,")
      str.append("kernelStream")
      str.append(">>>")
      str.toString
    }

    id match {
      case "Process" | "ReduceSpecKernel1" | "HashReduce1" =>
        if (op.needsCombine) { 
          out.append(cudaLaunch("64"))
        }
        else {
          out.append("if(" + dimSize(opSize) +" > 65536) { printf(\"Kernel Launch Failure: Grid size %d for GPU is too large even with maximum blockSize " + blockSize + "!\\n\"," + dimSize(op.size) + "); assert(false); }\n")
          out.append(cudaLaunch(dimSize(opSize)))
        }
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => "**" + o._2) ++ conditionList(op).map(o => "bitmap_" + o._2 + ", scanmap_" + o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionSpecList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2, tempIn_" + o._2) ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ tempAllocs(op).map(t => t.sym) ++ List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx")
        out.append(args.mkString("(",",",");\n"))
      case "Combine" | "ReduceSpecKernel2" =>
        out.append("int num_blocks_" + id + " = min(64," + dimSize(opSize) + ");\n")
        out.append(cudaLaunch("1"))
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => "**" + o._2) ++ conditionList(op).map(o => "bitmap_" + o._2 + ", scanmap_" + o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionSpecList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2, tempIn_" + o._2) ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ tempAllocs(op).map(t => t.sym) ++ List("num_blocks_"+id+", tempMemSize, tempMemPtr, tempMemUsage, loopIdx")
        out.append(args.mkString("(",",",");\n"))
        for((odata,osym) <- reductionList(op)) {
          out.append(odata.loopFuncOutputType + " *temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        for((odata,osym) <- reductionSpecList(op)) {
          out.append(odata.loopFuncOutputType_2 + " *temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append(odata.loopFuncOutputType + " *temp1_" + osym + "_t = temp1_" + osym + ";\n")
          out.append("temp1_" + osym + " = temp1_" + osym + "_2;\n")
          out.append("temp1_" + osym + "_2 = temp1_" + osym + "_t;\n")
          out.append(odata.loopFuncOutputType_2 + " *temp2_" + osym + "_t = temp2_" + osym + ";\n")
          out.append("temp2_" + osym + " = temp2_" + osym + "_2;\n")
          out.append("temp2_" + osym + "_2 = temp2_" + osym + "_t;\n")
        }
      case "HashReduce2" =>
        out.append("int num_blocks_" + id + " = " + dimSize(opSize) + ";\n")
        out.append("while(num_blocks_" + id + " != 1) {\n")
        out.append(cudaLaunch(dimSize("num_blocks_"+id)))
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => "**" + o._2) ++ conditionList(op).map(o => "bitmap_" + o._2 + ", scanmap_" + o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ List("num_blocks_"+id) ++ List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx")
        out.append(args.mkString("(",",",");\n"))
        out.append("num_blocks_" + id + " = " + dimSize("num_blocks_"+id) + ";\n")
        for((odata,osym) <- hashReductionList(op)) {
          out.append(odata.loopFuncOutputType + " *temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        out.append("}\n")
      case "HashReduce" =>
        //for((odata,osym) <- hashReductionList(op)) {
          val osym = hashReductionList(op)(0)._2
          out.append(cudaLaunch(dimSize("host_offset_"+osym+"[0]")))
          val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => "**" + o._2) ++ conditionList(op).map(o => "bitmap_" + o._2 + ", scanmap_" + o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx")
          out.append(args.mkString("(",",",");\n"))
        //}
      case _ => error(id + " is not a known kernel type")
    }
  }

  private def writeKernelHeader(out: StringBuilder, op: OP_MultiLoop, id: String) {
    out.append("__global__ void ")
    out.append(kernelName(op))
    out.append(id)
    out.append('(')

    val params = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => op.outputType(Targets.Cuda, o._2) + " " + o._2) ++ conditionList(op).map(o => "unsigned int * bitmap_" + o._2 + ", unsigned int * scanmap_" + o._2) ++ reductionList(op).map(o => o._1.loopFuncOutputType + " *temp_" + o._2 + "," + o._1.loopFuncOutputType + " *temp_" + o._2 + "_2") ++ (reductionSpecList(op).map(o => o._1.loopFuncOutputType_2 + " *temp_" + o._2 + "," + o._1.loopFuncOutputType_2 + " *temp_" + o._2 + "_2," + o._1.loopFuncOutputType + " *tempIn_" + o._2)) ++ reductionTupleList(op).map(o => o._1.loopFuncOutputType + " *temp1_" + o._2 + "," + o._1.loopFuncOutputType + " *temp1_" + o._2 + "_2," + o._1.loopFuncOutputType_2 + " *temp2_" + o._2 + "," + o._1.loopFuncOutputType_2 + " *temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => o._1.loopFuncOutputType + " *key_" + o._2 + "," + o._1.loopFuncOutputType_2 + " *val_" + o._2 + ", int *offset_" + o._2 + ", int *idx_" + o._2)
    out.append(params.mkString(","))
    if (params.nonEmpty && op.getInputs.nonEmpty) out.append(',')
    writeInputs(out,op,false)
    if (params.nonEmpty || op.getInputs.nonEmpty) out.append(',')
    //if((id=="Combine") || (id=="HashReduce2")) {
    //  out.append("int size,")
    //}
    out.append(List("TEMP_"+op.id+" int size, size_t tempMemSize","char *tempMemPtr","int *tempMemUsage, int loopIdx").mkString(","))
    out.append(") {\n")
    if(op.needsCombine) {
      out.append("int idxX = blockIdx.x * 2 * blockDim.x + threadIdx.x;\n")
      out.append("int blockSize = 256;\n")
      out.append("int gridSize = blockSize * 2 * gridDim.x;\n")
    }
    else {
      out.append("int idxX = blockIdx.x * blockDim.x + threadIdx.x;\n")
    }
    out.append("int tid = threadIdx.x;\n")
    if(needsHashReduction(op)) {
      out.append("int chunkIdx = idxX / MAX_GROUP;\n")
      out.append("int chunkOffset = idxX % MAX_GROUP;\n")
    }
    addDeref(out, op)
    allocateSharedMem(out, op)
  }

  private def writeKernelFooter(out: StringBuilder) {
    out.append("}\n") //end if, end kernel
  }

  private def funcNameSuffix(op: OP_MultiLoop, sym: String):String = {
    op.id + "_" + sym
  }

  //TODO: Add temporary allocations here?
  private def lastInputArgs(op: OP_MultiLoop): List[String] = {
    //if(op.sizeIsConst)
    //  List("idxX") ++ tempAllocs(op).map(_.sym) ++ List("tempMemSize","tempMemPtr","tempMemUsage")
    //else 
    //  List("idxX",op.size) ++ tempAllocs(op).map(_.sym) ++ List("tempMemSize","tempMemPtr","tempMemUsage")
    List("idxX",opSize) ++ tempAllocs(op).map(_.sym) ++ List("tempMemSize","tempMemPtr","tempMemUsage")
  }

  private def writeProcessKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "Process")
    if(op.needsCombine) {    
      for((odata,osym) <- reductionList(op)) {
        out.append(odata.loopFuncOutputType + " localSum_" + osym + " = dev_init_" + funcNameSuffix(op,osym) + (odata.loopZeroInputs++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append(odata.loopFuncOutputType + " localSum1_" + osym + " = dev_init1_" + funcNameSuffix(op,osym) + (odata.loopZeroInputs++lastInputArgs(op)).mkString("(",",",");\n"))
        out.append(odata.loopFuncOutputType_2 + " localSum2_" + osym + " = dev_init2_" + funcNameSuffix(op,osym) + (odata.loopZeroInputs++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("while( idxX < " + opSize + ") {\n")
      for((odata,osym) <- collectList(op)++foreachList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + "(" + (odata.loopFuncInputs++lastInputArgs(op)).mkString(",") + ");\n")
      }
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("tempIn_" + osym + "[idxX] = dev_process_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionList(op)) {
        out.append("localSum_" + osym + " = dev_process_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++List("localSum_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("localSum1_" + osym + " = dev_process1_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
        out.append("localSum2_" + osym + " = dev_process2_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- hashReductionList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + (List("key_"+osym,"val_"+osym)++odata.loopFuncInputs++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("idxX += blockSize;\n")
      out.append("if (idxX < " + opSize + ") {\n")
      for((odata,osym) <- collectList(op)++foreachList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + "(" + (odata.loopFuncInputs++lastInputArgs(op)).mkString(",") + ");\n")
      }
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("tempIn_" + osym + "[idxX] = dev_process_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionList(op)) {
        out.append("localSum_" + osym + " = dev_process_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++List("localSum_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("localSum1_" + osym + " = dev_process1_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
        out.append("localSum2_" + osym + " = dev_process2_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- hashReductionList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + (List("key_"+osym,"val_"+osym)++odata.loopFuncInputs++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("}\n")
      out.append("idxX += (gridSize-blockSize);\n")
      out.append("}\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("smem_" + osym + "[threadIdx.x] = localSum_" + osym + ";\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("smem1_" + osym + "[threadIdx.x] = localSum1_" + osym + ";\n")
        out.append("smem2_" + osym + "[threadIdx.x] = localSum2_" + osym + ";\n")
      }
      out.append("__syncthreads();\n")
      for(blockSize <- List(512,256,128)) {
        out.append("if(blockSize >= " + blockSize + ") { if (threadIdx.x < " + blockSize/2 + ") { ")
        for((odata,osym) <- reductionList(op)) {
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("volatile " + odata.loopFuncOutputType + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("volatile " + odata.loopFuncOutputType + "* sdata1_" + osym + " = smem1_" + osym + ";\n")
        out.append("volatile " + odata.loopFuncOutputType_2 + "* sdata2_" + osym + " = smem2_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionList(op)) {
          if(isPrimitiveType(op.outputType(osym))) 
            out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          else 
            out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); __syncthreads();"))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          if(isPrimitiveType(op.outputType(osym))) { 
            out.append("sdata1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("sdata2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
          }
          else {
            out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
          }
        }
        out.append("}\n")
      }
      out.append("}\n")
      out.append("if(threadIdx.x == 0) {\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("temp_" + osym + "[blockIdx.x] = smem_" + osym + "[0];\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("temp1_" + osym + "[blockIdx.x] = smem1_" + osym + "[0];\n")
        out.append("temp2_" + osym + "[blockIdx.x] = smem2_" + osym + "[0];\n")
      }
      out.append("}\n")
    }
    else {
      out.append("if (idxX < " + opSize + ") {\n")
      for((odata,osym) <- collectList(op)++foreachList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + "(" + (odata.loopFuncInputs++lastInputArgs(op)).mkString(",") + ");\n")
      }
      out.append("}\n")
    }
    writeKernelFooter(out)
  }

  private def writeCombineKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "Combine")
    out.append("blockSize = 64;\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("smem_" + osym + "[threadIdx.x] = temp_" + osym + "[threadIdx.x];\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("smem1_" + osym + "[threadIdx.x] = temp1_" + osym + "[threadIdx.x];\n")
        out.append("smem2_" + osym + "[threadIdx.x] = temp2_" + osym + "[threadIdx.x];\n")
      }
      out.append("__syncthreads();\n")
      for(blockSize <- List(512,256,128)) {
        out.append("if(blockSize >= " + blockSize + ") { if (threadIdx.x < " + blockSize/2 + ") { ")
        for((odata,osym) <- reductionList(op)) {
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("volatile " + odata.loopFuncOutputType + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("volatile " + odata.loopFuncOutputType + "* sdata1_" + osym + " = smem1_" + osym + ";\n")
        out.append("volatile " + odata.loopFuncOutputType_2 + "* sdata2_" + osym + " = smem2_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionList(op)) {
          if(isPrimitiveType(op.outputType(osym))) 
            out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          else 
            out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); __syncthreads();"))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          if(isPrimitiveType(op.outputType(osym))) { 
            out.append("sdata1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("sdata2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
          } 
          else {
            out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
          }
        }
        out.append("}\n")
      }
      out.append("}\n")
      out.append("if(threadIdx.x == 0) {\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("temp_" + osym + "_2[blockIdx.x] = smem_" + osym + "[0];\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("temp1_" + osym + "_2[blockIdx.x] = smem1_" + osym + "[0];\n")
        out.append("temp2_" + osym + "_2[blockIdx.x] = smem2_" + osym + "[0];\n")
      }
      out.append("}\n")

    writeKernelFooter(out)
  }

  //TODO: Combine below codegen with above Process and Combine
  private def writeReduceSpecKernel1(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "ReduceSpecKernel1")  
      for((odata,osym) <- reductionSpecList(op)) {
        out.append(odata.loopFuncOutputType_2 + " localSum_" + osym + " = dev_spcinit_" + funcNameSuffix(op,osym) + "();\n")
      }
      out.append("while( idxX < " + opSize + ") {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("localSum_" + osym + " = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("localSum_"+osym,"tempIn_"+osym+"[idxX].dc_apply(loopIdx)")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("idxX += blockSize;\n")
      out.append("if (idxX < " + opSize + ") {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("localSum_" + osym + " = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("localSum_"+osym,"tempIn_"+osym+"[idxX].dc_apply(loopIdx)")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("}\n")
      out.append("idxX += (gridSize-blockSize);\n")
      out.append("}\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("smem_" + osym + "[threadIdx.x] = localSum_" + osym + ";\n")
      }
      out.append("__syncthreads();\n")
      for(blockSize <- List(512,256,128)) {
        out.append("if(blockSize >= " + blockSize + ") { if (threadIdx.x < " + blockSize/2 + ") { ")
        for((odata,osym) <- reductionList(op)++reductionSpecList(op)) {
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("volatile " + odata.loopFuncOutputType_2 + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionSpecList(op)) {
          out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append("}\n")
      }
      out.append("}\n")
      out.append("if(threadIdx.x == 0) {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("temp_" + osym + "[blockIdx.x] = smem_" + osym + "[0];\n")
      }
      out.append("}\n")
    writeKernelFooter(out)
  }

  private def writeReduceSpecKernel2(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "ReduceSpecKernel2")
    out.append("blockSize = 64;\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("smem_" + osym + "[threadIdx.x] = temp_" + osym + "[threadIdx.x];\n")
      }
      out.append("__syncthreads();\n")
      for(blockSize <- List(512,256,128)) {
        out.append("if(blockSize >= " + blockSize + ") { if (threadIdx.x < " + blockSize/2 + ") { ")
        for((odata,osym) <- reductionSpecList(op)) {
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("volatile " + odata.loopFuncOutputType_2 + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionSpecList(op)) {
          out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append("}\n")
      }
      out.append("}\n")
      out.append("if(threadIdx.x == 0) {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append(osym + ".dc_update(loopIdx, smem_" + osym + "[0]);\n")
      }
      out.append("}\n")

    writeKernelFooter(out)
  }

  private def writeCopyBackReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op) ++ reductionTupleList(op)) {
      if(!isPrimitiveType(op.outputType(osym))) { 
        out.append("__global__ void dc_copy_" + osym + "(" + odata.loopFuncOutputType + " from, " + odata.loopFuncOutputType + " to) {\n")
        out.append("\tto.dc_copy(from);\n")
        out.append("}\n")
      }
    }
  }  

  /*
  //TODO: If the framework tells that all the output symbols share the same keyFunc and condFunc, 
  //      then below can be more optimized by removing the redundant computations and if-else statements 
  private def writeHashReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "HashReduce1")
    for((odata,osym) <- hashReductionList(op)) {
      out.append("int groupIdx_" + osym + " = (chunkIdx<" + opSize + ") ? dev_keyFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs_2:+"chunkIdx").mkString("(",",",")") +  ": -1;\n")
    }
    for((odata,osym) <- hashReductionList(op)) {
      if (odata.hasCond) out.append("smem_" + osym + "[threadIdx.x] = dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      else out.append("smem_" + osym + "[threadIdx.x] = dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
    }
    for((odata,osym) <- hashReductionList(op)) {
      if (odata.hasCond) out.append("if(groupIdx_" + osym + "==chunkOffset && dev_cond_" + funcNameSuffix(op,osym) + "(" + (odata.loopCondInputs:+"chunkIdx").mkString(",") + ")) smem_" + osym + "[threadIdx.x] = dev_valFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"chunkIdx").mkString("(",",",");\n"))
      else out.append("if(groupIdx_" + osym + "==chunkOffset) smem_" + osym + "[threadIdx.x] = dev_valFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"chunkIdx").mkString("(",",",");\n"))
    }
    out.append("__syncthreads();\n")
    out.append("for(unsigned int s=1; s<blockDim.x/MAX_GROUP; s*=2) {\n")
    out.append("if((chunkIdx%(2*s))==0) { \n")
    for((odata,osym) <- hashReductionList(op)) {
      out.append("smem_" + osym + "[threadIdx.x] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+s*MAX_GROUP]","chunkIdx")).mkString("(",",",");\n"))
    }
    out.append("}\n")
    out.append("__syncthreads();\n")
    out.append("}\n")
    out.append("if(threadIdx.x / MAX_GROUP == 0) {\n")
    for((odata,osym) <- hashReductionList(op)) {
      out.append("temp_" + osym + "[blockIdx.x*MAX_GROUP+threadIdx.x] = smem_" + osym + "[threadIdx.x];\n")
    }
    out.append("}\n")
    writeKernelFooter(out)

    writeKernelHeader(out, op, "HashReduce2")
    for((odata,osym) <- hashReductionList(op)) {
      if (odata.hasCond) {
        out.append("smem_" + osym + "[threadIdx.x] = ((chunkIdx<size) && dev_cond_" + funcNameSuffix(op,osym) + "(" + (odata.loopCondInputs:+"chunkIdx").mkString(",") + ")) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      }
      else {
        out.append("smem_" + osym + "[threadIdx.x] = (chunkIdx < size) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      }
      out.append("__syncthreads();\n")
      out.append("for(unsigned int s=1; s<blockDim.x/MAX_GROUP; s*=2) {\n")
      out.append("if((chunkIdx%(2*s))==0) { \n")
      out.append("smem_" + osym + "[threadIdx.x] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+s*MAX_GROUP]","idxX")).mkString("(",",",");\n"))
      out.append("}\n")
      out.append("__syncthreads();\n")
      out.append("}\n")
      out.append("if(threadIdx.x / MAX_GROUP == 0) temp_" + osym + "_2[blockIdx.x*MAX_GROUP+threadIdx.x] = smem_" + osym + "[threadIdx.x];\n")
    }
    writeKernelFooter(out)
  }
  */

  private def writeHashReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "HashReduce") 
    out.append("int start = offset_" + hashReductionList(op)(0)._2 + "[blockIdx.x+1];\n") 
    out.append("int end = offset_" + hashReductionList(op)(0)._2 + "[blockIdx.x+2];\n")
    out.append("idxX = threadIdx.x + start;\n")
      for((odata,osym) <- hashReductionList(op)) {
        out.append(odata.loopFuncOutputType_2 + " localSum_" + osym + " = dev_init_" + funcNameSuffix(op,osym) + (odata.loopZeroInputs++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("if(idxX < end) {\n")
      for((odata,osym) <- hashReductionList(op)) {
        out.append("localSum_" + osym + " = val_" + osym + "[idx_" + osym + "[idxX]];\n")
      }
      out.append("idxX += blockSize;\n")
      out.append("while( idxX < end ) {\n")
      for((odata,osym) <- hashReductionList(op)) {
        out.append("localSum_" + osym + " = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("localSum_"+osym,"val_"+osym+"[idx_" + osym + "[idxX]]")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("idxX += blockSize;\n")
      out.append("}\n")
      out.append("}\n")
      for((odata,osym) <- hashReductionList(op)) {
        out.append("smem_" + osym + "[threadIdx.x] = localSum_" + osym + ";\n")
      }
      out.append("__syncthreads();\n")
      for(blockSize <- List(512,256,128)) {
        out.append("if(blockSize >= " + blockSize + ") { if (threadIdx.x < " + blockSize/2 + ") { ")
        for((odata,osym) <- hashReductionList(op)) {
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- hashReductionList(op)) {
        out.append("volatile " + odata.loopFuncOutputType_2 + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- hashReductionList(op)) {
          out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append("}\n")
      }
      out.append("}\n")
      out.append("if(threadIdx.x == 0) {\n")
      for((odata,osym) <- hashReductionList(op)) {
        out.append(osym + ".dc_update(blockIdx.x,smem_" + osym + "[0]);\n")
      }
      out.append("}\n")
    writeKernelFooter(out)
  }

  private def writeInputList(op: DeliteOP, data: OPData, out: StringBuilder) {
    out.append(data.inputs.map(in => getSymGPU(in._2)).mkString(","))
  }

  private def writeOutputAllocs(op: DeliteOP, out: StringBuilder) {
      for ((odata,osym) <- op.getGPUMetadata(target).outputs if odata.resultType!="void") {// if !isPrimitiveType(op.outputType(osym))) {
        out.append("*" + osym)
        out.append(" = ")
        out.append(odata.func)
        out.append('(')
        writeInputList(op, odata, out)
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
      out.append("DeliteCudaMemcpyDtoHAsync((void*)%s_size_ptr,scanmap_%s+%s-1,sizeof(unsigned int));\n".format(osym,osym,opSize))
      out.append("DeliteCudaMemcpyDtoHAsync((void*)(%s_size_ptr+1),bitmap_%s+%s-1,sizeof(unsigned int));\n".format(osym,osym,opSize))
      out.append("*%s_size_ptr = *%s_size_ptr + *(%s_size_ptr+1);\n".format(osym,osym,osym))
    }
  }

  private def writeCopyBackReduceKernelCall(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op)) {
      if(isPrimitiveType(op.outputType(osym))) 
        out.append("DeliteCudaMemcpyDtoDAsync((void*)(*%s),temp_%s,sizeof(%s));\n".format(osym,osym,odata.loopFuncOutputType))
      else {
        out.append(odata.loopFuncOutputType + " result_" + osym + ";\n")
        out.append("addEvent(kernelStream, d2hStream);\n")
        out.append("DeliteCudaMemcpyDtoHAsync((void*)&result_" + osym + ", temp_" + osym + ", sizeof(" + odata.loopFuncOutputType + "));\n")
        out.append("*" + osym + " = result_" + osym + ".dc_alloc();\n")
        out.append("dc_copy_" + osym + "<<<dim3(1,1,1),dim3(1,1,1),0,kernelStream>>>(result_"+osym+",**"+osym+");\n")
      }
    }
    for((odata,osym) <- reductionTupleList(op)) {
      if(isPrimitiveType(op.outputType(osym)))
        out.append("DeliteCudaMemcpyDtoDAsync((void*)(*%s),temp1_%s,sizeof(%s));\n".format(osym,osym,odata.loopFuncOutputType))
      else {
        out.append(odata.loopFuncOutputType + " result_" + osym + ";\n")
        out.append("addEvent(kernelStream, d2hStream);\n")
        out.append("DeliteCudaMemcpyDtoHAsync((void*)&result_" + osym + ", temp1_" + osym + ", sizeof(" + odata.loopFuncOutputType + "));\n")
        out.append("*" + osym + " = result_" + osym + ".dc_alloc();\n")
        out.append("dc_copy_" + osym + "<<<dim3(1,1,1),dim3(1,1,1),0,kernelStream>>>(result_"+osym+",**"+osym+");\n")
      }
    }
  }

  private def writeHashReducePreKernelCall(out: StringBuilder, op: OP_MultiLoop) {
    for ((odata,osym) <- hashReductionList(op)) {
      out.append("thrust::device_ptr<int> idx_" + osym + "_thrust(idx_" + osym + ");\n")
      out.append("thrust::device_ptr<int> key_" + osym + "_thrust(key_" + osym + ");\n")
      out.append("thrust::sequence(idx_" + osym + "_thrust, idx_" + osym + "_thrust+" + opSize + ");\n")
      out.append("thrust::sort_by_key(key_" + osym + "_thrust, key_" + osym + "_thrust+" + opSize + ", idx_" + osym + "_thrust);\n")
      out.append("kernel_offset<<<dim3(1+(" + opSize + "-1)/1024,1,1),dim3(1024,1,1),0,kernelStream>>>(key_" + osym + ", idx_" + osym + ", offset_" + osym + ", " + opSize + ");\n")
      out.append("int *host_offset_" + osym + " = (int *)malloc(sizeof(int)*" + opSize + ");\n")
      out.append("DeliteCudaMemcpyDtoHAsync((void*)host_offset_" + osym + ", (void*)offset_" + osym + ", sizeof(int)*" + opSize + ");\n")
      out.append(" *" + osym + " = (*" + osym + ")->dc_alloc(host_offset_" + osym + "[0]);\n")
    }
  }

  private def writeScanKernel(out: StringBuilder, op: OP_MultiLoop) {
    //exclusive scan
    for ((odata,osym) <- conditionList(op) if odata.loopType=="COLLECT") {
      out.append("thrust::device_ptr<unsigned int> bitmap_" + osym + "_thrust(bitmap_" + osym + ");\n")
      out.append("thrust::device_ptr<unsigned int> scanmap_" + osym + "_thrust(scanmap_" + osym + ");\n")
      out.append("thrust::exclusive_scan(bitmap_" + osym + "_thrust, bitmap_" + osym + "_thrust+" + opSize + ", scanmap_" + osym + "_thrust);\n")
    }
  }

  // Allocate & Register temporary memory for filter and reduction operations
  private def makeTemps(out: StringBuilder, op: OP_MultiLoop) {
    allocateMaps(out, op)
    allocateTemps(out, op)
  }

  // Allocate bitmap and scanmap for filter operations
  private def allocateMaps(out: StringBuilder, op: OP_MultiLoop) {
    for (name <- List("bitmap_", "scanmap_")) {
      for ((odata,osym) <- conditionList(op)) {
        out.append("unsigned int * " + name + osym + ";\n")
        out.append("DeliteCudaMallocTemp((void**)&" + name + osym + ", " + opSize + "*sizeof(unsigned int));\n")
      }
    }
  }

  // Allocate temporary outputs for reduction operations
  //TODO: Add check for the temporary buffer overflow and fall back to normal CudaMalloc
  private def allocateTemps(out: StringBuilder, op: OP_MultiLoop) {
    val dimSize = 64
    for (temp <- tempAllocs(op)) {
      out.append(temp.tp + " *" + temp.sym + ";\n")
      out.append("DeliteCudaMallocTemp((void**)&" + temp.sym + ", sizeof(" + temp.tp + ")*max(" + opSize + "+" + blockSizeConfig(op) + ",2*64*256)*" + temp.size + ");\n")
    }
    for ((odata,osym) <- reductionList(op)) {
      out.append(odata.loopFuncOutputType + " *temp_" + osym + ";\n")
      out.append(odata.loopFuncOutputType + " *temp_" + osym + "_2;\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + ", " + dimSize + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + "_2, " + dimSize + "*sizeof(" + odata.loopFuncOutputType + "));\n")
    }
    for ((odata,osym) <- reductionSpecList(op)) {
      out.append(odata.loopFuncOutputType + " *tempIn_" + osym + ";\n")
      out.append("DeliteCudaMallocTemp((void**)&tempIn_" + osym + ", max(" + opSize + "+" + blockSizeConfig(op) + ",2*64*256)*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append(odata.loopFuncOutputType_2 + " *temp_" + osym + ";\n")
      out.append(odata.loopFuncOutputType_2 + " *temp_" + osym + "_2;\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + ", " + dimSize + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + "_2, " + dimSize + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
    }
    for ((odata,osym) <- reductionTupleList(op)) {
      out.append(odata.loopFuncOutputType + " *temp1_" + osym + ";\n")
      out.append(odata.loopFuncOutputType + " *temp1_" + osym + "_2;\n")
      out.append(odata.loopFuncOutputType_2 + " *temp2_" + osym + ";\n")
      out.append(odata.loopFuncOutputType_2 + " *temp2_" + osym + "_2;\n")
      out.append("DeliteCudaMallocTemp((void**)&temp1_" + osym + ", " + dimSize + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp1_" + osym + "_2, " + dimSize + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp2_" + osym + ", " + dimSize + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp2_" + osym + "_2, " + dimSize + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
    }
    //TODO: Change for hash-reduce
    for ((odata,osym) <- hashReductionList(op)) {
      out.append(odata.loopFuncOutputType + " *key_" + osym + ";\n")
      out.append(odata.loopFuncOutputType_2 + " *val_" + osym + ";\n")
      out.append("int *offset_" + osym + ";\n")
      out.append("int *idx_" + osym + ";\n")
      out.append("DeliteCudaMallocTemp((void**)&key_" + osym + ", " + opSize + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&val_" + osym + ", " + opSize + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&offset_" + osym + ", " + opSize + "*sizeof(int));\n")
      out.append("DeliteCudaMallocTemp((void**)&idx_" + osym + ", " + opSize + "*sizeof(int));\n")
    }
    out.append("int *tempMemUsage;\n")
    out.append("DeliteCudaMallocTemp((void**)&tempMemUsage,sizeof(int)*"+opSize+");\n")
    out.append("DeliteCudaMemset((void*)tempMemUsage,0,sizeof(int)*"+opSize+");\n")
    out.append("size_t tempMemSize = tempCudaMemAvailable()/"+opSize+";\n")
    out.append("char *tempMemPtr;\n")
    out.append("DeliteCudaMallocTemp((void**)&tempMemPtr,tempMemSize);\n")
  }

  /* Allocate shared memory for reduction operation */
  //TODO: Check if the local memory requirement is over the maximum (then need to chunk the kernels)
  private def allocateSharedMem(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op))
      out.append("__shared__ " + odata.loopFuncOutputType + " smem_" + osym + "[256];\n")
    for((odata,osym) <- reductionSpecList(op))
      out.append("__shared__ " + odata.loopFuncOutputType_2 + " smem_" + osym + "[256];\n")
    for((odata,osym) <- reductionTupleList(op)) {
      out.append("__shared__ " + odata.loopFuncOutputType + " smem1_" + osym + "[256];\n")
      out.append("__shared__ " + odata.loopFuncOutputType_2 + " smem2_" + osym + "[256];\n")
    }
    for((odata,osym) <- hashReductionList(op))
      out.append("__shared__ " + odata.loopFuncOutputType_2 + " smem_" + osym + "[256];\n")
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
      else {
        if (!first) out.append(", ")
        first = false
        out.append(op.inputType(target,sym))
        if (reference) out.append("*")
        out.append(" " + sym)
      }
    }
  }

  private def getSymGPU(name: String) = name

}
