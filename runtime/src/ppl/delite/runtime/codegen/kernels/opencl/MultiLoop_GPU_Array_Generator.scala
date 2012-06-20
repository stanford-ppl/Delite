package ppl.delite.runtime.codegen.kernels.opencl

import ppl.delite.runtime.codegen.{OpenCLGPUExecutableGenerator, OpenCLCompile, OpenCLMainGenerator}
import tools.nsc.io._
import ppl.delite.runtime.graph.ops.{OP_Executable, DeliteOP, OP_MultiLoop}
import ppl.delite.runtime.graph.targets.{OPData, Targets}
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.Config

// TODO: Optimizations
// 1. For Reduce/TupleReduce/HashReduce, remove unnecessary scan operation (only used for Collection type).
//   For those cases the condition kernel does not have to be called separately: Just do in place with HashReduce.
//   This will remove the memory allocations for bitmap and related memory accesses.
// 2. Combine all condition checks and reduction templates for each symbol into a single one within a MultiLoop kernel.

object MultiLoop_GPU_Array_Generator extends OpenCLGPUExecutableGenerator {

  def executableName = error("MultiLoop is not a stand-alone executable")

  val unwrappedList = ArrayBuffer[(String,String)]()

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

  private def reductionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => o._1.loopType=="REDUCE")
  }
  private def reductionTupleList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => o._1.loopType=="REDUCE_TUPLE")
  }
  private def hashReductionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => o._1.loopType=="HASH_REDUCE")
  }
  private def conditionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.hasCond && o._1.loopType=="COLLECT")
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
    val (src_kernel,src_host) = makeKernel(op)
    val header = makeHeader(op)
    OpenCLCompile.addHeader(header, kernelName(op))
    //if(!Config.noRegenerate) OpenCLCompile.addGlobalKernel(src_kernel)
    OpenCLCompile.addGlobalKernel(src_kernel)
    OpenCLCompile.addSource(src_host, kernelName(op))
    op
  }

  private def makeHeader(op: OP_MultiLoop) = {
    val out = new StringBuilder
    writeLauncherHeader(out, op)
    out.append(";\n")
    out.toString
  }

  private def makeKernel(op: OP_MultiLoop) = {
    val out_kernel = new StringBuilder
    val out_host = new StringBuilder
    writeFileHeader(out_host, op)
    makeKernels(out_kernel, op)
    writeKernelLauncher(out_host, op)
    (out_kernel.toString,out_host.toString)
  }

  private def updateOP(op: OP_MultiLoop) {
    op.setKernelName(kernelName(op))
  }

  private def kernelName(op: OP_MultiLoop) = {
    "MultiLoop_GPU_Array_" + op.id
  }

  private def writeFileHeader(out: StringBuilder, op: OP_MultiLoop) {
    out.append("#include <CL/cl.h>\n")
    out.append("#include \"helperFuncs.h\"\n")
    out.append("#define MAX_GROUP 4\n")  //TODO: This will get removed by generelizing the GPU HashReduce
    //out.append("#include \"" + op.id + ".cu\"\n")
    //out.append("extern cl_context context;\n")
    out.append("extern cl_command_queue command_queue;\n")
    out.append("extern cl_program program;\n")
    out.append("extern cl_int ret;")
  }

  private def writeLauncherHeader(out: StringBuilder, op: OP_MultiLoop) {
    out.append("void ")
    out.append(kernelName(op))
    out.append(op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => op.outputType(Targets.OpenCL,o._2)!="void").map(o => op.outputType(Targets.OpenCL, o._2) + "** " + o._2).mkString("(",", ",""))
    if ((op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => op.outputType(Targets.OpenCL,o._2)!="void").size>0) && (op.getInputs.size>0)) out.append(", ")
    writeInputs(out,op,true)
    out.append(")\n")
  }

  private def writeFooter(out: StringBuilder) {
    out.append("}\n")
  }

  private def makeKernels(out: StringBuilder, op: OP_MultiLoop) {
    if(needsCondition(op)) writeConditionKernel(out, op)
    if(needsHashReduction(op)) {
      writeHashReduceKernel(out,op)
    } else {
      writeMapReduceKernel(out, op)
      if(needsReduction(op)) writeReduceKernel(out,op)
      if(needsReductionTuple(op)) writeReduceTupleKernel(out,op)
    }
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

    if(needsHashReduction(op)) {
      writeKernelCall(out, op, "HashReduce1")
      writeKernelCall(out, op, "HashReduce2")
    } else {
      writeKernelCall(out, op, "MapReduce")
      if(needsReduction(op)) writeKernelCall(out, op, "Reduce")
      if(needsReductionTuple(op)) writeKernelCall(out, op, "ReduceTuple")
    }
    writeCopyBackReduceKernel(out, op)

    writeFooter(out)
  }

  private def writeKernelCall(out: StringBuilder, op: OP_MultiLoop, id: String) {
    var argIdx = 0

    def dimSize(size: String) = if(id=="HashReduce1" || id=="HashReduce2") "1 +((" + size + "-1)/(" + blockSize + "/MAX_GROUP))"
                  else "1 +((" + size + "-1)/" + blockSize + ")"
    def blockSize = if(id=="HashReduce1" || id=="HashReduce2") "512" else "1024"
    def deref(op: DeliteOP, tp: String) = if (isPrimitiveType(op.outputType(tp))) "" else "*"
    def clLaunch: String = {
      val str = new StringBuilder
      str.append("ret = clEnqueueNDRangeKernel(command_queue, kernel_" + id + ",1, NULL,")
      str.append("&global_size_"+id+",")
      str.append("&local_size_"+id+",")
      str.append("0, NULL, NULL);\n")
      str.append("if(ret != CL_SUCCESS) printf(\"Error while clEnqueueNDRangeKernel\\n\");\n")
      //str.append("else printf(\"Success during clLaunch\\n\");\n")
      str.toString
    }
    def putArg(tp: String, sym: String) {
      tp match {
        case "DeliteArray_bool" | "DeliteArray_char" | "DeliteArray_CHAR" | "DeliteArray_short" | "DeliteArray_int" | "DeiteArray_long" | "DeliteArray_float" | "DeliteArray_double" =>
          out.append("ret = clSetKernelArg(kernel_"+id+","+argIdx+", sizeof(cl_mem), (void *)&(("+sym+").data));\n")
          argIdx = argIdx + 1
          out.append("ret = clSetKernelArg(kernel_"+id+","+argIdx+", sizeof(int), (void *)&(("+sym+").length));\n")
          argIdx = argIdx + 1
        case "bool *" | "char *" | "CHAR *" | "short *" | "int *" | "long *" | "float *" | "double *" =>
          out.append("ret = clSetKernelArg(kernel_"+id+","+argIdx+", sizeof(cl_mem), (void *)&"+sym+");\n")
          argIdx = argIdx + 1
        case "bool" | "char" | "CHAR" | "short" | "int" | "long" | "float" | "double" =>
          out.append("ret = clSetKernelArg(kernel_"+id+","+argIdx+", sizeof(" + tp + "), (void *)&"+sym+");\n")
          argIdx = argIdx + 1
        case _ => throw new Exception("purArg: Cannot put argument of type :" + tp)
      }
    }

    out.append("cl_kernel kernel_" + id + " = clCreateKernel(program, \"" + kernelName(op) + id + "\",&ret);\n")
    out.append("if(ret != CL_SUCCESS) printf(\"ERROR during clCreateKernel\\n\");\n")
    //out.append("else printf(\"Success during clCreateKernel\\n\");\n")

    out.append("size_t local_size_" + id + " = " + blockSize + ";\n")
    out.append("size_t num_blocks_" + id + " = " + dimSize(op.size) + ";\n")
    out.append("size_t global_size_" + id + " = local_size_" + id + " * num_blocks_" + id + ";\n")


    //"ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&a_mem_obj);"
    val metadata = op.getGPUMetadata(target)

    id match {
      case "Cond" =>
        //out.append("if(" + dimSize(op.size) +" > 65536) { printf(\"Grid size for GPU is too large!\\n\"); assert(false); }\n")
        for(o <- conditionList(op)) { putArg("int *","bitmap_"+o._2); }
        for((in,sym) <- op.getInputs) {
          if (metadata.inputs.contains((in,sym))) putArg(metadata.inputs((in,sym)).resultType,deref(in,sym)+sym+(if(needDeref(op,in,sym)) "_ptr" else ""))
          else putArg(getCPrimitiveType(in.outputType(sym)),sym)
        }
        out.append(clLaunch)
      case "MapReduce" | "HashReduce1"=>
        //out.append("if(" + dimSize(op.size) +" > 65536) { printf(\"Grid size for GPU is too large!\\n\"); assert(false); }\n")
        op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).foreach(o => putArg(op.outputType(Targets.OpenCL,o._2),"**"+o._2))
        for(o <- conditionList(op)) { putArg("int *","bitmap_"+o._2); putArg("int *","scanmap_"+o._2); }
        for(o <- reductionList(op)++hashReductionList(op)) { putArg(o._1.loopFuncOutputType+" *","temp_"+o._2); putArg(o._1.loopFuncOutputType+" *","temp_"+o._2+"_2"); }
        for(o <- reductionTupleList(op)) { putArg(o._1.loopFuncOutputType+" *","temp_1_"+o._2); putArg(o._1.loopFuncOutputType+" *","temp_1_"+o._2+"_2"); putArg(o._1.loopFuncOutputType_2+" *","temp_2_"+o._2); putArg(o._1.loopFuncOutputType_2+" *","temp_2_"+o._2+"_2"); }
        for((in,sym) <- op.getInputs) {
          if (metadata.inputs.contains((in,sym))) putArg(metadata.inputs((in,sym)).resultType,deref(in,sym)+sym+(if(needDeref(op,in,sym)) "_ptr" else ""))
          else putArg(getCPrimitiveType(in.outputType(sym)),sym)
        }
        out.append(clLaunch)
      case "Reduce" | "ReduceTuple" | "HashReduce2" =>
        //out.append("int num_blocks_" + id + " = " + dimSize(op.size) + ";\n")
        out.append("while(num_blocks_" + id + " != 1) {\n")
        //out.append(clLaunch(""+dimSize("num_blocks_"+id)+",1,1)"))
        op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).foreach(o => putArg(op.outputType(Targets.OpenCL,o._2),"**"+o._2))
        for(o <- conditionList(op)) { putArg("int *","bitmap_"+o._2); putArg("int *","scanmap_"+o._2); }
        for(o <- reductionList(op)++hashReductionList(op)) { putArg(o._1.loopFuncOutputType+" *","temp_"+o._2); putArg(o._1.loopFuncOutputType+" *","temp_"+o._2+"_2"); }
        for(o <- reductionTupleList(op)) { putArg(o._1.loopFuncOutputType+" *","temp_1_"+o._2); putArg(o._1.loopFuncOutputType+" *","temp_1_"+o._2+"_2"); putArg(o._1.loopFuncOutputType_2+" *","temp_2_"+o._2); putArg(o._1.loopFuncOutputType_2+" *","temp_2_"+o._2+"_2"); }
        for((in,sym) <- op.getInputs) {
          if (metadata.inputs.contains((in,sym))) putArg(metadata.inputs((in,sym)).resultType,deref(in,sym)+sym+(if(needDeref(op,in,sym)) "_ptr" else ""))
          else putArg(getCPrimitiveType(in.outputType(sym)),sym)
        }
        putArg("int", "num_blocks_"+id)

        out.append("num_blocks_" + id + " = " + dimSize("num_blocks_"+id) + ";\n")
        out.append("global_size_" + id + " = local_size_" + id + " * num_blocks_" + id + ";\n")

        out.append(clLaunch)

        for((odata,osym) <- reductionList(op)++hashReductionList(op)) {
          out.append("cl_mem temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append("cl_mem temp_1_" + osym + "_t = temp_1_" + osym + ";\n")
          out.append("temp_1_" + osym + " = temp_1_" + osym + "_2;\n")
          out.append("temp_1_" + osym + "_2 = temp_1_" + osym + "_t;\n")
          out.append("cl_mem temp_2_" + osym + "_t = temp_2_" + osym + ";\n")
          out.append("temp_2_" + osym + " = temp_2_" + osym + "_2;\n")
          out.append("temp_2_" + osym + "_2 = temp_2_" + osym + "_t;\n")
        }
        out.append("}\n")

      case _ => error(id + " is not a known kernel type")
    }
    out.append("clFinish(command_queue);\n")
  }

  private def writeKernelHeader(out: StringBuilder, op: OP_MultiLoop, id: String) {
    out.append("__kernel void ")
    out.append(kernelName(op))
    out.append(id)
    out.append('(')

    id match {
      case "Cond" =>
        val params = conditionList(op).map(o => "__global unsigned int * bitmap_" + o._2)
        out.append(params.mkString(","))
        if (params.nonEmpty && op.getInputs.nonEmpty) out.append(',')
        writeInputs_kernel(out,op)
      case "MapReduce" | "Reduce" | "ReduceTuple" | "HashReduce1" | "HashReduce2" =>
        val params = op.getGPUMetadata(Targets.OpenCL).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => unwrapObjects(op.outputType(Targets.OpenCL,o._2),o._2)) ++ conditionList(op).map(o => "__global unsigned int * bitmap_" + o._2 + ", __global unsigned int * scanmap_" + o._2) ++ (reductionList(op)++hashReductionList(op)).map(o => "__global " + o._1.loopFuncOutputType + " *temp_" + o._2 + ", __global " + o._1.loopFuncOutputType + " *temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => "__global " + o._1.loopFuncOutputType + " *temp_1_" + o._2 + ", __global " + o._1.loopFuncOutputType + " *temp_1_" + o._2 + "_2, __global " + o._1.loopFuncOutputType_2 + " *temp_2_" + o._2 + ", __global " + o._1.loopFuncOutputType_2 + " *temp_2_" + o._2 + "_2")
        out.append(params.mkString(","))
        if (params.nonEmpty && op.getInputs.nonEmpty) out.append(',')
        writeInputs_kernel(out,op)
        if((id=="Reduce") || (id=="ReduceTuple") || (id=="HashReduce2")) {
          out.append(", int size")
        }
      case _ => error(id + " is not a known kernel type")
    }

    out.append(") {\n")
    out.append("int idxX = get_global_id(0);\n")
    out.append("int local_idxX = get_local_id(0);\n")
    out.append("int group_idxX = get_group_id(0);\n")
    out.append("int group_sizeX = get_local_size(0);\n")

    if(needsHashReduction(op)) {
      out.append("int chunkIdx = idxX / MAX_GROUP;\n")
      out.append("int chunkOffset = idxX % MAX_GROUP;\n")
    }
    wrapObjects(out)
    addDeref(out, op)
    allocateSharedMem(out, op)
  }

  /* Allocate shared memory for reduction operation */
  //TODO: Check if the local memory requirement is over the maximum (then need to chunk the kernels)
  private def allocateSharedMem(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op))
      out.append("__local " + odata.loopFuncOutputType + " smem_" + osym + "[1024];\n")
    for((odata,osym) <- reductionTupleList(op)) {
      out.append("__local " + odata.loopFuncOutputType + " smem_1_" + osym + "[1024];\n")
      out.append("__local " + odata.loopFuncOutputType_2 + " smem_2_" + osym + "[1024];\n")
    }
    for((odata,osym) <- hashReductionList(op))
      out.append("__local " + odata.loopFuncOutputType + " smem_" + osym + "[512];\n")
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
    for((odata,osym) <- op.getGPUMetadata(Targets.OpenCL).outputs) {
      odata.loopType match {
        case "COLLECT" =>
          if (odata.hasCond) out.append("if (idxX<" + op.size + " && bitmap_" + osym + "[idxX]==1) {\n")
          else out.append("if (idxX < " + op.size + ") {\n")
          out.append(odata.loopFuncOutputType + " collect_" + osym + " = dev_collect_" + funcNameSuffix(op,osym) + "(" + (odata.loopFuncInputs:+"idxX").mkString(",") + ");\n")
          if(odata.hasCond) out.append(op.outputType(Targets.OpenCL,osym) + "_update(" + osym + ", scanmap_" + osym + "[idxX], collect_" + osym + ");\n")
          else out.append(op.outputType(Targets.OpenCL,osym) + "_update(" + osym + ",idxX, collect_" + osym + ");\n")
          out.append("}\n")
        case "FOREACH" =>
          out.append("if (idxX < " + op.size + ") {\n")
          out.append("dev_foreach_" + funcNameSuffix(op,osym) + "(" + odata.loopFuncInputs.mkString("",",",",") + "idxX);\n")
          out.append("}\n")
        case "REDUCE" =>
          if (odata.hasCond) out.append("smem_" + osym + "[local_idxX] = ((idxX<" + op.size + ") && (bitmap_" + osym + "[idxX]==1)) ? dev_collect_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") +  ": dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
          else out.append("smem_" + osym + "[local_idxX] = (idxX < " + op.size + ") ? dev_collect_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") +  ": dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
          out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
          out.append("for(unsigned int s=1; s<group_sizeX; s*=2) {\n")
          out.append("if((idxX%(2*s))==0) { \n")
          out.append("smem_" + osym + "[local_idxX] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[local_idxX]","smem_"+osym+"[local_idxX+s]","idxX")).mkString("(",",",");\n"))
          out.append("}\n")
          out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
          out.append("}\n")
          out.append("if(local_idxX==0) temp_" + osym + "[group_idxX] = smem_" + osym + "[0];\n")
        case "REDUCE_TUPLE" =>
          if (odata.hasCond) {
            out.append("smem_1_" + osym + "[local_idxX] = ((idxX<" + op.size + ") && (bitmap_" + osym + "[idxX]==1)) ? dev_collect_1_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") + " : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
            out.append("smem_2_" + osym + "[local_idxX] = ((idxX<" + op.size + ") && (bitmap_" + osym + "[idxX]==1)) ? dev_collect_2_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs_2:+"idxX").mkString("(",",",")") + " : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
          }
          else {
            out.append("smem_1_" + osym + "[local_idxX] = (idxX < " + op.size + ") ? dev_collect_1_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"idxX").mkString("(",",",")") + " : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
            out.append("smem_2_" + osym + "[local_idxX] = (idxX < " + op.size + ") ? dev_collect_2_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs_2:+"idxX").mkString("(",",",")") + " : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
          }
          out.append("if(idxX<" + op.size + ") smem_1_" + osym + "[local_idxX] = dev_reduce_seq_1_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs ++ List("dev_zero_1_"+funcNameSuffix(op,osym)+odata.loopZeroInputs.mkString("(",",",")"),"dev_zero_2_"+funcNameSuffix(op,osym)+odata.loopZeroInputs_2.mkString("(",",",")"),"smem_1_"+osym+"[local_idxX]","smem_2_"+osym+"[local_idxX]","idxX")).mkString("(",",",");\n"))
          out.append("if(idxX<" + op.size + ") smem_2_" + osym + "[local_idxX] = dev_reduce_seq_2_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs_2 ++ List("dev_zero_1_"+funcNameSuffix(op,osym)+odata.loopZeroInputs.mkString("(",",",")"),"dev_zero_2_"+funcNameSuffix(op,osym)+odata.loopZeroInputs_2.mkString("(",",",")"),"smem_1_"+osym+"[local_idxX]","smem_2_"+osym+"[local_idxX]","idxX")).mkString("(",",",");\n"))
          out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
          out.append("for(unsigned int s=1; s<group_sizeX; s*=2) {\n")
          out.append("if((idxX%(2*s))==0) { \n")
          out.append("smem_1_" + osym + "[local_idxX] = dev_reduce_par_1_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs ++ List("smem_1_"+osym+"[local_idxX]","smem_2_"+osym+"[local_idxX]","smem_1_"+osym+"[local_idxX+s]","smem_2_"+osym+"[local_idxX+s]","idxX")).mkString("(",",",");\n"))
          out.append("smem_2_" + osym + "[local_idxX] = dev_reduce_par_2_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs_2 ++ List("smem_1_"+osym+"[local_idxX]","smem_2_"+osym+"[local_idxX]","smem_1_"+osym+"[local_idxX+s]","smem_2_"+osym+"[local_idxX+s]","idxX")).mkString("(",",",");\n"))
          out.append("}\n")
          out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
          out.append("}\n")
          out.append("if(local_idxX==0) {\n")
          out.append("temp_1_" + osym + "[group_idxX] = smem_1_" + osym + "[0];\n")
          out.append("temp_2_" + osym + "[group_idxX] = smem_2_" + osym + "[0];\n")
          out.append("}\n")
      }
    }
    writeKernelFooter(out)
  }

  //TODO: Use more efficient reduction algorithm
  private def writeReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "Reduce")
    for((odata,osym) <- reductionList(op)) {
      if (odata.hasCond) out.append("smem_" + osym + "[local_idxX] = ((idxX<size) && (bitmap_" + osym + "[idxX]==1)) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      else out.append("smem_" + osym + "[local_idxX] = (idxX < size) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
      out.append("for(unsigned int s=1; s<group_sizeX; s*=2) {\n")
      out.append("if((idxX%(2*s))==0) { \n")
      out.append("smem_" + osym + "[local_idxX] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[local_idxX]","smem_"+osym+"[local_idxX+s]","idxX")).mkString("(",",",");\n"))
      out.append("}\n")
      out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
      out.append("}\n")
      out.append("if(local_idxX==0) temp_" + osym + "_2[group_idxX] = smem_" + osym + "[0];\n")
    }
    writeKernelFooter(out)
  }

  private def writeReduceTupleKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "ReduceTuple")
    for((odata,osym) <- reductionTupleList(op)) {
      if (odata.hasCond) {
        out.append("smem_1_" + osym + "[local_idxX] = ((idxX<size) && (bitmap_" + osym + "[idxX]==1)) ? temp_1_" + osym + "[idxX] : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
        out.append("smem_2_" + osym + "[local_idxX] = ((idxX<size) && (bitmap_" + osym + "[idxX]==1)) ? temp_2_" + osym + "[idxX] : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
      }
      else {
        out.append("smem_1_" + osym + "[local_idxX] = (idxX < size) ? temp_1_" + osym + "[idxX] : dev_zero_1_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
        out.append("smem_2_" + osym + "[local_idxX] = (idxX < size) ? temp_2_" + osym + "[idxX] : dev_zero_2_" + funcNameSuffix(op,osym) + odata.loopZeroInputs_2.mkString("(",",",");\n"))
      }
      out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
      out.append("for(unsigned int s=1; s<group_sizeX; s*=2) {\n")
      out.append("if((idxX%(2*s))==0) { \n")
      out.append("smem_1_" + osym + "[local_idxX] = dev_reduce_par_1_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs ++ List("smem_1_"+osym+"[local_idxX]","smem_2_"+osym+"[local_idxX]","smem_1_"+osym+"[local_idxX+s]","smem_2_"+osym+"[local_idxX+s]","idxX")).mkString("(",",",");\n"))
      out.append("smem_2_" + osym + "[local_idxX] = dev_reduce_par_2_" + funcNameSuffix(op,osym) + (odata.loopReduceParInputs_2 ++ List("smem_1_"+osym+"[local_idxX]","smem_2_"+osym+"[local_idxX]","smem_1_"+osym+"[local_idxX+s]","smem_2_"+osym+"[local_idxX+s]","idxX")).mkString("(",",",");\n"))
      out.append("}\n")
      out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
      out.append("}\n")
      out.append("if(local_idxX==0) {\n")
      out.append("temp_1_" + osym + "_2[group_idxX] = smem_1_" + osym + "[0];\n")
      out.append("temp_2_" + osym + "_2[group_idxX] = smem_2_" + osym + "[0];\n")
      out.append("}\n")
    }
    writeKernelFooter(out)
  }

  private def writeHashReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "HashReduce1")
    for((odata,osym) <- hashReductionList(op)) {
      out.append("int groupIdx_" + osym + " = (chunkIdx<" + op.size + ") ? dev_keyFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs_2:+"chunkIdx").mkString("(",",",")") +  ": -1;\n")
    }
    for((odata,osym) <- hashReductionList(op)) {
      if (odata.hasCond) out.append("smem_" + osym + "[local_idxX] = dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      else out.append("smem_" + osym + "[local_idxX] = dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
    }
    for((odata,osym) <- hashReductionList(op)) {
      if (odata.hasCond) out.append("if(groupIdx_" + osym + "==chunkOffset && dev_cond_" + funcNameSuffix(op,osym) + "(" + (odata.loopCondInputs:+"chunkIdx").mkString(",") + ")) smem_" + osym + "[local_idxX] = dev_valFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"chunkIdx").mkString("(",",",");\n"))
      else out.append("if(groupIdx_" + osym + "==chunkOffset) smem_" + osym + "[local_idxX] = dev_valFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"chunkIdx").mkString("(",",",");\n"))
    }
    out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
    out.append("for(unsigned int s=1; s<group_sizeX/MAX_GROUP; s*=2) {\n")
    out.append("if((chunkIdx%(2*s))==0) { \n")
    for((odata,osym) <- hashReductionList(op)) {
      out.append("smem_" + osym + "[local_idxX] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[local_idxX]","smem_"+osym+"[local_idxX+s*MAX_GROUP]","chunkIdx")).mkString("(",",",");\n"))
    }
    out.append("}\n")
    out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
    out.append("}\n")
    out.append("if(local_idxX / MAX_GROUP == 0) { \n")
    for((odata,osym) <- hashReductionList(op)) {
      out.append("temp_" + osym + "[group_idxX*MAX_GROUP+local_idxX] = smem_" + osym + "[local_idxX];\n")
    }
    out.append("}\n")
    writeKernelFooter(out)

    writeKernelHeader(out, op, "HashReduce2")
    for((odata,osym) <- hashReductionList(op)) {
      if (odata.hasCond) {
        out.append("smem_" + osym + "[local_idxX] = ((chunkIdx<size) && dev_cond_" + funcNameSuffix(op,osym) + "(" + (odata.loopCondInputs:+"chunkIdx").mkString(",") + ")) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      }
      else {
        out.append("smem_" + osym + "[local_idxX] = (chunkIdx < size) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.loopZeroInputs.mkString("(",",",");\n"))
      }
      out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
      out.append("for(unsigned int s=1; s<group_sizeX/MAX_GROUP; s*=2) {\n")
      out.append("if((chunkIdx%(2*s))==0) { \n")
      out.append("smem_" + osym + "[local_idxX] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.loopReduceInputs++List("smem_"+osym+"[local_idxX]","smem_"+osym+"[local_idxX+s*MAX_GROUP]","idxX")).mkString("(",",",");\n"))
      out.append("}\n")
      out.append("barrier(CLK_LOCAL_MEM_FENCE);\n")
      out.append("}\n")
      out.append("if(local_idxX / MAX_GROUP == 0) temp_" + osym + "_2[group_idxX*MAX_GROUP+local_idxX] = smem_" + osym + "[local_idxX];\n")
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
        if(odata.inputs.nonEmpty && (odata.loopType=="COLLECT"||odata.loopType=="HASH_REDUCE")) out.append(',')
        if(odata.loopType=="HASH_REDUCE") out.append("MAX_GROUP")
        if(odata.loopType=="COLLECT" && !odata.hasCond) out.append(op.asInstanceOf[OP_MultiLoop].size)
        else if(odata.loopType=="COLLECT" && odata.hasCond) out.append("*" + osym + "_size_ptr")
        out.append(");\n")
        out.append("clMemoryMap->insert(pair<void*,list<cl_mem>*>(")
        out.append("*" + osym)
        out.append(",lastAlloc));\n")
        out.append("lastAlloc = new list<cl_mem>();\n")
      }
  }

  private def writeCopyBackKernel(out: StringBuilder, op: OP_MultiLoop) {
			/*
    for((odata,osym) <- conditionList(op)) {
      out.append("unsigned int *%s_size_ptr;\n".format(osym))
      out.append("DeliteOpenCLMallocHost((void**)&%s_size_ptr,2*sizeof(unsigned int));\n".format(osym))
      out.append("DeliteOpenCLMemcpyDtoHAsync((void*)%s_size_ptr,scanmap_%s+%s-1,sizeof(unsigned int));\n".format(osym,osym,op.size))
      out.append("DeliteOpenCLMemcpyDtoHAsync((void*)(%s_size_ptr+1),bitmap_%s+%s-1,sizeof(unsigned int));\n".format(osym,osym,op.size))
      out.append("*%s_size_ptr = *%s_size_ptr + *(%s_size_ptr+1);\n".format(osym,osym,osym))
    }
		*/
  }

  private def writeCopyBackReduceKernel(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op)) {
      out.append("DeliteOpenCLMemcpyDtoDAsync((void*)(*%s),temp_%s,sizeof(%s));\n".format(osym,osym,odata.loopFuncOutputType))
    }
    for((odata,osym) <- reductionTupleList(op)) {
      out.append("DeliteOpenCLMemcpyDtoDAsync((void*)(*%s),temp_1_%s,sizeof(%s));\n".format(osym,osym,odata.loopFuncOutputType))
    }
    for((odata,osym) <- hashReductionList(op)) {
      out.append("DeliteOpenCLMemcpyDtoDAsync(((**%s).data),temp_%s,sizeof(%s)*MAX_GROUP);\n".format(osym,osym,odata.loopFuncOutputType))
    }
  }

  private def writeScanKernel(out: StringBuilder, op: OP_MultiLoop) {
			/*
    //exclusive scan
    for ((odata,osym) <- conditionList(op)) {
      out.append("thrust::device_ptr<unsigned int> bitmap_" + osym + "_thrust(bitmap_" + osym + ");\n")
      out.append("thrust::device_ptr<unsigned int> scanmap_" + osym + "_thrust(scanmap_" + osym + ");\n")
      out.append("thrust::exclusive_scan(bitmap_" + osym + "_thrust, bitmap_" + osym + "_thrust+" + op.size + ", scanmap_" + osym + "_thrust);\n")
    }
		*/
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
        out.append("cl_mem " + name + osym + " = DeliteOpenCLMalloc(" + op.size + "*sizeof(unsigned int));\n")
      }
    }
  }

  // Allocate temporary outputs for reduction operations
  private def allocateTemps(out: StringBuilder, op: OP_MultiLoop) {
    for ((odata,osym) <- reductionList(op)) {
      //out.append(odata.loopFuncOutputType + " *temp_" + osym + ";\n")
      out.append("cl_mem temp_" + osym + " = DeliteOpenCLMalloc(" + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      //out.append(odata.loopFuncOutputType + " *temp_" + osym + "_2;\n")
      out.append("cl_mem temp_" + osym + "_2 = DeliteOpenCLMalloc(" + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
    }
    for ((odata,osym) <- reductionTupleList(op)) {
      //out.append(odata.loopFuncOutputType + " *temp_1_" + osym + ";\n")
      out.append("cl_mem temp_1_" + osym + " = DeliteOpenCLMalloc(" + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      //out.append(odata.loopFuncOutputType + " *temp_1_" + osym + "_2;\n")
      out.append("cl_mem temp_1_" + osym + "_2 = DeliteOpenCLMalloc(" + op.size + "*sizeof(" + odata.loopFuncOutputType + "));\n")
      //out.append(odata.loopFuncOutputType_2 + " *temp_2_" + osym + ";\n")
      out.append("cl_mem temp_2_" + osym + " = DeliteOpenCLMalloc(" + op.size + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
      //out.append(odata.loopFuncOutputType_2 + " *temp_2_" + osym + "_2;\n")
      out.append("cl_mem temp_2_" + osym + "_2 = DeliteOpenCLMalloc(" + op.size + "*sizeof(" + odata.loopFuncOutputType_2 + "));\n")
    }
    for ((odata,osym) <- hashReductionList(op)) {
      //out.append(odata.loopFuncOutputType + " *temp_" + osym + ";\n")
      out.append("cl_mem temp_" + osym + " = DeliteOpenCLMalloc((1+(" + op.size + "-1)/(512/MAX_GROUP))*sizeof(" + odata.loopFuncOutputType + ")*MAX_GROUP);\n")
      //out.append(odata.loopFuncOutputType + " *temp_" + osym + "_2;\n")
      out.append("cl_mem temp_" + osym + "_2 = DeliteOpenCLMalloc((1+(" + op.size + "-1)/(512/MAX_GROUP))*sizeof(" + odata.loopFuncOutputType + ")*MAX_GROUP);\n")
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

  private def writeInputs_kernel(out: StringBuilder, op: OP_MultiLoop) {
    var first = true
    val metadata = op.getGPUMetadata(target)

    for ((in, sym) <- op.getInputs) {
      if (metadata.inputs.contains((in,sym))) {
        if (!first) out.append(", ")
        first = false
        out.append(unwrapObjects(metadata.inputs((in,sym)).resultType,sym))
        //out.append(" " + sym)
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

  def unwrapObjects(tp: String, sym: String): String = {
    tp match {
      case "DeliteArray_bool" | "DeliteArray_char" | "DeliteArray_CHAR" | "DeliteArray_short" | "DeliteArray_int" | "DeiteArray_long" | "DeliteArray_float" | "DeliteArray_double" =>
        unwrappedList.append((tp,sym))
        "__global " + tp.drop(12) + " *" + sym + "_data, int " + sym + "_length"
      case _ => throw new Exception("unwrapObjects: Cannot unwrap given type " + tp)
    }
  }

  def wrapObjects(out: StringBuilder) {
    unwrappedList.foreach(o => out.append(o._1 + " " + o._2 + "; " + o._2 + ".data = " + o._2 + "_data; " + o._2 + ".length = " + o._2 + "_length;\n"))
    unwrappedList.clear
  }

  override def getSymGPU(name: String) = name

}
