package ppl.delite.runtime.codegen.kernels.cuda

import tools.nsc.io._
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.{OPData, Targets, TempAlloc}
import collection.mutable.{ArrayBuffer,HashSet}
import ppl.delite.runtime.codegen.sync.JNIFuncs
import ppl.delite.runtime.codegen.{CppCompile, CppExecutableGenerator, CudaExecutableGenerator, CudaCompile}

// TODO: Optimizations
// 1. For Reduce/TupleReduce/HashReduce, remove unnecessary scan operation (only used for Collection type).
//   For those cases the condition kernel does not have to be called separately: Just do in place with HashReduce.
//   This will remove the memory allocations for bitmap and related memory accesses.
// 2. Combine all condition checks and reduction templates for each symbol into a single one within a MultiLoop kernel.

object MultiLoop_GPU_Array_Generator extends JNIFuncs {

  val target = Targets.Cuda

  private val keyType = "int"  //TODO: Get from DEG metadata

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
  /*
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
  */

  def useMultiDimMapping(op: OP_MultiLoop) = getMapping(op).length > 0

  private def collectList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.elemType.startsWith("COLLECT"))
  }
  private def foreachList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.elemType=="FOREACH")
  }
  private def reductionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.elemType=="REDUCE")
  }
  private def reductionSpecList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.elemType=="REDUCE_SPEC")
  }
  private def reductionTupleList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.elemType=="REDUCE_TUPLE")
  }
  private def hashReductionList(op: OP_MultiLoop): List[(OPData,String)] = {
    op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.elemType=="HASH_REDUCE" || o._1.elemType=="HASH_REDUCE_SPEC")
  }
  private def hashReductionListPerKey(op: OP_MultiLoop): Set[List[String]] = {
    val keyGroup = new HashSet[List[String]]
    for(o <- hashReductionList(op)) {
      keyGroup.add(o._1.getInputs("key"))
    }
    keyGroup.toSet
  }
  // Separate condition kernel only needed for COLLECT with condition (i.e., need scan phase later)
  //private def conditionList(op: OP_MultiLoop): List[(OPData,String)] = {
  //  op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.elemType=="COLLECT_BUF")
    //op.getGPUMetadata(Targets.Cuda).outputs.filter(o => o._1.hasCond)
  //}

  private def tempAllocs(op: OP_MultiLoop): List[TempAlloc] = {
    op.getGPUMetadata(Targets.Cuda).temps
  }

  private def needDeref(op:OP_MultiLoop, in: DeliteOP, sym:String): Boolean = {
    if(isPrimitiveType(in.outputType(sym))) {
      in match {
        case n:OP_Nested => false // referentialPrimitive is returned as a normal primitive type from nested OPs (converted internally)
        case i:OP_Input if(i.op.isInstanceOf[OP_Nested]) => false
        case i:OP_Input if(i.op.scheduledResource == op.scheduledResource) => true
        case _ if(in.scheduledResource == op.scheduledResource) => true
        case _ => false
      }
    }
    else 
      false 
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
    out.append("#include \"" + target + "helperFuncs.h\"\n")
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
    out.append("#include \"" + target + "helperFuncs.h\"\n")
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
    out.append(getInputParams(op,true).mkString(","))
    out.append(", int size")
    out.append(")\n")
  }

  private def writeFooter(out: StringBuilder) {
    out.append("}\n")
  }

  private def makeKernels(out: StringBuilder, op: OP_MultiLoop) {
    
    writeProcessKernel(out, op)

    if(op.needsPostProcess)
      writePostProcessKernel(out, op)
    if(op.needsCombine) {
      writeCombineKernel(out,op)
      writeReduceSpecKernel(out,op)
      writeReduceSpecKernel2(out,op)
      writeCopyBackReduceKernel(out, op)
    }
    if(hashReductionList(op).filter(o => o._1.elemType=="HASH_REDUCE").nonEmpty)
      writeHashReduceKernel(out,op,false)
    if(hashReductionList(op).filter(o => o._1.elemType=="HASH_REDUCE_SPEC").nonEmpty)
      writeHashReduceKernel(out,op,true)
  }

  // emit the main multiloop method that is called in execution plans
  private def writeKernelLauncher(out: StringBuilder, op: OP_MultiLoop) {
    writeLauncherHeader(out, op)
    out.append("{\n")

    // declare & initialize activation record
    out.append("activation_" + op.id + " act;\n")
    out.append("memset((void*)&act, 0, sizeof(activation_"+op.id+"));\n")
    out.append("act.size = " + opSize + ";\n")
    
    // Initialize temporary memory for this kernel
    out.append("tempCudaMemReset();\n")
    out.append("int loopIdx = 0;\n")
    
    // allocate temporary memories
    makeTemps(out, op)

    // split output symbols depending on whether it could be allocated in advance or not
    val (output_dynamic, output_static) = op.getGPUMetadata(target).outputs.partition(o => o._1.elemType=="COLLECT_BUF")
    
    // allocate outputs for non-buffer type loops
    writeOutputAllocs(out, op, output_static++output_dynamic) // TODO: remove output_dynamic from the list

    // call process kernel
    writeKernelCall(out, op, "Process")
    
    // call scan and copy back kernel for buffer type loops
    writeScanKernel(out, op)
    writeCopyBackKernel(out, op)
    
    // allocate outputs for buffer type loops (size is only known after scan kernel)
    writeOutputAllocs(out, op, output_dynamic)

    if(op.needsPostProcess)
      writeKernelCall(out, op, "PostProcess")

    //TODO: put guard
    writeHashReducePreKernelCall(out, op)

    if(op.needsCombine) { 
      writeKernelCall(out, op, "Combine")
    }

    if(hashReductionList(op).filter(o => o._1.elemType=="HASH_REDUCE").nonEmpty)
      writeKernelCall(out, op, "HashReduce")
    
    if(hashReductionList(op).filter(o => o._1.elemType=="HASH_REDUCE_SPEC").nonEmpty)
      writeKernelCall(out, op, "HashReduceSpec")

    writeCopyBackReduceKernelCall(out, op)
  
    if(reductionSpecList(op).nonEmpty)      
      writeKernelCall(out, op, "ReduceSpec")

    /*
    for ((odata,osym) <- reductionSpecList(op)) {
      out.append("loopIdx = 0;\n")
      out.append(odata.getType("mA") + " result_" +  osym + ";\n")
      out.append("addEvent(kernelStream, d2hStream);\n")
      out.append("DeliteCudaMemcpyDtoHAsync((void*)&result_"+osym+",(void*)tempIn_"+osym+",sizeof("+odata.getType("mA")+"));\n")
      out.append("*" + osym + " = result_" + osym + ".dc_alloc();\n")
      out.append("while(loopIdx < result_" + osym + ".dc_size()) {\n")
      writeKernelCall(out, op, "ReduceSpecKernel1")
      writeKernelCall(out, op, "ReduceSpecKernel2")
      out.append("loopIdx += 1;\n")
      out.append("}\n")
    }
    */

    // call finalizer to create the result symbol
    writeFinalizer(out, op)

    writeFooter(out)
  }

  private def blockSizeConfig(op: OP_MultiLoop): String = if(op.needsCombine) "256" else "512"

  private def opSize = "size"

  private def getMapping(op: OP_MultiLoop) = {
    op.getGPUMetadata(Targets.Cuda).mapping
  }

  // New implementation of writeKernelCall
  private def writeKernelCall(out: StringBuilder, op: OP_MultiLoop, id: String) {
    val funcName = kernelName(op) + id

    def deref(op: DeliteOP, tp: String) = if (isPrimitiveType(op.outputType(tp))) "" else "*"
    val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => if(o._1.elemType.startsWith("COLLECT")) "*act."+o._2+"_data" else "*act."+o._2) ++
    //reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionSpecList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2, tempIn_" + o._2) ++
    //reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++
    //hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", " + o._2 + "_hash_data, offset_" + o._2 + ", idx_" + o._2) ++
    op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++
    //tempAllocs(op).map(t => t.sym) ++
    List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx, act")

    val asserts = ArrayBuffer[String]()

    //TODO: get below parameters values at runtime by calling CUDA API
    val MAX_GRID_DIM = List(65535,65535,65535)
    val MAX_BLOCK_DIM = List(1024,1024,64)
    val MAX_THREADS = 1024

    // a grid can have x, y, and z dimension
    val dimSize = List("x","y","z") map { dim =>
      getMapping(op).find(_.dim == dim) match {
        case Some(m) if(m.spanTpe=="one") =>
          // span type one will launch multiple thread blocks
          // TODO: handle when the size is too big
          val maxGridDim = MAX_GRID_DIM(dim match {case "x" => 0; case "y" => 1; case "z" => 2; case _ => throw new RuntimeException("unknown dim"+dim);})
          "min(" + maxGridDim + ",1 + (" + m.spanSize + " - 1)/" + m.size + ")"
        case _ => "1"
      }
    }

    // a thread block can have x, y, and z dimension
    val blockSize = List("x","y","z") map { dim =>
      getMapping(op).find(_.dim == dim) match {
        case Some(m) => m.size
        case _ => "1"
      }
    }

    // write kernel call
    id match {
      case "Process" if useMultiDimMapping(op) =>
        out.append(dimSize.zipWithIndex.map(e => "(" + e._1 + "<=" + MAX_GRID_DIM(e._2) + ")").mkString("if(!(","&&",")) printf(\"[WARNING] Grid size too big..\\n\");\n"))
        out.append(blockSize.zipWithIndex.map(e => "(" + e._1 + "<=" + MAX_BLOCK_DIM(e._2) + ")").mkString("assert(","&&",");\n"))
        out.append(blockSize.mkString("assert((","*",")<="+MAX_THREADS+");\n"))
        out.append(funcName + "<<<" + dimSize.mkString("dim3(",",",")") + "," + blockSize.mkString("dim3(",",",")") + ",0,kernelStream>>>")
        out.append(args.mkString("(",",",");\n"))
      case _ if useMultiDimMapping(op) =>
        assert(false, "Runtime codegen for multi-dim mapping other than process function is not implemented")
      case _ => writeKernelCall_old(out, op, id)
    }
  }

  // Old implementation of writeKernelCall
  private def writeKernelCall_old(out: StringBuilder, op: OP_MultiLoop, id: String) {
    val blockSize = blockSizeConfig(op) 

    def dimSize(size: String) = //if(id=="HashReduce1" || id=="HashReduce2") "1 +((" + size + "-1)/(" + blockSize + "/MAX_GROUP))"
                                if(id=="HashReduce" || id=="HashReduceSpec") size
                                else if(op.needsCombine && id!="PostProcess") "1 +((" + size + "-1)/" + blockSize + "/2)"
                                else "1 +((" + size + "-1)/" + blockSize + ")"
    def deref(op: DeliteOP, tp: String) = if (isPrimitiveType(op.outputType(tp))) "" else "*"
    def cudaLaunch(dimConfig: String): String = {
      val str = new StringBuilder
      str.append(kernelName(op))
      str.append(id)
      str.append("<<<dim3(") //kernel dimensions
      /*
      if(op.getGPUMetadata(Targets.Cuda).mapping.length > 0) {
        for(dim <- List("x","y")) {
          op.getGPUMetadata(Targets.Cuda).mapping.find(_.dim==dim) match {
            case Some(m) if(m.spanTpe=="one") => str.append("1 + (" + m.spanSize + " - 1)/" + m.size + ",")
            case _ => str.append("1,")
          }
        }
        str.append("1")
      }
      else
      */
        str.append(dimConfig)
      str.append("),dim3(")
      /*
      if(op.getGPUMetadata(Targets.Cuda).mapping.length > 0) {
        str.append(op.getGPUMetadata(Targets.Cuda).mapping.find(_.dim=="x").get.size)
        str.append(",")
        str.append(op.getGPUMetadata(Targets.Cuda).mapping.find(_.dim=="y").get.size)
        str.append(",1")
      }
      else {
        */
        str.append(blockSize)
        str.append(",1,1")
      //}
      str.append("),0,")
      str.append("kernelStream")
      str.append(">>>")
      str.toString
    }

    id match {
      case "Process" | "ReduceSpecKernel1" | "HashReduce1" | "PostProcess" =>
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => if(o._1.elemType.startsWith("COLLECT")) "*act."+o._2+"_data" else "*act."+o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionSpecList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2, tempIn_" + o._2) ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", " + o._2 + "_hash_data, offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ tempAllocs(op).map(t => t.sym) ++ List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx, act")
        if (op.needsPostProcess) {
          out.append(cudaLaunch(dimSize(opSize)+",1,1"))
          out.append(args.mkString("(",",",");\n"))
        }
        else if (op.needsCombine) { 
          out.append(cudaLaunch("64,1,1"))
          out.append(args.mkString("(",",",");\n"))
        }
        else {
          //out.append("if(" + dimSize(opSize) +" > 65536) { printf(\"Kernel Launch Failure: Grid size %d for GPU is too large even with maximum blockSize " + blockSize + "!\\n\"," + dimSize(op.size) + "); assert(false); }\n")
          out.append("if(" + dimSize(opSize) + " > 65535) {\n")
          out.append(cudaLaunch("65535,1,1"))
          out.append(args.mkString("(",",",");\n"))
          out.append("}\n")
          out.append("else {\n")
          out.append(cudaLaunch(dimSize(opSize)+",1,1"))
          out.append(args.mkString("(",",",");\n"))
          out.append("}\n")
        }
      case "Combine" | "ReduceSpecKernel2" =>
        out.append("int num_blocks_" + id + " = min(64," + dimSize(opSize) + ");\n")
        out.append(cudaLaunch("1,1,1"))
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => if(o._1.elemType.startsWith("COLLECT")) "*act."+o._2+"_data" else "*act."+o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionSpecList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2, tempIn_" + o._2) ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", " + o._2 + "_hash_data, offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ tempAllocs(op).map(t => t.sym) ++ List("num_blocks_"+id+", tempMemSize, tempMemPtr, tempMemUsage, loopIdx, act")
        out.append(args.mkString("(",",",");\n"))
        for((odata,osym) <- reductionList(op)) {
          out.append(odata.getType("mA") + " *temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        for((odata,osym) <- reductionSpecList(op)) {
          out.append(odata.getType("dmR") + " *temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append(odata.getType("mA") + " *temp1_" + osym + "_t = temp1_" + osym + ";\n")
          out.append("temp1_" + osym + " = temp1_" + osym + "_2;\n")
          out.append("temp1_" + osym + "_2 = temp1_" + osym + "_t;\n")
          out.append(odata.getType("mB") + " *temp2_" + osym + "_t = temp2_" + osym + ";\n")
          out.append("temp2_" + osym + " = temp2_" + osym + "_2;\n")
          out.append("temp2_" + osym + "_2 = temp2_" + osym + "_t;\n")
        }
      case "HashReduce2" =>
        out.append("int num_blocks_" + id + " = " + dimSize(opSize) + ";\n")
        out.append("while(num_blocks_" + id + " != 1) {\n")
        out.append(cudaLaunch(dimSize("num_blocks_"+id)+",1,1"))
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => if(o._1.elemType.startsWith("COLLECT")) "*act."+o._2+"_data" else "*act."+o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", " + o._2 + "_hash_data, offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ List("num_blocks_"+id) ++ List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx, act")
        out.append(args.mkString("(",",",");\n"))
        out.append("num_blocks_" + id + " = " + dimSize("num_blocks_"+id) + ";\n")
        for((odata,osym) <- hashReductionList(op)) {
          out.append(odata.getType("mV") + " *temp_" + osym + "_t = temp_" + osym + ";\n")
          out.append("temp_" + osym + " = temp_" + osym + "_2;\n")
          out.append("temp_" + osym + "_2 = temp_" + osym + "_t;\n")
        }
        out.append("}\n")
      case "HashReduce" | "HashReduceSpec" =>
        //TODO: Call one kernel per group
        if(id == "HashReduce") {
          val osym = hashReductionList(op).filter(_._1.elemType == "HASH_REDUCE")(0)._2
          out.append(cudaLaunch(dimSize("act."+osym+"_numKeys,1,1")))
        }
        else {
          val osym = hashReductionList(op).filter(_._1.elemType == "HASH_REDUCE_SPEC")(0)._2
          out.append(cudaLaunch(dimSize("act."+osym+"_numKeys"+",res_"+osym+".dc_size(),1")))
        }
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => if(o._1.elemType.startsWith("COLLECT")) "*act."+o._2+"_data" else "*act."+o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", " + o._2 + "_hash_data, offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ tempAllocs(op).map(t => t.sym) ++ List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx, act")
        out.append(args.mkString("(",",",");\n"))
      case "ReduceSpec" =>
        val osym = reductionSpecList(op)(0)._2
        out.append(cudaLaunch("result_"+osym+".dc_size(),1,1"))
        val args = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => if(o._1.elemType.startsWith("COLLECT")) "*act."+o._2+"_data" else "*act."+o._2) ++ reductionList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2") ++ reductionSpecList(op).map(o => "temp_" + o._2 + ", temp_" + o._2 + "_2, tempIn_" + o._2) ++ reductionTupleList(op).map(o => "temp1_" + o._2 + ", temp1_" + o._2 + "_2, temp2_" + o._2 + ", temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => "key_" + o._2 + ", val_" + o._2 + ", " + o._2 + "_hash_data, offset_" + o._2 + ", idx_" + o._2) ++ op.getInputs.map(i => deref(i._1,i._2) + i._2 + (if(needDeref(op,i._1,i._2)) "_ptr" else "")) ++ tempAllocs(op).map(t => t.sym) ++ List("size, tempMemSize, tempMemPtr, tempMemUsage, loopIdx, act")
        out.append(args.mkString("(",",",");\n"))
      case _ => error(id + " is not a known kernel type")
    }
  }

  private def writeKernelHeader(out: StringBuilder, op: OP_MultiLoop, id: String) {
    out.append("__global__ void ")
    out.append(kernelName(op))
    out.append(id)
    out.append('(')

    val params = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => op.outputType(Targets.Cuda, o._2) + " " + o._2) ++ reductionList(op).map(o => o._1.getType("mA") + " *temp_" + o._2 + "," + o._1.getType("mA") + " *temp_" + o._2 + "_2") ++ (reductionSpecList(op).map(o => o._1.getType("dmR") + " *temp_" + o._2 + "," + o._1.getType("dmR") + " *temp_" + o._2 + "_2," + o._1.getType("mA") + " *tempIn_" + o._2)) ++ reductionTupleList(op).map(o => o._1.getType("mA") + " *temp1_" + o._2 + "," + o._1.getType("mA") + " *temp1_" + o._2 + "_2," + o._1.getType("mB") + " *temp2_" + o._2 + "," + o._1.getType("mB") + " *temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => o._1.getType("mK") + " *key_" + o._2 + "," + o._1.getType("mV") + " *val_" + o._2 + ", " + o._1.getType("mV") + " *" + o._2 + "_hash_data, int *offset_" + o._2 + ", int *idx_" + o._2)
    out.append(params.mkString(","))
    if (params.nonEmpty && op.getInputs.nonEmpty) out.append(',')
    out.append(getInputParams(op,false).mkString(","))
    if (params.nonEmpty || op.getInputs.nonEmpty) out.append(',')
    out.append(List("TEMP_"+op.id+" int size, size_t tempMemSize","char *tempMemPtr","int *tempMemUsage, int loopIdx, activation_"+op.id+" act").mkString(","))
    out.append(") {\n")
    if(op.needsCombine && id != "PostProcess") {
      out.append("int idxX = blockIdx.x * 2 * blockDim.x + threadIdx.x;\n")
      out.append("int blockSize = 256;\n")
      out.append("int gridSize = blockSize * 2 * gridDim.x;\n")
    }
    else {
      out.append("int idxX = blockIdx.x * blockDim.x + threadIdx.x;\n")
      out.append("int x = idxX;\n")
      out.append("int y = blockIdx.y * blockDim.y + threadIdx.y;\n")
    }
    out.append("int tid = threadIdx.x;\n")
    addDeref(out, op)
    allocateSharedMem(out, op)
  }

  private def writeKernelHeader_new(out: StringBuilder, op: OP_MultiLoop, id: String) {
    val funcName = kernelName(op) + id
    val dimVariables = getMapping(op).map(m => "int %1$s = blockIdx.%1$s * blockDim.%1$s + threadIdx.%1$s;".format(m.dim))
    val params = op.getGPUMetadata(Targets.Cuda).outputs.filter(o => !isPrimitiveType(op.outputType(o._2))).map(o => op.outputType(Targets.Cuda, o._2) + " " + o._2) ++ reductionList(op).map(o => o._1.getType("mA") + " *temp_" + o._2 + "," + o._1.getType("mA") + " *temp_" + o._2 + "_2") ++ (reductionSpecList(op).map(o => o._1.getType("dmR") + " *temp_" + o._2 + "," + o._1.getType("dmR") + " *temp_" + o._2 + "_2," + o._1.getType("mA") + " *tempIn_" + o._2)) ++ reductionTupleList(op).map(o => o._1.getType("mA") + " *temp1_" + o._2 + "," + o._1.getType("mA") + " *temp1_" + o._2 + "_2," + o._1.getType("mB") + " *temp2_" + o._2 + "," + o._1.getType("mB") + " *temp2_" + o._2 + "_2") ++ hashReductionList(op).map(o => o._1.getType("mK") + " *key_" + o._2 + "," + o._1.getType("mV") + " *val_" + o._2 + ", " + o._1.getType("mV") + " *" + o._2 + "_hash_data, int *offset_" + o._2 + ", int *idx_" + o._2) ++
                 getInputParams(op,false) ++
                 List("TEMP_"+op.id+" int size, size_t tempMemSize","char *tempMemPtr","int *tempMemUsage, int loopIdx, activation_"+op.id+" act")

    out.append("__global__ void " + funcName)
    out.append(params.mkString("(",",",") {\n"))
    out.append(dimVariables.mkString("","\n","\n"))

    //TODO: enable below
    //This is to add * for the input arguments that are primitive types and gerenated from a GPU kernel (stored in a dev mem ptr)
    //addDeref(out, op)
    //allocateSharedMem(out, op)
  }


  private def writeKernelFooter(out: StringBuilder) {
    out.append("}\n") //end if, end kernel
  }

  private def funcNameSuffix(op: OP_MultiLoop, syms: List[String]): String = {
    op.id + "_" + syms.mkString("")
  }
  
  private def funcNameSuffix(op: OP_MultiLoop, sym: String):String = funcNameSuffix(op, List(sym))

  //TODO: Add temporary allocations here?
  private def lastInputArgs(op: OP_MultiLoop): List[String] = {
    if(op.getGPUMetadata(Targets.Cuda).mapping.length > 0)
      List(opSize) ++ tempAllocs(op).map(_.sym) ++ List("tempMemSize","tempMemPtr","tempMemUsage","act")
    else
      List("idxX",opSize) ++ tempAllocs(op).map(_.sym) ++ List("tempMemSize","tempMemPtr","tempMemUsage","act")
  }

  private def writeProcessKernel(out: StringBuilder, op: OP_MultiLoop) {
    if (useMultiDimMapping(op)) {
      writeKernelHeader_new(out, op, "Process")
      if (op.needsCombine) {
        assert(false, "combine is not implemented for multi-dim mapping")
      }
      else {
        val spanOnes = getMapping(op).filter(_.spanTpe=="one")
        //spanOnes foreach { s =>
        //  out.append("while( " + s.dim + " < " + s.spanSize + ") {\n")
        //}
        for((odata,osym) <- collectList(op)++foreachList(op)) {
          out.append("dev_process_" + funcNameSuffix(op,osym) + "(" + (odata.getInputs("process")++lastInputArgs(op)).mkString(",") + ");\n")
        }
        //spanOnes.reverse foreach { s =>
        //  out.append("%1$s += blockDim.%1$s * gridDim.%1$s;\n".format(s.dim))
        //  out.append("}\n")
        //}
      }
      writeKernelFooter(out)
    }
    else
      writeProcessKernel_old(out, op)
  }

  private def writeProcessKernel_old(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "Process")
    if(op.needsCombine) {    
      for((odata,osym) <- reductionList(op)) {
        out.append(odata.getType("mA") + " localSum_" + osym + " = dev_init_" + funcNameSuffix(op,osym) + (odata.getInputs("init")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append(odata.getType("mA") + " localSum1_" + osym + " = dev_init1_" + funcNameSuffix(op,osym) + (odata.getInputs("init")++lastInputArgs(op)).mkString("(",",",");\n"))
        out.append(odata.getType("mB") + " localSum2_" + osym + " = dev_init2_" + funcNameSuffix(op,osym) + (odata.getInputs("init")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("while( idxX < " + opSize + ") {\n")
      for((odata,osym) <- collectList(op)++foreachList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + "(" + (odata.getInputs("process")++lastInputArgs(op)).mkString(",") + ");\n")
      }
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("tempIn_" + osym + "[idxX] = dev_process_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionList(op)) {
        out.append("localSum_" + osym + " = dev_process_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++List("localSum_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("localSum1_" + osym + " = dev_process1_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
        out.append("localSum2_" + osym + " = dev_process2_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for(keyList <- hashReductionListPerKey(op)) {
        val inputs = keyList flatMap {s => List("key_"+s,"val_"+s)}
        val odata = hashReductionList(op).find(o=>keyList.contains(o._2)).get._1
        out.append("dev_process_" + funcNameSuffix(op,keyList) + (inputs++odata.getInputs("process")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("idxX += blockSize;\n")
      out.append("if (idxX < " + opSize + ") {\n")
      for((odata,osym) <- collectList(op)++foreachList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + "(" + (odata.getInputs("process")++lastInputArgs(op)).mkString(",") + ");\n")
      }
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("tempIn_" + osym + "[idxX] = dev_process_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionList(op)) {
        out.append("localSum_" + osym + " = dev_process_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++List("localSum_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("localSum1_" + osym + " = dev_process1_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
        out.append("localSum2_" + osym + " = dev_process2_" + funcNameSuffix(op,osym) + (odata.getInputs("process")++List("localSum1_"+osym,"localSum2_"+osym)++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      for(keyList <- hashReductionListPerKey(op)) {
        val inputs = keyList flatMap {s => List("key_"+s,"val_"+s)}
        val odata = hashReductionList(op).find(o=>keyList.contains(o._2)).get._1
        out.append("dev_process_" + funcNameSuffix(op,keyList) + (inputs++odata.getInputs("process")++lastInputArgs(op)).mkString("(",",",");\n"))
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
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("volatile " + odata.getType("mA") + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("volatile " + odata.getType("mA") + "* sdata1_" + osym + " = smem1_" + osym + ";\n")
        out.append("volatile " + odata.getType("mB") + "* sdata2_" + osym + " = smem2_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionList(op)) {
          if(isPrimitiveType(op.outputType(osym))) 
            out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          else 
            out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); __syncthreads();"))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          if(isPrimitiveType(op.outputType(osym))) { 
            out.append("sdata1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("sdata2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
          }
          else {
            out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
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
      if(op.getGPUMetadata(Targets.Cuda).mapping.length > 0) {
        out.append("while( " + op.getGPUMetadata(Targets.Cuda).mapping.find(_.level==0).get.dim + " < " + opSize + ") {\n")
      }
      else
        out.append("while (idxX < " + opSize + ") {\n")
      for((odata,osym) <- collectList(op)++foreachList(op)) {
        out.append("dev_process_" + funcNameSuffix(op,osym) + "(" + (odata.getInputs("process")++lastInputArgs(op)).mkString(",") + ");\n")
      }
      if(op.getGPUMetadata(Targets.Cuda).mapping.length > 0)
        out.append("%1$s += blockDim.%1$s * gridDim.%1$s;\n".format(op.getGPUMetadata(Targets.Cuda).mapping.find(_.level==0).get.dim))
      else
        out.append("idxX += blockDim.x * gridDim.x;\n")
      out.append("}\n")
    }
    writeKernelFooter(out)
  }

  private def writePostProcessKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "PostProcess")
    out.append("while(idxX < " + opSize + ") {\n") 
    for((odata,osym) <- collectList(op)) {
      out.append("dev_postprocess_" + funcNameSuffix(op,osym) + "(" + (odata.getInputs("postprocess")++lastInputArgs(op)).mkString(",") + ");\n")
    }
    out.append("idxX += blockDim.x * gridDim.x;\n")
    out.append("}\n")
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
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionList(op)) {
        out.append("volatile " + odata.getType("mA") + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for((odata,osym) <- reductionTupleList(op)) {
        out.append("volatile " + odata.getType("mA") + "* sdata1_" + osym + " = smem1_" + osym + ";\n")
        out.append("volatile " + odata.getType("mB") + "* sdata2_" + osym + " = smem2_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionList(op)) {
          if(isPrimitiveType(op.outputType(osym))) 
            out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          else 
            out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); __syncthreads();"))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          if(isPrimitiveType(op.outputType(osym))) { 
            out.append("sdata1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("sdata2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata1_"+osym+"[threadIdx.x]","sdata2_"+osym+"[threadIdx.x]","sdata1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","sdata2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
          } 
          else {
            out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
            out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))  
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
  private def writeReduceSpecKernel(out: StringBuilder, op: OP_MultiLoop) {
    writeKernelHeader(out, op, "ReduceSpec")  
      out.append("idxX = threadIdx.x;\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append(odata.getType("dmR") + " localSum_" + osym + " = dev_spcinit_" + funcNameSuffix(op,osym) + "();\n")
      }
      out.append("while( idxX < " + opSize + ") {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("localSum_" + osym + " = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("localSum_"+osym,"tempIn_"+osym+"[idxX].dc_apply(blockIdx.x)")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("idxX += blockSize;\n")
      out.append("}\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("smem_" + osym + "[threadIdx.x] = localSum_" + osym + ";\n")
      }
      out.append("__syncthreads();\n")
      for(blockSize <- List(512,256,128)) {
        out.append("if(blockSize >= " + blockSize + ") { if (threadIdx.x < " + blockSize/2 + ") { ")
        for((odata,osym) <- reductionList(op)++reductionSpecList(op)) {
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        for((odata,osym) <- reductionTupleList(op)) {
          out.append("smem1_" + osym + "[threadIdx.x] = dev_combine1_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
          out.append("smem2_" + osym + "[threadIdx.x] = dev_combine2_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem1_"+osym+"[threadIdx.x]","smem2_"+osym+"[threadIdx.x]","smem1_"+osym+"[threadIdx.x+" + blockSize/2 + "]","smem2_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("volatile " + odata.getType("dmR") + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionSpecList(op)) {
          out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append("}\n")
      }
      out.append("}\n")
      out.append("if(threadIdx.x == 0) {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append(osym + ".dc_update(blockIdx.x, smem_" + osym + "[0]);\n")
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
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- reductionSpecList(op)) {
        out.append("volatile " + odata.getType("dmR") + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- reductionSpecList(op)) {
          out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
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
        out.append("__global__ void dc_copy_" + osym + "(" + odata.getType("mA") + " from, " + odata.getType("mA") + " to) {\n")
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
      if (odata.hasCond) out.append("smem_" + osym + "[threadIdx.x] = dev_zero_" + funcNameSuffix(op,osym) + odata.getInputs("init").mkString("(",",",");\n"))
      else out.append("smem_" + osym + "[threadIdx.x] = dev_zero_" + funcNameSuffix(op,osym) + odata.getInputs("init").mkString("(",",",");\n"))
    }
    for((odata,osym) <- hashReductionList(op)) {
      if (odata.hasCond) out.append("if(groupIdx_" + osym + "==chunkOffset && dev_cond_" + funcNameSuffix(op,osym) + "(" + (odata.loopCondInputs:+"chunkIdx").mkString(",") + ")) smem_" + osym + "[threadIdx.x] = dev_valFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"chunkIdx").mkString("(",",",");\n"))
      else out.append("if(groupIdx_" + osym + "==chunkOffset) smem_" + osym + "[threadIdx.x] = dev_valFunc_" + funcNameSuffix(op,osym) + (odata.loopFuncInputs:+"chunkIdx").mkString("(",",",");\n"))
    }
    out.append("__syncthreads();\n")
    out.append("for(unsigned int s=1; s<blockDim.x/MAX_GROUP; s*=2) {\n")
    out.append("if((chunkIdx%(2*s))==0) { \n")
    for((odata,osym) <- hashReductionList(op)) {
      out.append("smem_" + osym + "[threadIdx.x] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+s*MAX_GROUP]","chunkIdx")).mkString("(",",",");\n"))
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
        out.append("smem_" + osym + "[threadIdx.x] = ((chunkIdx<size) && dev_cond_" + funcNameSuffix(op,osym) + "(" + (odata.loopCondInputs:+"chunkIdx").mkString(",") + ")) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.getInputs("init").mkString("(",",",");\n"))
      }
      else {
        out.append("smem_" + osym + "[threadIdx.x] = (chunkIdx < size) ? temp_" + osym + "[idxX] : dev_zero_" + funcNameSuffix(op,osym) + odata.getInputs("init").mkString("(",",",");\n"))
      }
      out.append("__syncthreads();\n")
      out.append("for(unsigned int s=1; s<blockDim.x/MAX_GROUP; s*=2) {\n")
      out.append("if((chunkIdx%(2*s))==0) { \n")
      out.append("smem_" + osym + "[threadIdx.x] = dev_reduce_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+s*MAX_GROUP]","idxX")).mkString("(",",",");\n"))
      out.append("}\n")
      out.append("__syncthreads();\n")
      out.append("}\n")
      out.append("if(threadIdx.x / MAX_GROUP == 0) temp_" + osym + "_2[blockIdx.x*MAX_GROUP+threadIdx.x] = smem_" + osym + "[threadIdx.x];\n")
    }
    writeKernelFooter(out)
  }
  */

  private def writeHashReduceKernel(out: StringBuilder, op: OP_MultiLoop, unrolled: Boolean) {
    val symList = if(unrolled) hashReductionList(op).filter(_._1.elemType=="HASH_REDUCE_SPEC")
                  else hashReductionList(op).filter(_._1.elemType=="HASH_REDUCE")
    if(unrolled) 
      writeKernelHeader(out, op, "HashReduceSpec")
    else 
      writeKernelHeader(out, op, "HashReduce")
    out.append("int start = offset_" + hashReductionList(op)(0)._2 + "[blockIdx.x+1];\n") 
    out.append("int end = offset_" + hashReductionList(op)(0)._2 + "[blockIdx.x+2];\n")
    out.append("idxX = threadIdx.x + start;\n")
      
      for((odata,osym) <- symList) {
        if(unrolled)
          out.append(odata.getType("dmR") + " localSum_" + osym + " = dev_spcinit_" + funcNameSuffix(op,osym) + "();\n")
        else
          out.append(odata.getType("mV") + " localSum_" + osym + " = dev_init_" + funcNameSuffix(op,osym) + (odata.getInputs("init")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("if(idxX < end) {\n")
      for((odata,osym) <- symList) {
        if(unrolled)
          out.append("localSum_" + osym + " = val_" + osym + "[idx_" + osym + "[idxX]].dc_apply(blockIdx.y);\n")
        else 
          out.append("localSum_" + osym + " = val_" + osym + "[idx_" + osym + "[idxX]];\n")
      }
      out.append("idxX += blockSize;\n")
      out.append("while( idxX < end ) {\n")
      for((odata,osym) <- symList) {
        if(unrolled)
          out.append("localSum_" + osym + " = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("localSum_"+osym,"val_"+osym+"[idx_" + osym + "[idxX]].dc_apply(blockIdx.y)")++lastInputArgs(op)).mkString("(",",",");\n"))
        else
          out.append("localSum_" + osym + " = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("localSum_"+osym,"val_"+osym+"[idx_" + osym + "[idxX]]")++lastInputArgs(op)).mkString("(",",",");\n"))
      }
      out.append("idxX += blockSize;\n")
      out.append("}\n")
      out.append("}\n")
      for((odata,osym) <- symList) {
        out.append("smem_" + osym + "[threadIdx.x] = localSum_" + osym + ";\n")
      }
      out.append("__syncthreads();\n")
      for(blockSize <- List(512,256,128)) {
        out.append("if(blockSize >= " + blockSize + ") { if (threadIdx.x < " + blockSize/2 + ") { ")
        for((odata,osym) <- symList) {
          out.append("smem_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("smem_"+osym+"[threadIdx.x]","smem_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append(" } __syncthreads(); }\n")
      }
      out.append("if(threadIdx.x < 32) {\n")
      for((odata,osym) <- symList) {
        if(unrolled)
          out.append("volatile " + odata.getType("dmR") + "* sdata_" + osym + " = smem_" + osym + ";\n")
        else
          out.append("volatile " + odata.getType("mV") + "* sdata_" + osym + " = smem_" + osym + ";\n")
      }
      for(blockSize <- List(64,32,16,8,4,2)) {
        out.append("if (blockSize >= " + blockSize + ") { ")
        for((odata,osym) <- symList) {
          out.append("sdata_" + osym + "[threadIdx.x] = dev_combine_" + funcNameSuffix(op,osym) + (odata.getInputs("combine")++List("sdata_"+osym+"[threadIdx.x]","sdata_"+osym+"[threadIdx.x+" + blockSize/2 + "]")++lastInputArgs(op)).mkString("(",",","); "))
        }
        out.append("}\n")
      }
      out.append("}\n")
      out.append("if(threadIdx.x == 0) {\n")
      for((odata,osym) <- symList) {
        if(unrolled)
          out.append(osym + "_hash_data[blockIdx.x].dc_update(blockIdx.y,smem_" + osym + "[0]);\n")
        else
          out.append(osym + "_hash_data[blockIdx.x] = smem_" + osym + "[0];\n") 
        out.append(osym + "_update" + (List(osym,"blockIdx.x",osym+"_hash_data[blockIdx.x]")++lastInputArgs(op)).mkString("(",",",");\n")) 
      }
      out.append("}\n")
    writeKernelFooter(out)
  }

  private def writeOutputAllocs(out: StringBuilder, op: OP_MultiLoop, outputs: List[(OPData,String)]) {
    for ((odata,osym) <- outputs if op.outputType(target,osym)!="void") {
      // call allocation function
      out.append("alloc_" + osym)
      out.append('(')
      out.append((odata.getInputs("alloc").map(getSymGPU) :+ "act").mkString(","))
      out.append(");\n")
      // TODO: remove below 5 lines
      //if(!isPrimitiveType(op.outputType(osym))) {
      if(collectList(op).map(_._2).contains(osym)) {
        out.append("*" + osym)
        out.append(" = ")
        out.append("act." + osym + "_data;\n")
      }
      // register allocations related to this output symbol
      out.append("cudaMemoryMap->insert(std::pair<void*,std::list<void*>*>(")
      out.append("*" + osym)
      out.append(",lastAlloc));\n")
      out.append("lastAlloc = new std::list<void*>();\n")
    }
  }

  private def writeFinalizer(out: StringBuilder, op: OP_MultiLoop) {
    for ((odata,osym) <- collectList(op)) { 
      // call finalizer function
      out.append("finalizer_" + osym)
      out.append('(')
      out.append((odata.getInputs("finalizer").map(getSymGPU) :+ "act").mkString(","))
      out.append(");\n")
      
      // insert possibly additional allocations to the list
      //out.append("assert(cudaMemoryMap->find(*" + osym + ") != (std::map<void*,std::list<void*>*>::end));\n")
      out.append("std::list<void*> *allocs_" + osym + " = cudaMemoryMap->find(act." + osym + ")->second;\n")
      out.append("allocs_"+osym+"->insert(allocs_"+osym+"->end(),lastAlloc->begin(),lastAlloc->end());\n")
      out.append("lastAlloc = new std::list<void*>();\n")
    }

    for (osym <- op.getOutputs if op.outputType(target,osym)!="void") {
      // set the output symbol from act record
      out.append("*" + osym)
      out.append(" = ")
      out.append("act." + osym + ";\n")
    }
  }

  private def writeCopyBackKernel(out: StringBuilder, op: OP_MultiLoop) {
    out.append("if(" + opSize + " != 0) {\n") //for loop size zero
    for((odata,osym) <- collectList(op) if(odata.elemType == "COLLECT_BUF")) {
      out.append("unsigned int *%s_size_ptr;\n".format(osym))
      out.append("DeliteCudaMallocHost((void**)&%s_size_ptr,2*sizeof(unsigned int));\n".format(osym))
      out.append("DeliteCudaMemcpyDtoHAsync((void*)%s_size_ptr,%s_scanmap+%s-1,sizeof(unsigned int));\n".format(osym,osym,opSize))
      out.append("DeliteCudaMemcpyDtoHAsync((void*)(%s_size_ptr+1),%s_bitmap+%s-1,sizeof(unsigned int));\n".format(osym,osym,opSize))
      out.append("*%s_size_ptr = *%s_size_ptr + *(%s_size_ptr+1);\n".format(osym,osym,osym))
      out.append("act." + osym + "_conditionals = *" + osym + "_size_ptr;\n")
    }
    out.append("}\n")
  }

  private def writeCopyBackReduceKernelCall(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op)) {
      if(isPrimitiveType(op.outputType(osym))) 
        out.append("DeliteCudaMemcpyDtoDAsync((void*)(act.%s),temp_%s,sizeof(%s));\n".format(osym,osym,odata.getType("mA")))
      else {
        out.append(odata.getType("mA") + " result_" + osym + ";\n")
        out.append("addEvent(kernelStream, d2hStream);\n")
        out.append("DeliteCudaMemcpyDtoHAsync((void*)&result_" + osym + ", temp_" + osym + ", sizeof(" + odata.getType("mA") + "));\n")
        out.append("act." + osym + " = result_" + osym + ".dc_alloc();\n")
        out.append("dc_copy_" + osym + "<<<dim3(1,1,1),dim3(1,1,1),0,kernelStream>>>(result_"+osym+",*(act."+osym+"));\n")
      }
    }
    for((odata,osym) <- reductionTupleList(op)) {
      if(isPrimitiveType(op.outputType(osym)))
        out.append("DeliteCudaMemcpyDtoDAsync((void*)(act.%s),temp1_%s,sizeof(%s));\n".format(osym,osym,odata.getType("mA")))
      else {
        out.append(odata.getType("mA") + " result_" + osym + ";\n")
        out.append("addEvent(kernelStream, d2hStream);\n")
        out.append("DeliteCudaMemcpyDtoHAsync((void*)&result_" + osym + ", temp1_" + osym + ", sizeof(" + odata.getType("mA") + "));\n")
        out.append("act." + osym + " = result_" + osym + ".dc_alloc();\n")
        out.append("dc_copy_" + osym + "<<<dim3(1,1,1),dim3(1,1,1),0,kernelStream>>>(result_"+osym+",*(act."+osym+"));\n")
      }
    }
    for((odata,osym) <- reductionSpecList(op)) {
      out.append(odata.getType("mA") + " result_" +  osym + ";\n")
      out.append("addEvent(kernelStream, d2hStream);\n")
      out.append("DeliteCudaMemcpyDtoHAsync((void*)&result_"+osym+",(void*)tempIn_"+osym+",sizeof("+odata.getType("mA")+"));\n")
      out.append("act." + osym + " = result_" + osym + ".dc_alloc();\n")
      out.append("*" + osym + " = act." + osym + ";\n") //TODO: Remove this
    }
  }

  private def writeHashReducePreKernelCall(out: StringBuilder, op: OP_MultiLoop) {   
    for ((odata,osym) <- hashReductionList(op)) {
      out.append("thrust::device_ptr<int> idx_" + osym + "_thrust(idx_" + osym + ");\n")
      out.append("thrust::device_ptr<int> key_" + osym + "_thrust(key_" + osym + ");\n")
      out.append("thrust::sequence(idx_" + osym + "_thrust, idx_" + osym + "_thrust+" + opSize + ");\n")
      out.append("thrust::sort_by_key(key_" + osym + "_thrust, key_" + osym + "_thrust+" + opSize + ", idx_" + osym + "_thrust);\n")
      out.append("kernel_offset<<<dim3(1+(" + opSize + "-1)/1024,1,1),dim3(1024,1,1),0,kernelStream>>>(key_" + osym + ", idx_" + osym + ", offset_" + osym + ", " + opSize + ");\n")
      out.append("addEvent(kernelStream,d2hStream);\n")
      out.append("DeliteCudaMemcpyDtoHAsync((void*)(&act." + osym + "_numKeys), (void*)offset_" + osym + ", sizeof(int));\n")
      out.append("//create temporary output collection\n")
      out.append("DeliteCudaMallocTemp((void**)(&" + osym + "_hash_data), sizeof(" + odata.getType("mV") + ")*act." + osym + "_numKeys);\n")
      //out.append(" *" + osym + " = (*" + osym + ")->dc_alloc(act." + osym + "_numKeys);\n")
      out.append(odata.getType("mV") + " res_" + osym + ";\n")
      if(odata.elemType=="HASH_REDUCE_SPEC") {
        out.append("for(int i=0; i<act." + osym + "_numKeys; i++) {\n")
        out.append("DeliteCudaMemcpyDtoHAsync((void*)&res_" + osym + ", (void*)val_"+osym+", sizeof(" + odata.getType("mV") + "));\n")
        out.append(odata.getType("mV") + " *x = res_" + osym + ".dc_alloc();\n")
        out.append("DeliteCudaMemcpyHtoDAsync((void*)(" + osym + "_hash_data+i), (void*)x, sizeof(" + odata.getType("mV") + "));\n")
        out.append("}\n")
      }
      out.append("addEvent(h2dStream,kernelStream);\n")
    }
    writeOutputAllocs(out, op, hashReductionList(op))
  }

  private def writeScanKernel(out: StringBuilder, op: OP_MultiLoop) {
    //exclusive scan
    for((odata,osym) <- collectList(op) if(odata.elemType == "COLLECT_BUF")) {
      out.append("thrust::device_ptr<unsigned int> " + osym + "_bitmap_thrust(" + osym + "_bitmap);\n")
      out.append("thrust::device_ptr<unsigned int> " + osym + "_scanmap_thrust(" + osym + "_scanmap);\n")
      out.append("thrust::exclusive_scan(" + osym + "_bitmap_thrust, " + osym + "_bitmap_thrust+" + opSize + ", " + osym + "_scanmap_thrust);\n")
    }
  }

  // Allocate & Register temporary memory for filter and reduction operations
  private def makeTemps(out: StringBuilder, op: OP_MultiLoop) {
    allocateMaps(out, op)
    allocateTemps(out, op)
  }

  // Allocate bitmap and scanmap for filter operations
  private def allocateMaps(out: StringBuilder, op: OP_MultiLoop) {
    for (name <- List("_bitmap", "_scanmap")) {
      for((odata,osym) <- collectList(op) if(odata.elemType == "COLLECT_BUF")) {
        out.append("unsigned int * " + osym + name + ";\n")
        out.append("DeliteCudaMallocTemp((void**)&" + osym + name + ", " + opSize + "*sizeof(unsigned int));\n")
        out.append("act." + osym + name + " = " + osym + name + ";\n")
      }
    }
    for((odata,osym) <- collectList(op) if(odata.elemType == "COLLECT_BUF")) {
      out.append("DeliteCudaMemset((void*)" + osym + "_bitmap, 0," + opSize + "*sizeof(unsigned int));\n")
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
      out.append(odata.getType("mA") + " *temp_" + osym + ";\n")
      out.append(odata.getType("mA") + " *temp_" + osym + "_2;\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + ", " + dimSize + "*sizeof(" + odata.getType("mA") + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + "_2, " + dimSize + "*sizeof(" + odata.getType("mA") + "));\n")
    }
    for ((odata,osym) <- reductionSpecList(op)) {
      out.append(odata.getType("mA") + " *tempIn_" + osym + ";\n")
      out.append("DeliteCudaMallocTemp((void**)&tempIn_" + osym + ", max(" + opSize + "+" + blockSizeConfig(op) + ",2*64*256)*sizeof(" + odata.getType("mA") + "));\n")
      out.append(odata.getType("dmR") + " *temp_" + osym + ";\n")
      out.append(odata.getType("dmR") + " *temp_" + osym + "_2;\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + ", " + dimSize + "*sizeof(" + odata.getType("dmR") + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp_" + osym + "_2, " + dimSize + "*sizeof(" + odata.getType("dmR") + "));\n")
    }
    for ((odata,osym) <- reductionTupleList(op)) {
      out.append(odata.getType("mA") + " *temp1_" + osym + ";\n")
      out.append(odata.getType("mA") + " *temp1_" + osym + "_2;\n")
      out.append(odata.getType("mB") + " *temp2_" + osym + ";\n")
      out.append(odata.getType("mB") + " *temp2_" + osym + "_2;\n")
      out.append("DeliteCudaMallocTemp((void**)&temp1_" + osym + ", " + dimSize + "*sizeof(" + odata.getType("mA") + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp1_" + osym + "_2, " + dimSize + "*sizeof(" + odata.getType("mA") + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp2_" + osym + ", " + dimSize + "*sizeof(" + odata.getType("mB") + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&temp2_" + osym + "_2, " + dimSize + "*sizeof(" + odata.getType("mB") + "));\n")
    }
    //TODO: Change for hash-reduce
    for ((odata,osym) <- hashReductionList(op)) {
      out.append(odata.getType("mK") + " *key_" + osym + ";\n")
      out.append(odata.getType("mV") + " *val_" + osym + ";\n")
      out.append(odata.getType("mV") + " *" + osym + "_hash_data;\n")
      out.append("int *offset_" + osym + ";\n")
      out.append("int *idx_" + osym + ";\n")
      out.append("DeliteCudaMallocTemp((void**)&key_" + osym + ", " + opSize + "*sizeof(" + odata.getType("mK") + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&val_" + osym + ", " + opSize + "*sizeof(" + odata.getType("mV") + "));\n")
      out.append("DeliteCudaMallocTemp((void**)&offset_" + osym + ", " + opSize + "*sizeof(int));\n")
      out.append("DeliteCudaMallocTemp((void**)&idx_" + osym + ", " + opSize + "*sizeof(int));\n")
    }
    out.append("int *tempMemUsage;\n")
    out.append("DeliteCudaMallocTemp((void**)&tempMemUsage,sizeof(int)*"+opSize+");\n")
    out.append("DeliteCudaMemset((void*)tempMemUsage,0,sizeof(int)*"+opSize+");\n")
    out.append("size_t tempMemSize = (" + opSize + "==0) ? tempCudaMemAvailable() : tempCudaMemAvailable()/"+opSize+";\n")
    out.append("char *tempMemPtr;\n")
    out.append("DeliteCudaMallocTemp((void**)&tempMemPtr,tempMemSize);\n")
  }

  /* Allocate shared memory for reduction operation */
  //TODO: Check if the local memory requirement is over the maximum (then need to chunk the kernels)
  private def allocateSharedMem(out: StringBuilder, op: OP_MultiLoop) {
    for((odata,osym) <- reductionList(op))
      out.append("__shared__ " + odata.getType("mA") + " smem_" + osym + "[256];\n")
    for((odata,osym) <- reductionSpecList(op))
      out.append("__shared__ " + odata.getType("dmR") + " smem_" + osym + "[256];\n")
    for((odata,osym) <- reductionTupleList(op)) {
      out.append("__shared__ " + odata.getType("mA") + " smem1_" + osym + "[256];\n")
      out.append("__shared__ " + odata.getType("mB") + " smem2_" + osym + "[256];\n")
    }
    for((odata,osym) <- hashReductionList(op).filter(_._1.elemType=="HASH_REDUCE"))
      out.append("__shared__ " + odata.getType("mV") + " smem_" + osym + "[256];\n")
    for((odata,osym) <- hashReductionList(op).filter(_._1.elemType=="HASH_REDUCE_SPEC"))
      out.append("__shared__ " + odata.getType("dmR") + " smem_" + osym + "[256];\n")
  }

  // This method is shared for launcher function generation and kernel function generation.
  // Launcher function wants pointer type input for objects
  // Kernel function wants dereferenced type input for objects (without *)
  // This is indicated by the input 'reference'
  private def getInputParams(op: OP_MultiLoop, reference: Boolean) = {
    op.getInputs.map { i =>
      val (in,sym) = i
      if (!isPrimitiveType(in.outputType(sym))) {
        op.inputType(target,sym) + (if(reference) "*" else "") + " " + sym
      }
      else {
        // This is to handle primitive type inputs stored in GPU device memory (generated by another GPU kernel).
        // The kernel gets a pointer to the location and dereference it at the beginning of the kernel to get the value.
        if(needDeref(op,in,sym)) {
          getCPrimitiveType(in.outputType(sym)) + " *" + sym + "_ptr"
        }
        else {
          getCPrimitiveType(in.outputType(sym)) + " " + sym
        }
      }
    }
  }

  private def getSymGPU(name: String) = name

}
