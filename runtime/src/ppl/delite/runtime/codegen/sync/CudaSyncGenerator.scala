package ppl.delite.runtime.codegen.sync

import ppl.delite.runtime.graph.ops.DeliteOP
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.CudaExecutableGenerator

/* trait CudaSyncGenerator extends CudaExecutableGenerator with JNIFuncs {

  protected def writeEventFunction(out: StringBuilder) {
    out.append("void addEvent(cudaStream_t fromStream, cudaStream_t toStream) {\n")
    out.append("cudaEvent_t event;\n")
    out.append("cudaEventCreateWithFlags(&event, cudaEventDisableTiming);\n")
    out.append("cudaEventRecord(event, fromStream);\n");

    out.append("cudaStreamWaitEvent(toStream, event, 0);\n")

    out.append("cudaEventDestroy(event);\n")
    out.append('}')
    out.append('\n')
  }

  protected def writeHostEventFunction(out: StringBuilder) {
    out.append("cudaEvent_t addHostEvent(cudaStream_t stream) {\n")
    out.append("cudaEvent_t event;\n")
    out.append("cudaEventCreateWithFlags(&event, cudaEventDisableTiming | cudaEventBlockingSync);\n")
    out.append("cudaEventRecord(event, stream);\n");
    out.append("return event;\n")
    out.append('}')
    out.append('\n')
  }

  protected def writeInputCopy(op: DeliteOP, sym: String, function: String, opType: String, out: StringBuilder, firstAlloc: Boolean) {
    //copy data from CPU to GPU
    if(firstAlloc) {
      out.append(opType)
      out.append("* ")
    }
    out.append(getSymGPU(sym))
    out.append(" = ")
    out.append(function)
    out.append('(')
    out.append("env,") //JNI environment pointer
    out.append(getSymCPU(sym)) //jobject
    out.append(");\n")
    writeMemoryAdd(sym)
  }

  protected def writeInputCast(op: DeliteOP, sym: String, out: StringBuilder) {
    if (op.outputType(sym) != "Unit") {
      out.append(getCPrimitiveType(op.outputType(sym))) //C primitive
      out.append(' ')
      out.append(getSymGPU(sym))
      out.append(" = ")
      out.append('(') //cast
      out.append(getCPrimitiveType(op.outputType(sym))) //C primitive
      out.append(')')
      out.append(getSymCPU(sym)) //J primitive
      out.append(';')
      out.append('\n')
    }
  }

  protected def writeGetterFrees(getterList: ArrayBuffer[String], out: StringBuilder) {
    for (g <- getterList) {
      out.append("env->DeleteLocalRef(")
      out.append(g)
      out.append(");\n")
    }
  }

  protected def writeSetters(op: DeliteOP, location: Int, out: StringBuilder) {
    for ((in,name) <- op.getGPUMetadata(target).inputs.keys) {
      if (op.getMutableInputs.contains(in,name)) {
        //copy any mutated inputs from GPU to CPU
        val inData = op.getGPUMetadata(target).inputs(in,name)
        out.append(inData.funcReturn)
        out.append("(env,") //JNI environment pointer
        out.append(getSymCPU(name)) //jobject
        out.append(',')
        out.append(getSymGPU(name)) //C++ object
        out.append(");\n")
      }
    }

    for (name <- op.getOutputs) {
      var deleteLocalRef = false
      op.getGPUMetadata(target).outputs.find(o => (o._2==name)&&(o._1.resultType!="void")) match {
        case Some((data, n)) => {
          //copy output from GPU to CPU
          out.append(getJNIType(op.outputType(name))) //jobject
          out.append(' ')
          out.append(getSymCPU(name))
          out.append(" = ")
          out.append(data.funcReturn)
          out.append('(')
          out.append("env,") //JNI environment pointer
          out.append(getSymGPU(name)) //C++ object
          out.append(");\n")
          if(!isPrimitiveType(op.outputType(name))) deleteLocalRef = true
        }
        case None => //do nothing
      }

      //set data as available to CPU
      out.append("env->CallStaticVoidMethod(cls")
      out.append(location)
      out.append(",env->GetStaticMethodID(cls")
      out.append(location)
      out.append(",\"set")
      out.append(getScalaSym(op, name))
      out.append("\",\"(")
      out.append(getJNIArgType(op.outputType(name)))
      out.append(")V\"),")
      if (op.outputType(name) == "Unit") out.append("boxedUnit") else out.append(getSymCPU(name))
      out.append(");\n")

      if (deleteLocalRef) {
        out.append("env->DeleteLocalRef(")
        out.append(getSymCPU(name))
        out.append(");\n")
      }
    }
  }

  protected def writeMemoryAdd(sym: String, out: StringBuilder) {
    out.append("cudaMemoryMap->insert(pair<void*,list<void*>*>(")
    out.append(getSymGPU(sym))
    out.append(",lastAlloc));\n")
    out.append("lastAlloc = new list<void*>();\n")
  }

  protected def writeDataFrees(op: DeliteOP, out: StringBuilder, available: ArrayBuffer[(DeliteOP,String)])(implicit aliases: AliasTable[(DeliteOP,String)]) {
    var count = 0
    val freeItem = "freeItem_" + op.id

    def writeFreeInit() {
      out.append("FreeItem ")
      out.append(freeItem)
      out.append(";\n")
      out.append(freeItem)
      out.append(".keys = new list< pair<void*,bool> >();\n")
    }

    def writeFree(sym: String, isPrim: Boolean) {
      if (count == 0) writeFreeInit()
      out.append("pair<void*,bool> ")
      out.append(getSymGPU(sym))
      out.append("_pair(")
      out.append(getSymGPU(sym))
      out.append(",")
      out.append(isPrim) //Do not free this ptr using free() : primitive type pointers points to device memory
      out.append(");\n")
      out.append(freeItem)
      out.append(".keys->push_back(")
      out.append(getSymGPU(sym))
      out.append("_pair);\n")
    }

    def opFreeable(op: DeliteOP, sym: String) = {
      //TODO: Better to make OP_Condition extending OP_Executable?
      (op.isInstanceOf[OP_Executable] || op.isInstanceOf[OP_Condition]) && available.contains(op,sym)
    }

    def outputFreeable(op: DeliteOP, sym: String) = {
      if(op.outputType(sym) == "Unit") false
      else true
    }

    //free temps
    for ((temp, name) <- op.getGPUMetadata(target).temps) {
      writeFree(name,false)
      count += 1
    }

    //free outputs
    for (name <- op.getOutputs if(opFreeable(op,name) && outputFreeable(op,name))) {
      if (op.getConsumers.filter(c => c.getInputs.contains((op,name)) && c.scheduledResource == op.scheduledResource).size == 0) {
        writeFree(name,isPrimitiveType(op.outputType(name)))
        count += 1
      }
    }

    //free inputs
    for ((in,name) <- op.getInputs if(opFreeable(in,name) && outputFreeable(in,name))) {
      var free = true
      if (isPrimitiveType(in.outputType(name)) && (in.scheduledResource!=op.scheduledResource)) free = false
      for ((i,n) <- aliases.get(in,name); c <- i.getConsumers.filter(c => c.getInputs.contains(i,n) && c.scheduledResource == op.scheduledResource)) {
        if (!available.map(_._1).contains(c)) free = false
      }
      if (free) {
        writeFree(name,isPrimitiveType(in.outputType(name)))
        count += 1
      }
    }

    if (count > 0) {
      //sync on kernel stream (if copied back guaranteed to have completed, so don't need sync on d2h stream)
      out.append(freeItem)
      out.append(".event = addHostEvent(kernelStream);\n")
      out.append("freeList->push(")
      out.append(freeItem)
      out.append(");\n")
    }
  }

} */
