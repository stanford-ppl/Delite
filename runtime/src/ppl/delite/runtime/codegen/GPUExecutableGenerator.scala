package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.{OPData, Targets}

trait GPUExecutableGenerator extends CppExecutableGenerator {

  protected def target: Targets.Value

  protected def writeKernelCall(op: DeliteOP)

  protected def writeMemoryAdd(sym: String) //TODO: how do we want to handle memory management?

  override protected def writeFunctionCall(op: DeliteOP) = op match {
    case n: OP_Nested => writeNestedFunctionCall(op) //TODO: super.writeFunctionCall(op)?
    case e: OP_External => writeLibraryCall(op)
    case m: OP_MultiLoop => writeMultiKernelCall(op)
    case _ => writeKernelCall(op)
  }

  protected def writeOutputAllocs(op: DeliteOP) {
    if (op.isInstanceOf[OP_Executable]) {
      for ((data,name) <- op.getGPUMetadata(target).outputs if data.resultType!="void") {
        out.append(op.outputType(Targets.Cuda,name) + "* " + getSymGPU(name))
        if (op.isInstanceOf[OP_MultiLoop])
          out.append(";\n")
        else {
          out.append(" = " + data.func + "(")
          writeInputList(op, data)
          out.append(");\n")
          writeMemoryAdd(name)
        }
      }
    }
  }

  protected def writeTempAllocs(op: DeliteOP) {
    for ((temp,name) <- op.getGPUMetadata(target).temps) {
      out.append(temp.resultType + "* " + getSymGPU(name) + " = " + temp.func)
      out.append(temp.inputs.map(in => getSymGPU(in._2)).mkString("(",",",");\n"))
      writeMemoryAdd(name)
    }
  }

  protected def writeInputList(op: DeliteOP, data: OPData) {
    out.append(data.inputs.map(in => getSymGPU(in._2)).mkString(","))
  }

  protected def writeInputs(op: DeliteOP, dereference: Boolean = true) {
    def deref(in: DeliteOP, name: String) = if (!isPrimitiveType(in.outputType(name)) && dereference) "*" else ""
    out.append(op.getInputs.map(in => deref(in._1,in._2) + getSymGPU(in._2)).mkString(","))
  }

  protected def writeTemps(op: DeliteOP, dereference: Boolean = true) {
    val deref = if (dereference) "*" else ""
    out.append(op.getGPUMetadata(target).temps.map(t => "," + deref + getSymGPU(t._2))).mkString("")
  }

  protected def writeMultiKernelCall(op: DeliteOP) {
    if (op.task == null) return //dummy op
    out.append(op.task) //kernel name
    out.append('(')
    var first = true
    for ((data,name) <- (op.getGPUMetadata(target).outputs) if data.resultType!="void") {
      if(!first) out.append(',')
      out.append("&" + getSymGPU(name))
      first = false
    }
    if (!first && (op.getInputs.length > 0 || op.getGPUMetadata(target).temps.length > 0)) out.append(",")
    writeInputs(op, false) //then all op inputs
    writeTemps(op, false) //then all op temporaries
    out.append(");\n")
  }

  protected def writeLibraryCall(op: DeliteOP) {
    if (op.task == null) return //dummy op
    out.append(op.task) //kernel name
    out.append('(')
    assert(op.getOutputs.size == 1) //TODO: what does libCall support?
    for (name <- op.getOutputs) {
      if (op.outputType(name) != "Unit") {
        out.append("*" + getSymGPU(name) + ",")
      }
    }
    writeInputs(op) //then all op inputs
    //out.append(",kernelStream") //TODO: what other libraries besides cuBlas do we use? how do we use the stream only with supported libraries?
    out.append(");\n")
  }

  //TODO: this function provided by C++ generator? while, if, etc. entirely host constructs!
  protected def writeNestedFunctionCall(op: DeliteOP) {
    if (op.outputType != "Unit") {
      out.append(op.outputType(Targets.Cuda))
      out.append(' ')
      // GPU nested block can only return when both condition branches are returned by GPU,
      // meaning that the return object will be a pointer type
      if(op.outputType != "Unit") out.append('*')
      out.append(getSymGPU(op.getOutputs.head))
      out.append(" = ")
    }
    out.append(op.task)
    out.append('(')
    var first = true
    for ((input,sym) <- op.getInputs) {
      if (op.getGPUMetadata(target).inputs.contains(input,sym) || isPrimitiveType(input.outputType(sym))) {
        if (!first) out.append(',')
        first = false
        out.append(getSymGPU(sym))
        if ((op.getMutableInputs contains (input,sym)) && (input.getConsumers.filter(_.scheduledResource!=input.scheduledResource).nonEmpty)) {
          out.append(',')
          out.append(getSymCPU(sym))
        }
      }
    }
    out.append(");\n")
  }

  protected def getSymGPU(name: String) = "xG"+name

}
