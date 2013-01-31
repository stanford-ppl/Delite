package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.ops.{DeliteOP, OP_MultiLoop, OP_FileReader}
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config


object RPC_Generator {

  def makeKernel(op: DeliteOP, kernelPath: String) {
    val out = new StringBuilder

    updateOP(op)
    writeHeader(out, op, kernelPath)
    writeKernel(out, op)
    writeFooter(out)

    ScalaCompile.addSource(out.toString, kernelName(op))
  }

  private def updateOP(op: DeliteOP) {
    op match {
      case m: OP_MultiLoop => m.setKernelName(kernelName(op))
      case f: OP_FileReader => f.setKernelName(kernelName(op))
      case _ => sys.error("RPC call for unknown op type: " + op)
    }
  }

  private def writeHeader(out: StringBuilder, op: DeliteOP, kernelPath: String) {
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import ppl.delite.runtime.messages.Messages._\n")
    out.append("import ppl.delite.runtime.messages.Serialization\n")
    ScalaExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName(op))
    out.append(" {\n")
    writeObjectApply(out, op)
  }

  private def writeFooter(out: StringBuilder) {
    out.append("}\n}\n") //close apply and object
  }

  private def writeObjectApply(out: StringBuilder, op: DeliteOP) {
    out.append("def apply(")
    out.append(op.getInputs.map{case (in, name) => name + ": " + in.outputType(name)}.mkString(", ")) //TODO: remap RemoteArray
    out.append(") = {\n")
  }

  private def kernelName(op: DeliteOP) = {
    "RPC_" + op.id
  }

  private def writeKernel(out: StringBuilder, op: DeliteOP) {
    //build serialized input list
    for ((in, name) <- op.getInputs) {
      out.append("val " + name + "_bytes = Serialization.serialize(" + name + ")\n") 
    }

    //launch
    val tpe = if (op.isInstanceOf[OP_MultiLoop]) "RemoteOp.Type.MULTILOOP" else "RemoteOp.Type.INPUT"
    out.append("val res = ppl.delite.runtime.DeliteMesosScheduler.launchAllSlaves(\"" + op.id + "\"," + tpe)
    out.append(op.getInputs.map(_._2 + "_bytes").mkString(",",",",")\n"))

    //construct outputs
    val outputs = op.getOutputs.toSeq.sortBy(o => o)
    out.append("val act = new activation_" + op.id + "\n")
    var outIdx = 0
    for (output <- op.getOutputs) {
      out.append("act." + output + " = Serialization.deserialize(classOf[" + op.outputType(output) + "], res(0).getOutput(" + outIdx + ")).asInstanceOf[" + op.outputType(output) + "]\n")
      outIdx += 1
    }
    
    //reduce results from other slaves
    out.append("val closure = ")
    op match {
      case _:OP_MultiLoop => 
        out.append("kernel_" + op.id)
        out.append(op.getInputs.map(in => "null.asInstanceOf[" + in._1.outputType(in._2) + "]").mkString("(",",",")\n"))
      case _:OP_FileReader => 
        out.append("new activation_" + op.id + "\n")
    }
    out.append("var slaveIdx = 1\n")
    out.append("while (slaveIdx < res.length) {\n") //TODO: could parallelize reduce across slaves, then send result to master
      out.append("val act2 = new activation_" + op.id + "\n")
      outIdx = 0
      for (output <- op.getOutputs) {
        out.append("act2." + output + " = Serialization.deserialize(classOf[" + op.outputType(output) + "], res(slaveIdx).getOutput(" + outIdx + ")).asInstanceOf[" + op.outputType(output) + "]\n")
        outIdx += 1
      }
      out.append("closure.combine(act,act2)\n")
      out.append("slaveIdx += 1\n")
    out.append("}\n")

    out.append("act\n")
  }

}
