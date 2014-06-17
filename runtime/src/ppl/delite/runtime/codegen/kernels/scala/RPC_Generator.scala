package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.{Empty, Interval, Stencil}
import ppl.delite.runtime.graph.ops.{DeliteOP, OP_MultiLoop, OP_FileReader}
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config


object RPC_Generator {

  def makeKernel(op: DeliteOP, graph: DeliteTaskGraph) {
    val out = new StringBuilder

    updateOP(op)
    writeHeader(out, op, graph)
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

  private def writeHeader(out: StringBuilder, op: DeliteOP, graph: DeliteTaskGraph) {
    ScalaExecutableGenerator.writePackage(graph, out)
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import ppl.delite.runtime.profiler.MemoryProfiler\n")
    out.append("import ppl.delite.runtime.messages.Messages._\n")
    out.append("import ppl.delite.runtime.messages.Serialization\n")
    out.append("import ppl.delite.runtime.graph._\n")
    ScalaExecutableGenerator.writePath(graph, out)
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
    //launch
    val tpe = if (op.isInstanceOf[OP_MultiLoop]) "RemoteOp.Type.MULTILOOP" else "RemoteOp.Type.INPUT"
    out.append("val res = ppl.delite.runtime.DeliteMesosScheduler.launchAllSlaves(\"" + op.id + "\"," + tpe)
    
    val stencils = if (op.isInstanceOf[OP_MultiLoop]) 
      op.getInputs.map(in => in._1.stencilOrElse(in._2)(Empty)).map(_ match {
        case Interval(start,stride,length) => "KnownInterval("+start+","+stride+","+length+")"
        case other => other.toString
      }) else Seq()
    out.append(stencils.mkString(", Seq(",",",")"))
    out.append(op.getInputs.map(_._2).mkString(", Seq(",",","))\n"))

    //construct outputs
    out.append("val act = activation_" + op.id + ".deserialize(res(0).getOutputList)\n")
    
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
      out.append("val act2 = activation_" + op.id + ".deserialize(res(slaveIdx).getOutputList)\n")
      out.append("closure.combine(act,act2)\n")
      out.append("slaveIdx += 1\n")
    out.append("}\n")

    op match {
      case m: OP_MultiLoop if m.needsCombine => out.append("closure.finalize(act)\n") //TODO: should this be called by master, slaves, or both?
      case _ =>
    }

    out.append("act\n")
  }

}
