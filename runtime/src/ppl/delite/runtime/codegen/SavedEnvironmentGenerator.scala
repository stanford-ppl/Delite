package ppl.delite.runtime.codegen

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops.{Arguments,EOP}


object SavedEnvironmentGenerator {

  def generateEnvironment(graph:DeliteTaskGraph) {
    val out = new StringBuilder
    val appName = if (graph.appName == "") "DeliteApplication" else graph.appName
    val packageName = ScalaExecutableGenerator.getPackage(graph)
    val args = graph.ops.filter(_.isInstanceOf[Arguments]).map(_.asInstanceOf[Arguments]).toSeq.sortBy(_.argIdx)
    val argNames = args.map("in"+_.argIdx) //TODO: user-friendly arg names
    val argTypes = args.map(_.outputType)
    val eop = graph.result._1.asInstanceOf[EOP]
    val res = eop.outputType

    out.append("import ppl.delite.runtime.{Config,Delite}\n")
    out.append("object "+appName+" extends (("+argTypes.mkString(",")+")=>("+res+")) {\n")
    out.append("def apply("+argNames.zip(argTypes).map(a => a._1+":"+a._2).mkString(", ")+"): "+res+" = {\n")
    out.append("initEnv()\n")
    out.append("val res = Delite.executeCached(\""+packageName+"\","+argNames.mkString(",")+")\n")
    if (eop.result._1 == "symbol") out.append("res.asInstanceOf["+res+"]\n") else out.append(eop.result._2+"\n")
    out.append("}\n")

    out.append("def initEnv() {\n")
    out.append("Config.numThreads = " + Config.numThreads + "\n")
    out.append("Config.numCpp = " + Config.numCpp + "\n")
    out.append("Config.numCuda = " + Config.numCuda + "\n")
    out.append("Config.numOpenCL = " + Config.numOpenCL + "\n")
    out.append("Config.executor = \"" + Config.executor + "\"\n")
    out.append("Config.performWalk = false\n")
    val expected = for (i <- 0 until graph.schedule.numResources if !graph.schedule(i).isEmpty) yield i
    out.append("Delite.expectedResources = Seq(" + expected.mkString(",") + ")\n")
    out.append("}\n}\n")

    ScalaCompile.addSource(out.toString, "SavedApp")
  }

}
