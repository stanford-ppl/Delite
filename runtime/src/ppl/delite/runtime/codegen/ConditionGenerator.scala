package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.scheduler.OpList
import ppl.delite.runtime.graph.ops.{Send, DeliteOP, OP_Condition}
import sync._
import ppl.delite.runtime.codegen.hosts.Hosts

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:33 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait ConditionGenerator extends NestedGenerator {

  val condition: OP_Condition
  val nested = condition

  def makeExecutable() {
    val hasOutput = condition.outputType != "Unit"

    updateOP()
    //header
    writeHeader()
    writeMethodHeader()

    //output predicate
    if (condition.predicateValue == "") {
      addKernelCalls(condition.predicateGraph.schedule(location))
    }

    //if block
    beginConditionBlock()
    if (condition.predicateValue == "") writeOutput(condition.predicateGraph.result._1, condition.predicateGraph.result._2, false)
    else writeValue(condition.predicateValue, false)
    endConditionBlock()

    //then block
    beginThenBlock()
    if (condition.thenValue == "") {
      addKernelCalls(condition.thenGraph.schedule(location))
      if (hasOutput) {
        writeReturn(false)
        writeOutput(condition.thenGraph.result._1, condition.thenGraph.result._2)
      }
    }
    else if (hasOutput) {
      writeReturn(false)
      writeValue(condition.thenValue)
    }
    endThenBlock()

    //else block
    beginElseBlock()
    if (condition.elseValue == "") {
      addKernelCalls(condition.elseGraph.schedule(location))
      if (hasOutput) {
        writeReturn(false)
        writeOutput(condition.elseGraph.result._1, condition.elseGraph.result._2)
      }
    }
    else if (hasOutput) {
      writeReturn(false)
      writeValue(condition.elseValue)
    }
    endElseBlock()

    writeMethodFooter()
    writeFooter()

    writeSyncObject()

    addSource(out.toString)
  }

  protected def beginConditionBlock()
  protected def endConditionBlock()

  protected def beginThenBlock()
  protected def endThenBlock()

  protected def beginElseBlock()
  protected def endElseBlock()

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], host: Hosts.Value) = {
    host match {
      case Hosts.Scala => new ScalaConditionGenerator(condition, location, kernelPath) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Hosts.Cpp => new CppConditionGenerator(condition, location, kernelPath) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + host.toString)
    }
  }

}

class ScalaConditionGenerator(val condition: OP_Condition, val location: Int, val kernelPath: String)
  extends ConditionGenerator with ScalaNestedGenerator with ScalaSyncGenerator {

  protected def beginConditionBlock() {
    out.append("if (")
  }

  protected def endConditionBlock() {
    out.append(')')
  }

  protected def beginThenBlock() {
    out.append(" {\n")
  }

  protected def endThenBlock() {
    out.append("}\n")
  }

  protected def beginElseBlock() {
    out.append("else {\n")
  }

  protected def endElseBlock() {
    out.append("}\n")
  }

  override protected def getSym(op: DeliteOP, name: String) = ConditionCommon.getSym(condition, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = ConditionCommon.getSync(condition, baseId, op, name)

  def executableName(location: Int) = "Condition_" + baseId + "_" + location

}

class CppConditionGenerator(val condition: OP_Condition, val location: Int, val kernelPath: String)
  extends ConditionGenerator with CppNestedGenerator with CppSyncGenerator {


  protected def beginConditionBlock() {
    out.append("if (")
  }

  protected def endConditionBlock() {
    out.append(')')
  }

  protected def beginThenBlock() {
    out.append(" {\n")
  }

  protected def endThenBlock() {
    out.append("}\n")
  }

  protected def beginElseBlock() {
    out.append("else {\n")
  }

  protected def endElseBlock() {
    out.append("}\n")
  }

  override protected def getSym(op: DeliteOP, name: String) = ConditionCommon.getSym(condition, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = ConditionCommon.getSync(condition, baseId, op, name)

  def executableName(location: Int) = "Condition_" + baseId + "_" + location
}

/*
class CudaGPUConditionGenerator(condition: OP_Condition, location: Int) extends GPUConditionGenerator(condition, location, Targets.Cuda) with CudaGPUExecutableGenerator {
  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    CudaMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaConditionGenerator(condition, location, target).emitScala(syncList), executableName)
  }
}

class OpenCLGPUConditionGenerator(condition: OP_Condition, location: Int) extends GPUConditionGenerator(condition, location, Targets.OpenCL) with OpenCLGPUExecutableGenerator {
  def makeExecutable() {
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added
    updateOP()
    OpenCLMainGenerator.addFunction(emitCpp(syncList))
    ScalaCompile.addSource(new GPUScalaConditionGenerator(condition, location, target).emitScala(syncList), executableName)
  }
}

abstract class GPUConditionGenerator(condition: OP_Condition, location: Int, target: Targets.Value) extends GPUNestedGenerator(condition, location, target) {
  def makeExecutable(): Unit

  def emitCpp(syncList: ArrayBuffer[DeliteOP]) = {
    val out = new StringBuilder //the output string
    val hasOutput = condition.outputType != "Unit"
    val inputOps = (condition.predicateGraph.inputOps ++ condition.thenGraph.inputOps ++ condition.elseGraph.inputOps)
    val inputs = (condition.predicateGraph.inputs ++ condition.thenGraph.inputs ++ condition.elseGraph.inputs)
    implicit val aliases = new AliasTable[(DeliteOP,String)]

    writeFunctionHeader(out)
    val locations = condition.nestedGraphs.flatMap(_.ops.map(_.scheduledResource)).toSet union Set(location)

    writeJNIInitializer(locations, out)

    val available = new ArrayBuffer[(DeliteOP,String)]
    val awaited = new ArrayBuffer[DeliteOP]

    //output predicate
    if (condition.predicateValue == "") {
      available ++= inputs
      awaited ++= inputOps
      addKernelCalls(condition.predicateGraph.schedule(location), location, available, awaited, syncList, out)
    }

    //write if
    out.append("if (")
    if (condition.predicateValue == "") out.append(getSymGPU(condition.predicateGraph.result._2))
    else out.append(condition.predicateValue)
    out.append(") {\n")

    //output if body
    if (condition.thenValue == "") {
      if (condition.isReturner && hasOutput) aliases.add(condition.thenGraph.result,Pair(condition,condition.id))
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputOps
      addKernelCalls(condition.thenGraph.schedule(location), location, available, awaited, syncList, out)
      if (condition.isReturner && hasOutput) aliases.clear

      if (hasOutput) {
        out.append("return ")
        out.append(getSymGPU(condition.thenGraph.result._2))
      }
    }
    else if (hasOutput) {
      out.append("return ")
      out.append(condition.thenValue)
    }
    if (hasOutput) out.append(";\n")

    //print else
    out.append("} else {\n")

    //output else body
    if (condition.elseValue == "") {
      if (condition.isReturner && hasOutput) aliases.add(condition.elseGraph.result,Pair(condition,condition.id))
      available.clear()
      available ++= inputs
      awaited.clear()
      awaited ++= inputOps
      addKernelCalls(condition.elseGraph.schedule(location), location, available, awaited, syncList, out)
      if (condition.isReturner && hasOutput) aliases.clear

      if (hasOutput) {
        out.append("return ")
        out.append(getSymGPU(condition.elseGraph.result._2))
      }
    }
    else if (hasOutput) {
      out.append("return ")
      out.append(condition.elseValue)
    }
    if (hasOutput) out.append(";\n")

    //print end of if and function
    out.append("} // end if\n")
    writeJNIFinalizer(locations, out)
    out.append("} // end Cond Function\n")

    out.toString
  }

  override protected def getScalaSym(op: DeliteOP, name: String) = ConditionCommon.getSym(condition, baseId, op, name)

  protected def executableName = "Condition_" + baseId + "_"

}

class GPUScalaConditionGenerator(condition: OP_Condition, location: Int, target: Targets.Value) extends GPUScalaNestedGenerator(condition, location, target) {
  override protected def executableName = "Condition_" + baseId + "_"
  override protected def getSym(op: DeliteOP, name: String) = ConditionCommon.getSym(condition, baseId, op, name)
  override protected def getSync(op: DeliteOP, name: String) = ConditionCommon.getSync(condition, baseId, op, name)
}
*/
private[codegen] object ConditionCommon {
  private def suffix(condition: OP_Condition, baseId: String, op: DeliteOP, name: String) = {
    if (condition.predicateGraph.ops.contains(op))
      baseId + "P_" + name
    else if (condition.thenGraph.ops.contains(op))
      baseId + "T_" + name
    else if (condition.elseGraph.ops.contains(op))
      baseId + "E_" + name
    else //input
      baseId + "_" + name
  }

  def getSym(condition: OP_Condition, baseId: String, op: DeliteOP, name: String) = "x" + suffix(condition, baseId, op, name)
  def getSync(condition: OP_Condition, baseId: String, op: DeliteOP, name: String) = "Result_" + suffix(condition, baseId, op, name)
}
