package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.scheduler.OpList
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops.{Send, DeliteOP, OP_Condition}
import sync._
import ppl.delite.runtime.graph.targets.Targets

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

  // cond variable is declared by the first scheduled resources for each target
  def declCondVar: Boolean = {
    val targetResources = Targets.resourceIDs(Targets.getByLocation(location))
    val scheduledIDs = targetResources.filter(i => condition.nestedGraphs.map(_.schedule(i).isEmpty) contains false)
    if (scheduledIDs.head == location) true
    else false
  }

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
    setConditionVariable(true)
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
    setConditionVariable(false)
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

  protected def setConditionVariable(cond: Boolean)

  protected def beginConditionBlock()
  protected def endConditionBlock()

  protected def beginThenBlock()
  protected def endThenBlock()

  protected def beginElseBlock()
  protected def endElseBlock()

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value) = {
    target match {
      case Targets.Scala => new ScalaConditionGenerator(condition, location, graph) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Targets.Cpp => new CppConditionGenerator(condition, location, graph) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + target.toString)
    }
  }

}

class ScalaConditionGenerator(val condition: OP_Condition, val location: Int, val graph: DeliteTaskGraph)
  extends ConditionGenerator with ScalaNestedGenerator with ScalaSyncGenerator {

  override protected def writeMethodHeader() {
    out.append("var " + condition.id.split('_').head + "_cond :Boolean = false\n")
    super.writeMethodHeader()
  }

  protected def setConditionVariable(cond: Boolean) {
    out.append(condition.id.split('_').head + "_cond = " + cond + "\n")
  }
  
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

class CppConditionGenerator(val condition: OP_Condition, val location: Int, val graph: DeliteTaskGraph)
  extends ConditionGenerator with CppNestedGenerator with CppSyncGenerator {

  override protected def writeMethodHeader() {
    if (declCondVar)
      out.append("bool " + condition.id.split('_').head + "_cond;\n")
    super.writeMethodHeader()
  }

  protected def setConditionVariable(cond: Boolean) {
    out.append(condition.id.split('_').head + "_cond = " + cond + ";\n")
  }

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

class CudaConditionGenerator(val condition: OP_Condition, val location: Int, val graph: DeliteTaskGraph)
  extends ConditionGenerator with CudaNestedGenerator with CudaSyncGenerator {

  override protected def writeMethodHeader() {
    if (declCondVar)
      out.append("bool " + condition.id.split('_').head + "_cond;\n")
    super.writeMethodHeader()
  }

  protected def setConditionVariable(cond: Boolean) {
    out.append(condition.id.split('_').head + "_cond = " + cond + ";\n")
  }

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
  def getSync(condition: OP_Condition, baseId: String, op: DeliteOP, name: String) = "Result_" + op.id + "_" + suffix(condition, baseId, op, name)
}
