package ppl.tests.scalatest

import org.scalatest._
import ppl.delite.runtime.Config
import ppl.delite.runtime.profiler._
import scala.collection.mutable.{ArrayBuffer, HashMap}

case class DNodeModel(id: Int, parentName: Types.DNodeName,
  targetsSupported: Int, level: Int,
  _type: DNodeType.Value,
  inputNodes: ArrayBuffer[String],
  outputNodes: ArrayBuffer[String],
  childNodes: ArrayBuffer[String])

class ProfilerSuite extends Suite {

  val sep = java.io.File.separator
  val inputsDirectory = sys.env.get("DELITE_HOME").map(_ + sep).getOrElse("") + "runtime/tests/ppl/tests/scalatest/inputs/ProfilerSuite/input_1" 
  val degFilePath = inputsDirectory + sep + "input_1.deg"

  def compareDNodes(expected: DNodeModel, actual: DNode, nodeNameToId: HashMap[Types.DNodeName, Types.DNodeId]) = { 
    def helper(expectedNames: ArrayBuffer[String], actualIds: ArrayBuffer[Types.DNodeId]) = { 
      val expectedIds = expectedNames.map(n => nodeNameToId(n)).toSet
      assertResult(expectedIds)(actualIds.toSet)
    }   

    assertResult(expected.id)(actual.id)
    assertResult(expected.parentName) {
      if (actual.parent == null) "" else actual.parent.name
    }   

    assertResult(expected.targetsSupported)(actual.targetsSupported)
    assertResult(expected.level)(actual.level)
    assertResult(expected._type)(actual._type)

    helper(expected.inputNodes, actual.inputNodes)
    helper(expected.outputNodes, actual.outputNodes)
    helper(expected.childNodes, actual.childNodes.map(n => n.id))
  } 

  def checkDNode(depGraph: DependencyGraph, name: String,
    parentName: Types.DNodeName, targetsSupported: Int,
    level: Int, _type: DNodeType.Value,
    inputNodes: ArrayBuffer[String],
    outputNodes: ArrayBuffer[String],
    childNodes: ArrayBuffer[String]) {
    
    val id = depGraph.nodeNameToId(name)
    val model = DNodeModel(id, parentName, targetsSupported, level, _type, inputNodes, outputNodes, childNodes)
    compareDNodes(model, depGraph.idToDNode(id), depGraph.nodeNameToId)
  }

  def testProcessDegFile {
    val depGraph = PostProcessor.processDegFile(degFilePath)

    checkDNode(depGraph, "x0", "", 1, 0, DNodeType.SingleTask, ArrayBuffer(), ArrayBuffer("x1", "x4"), ArrayBuffer())    
    checkDNode(depGraph, "x1", "", 1, 0, DNodeType.SingleTask, ArrayBuffer("x0"), ArrayBuffer("x3"), ArrayBuffer())          
    checkDNode(depGraph, "x2", "", 1, 0, DNodeType.WhileLoop, ArrayBuffer(), ArrayBuffer(), ArrayBuffer("x3", "x4", "x5", "x6", "eog"))

    checkDNode(depGraph, "x3", "x2", 1, 1, DNodeType.SingleTask, ArrayBuffer("x1"), ArrayBuffer("x8"), ArrayBuffer())    
    checkDNode(depGraph, "x4", "x2", 1, 1, DNodeType.SingleTask, ArrayBuffer("x0"), ArrayBuffer("x6", "x7", "x8"), ArrayBuffer())
    checkDNode(depGraph, "x5", "x2", 1, 1, DNodeType.WhileLoop, ArrayBuffer(), ArrayBuffer(), ArrayBuffer("x7","x8","x9", "x10"))    
    checkDNode(depGraph, "x6", "x2", 1, 1, DNodeType.SingleTask, ArrayBuffer("x4"), ArrayBuffer("x11"), ArrayBuffer())    

    checkDNode(depGraph, "x7", "x5", 1, 2, DNodeType.SingleTask, ArrayBuffer("x4"), ArrayBuffer("x8"), ArrayBuffer())   
    checkDNode(depGraph, "x8", "x5", 1, 2, DNodeType.SingleTask, ArrayBuffer("x3","x4","x7"), ArrayBuffer(), ArrayBuffer())    
    checkDNode(depGraph, "x9", "x5", 1, 2, DNodeType.SingleTask, ArrayBuffer(), ArrayBuffer("x10"), ArrayBuffer())    
    checkDNode(depGraph, "x10", "x5", 1, 2, DNodeType.SingleTask, ArrayBuffer("x9"), ArrayBuffer(), ArrayBuffer())    

    checkDNode(depGraph, "x11", "", 3, 0, DNodeType.MultiLoop, ArrayBuffer("x6"), ArrayBuffer(), ArrayBuffer())
  }

  def checkExecutionSummary(executionProfile: ExecutionProfile, name: Types.TNodeName,
    totalTime: Long, execTime: Long, syncTime: Long) {
    
    if (executionProfile.summaries.contains(name)) {
      val s = executionProfile.summaries(name)
      assertResult(totalTime, " [Mismatch in totalTime for " + name)(s.totalTime)
      assertResult(execTime, " [Mismatch in execTime for " + name)(s.execTime)
      assertResult(syncTime, " [Mismatch in syncTime for " + name)(s.syncTime)
    }
    else 
      fail(" [No summary found for " + name + "]")
  }


  def testProcessRawProfileDataFile {
    Config.profileOutputDirectory = inputsDirectory

    val depGraph = PostProcessor.processDegFile(degFilePath)
    val executionProfile = new ExecutionProfile(depGraph, inputsDirectory)
    executionProfile.threadCount = 3
    executionProfile.init()

    checkExecutionSummary(executionProfile, "x0", 12, 12, 0)
    checkExecutionSummary(executionProfile, "x1", 4, 4, 0)
    checkExecutionSummary(executionProfile, "x2", 41, 41, 0)
    checkExecutionSummary(executionProfile, "x3", 7, 7, 0)
    checkExecutionSummary(executionProfile, "x4", 3, 3, 0)
    checkExecutionSummary(executionProfile, "x5", 28, 28, 0)
    checkExecutionSummary(executionProfile, "x6", 3, 3, 0)
    checkExecutionSummary(executionProfile, "x7", 12, 12, 0)
    checkExecutionSummary(executionProfile, "x8", 9, 9, 0)
    assert(!executionProfile.summaries.contains("x9"))
    assert(!executionProfile.summaries.contains("x10"))
    checkExecutionSummary(executionProfile, "x11", 29, 29, 0)
    checkExecutionSummary(executionProfile, "x11_0", 31, 31, 0)
    checkExecutionSummary(executionProfile, "x11_1", 28, 28, 0)
    checkExecutionSummary(executionProfile, "x11_2", 29, 29, 0)
  }
}
