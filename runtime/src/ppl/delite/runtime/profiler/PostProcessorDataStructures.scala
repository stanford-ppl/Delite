package ppl.delite.runtime
package profiler

import ppl.delite.runtime.Config
import _root_.scala.util.parsing.json.JSON
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source

object DNodeType extends Enumeration {
	type DNodeType = Value
	val WhileLoop, Conditional, MultiLoop, SingleTask, Foreach = Value
}

object TNodeType extends Enumeration {
	type TNodeType = Value
	val Kernel, KernelPartition, Sync, TicTocRegion = Value
}

object DependencyGraph {

	//implicit conversion to strings
	implicit def anyToString(obj: Any): String = obj.toString

	val dNodeConditionalSubOpTypes = Array("condOps", "thenOps", "elseOps")
	val dNodeWhileloopSubOpTypes = Array("condOps", "bodyOps")

	def dependencyGraphNew(degFile: String): DependencyGraph = {
		val contents = scala.io.Source.fromFile(degFile).mkString
    	import JSON._
    	phrase(root)(new lexical.Scanner(contents)) match {
			case Success(result,_) => buildDepGraphFromParsedJSON(resolveType(result))
			case f@NoSuccess(_,_) => throw new RuntimeException("Couldn't parse the DEG file:\n" + f.toString)
    	}
	}

	private def buildDepGraphFromParsedJSON(json: Any): DependencyGraph = {
		json match {
			case degm: Map[Any,Any] => try { 
				val depGraph = new DependencyGraph
				val deg = getFieldMap(degm, "DEG")
				val ops = getFieldList(deg, "ops")
				depGraph.dNodesNew(ops)
				return depGraph
			} catch { case e: Exception => e.printStackTrace; throw e; }
			case err@_ => mapNotFound(err)
    	}
  	}

  	private def getFieldString(map: Map[Any, Any], field:String): String = {
		map.get(field) match {
			case Some(field) => field
			case None => fieldNotFound(field, map)
		}
	}

	private def getFieldList(map: Map[Any, Any], field: String): List[Any] = {
	    map.get(field) match {
	        case Some(field) => field match {
	        	case list: List[Any] => list
	        	case err@_ => listNotFound(err)
		    }

		    case None => fieldNotFound(field, map)
	    }
  	}

  	private def getFieldMap(map: Map[Any, Any], field: String): Map[Any,Any] = {
	    map.get(field) match {
	    	case Some(field) => field match {
		        case map: Map[Any,Any] => map
		        case err@_ => mapNotFound(err)
	      	}

	      	case None => fieldNotFound(field, map)
	    }
	}

	private def fieldNotFound(field: String, obj: Any) = throw new RuntimeException("Expecting field [" + field + "], found: " + obj )
  	private def mapNotFound(err:Any) = throw new RuntimeException("Expecting a Map object, found: " + err)
  	private def listNotFound(err:Any) = throw new RuntimeException("Expecting a List object, found: " + err)
}

class DependencyGraph() {
	var levelMax: Int = 0
	var nodeNameToId = new HashMap[Types.DNodeName, Types.DNodeId]
	var idToDNode = new HashMap[Types.DNodeId, DNode]
	var _loopNodes = new ArrayBuffer[DNode]

	def dNode(name: String): DNode = {
		idToDNode(nodeNameToId(name))
	}

	def loopNodes(): ArrayBuffer[DNode] = { _loopNodes }

	def dNodesNew(ops: List[Any], parent: DNode = null) {
	    for(_op <- ops) {
			val op = _op.asInstanceOf[Map[Any, Any]]
			dNodeAdd( opToDNode(op, parent) )
	    }
  	}

  	def string() {
  		for ((k,v) <- nodeNameToId) {
  			Predef.println("========================================================")
  			Predef.println("Node: " + k + "  Id: " + v)
  			Predef.println(idToDNode(v).string())
  		}
  	}

	private def dNodeAdd(n: DNode) {
		if (n != null) {
			nodeNameToId += n.name -> n.id
			idToDNode += n.id -> n
			levelMax = math.max(levelMax, n.level)

			n._type match {
				case DNodeType.WhileLoop => {
					loopNodes.append(n)
				}

				case DNodeType.Conditional => {
					loopNodes.append(n)
				} 

				case DNodeType.MultiLoop => loopNodes.append(n)
				case _ => // do nothing
			}
		}
	}

	private def opToDNode(op: Map[Any, Any], parent: DNode): DNode = {
		val opType = DependencyGraph.getFieldString(op, "type")
		opType match {
				case "Arguments" => dNodeArgsNew(op, DNodeType.SingleTask)
				case "SingleTask" | "External" | "Input" => dNodeGenericNew(op, parent, DNodeType.SingleTask) 
				case "MultiLoop" => dNodeGenericNew(op, parent, DNodeType.MultiLoop) 
				case "Foreach" => dNodeGenericNew(op, parent, DNodeType.Foreach) 
				case "Conditional" => dNodeConditionalNew( op, parent )
				case "WhileLoop" => dNodeWhileLoopNew( op, parent )
				case "EOP" | "EOG" => null
			}
	}

	private def dNodeArgsNew(op: Map[Any, Any], _type: DNodeType.Value): DNode = {
		val name = DependencyGraph.getFieldString(op, "kernelId")
		new DNode(name, null, _type)
	}

	private def dNodeGenericNew(op: Map[Any, Any], parent: DNode, _type: DNodeType.Value): DNode = {	
		val name = DependencyGraph.getFieldString(op, "kernelId")
		val node = new DNode(name, parent, _type)

		DependencyGraph.getFieldList(op, "inputs").map( str => str.asInstanceOf[String] ).foreach( n => node.inputNodeAdd(n) )
		node
	}

	private def dNodeConditionalNew(op: Map[Any, Any], parent: DNode): DNode = {
		val name = DependencyGraph.getFieldString(op, "outputId")
		val node = new DNode(name, parent, DNodeType.WhileLoop)

		DependencyGraph.dNodeConditionalSubOpTypes.foreach(st => {
			val subOps = DependencyGraph.getFieldList(op, "condOps")
			dNodesNew( subOps.map(op => op.asInstanceOf[Map[Any, Any]]), node )
		})

		node
	}

	private def dNodeWhileLoopNew(op: Map[Any, Any], parent: DNode): DNode = {
		val name = DependencyGraph.getFieldString(op, "outputId")
		val node = new DNode(name, parent, DNodeType.WhileLoop)

		DependencyGraph.dNodeWhileloopSubOpTypes.foreach(st => {
			val subOps = DependencyGraph.getFieldList(op, "condOps")
			dNodesNew( subOps.map(op => op.asInstanceOf[Map[Any, Any]]), node )
		})

		node
	}
}


object DNode {
	private var _nextId: Int = -1

	def nextId(): Int = {
		_nextId += 1
		_nextId 
	}
}

class DNode(_name: Types.DNodeName, _parent: DNode, t: DNodeType.Value) {
	val name = _name
	val parent = _parent
	val _type = t
	val id = DNode.nextId()
	val level: Int = if (_parent == null) 0 else _parent.level + 1
	var targetsSupported = Array(false, false, false) // order of indices -> scala, cpp, cuda
	val inputNodes = new ArrayBuffer[String]
	val childNodes = new ArrayBuffer[DNode]

	if (_parent != null) {
		_parent.childNodeAdd(this)
	}

	def inputNodeAdd(cn: String) { inputNodes.append(cn) }
	def childNodeAdd(cn: DNode) { childNodes.append(cn) }

	def cudaSupported(): Boolean = { targetsSupported(PostProcessor.TARGET_CUDA) }

	def string() {
		Predef.println("    Name: " + name + " Id: " + id + " Type: " + _type.toString)
		Predef.println("    Inputs: " + inputNodes)
		Predef.println("    Child Nodes: " + childNodes.map(cn => cn.name))
	}
}

object Types {
	type DNodeId = Int
	type TNodeId = Int
	type LevelId = Int
	type DNodeName = String
	type TNodeName = String
}

object ExecutionProfile {

	var depGraph: DependencyGraph = null

	def dNode(tNodeName: String): DNode = {
		val arr = tNodeName.split("_")
		return depGraph.dNode(arr(0))
	}

	def tNodeType(tNodeName: String): TNodeType.Value = {
		if (tNodeName(0) == '_') {
			return TNodeType.Sync
		} else if (tNodeName.contains("_")) {
			return TNodeType.KernelPartition
		}

		return TNodeType.Kernel
	}

	def parentLoopKernelName(tNodeName: String): Option[String] = {
		val arr = tNodeName.split("_")
		if (arr.length > 1) {
			return Some(arr(0))
		}

		return None
	}
}

class ExecutionProfile(_rawProfileDataFile: String, _depGraph: DependencyGraph) {
	val rawProfileDataFile: String = _rawProfileDataFile
	val depGraph: DependencyGraph = _depGraph

	var threadCount: Int = 1
	var threadScalaCount: Int = 0
	var threadCppCount: Int = 0
	var threadCudaCount: Int = 0
	var jvmUpTimeAtAppStart: Long = 0
	var appStartTime: Long = 0
	var appTotalTime: Long = 0
	var appEndTime: Long = 0
	var targetsEnabled = Array(false, false, false) // order of indices -> scala, cpp, cuda
	var summaries = new HashMap[Types.TNodeName, ExecutionSummary] // TrieMap is the thread-safe version of HashMap
	var timelineData = new TimelineData(_depGraph.levelMax)
	var ticTocTNodes = new ArrayBuffer[TicTocTNode]
	var threadTNodes = new ArrayBuffer[ThreadTNode]

	// populate the data that's common across threads, eg: numThreads, etc.
	def init() {
		ExecutionProfile.depGraph = _depGraph
		// TODO: Parametrize this path parameter
		val profileOutputDirectory = "/Users/jithinpt/Documents/Acads/Rotation_with_Kunle/check-in/hyperdsl/published/SimpleVector/profile"
		parseThreadSpecificProfileDataFiles( profileOutputDirectory + "/profile_t_" )
		updateTimeTakenByPartitionedKernels()
	}

	def tNodeTotalTime(tNodeName: String): Long = {
		if (summaries.contains(tNodeName)) {
			return summaries(tNodeName).totalTime
		}

		return 0
	}

	def parseThreadSpecificProfileDataFiles(fileNamePrefix: String) {
		val levelToTNodesYetToBeAssignedParent = new HashMap[Int, ArrayBuffer[TNode]]
		for (level <- 0 to depGraph.levelMax) {
			levelToTNodesYetToBeAssignedParent += level -> new ArrayBuffer[TNode]
		}

		for (tid <- 0 to (threadCount - 1)) {			
			val fn = fileNamePrefix + tid + ".csv"
			for (line <- Source.fromFile(fn).getLines()) {
				val arr = line.split(",")
				val duration = arr(2).toLong
				if (duration > 0) { // Optimization: filter out all nodes with duration == 0
					val tNode = new TNode(arr(0), tid, arr(1).toLong, duration)
					val lev = tNode.level
					if (lev >= 0) {
						if (lev < depGraph.levelMax) {
							val a = levelToTNodesYetToBeAssignedParent(lev + 1)
							a.foreach(n => { 
								n.parentTNode = tNode
								tNode.childNodeNew(n) 
							})
							a.clear()
						}

						if (lev > 0) {
							levelToTNodesYetToBeAssignedParent(lev).append(tNode)
						}
					}

					updateSummary(tNode)
					timelineData.tNodeNew(tNode)
				}
			}
		}
	}

	// Update the summary of corresponding dNode, eg: totalExecTime, etc.
	private def updateSummary(tNode: TNode) {
		val n = tNode.name
		if (!summaries.contains(n)) {
			summaries += n -> new ExecutionSummary()
		}

		tNode._type match {
			case TNodeType.Kernel | TNodeType.KernelPartition => summaries(n).totalTimeInc(tNode.duration)
			case _ => ()
		}
	}

	private def updateTimeTakenByPartitionedKernels() {
		val threadIdsRange = Range(0, threadCount)
		for (dNode <- depGraph.loopNodes) {
			if (!isTargetCuda(dNode)) {
				val headerNodeTotalTime = tNodeTotalTime(dNode.name + "_h")
				val partitionNodesMaxTime = threadIdsRange.map(tid => tNodeTotalTime(dNode.name + "_" + tid)).max
				val estimatedTotalTime = headerNodeTotalTime + partitionNodesMaxTime
				summaries += dNode.name -> new ExecutionSummary(headerNodeTotalTime + estimatedTotalTime)
			}
		}
	}

	private def updateSyncAndExecTimesOfKernels() {
		def computeSyncAndExecTimes(tNodes: ArrayBuffer[TNode]) {
			var totalSyncTime: Long = 0
			tNodes.foreach(tn => {
				val selfSyncTime = tn.childSyncTNodes.map(n => n.duration).sum
				val childrenSyncTime = tn.childKernelTNodes.map(n => n.syncTime).sum
				tn.syncTime = selfSyncTime + childrenSyncTime
				tn.execTime = tn.duration - tn.syncTime

				totalSyncTime += tn.syncTime
			})

			syncTimeIs(tNodes(0).name, totalSyncTime)
		}

		for (l <- depGraph.levelMax to 0 by -1) {
			val m = timelineData.tNodesAtLevel(l)
			for ((tNodeName,tNodes) <- m) {
				computeSyncAndExecTimes(tNodes)
			}
		}
	}

	private def updateMemUsageDataOfDNodes() {
		val aggrMemUsageStats = MemoryProfiler.aggregateStatsFromAllThreads()
		for (kv <- aggrStats) {
			val tNodeName = kv._1
  			val totalMemUsage = sum(kv._2)
  			memUsageIs(tNodeName, totalMemUsage)

  			val pn = ExecutionProfile.parentLoopKernelName(tNodeName)
  			pn match {
  				case Some(parent) => memUsageInc(parent, totalMemUsage)
  				case _ => // do nothing
  			}
  		}
	}

	private def syncTimeIs(tNodeName: String, st: Long) {
		val s = executionSummary(tNodeName)
		s.syncTime = st
	}

	private def memUsageIs(tNodeName: String, mu: Long) {
		val s = executionSummary(tNodeName)
		s.memUsage = mu
	}

	private def memUsageInc(tNodeName: String, inc: Long) {
		val s = executionSummary(tNodeName)
		s.memUsage += inc
	}

	private def isTargetCuda(dNode: DNode): Boolean = {
		return (dNode.cudaSupported() && targetsEnabled(PostProcessor.TARGET_CUDA))
	}

	private def executionSummary(tNodeName: String): ExecutionSummary = {
		if (!summaries.contains(tNodeName)) {
			val es = new ExecutionSummary
			summaries += tNodeName -> es
			return es
		}

		summaries(tNodeName)
	}
}

class TNode(_name: Types.TNodeName, _threadId: Int, _start: Long, _duration: Long) {
	val name = _name
	val threadId: Int = _threadId
	val start: Long = _start
	val duration: Long = _duration
	val end: Long = start + duration
	var execTime: Long = 0
	var syncTime: Long = 0
	val _type = ExecutionProfile.tNodeType(_name)
	val dNode = _type match {
		case TNodeType.Sync | TNodeType.TicTocRegion => null
		case _    => ExecutionProfile.dNode(_name)
	}

	var parentTNode: TNode = null
	val level = if (dNode != null) dNode.level else -1
	var childKernelTNodes = new ArrayBuffer[TNode]
	var childSyncTNodes = new ArrayBuffer[TNode]

	def childNodeNew(cn: TNode) {
		cn._type match {
			case TNodeType.Sync => childSyncTNodes.append(cn)
			case _ => childKernelTNodes.append(cn)
		}
		
	}
}

class MemAccessStats() {
	var l2CacheHitPct: Double = 0
	var l3CacheHitPct: Double = 0
}

class ExecutionSummary(_tt: Long = 0) {
	var totalTime: Long = _tt
	var execTime: Long = 0
	var syncTime: Long = 0
	var memUsage: Long = 0
	var memAccessStats: MemAccessStats = null

	def totalTimeInc(inc: Long) {
		totalTime += inc
	}
}

class TimelineData(_levelMax: Int) {
	val levelMax = _levelMax
	var levelToTNodes = new HashMap[Types.LevelId, HashMap[Types.TNodeName, ArrayBuffer[TNode]]]()

	for (level <- 0 to _levelMax) {
		levelToTNodes += level -> new HashMap[Types.TNodeName, ArrayBuffer[TNode]]
	}

	def tNodeNew(tNode: TNode) {
		val n = tNode.name
		if (!levelToTNodes(tNode.level).contains(n)) {
			levelToTNodes(tNode.level) += n -> new ArrayBuffer[TNode]
		}

		levelToTNodes(tNode.level)(n).append(tNode)
	}

	def tNodesAtLevel(level: Types.LevelId): HashMap[Types.TNodeName, ArrayBuffer[TNode]] = {
		return levelToTNodes(level)
	}
}

class DNodeMemAccessStats() {

}

class TicTocTNode(_name: String) {
	val name = _name
}

class ThreadTNode(_tid: Int) {
	val tid = _tid
}

