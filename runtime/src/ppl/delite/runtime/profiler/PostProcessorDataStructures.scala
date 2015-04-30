package ppl.delite.runtime
package profiler

import java.io.{File, PrintWriter}
import java.sql.{ Connection, DriverManager, ResultSet, SQLException, Statement }
import ppl.delite.runtime.Config
import _root_.scala.util.parsing.json.JSON
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source
import tools.nsc.io.Path

object DNodeType extends Enumeration {
	type DNodeType = Value
	val WhileLoop, Conditional, MultiLoop, SingleTask, Foreach, EOG, EOP = Value
}

object TNodeType extends Enumeration {
	type TNodeType = Value
	val Kernel, KernelPartition, Sync, TicTocRegion = Value
}

object Types {
	type DNodeId = Int
	type TNodeId = Int
	type LevelId = Int
	type DNodeName = String
	type TNodeName = String
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

	private def getSourceContext(op: Map[Any, Any]): String = {
		op.get("sourceContext") match {
			case Some(sc) => sc match {
				case map: Map[Any, Any] => map("fileName").asInstanceOf[String] + ":" + map("line").asInstanceOf[String]
				case _ => "_"
			}
			case _ => "_"
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

  	def dumpDepGraphInJSON(writer: PrintWriter, prefixSpace: String) {
  		val pre1 = prefixSpace + PostProcessor.tabs
		val pre2 = pre1 + PostProcessor.tabs
		val pre3 = pre2 + PostProcessor.tabs
		writer.println(prefixSpace + "\"DependencyGraph\": {")
		writer.println(pre1 + "\"nodes\": [")

  		for ((_, n) <- idToDNode) {
			writer.println(pre2 + "{ \"id\": " + n.id + "," + "\"name\": \"" + n.name + "\" },")
		}
		
		writer.println(pre2 + "{ \"id\": -1 }")
		writer.println(pre1 + "]")

		writer.println(pre1 + "\"edges\": [")
		for ((_, dNode) <- idToDNode) {
			val target = dNode.id
			dNode.inputNodes.map(n => nodeNameToId(n)).foreach(source => {
				writer.println(pre2 + "{ \"source\": " + source + "," + "\"target\": " + target + " },")		
			})
		}

		writer.println(pre2 + "{ \"source\": -1, \"target\": \"-1\" }")
		writer.println(pre1 + "]")

		writer.println(prefixSpace + "},")
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
		val sc = DependencyGraph.getSourceContext(op)
		opType match {
				case "Arguments" => dNodeArgsNew( op, DNodeType.SingleTask, sc )
				case "SingleTask" | "External" | "Input" => dNodeGenericNew( op, parent, DNodeType.SingleTask, sc ) 
				case "MultiLoop" => {
					val dNode = dNodeGenericNew( op, parent, DNodeType.MultiLoop, sc ) 
					DependencyGraph.getFieldList(op, "outputs").map(s => s.asInstanceOf[String])
								   .foreach(s => nodeNameToId += s -> dNode.id)
					dNode
				}
				case "Foreach" => dNodeGenericNew( op, parent, DNodeType.Foreach, sc ) 
				case "Conditional" => dNodeConditionalNew( op, parent, sc )
				case "WhileLoop" => dNodeWhileLoopNew( op, parent, sc )
				case "EOP" => new DNode( "eop", parent, DNodeType.EOP, sc )
				case "EOG" => new DNode( "eog", parent, DNodeType.EOG, sc )
			}
	}

	private def dNodeArgsNew(op: Map[Any, Any], _type: DNodeType.Value, sc: String): DNode = {
		val name = DependencyGraph.getFieldString(op, "kernelId")
		new DNode(name, null, _type, sc)
	}

	private def dNodeGenericNew(op: Map[Any, Any], parent: DNode, _type: DNodeType.Value, sc: String): DNode = {	
		val name = DependencyGraph.getFieldString(op, "kernelId")
		val node = new DNode(name, parent, _type, sc)

		DependencyGraph.getFieldList(op, "inputs").map( str => str.asInstanceOf[String] ).foreach( n => node.inputNodeAdd(n) )
		node
	}	

	private def dNodeConditionalNew(op: Map[Any, Any], parent: DNode, sc: String): DNode = {
		val name = DependencyGraph.getFieldString(op, "outputId")
		val node = new DNode(name, parent, DNodeType.WhileLoop, sc)

		DependencyGraph.dNodeConditionalSubOpTypes.foreach(st => {
			val subOps = DependencyGraph.getFieldList(op, st)
			dNodesNew( subOps.map(op => op.asInstanceOf[Map[Any, Any]]), node )
		})

		node
	}

	private def dNodeWhileLoopNew(op: Map[Any, Any], parent: DNode, sc: String): DNode = {
		val name = DependencyGraph.getFieldString(op, "outputId")
		val node = new DNode(name, parent, DNodeType.WhileLoop, sc)

		DependencyGraph.dNodeWhileloopSubOpTypes.foreach(st => {
			val subOps = DependencyGraph.getFieldList(op, st)
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

class DNode(_name: Types.DNodeName, _parent: DNode, t: DNodeType.Value, _sourceContext: String) {
	val name = _name
	val parent = _parent
	val _type = t
	val sourceContext = _sourceContext
	val id = DNode.nextId()
	val parentId = if (parent != null) parent.id else -1
	val level: Int = if (_parent == null) 0 else _parent.level + 1
	var targetsSupported = 0 // order of indices -> scala, cpp, cuda
	val inputNodes = new ArrayBuffer[String]
	val childNodes = new ArrayBuffer[DNode]

	if (_parent != null) {
		_parent.childNodeAdd(this)
	}

	def inputNodeAdd(cn: String) { inputNodes.append(cn) }
	def childNodeAdd(cn: DNode) { childNodes.append(cn) }

	//def cudaSupported(): Boolean = { targetsSupported(PostProcessor.TARGET_CUDA) }
	def cudaSupported(): Boolean = { (targetsSupported & PostProcessor.TARGET_CUDA) > 0 }

	def string() {
		Predef.println("    Name: " + name + " Id: " + id + " Type: " + _type.toString)
		Predef.println("    Inputs: " + inputNodes)
		Predef.println("    Child Nodes: " + childNodes.map(cn => cn.name))
	}
}

object TNode {
	private var _nextId: Int = -1

	def nextId(): Int = {
		_nextId += 1
		_nextId 
	}
}

class TNode(_name: Types.TNodeName, _threadId: Int, _start: Long, _duration: Long) {
	val name = _name
	val threadId: Int = _threadId
	val start: Long = _start
	val duration: Long = _duration
	val end: Long = start + duration
	val id = TNode.nextId()
	var execTime: Long = 0
	var syncTime: Long = 0
	val _type = ExecutionProfile.tNodeType(_name)
	val (dNode, dNodeName) = _type match {
		case TNodeType.Sync | TNodeType.TicTocRegion => (null, "")
		case _    => { 
			val n = ExecutionProfile.dNode(_name)
			(n, n.name)
		}
	}

	private var parentTNode: TNode = null
	var parentId: Types.TNodeId = -1;
	val level = if (dNode != null) dNode.level else -1
	var childKernelTNodes = new ArrayBuffer[TNode]
	var childSyncTNodes = new ArrayBuffer[TNode]
	val displayText = "" // TODO: This needs to be fixed

	def childNodeNew(cn: TNode) {
		cn._type match {
			case TNodeType.Sync => { childSyncTNodes.append(cn) }
			case _ => { childKernelTNodes.append(cn) }
		}
		
		cn.parentTNodeIs(this)
	}

	def parentTNodeIs(p: TNode) {
		parentTNode = p 
		if (p != null) { parentId = p.id }
	}

	def dNodeId(): Types.DNodeId = { if (dNode != null) dNode.id else -1 }
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
	var targetsEnabled = 1 // order of bits -> 0: scala, 1:cpp, 2:cuda
	var summaries = new HashMap[Types.TNodeName, ExecutionSummary] // TrieMap is the thread-safe version of HashMap
	var timelineData = new TimelineData(_depGraph.levelMax)
	var ticTocTNodes = new ArrayBuffer[TicTocTNode]
	var threadTNodes = new ArrayBuffer[ThreadTNode]

	private val dbPath = Config.profileOutputDirectory + "/profile.db"
	Path(dbPath).deleteIfExists()

	private val dbConn: Connection = DriverManager.getConnection("jdbc:sqlite:" + dbPath)
	private val dbStmt: Statement = dbConn.createStatement()

	def init() {
		ExecutionProfile.depGraph = _depGraph
		initDB()
		parseThreadSpecificProfileDataFiles( Config.profileOutputDirectory + "/profile_t_" )
		updateTimeTakenByPartitionedKernels()
	}

	//TODO: Rename this method
	def close() {
		dbStmt.close()
		dbConn.close()
	}

	def dumpAppDataInJSON(writer: PrintWriter, prefixSpace: String) {
		val sp = prefixSpace + PostProcessor.tabs
		writer.println(prefixSpace + "\"AppData\": {")
		writer.println(sp + "\"jvmUpTimeAtAppStart\": " + jvmUpTimeAtAppStart + ",")
	    writer.println(sp + "\"appStartTime\": " + appStartTime + ",")
	    writer.println(sp + "\"appTotalTime\": " + appTotalTime + ",")
	    writer.println(sp + "\"appEndTime\": " + appEndTime + ",")
	    writer.println(sp + "\"threadCount\": " + threadCount + ",")
	    writer.println(sp + "\"threadScalaCount\": " + threadScalaCount + ",")
	    writer.println(sp + "\"threadCppCount\": " + threadCppCount + ",")
	    writer.println(sp + "\"threadCudaCount\": " + threadCudaCount + ",")
	    writer.println(sp + "\"targetsEnabled\": " + targetsEnabledStr())
	    writer.println(prefixSpace + "},")
	}

	def dumpTimelineDataInJSON(writer: PrintWriter, prefixSpace: String) {
		val pre1 = prefixSpace + PostProcessor.tabs
		val pre2 = pre1 + PostProcessor.tabs
		val pre3 = pre2 + PostProcessor.tabs
		writer.println(prefixSpace + "\"TimelineData\": {")
		writer.println(pre1 + "\"nodes\": [")

		val nodesAtLevelZero = timelineData.tNodesAtLevel(0)
		for ((name, tNodes) <- nodesAtLevelZero) {
			for (n <- tNodes) {
				writer.println(pre2 + "{")
				writer.println(pre3 + "\"id\": " + n.id + ",")
				writer.println(pre3 + "\"lane\": " + n.threadId + ",")
				writer.println(pre3 + "\"start\": " + n.start + ",")
				writer.println(pre3 + "\"duration\": " + n.duration + ",")
				writer.println(pre3 + "\"end\": " + n.end + ",")
				writer.println(pre3 + "\"dNodeName\": \"" + n.dNodeName + "\",")
				writer.println(pre3 + "\"displayText\": \"" + n.displayText + "\"")
				writer.println(pre2 + "},")
			}
		}
		
		writer.println(pre2 + "{\"id\": -1}")
		writer.println(pre1 + "]")
		writer.println(prefixSpace + "},")
	}

	private def targetsEnabledStr(): String = {
		val a = new ArrayBuffer[String]
		if ((targetsEnabled & PostProcessor.TARGET_SCALA) > 0) { a.append("\"scala\"") }
		if ((targetsEnabled & PostProcessor.TARGET_CPP) > 0) { a.append("\"cpp\"") }
		if ((targetsEnabled & PostProcessor.TARGET_CUDA) > 0) { a.append("\"cuda\"") }

		"[" + a.mkString(",") + "]"
	}

	private def initDB() {
		//dbConn.setAutoCommit(false)
		val sql = " BEGIN TRANSACTION;" + 
		 		  " CREATE TABLE TNodes " +
						"(ID INT PRIMARY KEY  	 NOT NULL," +
						" NAME 			 	TEXT NOT NULL," +
						" START 		 	INT  NOT NULL," +
						" DURATION 		 	INT  NOT NULL," +
						" END 			 	INT  NOT NULL," +
						" THREAD_ID      	INT  NOT NULL," + 
						" EXEC_TIME		 	INT  NOT NULL," +
						" SYNC_TIME		 	INT  NOT NULL," +
						" TYPE		 	 	INT  NOT NULL," +
						" DNODE_ID		 	INT  NOT NULL," +
						" PARENT_ID		 	INT  NOT NULL," +
						" LEVEL		 	 	INT  NOT NULL," +
						" CHILD_KERNEL_IDS	TEXT NOT NULL," +
						" CHILD_SYNC_IDS	TEXT NOT NULL);\n" +
				  " CREATE TABLE DNodes " +
				  		"(ID INT PRIMARY KEY  NOT NULL," +
				  		" NAME 			 TEXT NOT NULL," +
				  		" TYPE 			 TEXT NOT NULL," +
				  		" PARENT_ID		 INT  NOT NULL," +
				  		" INPUT_NODE_IDS TEXT NOT NULL," +
				  		" CHILD_NODE_IDS TEXT NOT NULL," +
				  		" LEVEL          INT  NOT NULL," +
				  		" TARGETS_SUPP   INT  NOT NULL," +
				  		" SOURCE_CONTEXT TEXT NOT NULL);\n" +
				  " CREATE TABLE ExecutionSummaries " +
				  		"(NAME TEXT PRIMARY KEY NOT NULL," +
				  		" TOTAL_TIME 	INT  NOT NULL," +
				  		" EXEC_TIME  	INT  NOT NULL," +
				  		" SYNC_TIME  	INT  NOT NULL," +
				  		" MEM_USAGE  	INT  NOT NULL," +
				  		" L2_HIT_RATIO  REAL NOT NULL," +
				  		" L3_HIT_RATIO  REAL NOT NULL);\n" +
				  " CREATE TABLE DNodeTypes " +
				  		"(ID INT PRIMARY KEY NOT NULL,"+
				  		" NAME TEXT NOT NULL);\n" +
				  " INSERT INTO DNodeTypes (ID, NAME) VALUES(%d, 'WhileLoop');\n".format(DNodeType.WhileLoop.id) +
				  " INSERT INTO DNodeTypes (ID, NAME) VALUES(%d, 'Conditional');\n".format(DNodeType.Conditional.id) +
				  " INSERT INTO DNodeTypes (ID, NAME) VALUES(%d, 'MultiLoop');\n".format(DNodeType.MultiLoop.id) +
				  " INSERT INTO DNodeTypes (ID, NAME) VALUES(%d, 'SingleTask');\n".format(DNodeType.SingleTask.id) +
				  " INSERT INTO DNodeTypes (ID, NAME) VALUES(%d, 'Foreach');\n".format(DNodeType.Foreach.id) +
				  " INSERT INTO DNodeTypes (ID, NAME) VALUES(%d, 'EOG');\n".format(DNodeType.EOG.id) +
				  " INSERT INTO DNodeTypes (ID, NAME) VALUES(%d, 'EOP');\n".format(DNodeType.EOP.id) +
				  " CREATE TABLE TNodeTypes " +
				  		"(ID INT PRIMARY KEY NOT NULL,"+
				  		" NAME TEXT NOT NULL);\n" +
				  " INSERT INTO TNodeTypes (ID, NAME) VALUES(%d, 'Kernel');\n".format(TNodeType.Kernel.id) +
				  " INSERT INTO TNodeTypes (ID, NAME) VALUES(%d, 'KernelPartition');\n".format(TNodeType.KernelPartition.id) +
				  " INSERT INTO TNodeTypes (ID, NAME) VALUES(%d, 'Sync');\n".format(TNodeType.Sync.id) +
				  " INSERT INTO TNodeTypes (ID, NAME) VALUES(%d, 'TicTocRegion');\n".format(TNodeType.TicTocRegion.id) +
				  " COMMIT;"

	    Predef.println(sql)
		dbStmt.executeUpdate(sql)
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
								//n.parentTNode = tNode // This update is NOT required since its being done within the childNodeNew() method
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

					writeTNodeToDB(tNode)
				}
			}
		}
	}

	def writeDNodesToDB() {
		var sql = "BEGIN TRANSACTION;"
		for ((id, n) <- depGraph.idToDNode) {
			val inputNodesIds = if (n.inputNodes.length > 0) n.inputNodes.mkString(":") else "-1"
			val childNodeIds = if (n.childNodes.length > 0) n.childNodes.map(c => c.id).mkString(":") else "-1"
			sql += "INSERT INTO DNodes (ID,NAME,TYPE,PARENT_ID,INPUT_NODE_IDS,CHILD_NODE_IDS,LEVEL,TARGETS_SUPP, SOURCE_CONTEXT) VALUES (" + 
				   "%d,'%s',%d,%d,'%s','%s',%d,%d,'%s');\n".format(
				n.id, n.name, n._type.id, n.parentId, inputNodesIds, childNodeIds, n.level, n.targetsSupported, n.sourceContext)
		}

		sql += "COMMIT;"
		Predef.println(sql)
		dbStmt.executeUpdate(sql)
	}

	def writeExecutionSummariesToDB() {
		var sql = "BEGIN TRANSACTION;"
		for ((tNodeName, s) <- summaries) {
			sql += "INSERT INTO ExecutionSummaries (NAME,TOTAL_TIME,EXEC_TIME,SYNC_TIME,MEM_USAGE,L2_HIT_RATIO,L3_HIT_RATIO) VALUES (" +
				   "'%s',%d,%d,%d,%d,%f,%f);\n".format(
				   tNodeName, s.totalTime, s.execTime, s.syncTime, s.memUsage, s.l2CacheHitPct, s.l3CacheHitPct)
		}

		sql += "COMMIT;"
		Predef.println(sql)
		dbStmt.executeUpdate(sql)
	}

	private def writeTNodeToDB(n: TNode) {
		val childKernelIds = n.childKernelTNodes.map(c => c.id).mkString(":")
		val childSyncIds = n.childSyncTNodes.map(c => c.id).mkString(":")
		var sql = "INSERT INTO TNodes " + 
				   "(ID,NAME,START,DURATION,END,THREAD_ID, EXEC_TIME, SYNC_TIME, TYPE, DNODE_ID, PARENT_ID, LEVEL, CHILD_KERNEL_IDS, CHILD_SYNC_IDS) "+ 
				   "VALUES (%d,'%s',%s,%s,%s,%d,%d,%d,%d,%d,%d,%d,'%s','%s');".format(
				   n.id, n.name, n.start.toString, n.duration.toString, n.end.toString, n.threadId, n.execTime, n.syncTime, n._type.id, n.dNodeId(),
				   n.parentId, n.level, childKernelIds, childSyncIds)
		Predef.println(sql)
		dbStmt.executeUpdate(sql)
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
		for (kv <- aggrMemUsageStats) {
			val tNodeName = kv._1
  			val totalMemUsage = kv._2.sum
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
		//return (dNode.cudaSupported() && targetsEnabled(PostProcessor.TARGET_CUDA))
		return (dNode.targetsSupported & targetsEnabled & PostProcessor.TARGET_CUDA) > 0
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

class ExecutionSummary(_tt: Long = 0) {
	var totalTime: Long = _tt
	var execTime: Long = 0
	var syncTime: Long = 0
	var memUsage: Long = 0
	var l2CacheHitPct: Double = 0.0
	var l3CacheHitPct: Double = 0.0

	def totalTimeInc(inc: Long) { totalTime += inc }
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

class TicTocTNode(_name: String) {
	val name = _name
}

class ThreadTNode(_tid: Int) {
	val tid = _tid
}

