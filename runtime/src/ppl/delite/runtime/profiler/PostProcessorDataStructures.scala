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
  type TicTocNodeName = String
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

    case None => List[Any]()
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

  private def getSourceContext(op: Map[Any, Any]): SourceContext = {
    op.get("sourceContext") match {
      case Some(sc) => sc match {
        case map: Map[Any, Any] => {
          val fileName = map("fileName").asInstanceOf[String].split("/").last
          new SourceContext( fileName, map("line").asInstanceOf[String], map("opName").asInstanceOf[String] )
        }
        case _ => new SourceContext( "", "", "" )
      }
      case _ => new SourceContext( "", "", "" )
    }
  }

  private def getTargetsSupported(op: Map[Any, Any]): Int = {
    var target = 1
    if (op.contains("supportedTargets")) {
      val targetNames = getFieldList(op, "supportedTargets")
      if (targetNames.contains("scala")) { target |= PostProcessor.TARGET_SCALA }
      if (targetNames.contains("cpp")) { target |= PostProcessor.TARGET_CPP }
      if (targetNames.contains("cuda")) { target |= PostProcessor.TARGET_CUDA }
    }

    return target
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

  private var dNodeCount = 0

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
    val maxDNodeId = idToDNode.size - 1

    writer.println(prefixSpace + "\"DependencyGraph\": {")
    writer.println(pre1 + "\"nodes\": [")

    var n: DNode = null
    for (i <- 0 to maxDNodeId) {
      n = idToDNode(i)
      writer.println(pre2 + "{ \"id\": " + n.id + "," + "\"name\": \"" + n.name + "\", \"numInputs\": " + n.inputNodes.length + ", \"numOutputs\": " + n.outputNodes.length + " },")
    }
    
    writer.println(pre2 + "{ \"id\": -1 }")
    writer.println(pre1 + "],")
    writer.println(pre1 + "\"edges\": [")

    for (i <- 0 to maxDNodeId) {
      n = idToDNode(i)
      val target = n.id
      n.inputNodes.foreach(source => {
        writer.println(pre2 + "{ \"source\": " + source + "," + "\"target\": " + target + " },")    
      })
    }

    writer.println(pre2 + "{ \"source\": -1, \"target\": \"-1\" }")
    writer.println(pre1 + "]")
    writer.println(prefixSpace + "},")
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
    def helper( dNodeName: String, parent: DNode, _type: DNodeType.Value, sc: SourceContext, targetsSupported: Int ): DNode = {
      if ( nodeNameToId.contains( dNodeName ) ) {
        return dNode( dNodeName )
      }

      return new DNode( dNodeName, parent, _type, sc, targetsSupported )
    }

    val opType = DependencyGraph.getFieldString(op, "type")
    val sc = DependencyGraph.getSourceContext(op)
    val targetsSupported = DependencyGraph.getTargetsSupported(op)
    opType match {
        case "Arguments" => dNodeArgsNew( op, DNodeType.SingleTask, sc, targetsSupported )
        case "SingleTask" | "External" | "Input" => dNodeGenericNew( op, parent, DNodeType.SingleTask, sc, targetsSupported ) 
        case "MultiLoop" => {
          val dNode = dNodeGenericNew( op, parent, DNodeType.MultiLoop, sc, targetsSupported ) 
          DependencyGraph.getFieldList(op, "outputs").map(s => s.asInstanceOf[String])
                   .foreach(s => nodeNameToId += s -> dNode.id)
          dNode
        }
        case "Foreach" => dNodeGenericNew( op, parent, DNodeType.Foreach, sc, targetsSupported ) 
        case "Conditional" => dNodeConditionalNew( op, parent, sc, targetsSupported )
        case "WhileLoop" => dNodeWhileLoopNew( op, parent, sc, targetsSupported )
        case "EOP" => helper( "eop", parent, DNodeType.EOP, sc, targetsSupported )
        case "EOG" => helper( "eog", parent, DNodeType.EOG, sc, targetsSupported )
      }
  }

  private def dNodeArgsNew(op: Map[Any, Any], _type: DNodeType.Value, sc: SourceContext, targetsSupported: Int): DNode = {
    val name = DependencyGraph.getFieldString(op, "kernelId")
    new DNode( name, null, _type, sc, targetsSupported )
  }

  private def dNodeGenericNew(op: Map[Any, Any], parent: DNode, _type: DNodeType.Value, sc: SourceContext, targetsSupported: Int): DNode = {  
    val name = DependencyGraph.getFieldString(op, "kernelId")
    val node = new DNode( name, parent, _type, sc, targetsSupported )

    DependencyGraph.getFieldList(op, "inputs").map( str => str.asInstanceOf[String] ).foreach(n => { 
      val id = nodeNameToId(n)
      node.inputNodeAdd(id)
      dNode(n).outputNodeAdd(node.id)
    })
    node
  }  

  private def dNodeConditionalNew(op: Map[Any, Any], parent: DNode, sc: SourceContext, targetsSupported: Int): DNode = {
    val name = DependencyGraph.getFieldString(op, "outputId")
    val node = new DNode( name, parent, DNodeType.WhileLoop, sc, targetsSupported )

    DependencyGraph.dNodeConditionalSubOpTypes.foreach(st => {
      val subOps = DependencyGraph.getFieldList(op, st)
      dNodesNew( subOps.map(op => op.asInstanceOf[Map[Any, Any]]), node )
    })

    node
  }

  private def dNodeWhileLoopNew(op: Map[Any, Any], parent: DNode, sc: SourceContext, targetsSupported: Int): DNode = {
    val name = DependencyGraph.getFieldString(op, "outputId")
    val node = new DNode( name, parent, DNodeType.WhileLoop, sc, targetsSupported )

    DependencyGraph.dNodeWhileloopSubOpTypes.foreach(st => {
      val subOps = DependencyGraph.getFieldList(op, st)
      dNodesNew( subOps.map(op => op.asInstanceOf[Map[Any, Any]]), node )
    })

    node
  }
}

class SourceContext (val fileName: String, val line: String, val opName: String) {
  def string = fileName + ":" + line
}

object DNode {
  private var _nextId: Int = -1

  def nextId(): Int = {
    _nextId += 1
    _nextId 
  }
}

class DNode(val name: Types.DNodeName, val parent: DNode, val _type: DNodeType.Value, val sourceContext: SourceContext, val targetsSupported: Int) {
  val id = DNode.nextId()
  val (parentId, level: Int) = if (parent == null) (-1, 0) else (parent.id, parent.level + 1)

  private val _inputNodes = new ArrayBuffer[Types.DNodeId]
  private val _outputNodes = new ArrayBuffer[Types.DNodeId]
  private val _childNodes = new ArrayBuffer[DNode]

  if (parent != null) { parent.childNodeAdd(this) }

  def inputNodes() = { _inputNodes }
  def outputNodes() = { _outputNodes }
  def childNodes() = { _childNodes }

  def inputNodeAdd(n: Types.DNodeId) { _inputNodes.append(n) }
  def outputNodeAdd(n: Types.DNodeId) { _outputNodes.append(n) }
  def childNodeAdd(cn: DNode) { _childNodes.append(cn) }

  def cudaSupported(): Boolean = { (targetsSupported & PostProcessor.TARGET_CUDA) > 0 }
}

object TNode {
  private var _nextId: Int = -1

  def nextId(): Int = {
    _nextId += 1
    _nextId 
  }
}

class TNode(val name: Types.TNodeName, val threadId: Int, val start: Long, val duration: Long, val level: Int) {
  val end: Long = start + duration
  val id = TNode.nextId()
  var execTime: Long = duration
  var syncTime: Long = 0
  val _type = ExecutionProfile.tNodeType(name)
  val (dNode, dNodeName, displayText, dNodeId) = _type match { // displayText is for the timeline view in the UI
    case TNodeType.Sync => (null, "", "", -1)
    case TNodeType.TicTocRegion => (null, "", name, -1)
    case _    => { 
      val n = ExecutionProfile.dNode(name)
      (n, n.name, n.sourceContext.opName, n.id)
    }
  }

  private var _parentTNode: TNode = null
  private var _parentId: Types.TNodeId = -1;
  var childKernelTNodes = new ArrayBuffer[TNode]
  var childSyncTNodes = new ArrayBuffer[TNode]

  def childNodeNew(cn: TNode) {
    cn._type match {
      case TNodeType.Sync => { 
        childSyncTNodes.append(cn)
        execTime -= cn.duration
        syncTime += cn.duration
      }

      case _ => { 
        childKernelTNodes.append(cn)
        execTime -= cn.syncTime
        syncTime += cn.syncTime
      }
    }
    
    cn.parentTNodeIs(this)
  }

  def parentTNodeIs(p: TNode) {
    _parentTNode = p 
    _parentId = p.id
  }

  def parentId(): Types.TNodeId = { return _parentId }
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
    } else if ((tNodeName == "eop") || (tNodeName == "eog")) {
      return TNodeType.Kernel
    } else if (tNodeName(0) != 'x') {  // TODO: This logic would fail if a tic-toc node's name starts with 'x'
      return TNodeType.TicTocRegion
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

  def computeTargetsEnabled(): Int = {
    val scalaEnabled = if (Config.numThreads > 0) PostProcessor.TARGET_SCALA else 0
    val cppEnabled = if (Config.numCpp > 0) PostProcessor.TARGET_CPP else 0
    val cudaEnabled = if (Config.numCuda > 0) PostProcessor.TARGET_CUDA else 0

    return (scalaEnabled | cppEnabled | cudaEnabled)
  }
}

class ExecutionProfile(val depGraph: DependencyGraph, val profileOutputDirectory: String) {
  val threadScalaCount: Int = Config.numThreads
  val threadCppCount: Int = Config.numCpp
  val threadCudaCount: Int = Config.numCuda
  var threadCount: Int = threadScalaCount + threadCppCount + threadCudaCount
  val jvmUpTimeAtAppStart: Long = PerformanceTimer.jvmUpTimeAtAppStart
  val appStartTime: Long = PerformanceTimer.appStartTimeInMillis
  var appTotalTime: Long = 0
  var appEndTime: Long = 0
  val targetsEnabled = ExecutionProfile.computeTargetsEnabled() // order of bits -> 0: scala, 1:cpp, 2:cuda
  val summaries = new HashMap[Types.TNodeName, ExecutionSummary]
  val ticTocNodeSummaries = new HashMap[Types.TicTocNodeName, TicTocNodeSummary]
  val timelineData = new TimelineData(depGraph.levelMax)
  val ticTocTNodes = new ArrayBuffer[TicTocTNode]
  private val dbPath = profileOutputDirectory + "/" + PostProcessor.profileDBFileName
  new File(dbPath).delete()

  private val dbConn: Connection = DriverManager.getConnection("jdbc:sqlite:" + dbPath)
  private val dbStmt: Statement = dbConn.createStatement()

  def init() {
    ExecutionProfile.depGraph = depGraph
    initDB()

    val fileNamePrefix = profileOutputDirectory + "/profile_t_"
    parseThreadSpecificProfileDataFiles( fileNamePrefix )

    val tmp = profileOutputDirectory + "/profile_tic_toc_"
    parseTicTocRegionsDataFile( tmp )
    updateTimeTakenByPartitionedKernels()
    updateMemUsageDataOfDNodes()
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
      writer.println(sp + "\"isScalaEnabled\": " + (targetsEnabled & PostProcessor.TARGET_SCALA) + ",")
      writer.println(sp + "\"isCppEnabled\": " + (targetsEnabled & PostProcessor.TARGET_CPP) + ",")
      writer.println(sp + "\"isCudaEnabled\": " + (targetsEnabled & PostProcessor.TARGET_CUDA))
      writer.println(prefixSpace + "},")
  }

  def dumpTimelineDataInJSON(writer: PrintWriter, prefixSpace: String) {
    val pre1 = prefixSpace + PostProcessor.tabs
    val pre2 = pre1 + PostProcessor.tabs
    val pre3 = pre2 + PostProcessor.tabs
    writer.println(prefixSpace + "\"TimelineData\": {")
    writer.println(pre1 + "\"Timing\": [")

    val nodesAtLevelZero = timelineData.tNodesAtLevel(0)
    for ((name, tNodes) <- nodesAtLevelZero) {
      for (n <- tNodes) {
        writer.println(pre2 + "{")
        writer.println(pre3 + "\"id\": " + n.id + ",")
        writer.println(pre3 + "\"name\": \"" + name + "\",")
        writer.println(pre3 + "\"threadId\": " + n.threadId + ",")
        writer.println(pre3 + "\"start\": " + n.start + ",")
        writer.println(pre3 + "\"duration\": " + n.duration + ",")
        writer.println(pre3 + "\"end\": " + n.end + ",")
        writer.println(pre3 + "\"dNodeName\": \"" + n.dNodeName + "\",")
        writer.println(pre3 + "\"displayText\": \"" + n.displayText + "\"")
        writer.println(pre2 + "},")
      }
    }
    
    writer.println(pre2 + "{\"id\": -1}")
    writer.println(pre1 + "],")
    dumpTicTocRegionsDataInJSON( writer, pre1 )
    writer.println(prefixSpace + "},")
  }

  private def dumpTicTocRegionsDataInJSON( writer: PrintWriter, prefixSpace: String ) {
    val pre1 = prefixSpace + PostProcessor.tabs
    val pre2 = pre1 + PostProcessor.tabs

    def helper(n: TicTocTNode) {
      writer.println(pre1 + "{")
      writer.println(pre2 + "\"id\": " + n.id + ",")
      writer.println(pre2 + "\"name\": \"" + n.name + "\",")
      writer.println(pre2 + "\"start\": " + n.start + ",")
      writer.println(pre2 + "\"duration\": " + n.duration + ",")
      writer.println(pre2 + "\"end\": " + n.end)
    }

    val len = ticTocTNodes.length
    if (len > 0) {
      writer.println(prefixSpace + "\"TicTocRegions\": [")

      for (i <- 0 to (ticTocTNodes.length - 2)) {
        helper( ticTocTNodes(i) )
        writer.println(pre1 + "},")
      }

      helper( ticTocTNodes(len - 1) )
      writer.println(pre1 + "}")
      writer.println(prefixSpace + "]")
    }
  }

  private def initDB() {
    val sql = " BEGIN TRANSACTION;" + 
          " CREATE TABLE AppData " +
              "(JVM_UP_TIME_AT_APP_START INT PRIMARY KEY NOT NULL," +
              " APP_START_TIME    INT NOT NULL," +
              " APP_END_TIME      INT NOT NULL," +
              " APP_TOTAL_TIME    INT NOT NULL," +
              " THREAD_COUNT      INT NOT NULL," +
              " THREAD_SCALA_COUNT INT NOT NULL," +
              " THREAD_CPP_COUNT    INT NOT NULL," +
              " THREAD_CUDA_COUNT  INT NOT NULL," +
              " IS_SCALA_ENABLED    INT NOT NULL," +
              " IS_CPP_ENABLED    INT NOT NULL," +
              " IS_CUDA_ENABLED    INT NOT NULL);\n" +
           " CREATE TABLE TNodes " +
            "(id INT PRIMARY KEY     NOT NULL," +
            " name          TEXT NOT NULL," +
            " start        INT  NOT NULL," +
            " duration        INT  NOT NULL," +
            " end          INT  NOT NULL," +
            " threadId        INT  NOT NULL," + 
            " execTime       INT  NOT NULL," +
            " syncTime       INT  NOT NULL," +
            " type          INT  NOT NULL," +
            " dNodeId       INT  NOT NULL," +
            " parentId       INT  NOT NULL," +
            " level          INT  NOT NULL," +
            " childKernelIds  TEXT NOT NULL," +
            " childSyncIds    TEXT NOT NULL);\n" +
           " CREATE TABLE TicTocTNodes " +
            "(ID INT PRIMARY KEY     NOT NULL," +
            " NAME     TEXT NOT NULL," +
            " START   INT  NOT NULL," +
            " DURATION   INT  NOT NULL," +
            " END     INT  NOT NULL);\n" +
          " CREATE TABLE DNodes " +
              "(ID INT PRIMARY KEY   NOT NULL," +
              " NAME         TEXT NOT NULL," +
              " TYPE         INT  NOT NULL," +
              " PARENT_ID      INT  NOT NULL," +
              " INPUT_NODE_IDS  TEXT NOT NULL," +
              " OUTPUT_NODE_IDS TEXT NOT NULL," +
              " CHILD_NODE_IDS  TEXT NOT NULL," +
              " LEVEL           INT  NOT NULL," +
              " TARGETS_SUPP    INT  NOT NULL," +
              " SOURCE_CONTEXT  TEXT NOT NULL);\n" +
          " CREATE TABLE ExecutionSummaries " +
              "(NAME TEXT PRIMARY KEY NOT NULL," +
              " TOTAL_TIME    INT  NOT NULL," +
              " TOTAL_TIME_PCT REAL NOT NULL," +
              " EXEC_TIME     INT  NOT NULL," +
              " SYNC_TIME     INT  NOT NULL," +
              " MEM_USAGE     INT  NOT NULL);" +
          " CREATE TABLE TicTocNodeSummaries " +
              "(NAME TEXT PRIMARY KEY NOT NULL," +
              " TOTAL_TIME INT  NOT NULL);\n" +
          " CREATE TABLE DNodeTypes " +
              "(ID INT PRIMARY KEY NOT NULL,"+
              " NAME TEXT NOT NULL);\n" +
          " CREATE TABLE KernelMemAccessStats " +
              "(NAME TEXT PRIMARY KEY NOT NULL," +
              " L2_CACHE_MISS_PCT  INT  NOT NULL," +
              " L2_CACHE_MISSES    INT NOT NULL," +
              " L3_CACHE_MISS_PCT  INT  NOT NULL," +
              " L3_CACHE_MISSES    INT NOT NULL);\n" +
          " CREATE TABLE ArrayCacheAccessStats " +
              "(ID INT PRIMARY KEY NOT NULL,"+
              " NAME TEXT NOT NULL," +
              " L1_CACHE_MISS_PCT    INT  NOT NULL," +
              " L2_CACHE_MISS_PCT    INT  NOT NULL," +
              " L3_CACHE_MISS_PCT    INT  NOT NULL," +
              " LOCAL_DRAM_MISS_PCT  INT  NOT NULL," +
              " REMOTE_DRAM_MISS_PCT INT  NOT NULL);\n" +
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

    dbStmt.executeUpdate(sql)
  }

  def tNodeTotalTime(tNodeName: String): Long = {
    if (summaries.contains(tNodeName)) {
      return summaries(tNodeName).totalTime
    }

    return 0
  }

  def parseThreadSpecificProfileDataFiles( fileNamePrefix: String ) {
    val MIN_DURATION_MILLIS: Double = 1
    val levelToTNodesYetToBeAssignedParent = new HashMap[Int, ArrayBuffer[TNode]]
    val tNodesPendingWritesToDB = new ArrayBuffer[TNode]
    
    def createTNode(name: String, threadId: Int, start: Long, duration: Double, level: Int) {
      val tNode = new TNode(name, threadId, start, duration.toLong, level)
      tNodesPendingWritesToDB.append( tNode )

      if (level >= 0) {
        if (level < depGraph.levelMax) {
          val a = levelToTNodesYetToBeAssignedParent( level + 1 )
          a.foreach(n => { tNode.childNodeNew(n) })
          a.clear()
        }

        if (level > 0) {
          levelToTNodesYetToBeAssignedParent( level ).append( tNode )
        } else {
          writeTNodesToDB( tNodesPendingWritesToDB )
          tNodesPendingWritesToDB.clear()
        }
      }

      updateSummary(tNode)
      timelineData.tNodeNew(tNode)
    }

    for (tid <- 0 to (threadCount - 1)) {
      val isScala  = (tid < threadScalaCount)
      val levelToAggrTNodeStart = new HashMap[Int, Long] 
      val levelToAggrTNodeDuration = new HashMap[Int, Double]

      for (level <- 0 to (depGraph.levelMax + 1)) {
        levelToTNodesYetToBeAssignedParent += level -> new ArrayBuffer[TNode]
        levelToAggrTNodeStart += level -> -1
        levelToAggrTNodeDuration += level -> 0
      }  
    
      val fn = fileNamePrefix + tid + ".csv"
      for (line <- Source.fromFile(fn).getLines()) {
        val arr = line.split(",")
        val name = arr(0)

        val tmp = arr(1).toLong
        val start = if (isScala) (tmp - PerformanceTimer.appStartTimeInMillis) else tmp
        val duration = arr(2).toDouble
        val level = arr(3).toInt
        val aggrTNodeStart = levelToAggrTNodeStart(level)

        levelToAggrTNodeStart(level + 1) = -1
        levelToAggrTNodeDuration(level + 1) = 0

        if (duration < MIN_DURATION_MILLIS) {
          if (aggrTNodeStart < 0) { levelToAggrTNodeStart(level) = start }
          levelToAggrTNodeDuration += level -> (levelToAggrTNodeDuration(level) + duration)
        } else {
          if (aggrTNodeStart >= 0) {
            val d = levelToAggrTNodeDuration(level)
            if (d >= MIN_DURATION_MILLIS) { 
              createTNode(":aggregate", tid, aggrTNodeStart, d, level)
            }

            levelToAggrTNodeStart(level) = -1
            levelToAggrTNodeDuration(level) = 0
          }
          
          createTNode(name, tid, start, duration, level)
        }
      }

      val aggrTNodeStart = levelToAggrTNodeStart(0)
      if (aggrTNodeStart >= 0) {
        val d = levelToAggrTNodeDuration(0)
        if (d >= MIN_DURATION_MILLIS) { 
          createTNode(":aggregate", tid, aggrTNodeStart, d, 0)
        }
      }
    }
  }

  def parseTicTocRegionsDataFile( fileNamePrefix: String ) {
    val targets = List("scala", "cpp", "cuda")
    var id = 0
    for (tg <- targets) {
      val fn = fileNamePrefix + tg + ".csv"
      if (new File(fn).exists) {
        for (line <- Source.fromFile(fn).getLines()) {
          val arr = line.split(",")
          val name = arr(0)
          val tmp = arr(1).toLong
          val start = if (tg == "cpp") tmp else tmp - PerformanceTimer.appStartTimeInMillis 
          val duration = arr(2).toDouble.toLong
          ticTocTNodes.append( new TicTocTNode(id, name, start, duration, start + duration) )
          id += 1

          val s = ticTocNodeSummary( name )
          s.totalTimeInc( duration )

          if (name == "all") {
            appTotalTime = duration
            appEndTime = appStartTime + appTotalTime
          }
        }
      }
    }
  }

  def writeAppDataToDB() {
    val isScalaEnabled = (targetsEnabled & PostProcessor.TARGET_SCALA)
    val isCppEnabled = (targetsEnabled & PostProcessor.TARGET_CPP)
    val isCudaEnabled = (targetsEnabled & PostProcessor.TARGET_CUDA)
    var sql = "INSERT INTO AppData VALUES (%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d)".format(jvmUpTimeAtAppStart, appStartTime, appEndTime, appTotalTime, threadCount, threadScalaCount, threadCppCount, threadCudaCount, isScalaEnabled, isCppEnabled, isCudaEnabled)
    dbStmt.executeUpdate(sql)
  }

  def writeDNodesToDB() {
    var sql = "BEGIN TRANSACTION;"
    for ((id, n) <- depGraph.idToDNode) {
      val inputNodesIds = n.inputNodes.mkString(":")
      val outputNodeIds = n.outputNodes.mkString(":")
      val childNodeIds = n.childNodes.map(c => c.id).mkString(":")
      sql += "INSERT INTO DNodes (ID,NAME,TYPE,PARENT_ID,INPUT_NODE_IDS,OUTPUT_NODE_IDS,CHILD_NODE_IDS,LEVEL,TARGETS_SUPP, SOURCE_CONTEXT) VALUES (" + 
           "%d,'%s',%d,%d,'%s','%s','%s',%d,%d,'%s');\n".format(
           n.id, n.name, n._type.id, n.parentId, inputNodesIds, outputNodeIds, childNodeIds, n.level, n.targetsSupported, n.sourceContext.string)
    }

    sql += "COMMIT;"
    dbStmt.executeUpdate(sql)
  }

  def writeExecutionSummariesToDB() {
    var sql = "BEGIN TRANSACTION;"
    for ((tNodeName, s) <- summaries) {
      val totalTimePct: Double = (s.totalTime * 100) / appTotalTime
      sql += "INSERT INTO ExecutionSummaries (NAME,TOTAL_TIME,TOTAL_TIME_PCT,EXEC_TIME,SYNC_TIME,MEM_USAGE) VALUES (" +
           "'%s',%d,%f,%d,%d,%d);\n".format(
           tNodeName, s.totalTime, totalTimePct, s.execTime, s.syncTime, s.memUsage)
    }

    sql += "COMMIT;"
    dbStmt.executeUpdate(sql)
  }

  def writeTicTocTNodesToDB() {
    var sql = "BEGIN TRANSACTION;\n"

    for (n <- ticTocTNodes) {
      sql += "INSERT INTO TicTocTNodes(ID,NAME,START,DURATION,END) VALUES (%d,'%s',%d,%d,%d);\n".format(n.id, n.name, n.start, n.duration, n.end)
    }

    sql += "COMMIT;\n"
    dbStmt.executeUpdate(sql)
  }

  def writeTicTocNodeSummariesToDB() {
    var sql = "BEGIN TRANSACTION;\n"

    for ((name, s) <- ticTocNodeSummaries) {
      sql += "INSERT INTO TicTocNodeSummaries (NAME, TOTAL_TIME) VALUES ('%s',%d);\n".format(name, s.totalTime)
    }

    sql += "COMMIT;\n"
    dbStmt.executeUpdate(sql)
  }

  def writeKernelMemAccessStatsToDB() {
    if (threadCppCount > 0) {
      var sql = "BEGIN TRANSACTION;"
      val kernelToMemAccessStats = MemoryProfiler.aggregateMemAccessStats();

      for (kv <- kernelToMemAccessStats) {
        val kernel = kv._1.split("/").last
        val stats = kv._2
        sql += "INSERT INTO KernelMemAccessStats " +
        "(NAME, L2_CACHE_MISS_PCT, L2_CACHE_MISSES, L3_CACHE_MISS_PCT, L3_CACHE_MISSES) VALUES ('%s',%d,%d,%d,%d);\n".format( kernel,
                100 - Math.floor(stats.l2CacheHitRatio * 100).toInt, stats.l2CacheMisses, 
                100 - Math.floor(stats.l3CacheHitRatio * 100).toInt, stats.l3CacheMisses)
      }

      sql += "COMMIT;"
      dbStmt.executeUpdate(sql)
    }
  }

  private def summary(n: Types.TNodeName): ExecutionSummary = {
    if (!summaries.contains(n)) {
      summaries(n) = new ExecutionSummary()
    }

    summaries(n)
  }

  private def ticTocNodeSummary(n: Types.TicTocNodeName): TicTocNodeSummary = {
    if (!ticTocNodeSummaries.contains(n)) {
      ticTocNodeSummaries(n) = new TicTocNodeSummary()
    }

    ticTocNodeSummaries(n)
  }

  private def writeTNodesToDB( tNodes: ArrayBuffer[TNode] ) {
    var sql = " BEGIN TRANSACTION;\n"
    for (n <- tNodes) {
      val childKernelIds = n.childKernelTNodes.map(c => c.id).mkString(":")
      val childSyncIds = n.childSyncTNodes.map(c => c.id).mkString(":")
      sql += "INSERT INTO TNodes " + 
        "(id,name,start,duration,end,threadId, execTime, syncTime, type, dNodeId, parentId, level, childKernelIds, childSyncIds) "+ 
        "VALUES (%d,'%s',%s,%s,%s,%d,%d,%d,%d,%d,%d,%d,'%s','%s');\n".format(
        n.id, n.name, n.start.toString, n.duration.toString, n.end.toString, n.threadId, n.execTime, n.syncTime, n._type.id, n.dNodeId,
        n.parentId, n.level, childKernelIds, childSyncIds)
    }

    sql += " COMMIT;"
    dbStmt.executeUpdate( sql )
  }

  private def updateSummary(tNode: TNode) {
    tNode._type match {
      case TNodeType.Kernel | TNodeType.KernelPartition => {
        val summ = summary(tNode.name)
        summ.totalTimeInc(tNode.duration)
        summ.syncTimeInc(tNode.syncTime)
      }
      case _ => ()
    }
  }

  private def updateTimeTakenByPartitionedKernels() {
    def tryGetSummary(tNodeName: String): Option[ExecutionSummary] = {
      if (summaries.contains(tNodeName)) { Some(summaries(tNodeName)) }
      else { None }
    }

    val threadIdsRange = Range(0, threadCount)
    for (dNode <- depGraph.loopNodes) {
      if (!isTargetCuda(dNode)) {
        val headerNodeTotalTime = tNodeTotalTime(dNode.name + "_h")
        val partitionSummaries = threadIdsRange.map(tid => tryGetSummary(dNode.name + "_" + tid)).flatten
        val n = partitionSummaries.length
        val avgPartitionTotalTime: Long = if (n > 0) partitionSummaries.map(s => s.totalTime).sum / n else 0
        val avgPartitionSyncTime: Long = if (n > 0) partitionSummaries.map(s => s.syncTime).sum / n else 0
        val summ = new ExecutionSummary(headerNodeTotalTime + avgPartitionTotalTime)
        summ.syncTimeInc(avgPartitionSyncTime)
        summaries(dNode.name) = summ
      }
    }
  }

  private def updateMemUsageDataOfDNodes() {
    val aggrMemUsageStats = MemoryProfiler.aggregateMemAllocStatsFromAllThreads()
    for (kv <- aggrMemUsageStats) {
      val tNodeName = kv._1
        val totalMemUsage = kv._2
        memUsageIs(tNodeName, totalMemUsage)

        val pn = ExecutionProfile.parentLoopKernelName(tNodeName)
        pn match {
          case Some(parent) => memUsageInc(parent, totalMemUsage)
          case _ => // do nothing
        }
      }
  }

  private def memUsageIs(tNodeName: String, mu: Long) {
    val s = summary(tNodeName)
    s.memUsage = mu
  }

  private def memUsageInc(tNodeName: String, inc: Long) {
    val s = summary(tNodeName)
    s.memUsage += inc
  }

  private def isTargetCuda(dNode: DNode): Boolean = {
    return (dNode.targetsSupported & targetsEnabled & PostProcessor.TARGET_CUDA) > 0
  }
}

class ExecutionSummary(_tt: Long = 0) {
  var memUsage: Long = 0
  var l2CacheHitPct: Int = 0
  var l3CacheHitPct: Int = 0

  private var _totalTime: Long = _tt
  private var _execTime: Long = _tt
  private var _syncTime: Long = 0

  def totalTime: Long = { _totalTime }
  def execTime: Long = { _execTime }
  def syncTime: Long = { _syncTime }

  def totalTimeInc(inc: Long) { 
    _totalTime += inc 
    _execTime += inc
  }

  def syncTimeInc(inc: Long) {
    _execTime -= inc
    _syncTime += inc
  }
}

class TicTocNodeSummary() {
  private var _totalTime: Long = 0

  def totalTime: Long = { _totalTime }

  def totalTimeInc(inc: Long) { _totalTime += inc }
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

class TicTocTNode( val id: Int, val name: String, val start: Long, val duration: Long, val end: Long) { }

