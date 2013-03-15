package ppl.delite.runtime

import org.apache.mesos._
import org.apache.mesos.Protos._
import com.google.protobuf.ByteString
import java.util.concurrent.CountDownLatch
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.ArrayBlockingQueue
import java.util.{ArrayList,HashMap}
import ppl.delite.runtime.data._
import ppl.delite.runtime.executor.ThreadPool
import ppl.delite.runtime.codegen.DeliteExecutable
import ppl.delite.runtime.messages.Messages._
import ppl.delite.runtime.messages._
import ppl.delite.runtime.graph.ops.{DeliteOP,OP_MultiLoop}
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.data._

class DeliteMesosExecutor extends Executor {
  
  /**
   * Invoked once the executor driver has been able to successfully
   * connect with Mesos. In particular, a scheduler can pass some
   * data to it's executors through the {@link ExecutorInfo#data}
   * field. TODO(vinod): Add a new reregistered callback for when the executor
   * re-connects with a restarted slave.
   */
  def registered(driver: ExecutorDriver, executorInfo: ExecutorInfo, frameworkInfo: FrameworkInfo, slaveInfo: SlaveInfo) {
    println("Registered executor on " + slaveInfo.getHostname)
  }

  /**
   * Invoked when the executor re-registers with a restarted slave.
   */
  def reregistered(driver: ExecutorDriver, slaveInfo: SlaveInfo) {
    println("Re-registered executor on " + slaveInfo.getHostname)
  }

  /**
   * Invoked when the executor becomes "disconnected" from the slave
   * (e.g., the slave is being restarted due to an upgrade).
   */
  def disconnected(driver: ExecutorDriver) {
    println("WARNING: executor disconnected")
  }

  /**
   * Invoked when a task has been launched on this executor (initiated
   * via {@link Scheduler#launchTasks}. Note that this task can be
   * realized with a thread, a process, or some simple computation,
   * however, no other callbacks will be invoked on this executor
   * until this callback has returned.
   */
  def launchTask(driver: ExecutorDriver, task: TaskInfo) {
    def status(state: TaskState) = {
      TaskStatus.newBuilder
        .setTaskId(task.getTaskId)
        .setState(state)
        .build
    }

    def exceptionToString(e: Exception) = {
      val stringWriter = new java.io.StringWriter
      e.printStackTrace(new java.io.PrintWriter(stringWriter))
      stringWriter.toString
    }

    val runner = new Runnable {
      def run() {
        try {
          val info = LaunchInfo.parseFrom(task.getData)
          
          val networkId = DeliteMesosExecutor.network.id
          val taskStarted = TaskStatus.newBuilder.setTaskId(task.getTaskId).setState(TaskState.TASK_RUNNING)
            .setData(CommInfo.newBuilder.setSlaveIdx(info.getSlaveIdx).addSlaveAddress(networkId.host).addSlavePort(networkId.port).build.toByteString).build
          driver.sendStatusUpdate(taskStarted)

          val args = info.getArgList.toArray(new Array[String](0))

          DeliteMesosExecutor.networkMap.put(-1, ConnectionManagerId(info.getMasterAddress, info.getMasterPort))
          
          DeliteMesosExecutor.sendDebugMessage("my master is " + info.getMasterAddress + ":" + info.getMasterPort)
          DeliteMesosExecutor.sendDebugMessage("my connection is " + DeliteMesosExecutor.network.id.host + ":" + DeliteMesosExecutor.network.id.port)
          DeliteMesosExecutor.sendDebugMessage("my input args are " + args.mkString(", "))

          Delite.embeddedMain(args, Map())

          DeliteMesosExecutor.network.stop()
          driver.sendStatusUpdate(status(TaskState.TASK_FINISHED))
        }
        catch {
          case e: Exception => //serialize the exception and forward to the master
            DeliteMesosExecutor.sendDebugMessage(exceptionToString(e))
            driver.sendStatusUpdate(status(TaskState.TASK_FAILED))
        }
      }
    }

    new Thread(runner, "ExecutorMainThread").start()
  }

  /**
   * Invoked when a task running within this executor has been killed
   * (via {@link SchedulerDriver#killTask}). Note that no status
   * update will be sent on behalf of the executor, the executor is
   * responsible for creating a new TaskStatus (i.e., with
   * TASK_KILLED) and invoking {@link
   * ExecutorDriver#sendStatusUpdate}.
   */
  def killTask(driver: ExecutorDriver, taskId: TaskID) {
    println("WARNING: task killed")
    val status = TaskStatus.newBuilder
      .setTaskId(taskId)
      .setState(TaskState.TASK_KILLED)
      .build

    driver.sendStatusUpdate(status)
  }

  /**
   * Invoked when a framework message has arrived for this
   * executor. These messages are best effort; do not expect a
   * framework message to be retransmitted in any reliable fashion.
   */
  def frameworkMessage(driver: ExecutorDriver, data: Array[Byte]) {
    //DeliteMesosExecutor.sendDebugMessage("message received!")

    val mssg = DeliteMasterMessage.parseFrom(data)
    mssg.getType match {
      case DeliteMasterMessage.Type.OP => setRequest{ DeliteMesosExecutor.message = mssg.getOp }
      case DeliteMasterMessage.Type.DATA => setRequest{ DeliteMesosExecutor.message = mssg.getData }
      case DeliteMasterMessage.Type.INFO => DeliteMesosExecutor.processSlaves(mssg.getInfo)
      case DeliteMasterMessage.Type.DEBUG => DeliteMesosExecutor.sendDebugMessage(mssg.getDebug.getMessage) //echo
    }

    def setRequest(setWork: => Unit) {
      DeliteMesosExecutor.remoteLock.lock()
      try {
        setWork
        DeliteMesosExecutor.noWork = false
        DeliteMesosExecutor.hasWork.signal()
      }
      finally {
        DeliteMesosExecutor.remoteLock.unlock()
      }
    }
  }

  /**
   * Invoked when the executor should terminate all of it's currently
   * running tasks. Note that after a Mesos has determined that an
   * executor has terminated any tasks that the executor did not send
   * terminal status updates for (e.g., TASK_KILLED, TASK_FINISHED,
   * TASK_FAILED, etc) a TASK_LOST status update will be created.
   */
  def shutdown(driver: ExecutorDriver) {
    println("WARNING: executor shutting down")
  }

  /**
   * Invoked when a fatal error has occured with the executor and/or
   * executor driver. The driver will be aborted BEFORE invoking this
   * callback.
   */
  def error(driver: ExecutorDriver, message: String) {
    println("ERROR: " + message)
  }

}

object DeliteMesosExecutor {

  private var driver: MesosExecutorDriver = _

  private val remoteLock = new ReentrantLock
  private val hasWork = remoteLock.newCondition
  private var noWork = true
  private var message: Any = _

  private lazy val network: ConnectionManager = new ConnectionManager
  private val networkMap = new HashMap[Int,ConnectionManagerId]
  var numSlaves = 0
  var slaveIdx = 0
  
  private var graph: DeliteTaskGraph = _

  // task queues for workers 
  private val numResources = Config.numThreads + Config.numCpp + Config.numCuda + Config.numOpenCL
  case class Task(name: String, start: Int = -1, size: Int = -1, inputCopy: Array[Boolean] = null)
  private val taskQueues = new Array[ArrayBlockingQueue[Task]](numResources)
  for (i <- 0 until numResources) {
    taskQueues(i) = new ArrayBlockingQueue[Task](Config.taskQueueSize)
  }
  def getTask(resourceID: Int): Task = taskQueues(resourceID).take()
  def putTask(resourceID: Int, task: Task) = taskQueues(resourceID).put(task)

  // version numbers for objects
  case class VersionID(var w: Int, r:Array[Int])
  private val versionID = new scala.collection.mutable.HashMap[String,VersionID]
  private def updateVersionIDs(id: String, targets: List[Targets.Value]) {
    val outSyms = graph.totalOps.find(_.id == id).get.getOutputs
    for(sym <- outSyms) {
      updateVersionID(sym, targets)
    }
  }
  private def updateVersionID(id: String, targets: List[Targets.Value]) {
    val v = versionID.getOrElse(id, VersionID(0,new Array[Int](numResources)))
    v.w = v.w + 1
    for(target <- targets) v.r(Targets.resourceIDs(target)(0)) = v.w
    versionID.put(id, v) 
  }
  private def needsCopy(sym: String, target: Targets.Value): Boolean = {
    var stale = true
    if(versionID.contains(sym)) {
      val v = versionID.get(sym).get
      if(v.r(Targets.resourceIDs(target)(0)) == v.w) stale = false
    }
    stale
  }
  private def syncVersionID(sym: String, target: Targets.Value) {
    if(versionID.contains(sym)) {
      //sendDebugMessage("syncing " + sym + " for target " + target)
      val v = versionID.get(sym).get
      v.r(Targets.resourceIDs(target)(0)) = v.w
    }
    else {
      updateVersionID(sym, List(target))
    }
  }

  private var opTarget: Targets.Value = _

  var executor: ThreadPool = _
  var classLoader = this.getClass.getClassLoader
  val results = new HashMap[String,ArrayList[DeliteArray[_]]]

  def getResult(id: String, offset: Int) = {
    if(opTarget==Targets.Scala && needsCopy(id,Targets.Scala)) {
      putTask(Targets.resourceIDs(Targets.Cuda)(0),Task("get_"+id))
      val syncObjectCls = classLoader.loadClass("Sync_Executable"+Targets.resourceIDs(Targets.Cuda)(0)) 
      val r = syncObjectCls.getDeclaredMethods.find(_.getName == "get0_x" + id).get.invoke(null,Array():_*)
      syncVersionID(id, Targets.Scala)
      val dummySerialization = Serialization.serialize(r, true, id) // Just to update the result table with LocalDeliteArrays
      sendDebugMessage("slave: " + slaveIdx + " copied data from CUDA device for getResult().")
    }
    val res = results.get(id)
    if (res == null || res.size <= offset)
      throw new RuntimeException("data for " + id + "_" + offset + " not found") 
    res.get(offset)
  }

  def processSlaves(info: CommInfo) {
    slaveIdx = info.getSlaveIdx
    numSlaves = info.getSlaveAddressCount
    if (network.id != ConnectionManagerId(info.getSlaveAddress(slaveIdx), info.getSlavePort(slaveIdx)))
      throw new RuntimeException("ERROR: slaves socket addresses don't agree")
    for (i <- 0 until numSlaves) {
      networkMap.put(i, ConnectionManagerId(info.getSlaveAddress(i), info.getSlavePort(i)))
    }
    DeliteMesosExecutor.sendDebugMessage("my peers are " + info.getSlaveAddressList.toArray.mkString(", "))
  }

  def main(args: Array[String]) {
    network.onReceiveMessage((msg: Message, id: ConnectionManagerId) => { //TODO: refactor me
      sendDebugMessage("received request")
      val bytes = msg.asInstanceOf[BufferMessage].buffers(0).array
      val mssg = DeliteSlaveMessage.parseFrom(bytes)
      val response = mssg.getType match {
        case DeliteSlaveMessage.Type.DATA => getData(mssg.getData)
      }
      sendDebugMessage("satisfying remote read")
      Some(Message.createBufferMessage(response.toByteString.asReadOnlyByteBuffer, msg.id))
    })

    driver = new MesosExecutorDriver(new DeliteMesosExecutor)
    graph = Delite.loadDeliteDEG(args(0))
    driver.run()
  }

  def sendDebugMessage(message: String) {
    if (driver != null) {
      val mssg = DeliteSlaveMessage.newBuilder
        .setType(DeliteSlaveMessage.Type.DEBUG)
        .setDebug(DebugMessage.newBuilder.setMessage(message))
        .build
      driver.sendFrameworkMessage(mssg.toByteArray)
    }
    else println(message)
  }

  def awaitWork() {
    //sendDebugMessage("awaiting work!")

    var message: Any = null
    remoteLock.lock()
    try {
      while(noWork) {
        hasWork.await()
      }
      message = this.message
      noWork = true
    }
    finally {
      remoteLock.unlock()
    }

    message match {
      case work: RemoteOp if(scheduleOn(work)==Targets.Cuda) => launchWorkCuda(work)
      case work: RemoteOp => launchWorkScala(work)
      case request: RequestData => driver.sendFrameworkMessage(getData(request).toByteArray)
    }
    awaitWork()
  }

  def launchWorkCuda(op: RemoteOp) {
    opTarget = Targets.Cuda
    val id = op.getId.getId

    sendDebugMessage("CUDA: Launching op " + id + "on CUDA")

    //TODO: Why below is not working for struct type inputs?
    // Set sync objects for kernel inputs 
    /*
    val o = graph.totalOps.find(_.id == op.getId.getId).get
    val inputSyms = o.getInputs.map(i => i._2)
    val inputs = o.getInputs.map(i => (i._2,Targets.getClassType(o.inputType(i._2)))).toArray
    var idx = 0
    val args = for ((sym,tpe) <- inputs) yield {
      val arg = Serialization.deserialize(tpe, op.getInput(idx), true).asInstanceOf[Object]
      val syncObjectCls = classLoader.loadClass("Sync_Executable0")
      val method = syncObjectCls.getDeclaredMethods.find(m => m.getName.contains("set") && m.getName.contains(sym)).get
      method.invoke(null,Array(arg):_*)
      idx += 1
      arg
    }
    */
    
    // Set sync objects for kernel inputs 
    val o = graph.totalOps.find(_.id == op.getId.getId).get
    val inputSyms = o.getInputs.map(i => i._2)
    val cls = classLoader.loadClass("generated.scala.kernel_" + id)
    val method = cls.getDeclaredMethods.find(_.getName == "apply").get
    val types = method.getParameterTypes
    var idx = 0
    val args = for (tpe <- types) yield {
      Serialization.updated = false
      val arg = Serialization.deserialize(tpe, op.getInput(idx), true).asInstanceOf[Object]
      if(Serialization.updated) {
        //sendDebugMessage("updated! " + inputSyms(idx))
        updateVersionID(inputSyms(idx), List(Targets.Scala))
      }
      val syncObjectCls = classLoader.loadClass("Sync_Executable0")
      val method = syncObjectCls.getDeclaredMethods.find(m => m.getName.contains("set") && m.getName.contains(inputSyms(idx))).get
      method.invoke(null,Array(arg):_*)
      idx += 1
      arg
    }
    
    //sendDebugMessage("CUDA: Input copy is done")

    val s = System.currentTimeMillis()
    // Put task on the task queue
    val returnResult = ReturnResult.newBuilder.setId(op.getId)
    val inputCopy = inputSyms.map(i => needsCopy(i, Targets.Cuda)).toArray
    sendDebugMessage("inputCopy: " + inputCopy.mkString(","))
    val start = op.getStartIdx(slaveIdx)
    val size = if (op.getStartIdxCount > slaveIdx+1) op.getStartIdx(slaveIdx+1)-start else -1
    putTask(Targets.resourceIDs(Targets.Cuda)(0), Task(op.getId.getId, start, size, inputCopy))

    sendDebugMessage("CUDA: Put Task")
    for(i <- inputSyms) syncVersionID(i, Targets.Cuda)

    // Wait for the result 
    val syncObjectCls = classLoader.loadClass("Sync_Executable"+Targets.resourceIDs(Targets.Cuda)(0)) 
    val resultClass = classLoader.loadClass("generated.scala.activation_"+id)
    val kernelClass = classLoader.loadClass("generated.scala.kernel_"+id)
    val applyMethod = kernelClass.getDeclaredMethods.find(_.getName == "apply").get
    val multiLoop = applyMethod.invoke(null,args:_*)
    val initActMethod = multiLoop.getClass.getDeclaredMethods.find(_.getName == "initAct").get
    val result = initActMethod.invoke(multiLoop,Array():_*)
    for(output <- o.getOutputs) {
      val m = resultClass.getDeclaredMethods.find(_.getName == output + "_$eq").get 
      val r = syncObjectCls.getDeclaredMethods.find(_.getName == "get0_x" + output).get.invoke(null,Array():_*)
      m.invoke(result,Array(r):_*)
    }
    val finalizeM = resultClass.getDeclaredMethods.find(_.getName == "unwrap")
    finalizeM match {
      case Some(m) => m.invoke(result,Array():_*) // Hash type activation record
      case _ => 
    }
    
    val e = System.currentTimeMillis()
    sendDebugMessage("CUDA execution (op " + id + "):" + (e-s))

    // Update the version number
    //TODO: allow non-blocking calls for struct type outputs
    val blockingCall = o.asInstanceOf[OP_MultiLoop].needsCombine || o.asInstanceOf[OP_MultiLoop].needsPostProcess || o.getOutputs.map(o.outputType(_)).exists(t => !t.startsWith("ppl.delite.runtime.data.DeliteArray"))
    if(blockingCall) 
      updateVersionIDs(id, List(Targets.Cuda,Targets.Scala))
    else 
      updateVersionIDs(id, List(Targets.Cuda))

    // Send output message to master
    val serResults = result.getClass.getMethod("serialize").invoke(result).asInstanceOf[java.util.List[ByteString]]
    returnResult.addAllOutput(serResults)

    val mssg = DeliteSlaveMessage.newBuilder
      .setType(DeliteSlaveMessage.Type.RESULT)
      .setResult(returnResult)
      .build

    driver.sendFrameworkMessage(mssg.toByteArray)
  }

  def launchWorkScala(op: RemoteOp) {
    opTarget = Targets.Scala
    val id = op.getId.getId
    //sendDebugMessage("launching op " + id)
    
    val cls = op.getType match {
      case RemoteOp.Type.INPUT => classLoader.loadClass("generated.scala.kernel_"+id)
      case RemoteOp.Type.MULTILOOP => 
        loopStart = op.getStartIdx(slaveIdx)
        loopSize = if (op.getStartIdxCount > slaveIdx+1) op.getStartIdx(slaveIdx+1)-loopStart else -1
        //sendDebugMessage("looping from " + loopStart + " to " + (loopStart+loopSize))
        classLoader.loadClass("MultiLoopHeader_"+id)
      case other => throw new RuntimeException("unrecognized op type: " + other)
    }

    val method = cls.getDeclaredMethods.find(_.getName == "apply").get
    val types = method.getParameterTypes //or use the DEG?
    var idx = 0
    val args: Array[Object] = for (tpe <- types) yield {
      val arg = Serialization.deserialize(tpe, op.getInput(idx), true)
      idx += 1
      arg.asInstanceOf[Object]
    }

    val s = System.currentTimeMillis()
    val serResults = try { 
      val result = op.getType match {
        case RemoteOp.Type.INPUT =>
          method.invoke(null, args:_*)
        case RemoteOp.Type.MULTILOOP =>
          val header = method.invoke(null, args:_*) //TODO: for map-like ops we can return this immediately rather than waiting for work to complete
          val result = new Future[Any]

          for (i <- 0 until Config.numThreads) {
            val loopCls = classLoader.loadClass("MultiLoop_"+id+"_Chunk_"+i)
            val loopM = loopCls.getDeclaredMethods.find(_.getName == "apply").get
            val exec = new DeliteExecutable {
              def run() { try {
                val res = loopM.invoke(null, header)
                if (i == 0) {
                  result.set(res)
                }
              } catch {
                case i: java.lang.reflect.InvocationTargetException => if (i.getCause != null) throw i.getCause else throw i
              } }
            }
            executor.submitOne(i, exec)
          }
          result.get
      } 
      result.getClass.getMethod("serialize").invoke(result).asInstanceOf[java.util.List[ByteString]]
    } catch {
      case i: java.lang.reflect.InvocationTargetException => if (i.getCause != null) throw i.getCause else throw i
    }
    val e = System.currentTimeMillis()
    sendDebugMessage("Scala execution (op " + id + "):" + (e-s))

    // Update the version number for outputs
    updateVersionIDs(id, List(Targets.Scala))

    val returnResult = ReturnResult.newBuilder.setId(op.getId).addAllOutput(serResults)

    val mssg = DeliteSlaveMessage.newBuilder
      .setType(DeliteSlaveMessage.Type.RESULT)
      .setResult(returnResult)
      .build

    driver.sendFrameworkMessage(mssg.toByteArray)
  }

  def getData(request: RequestData) = {    
    opTarget=Targets.Scala
    val id = request.getId.getId.split("_")
    assert(id.length == 2)
    val key = id(0)
    val offset = id(1).toInt
    sendDebugMessage("requesting data " + key)
    val res = getResult(key, offset)
    val data = if (request.hasIdx) res.readAt(request.getIdx) else res

    val ser = Serialization.serialize(data)
    val mssg = DeliteSlaveMessage.newBuilder
      .setType(DeliteSlaveMessage.Type.RESULT)
      .setResult(ReturnResult.newBuilder
        .setId(request.getId)
        .addOutput(ser)
      ).build
    mssg
  }

  def requestData(id: String, location: Int, idx: Int): ReturnResult = {
    sendDebugMessage("requesting remote read of " + id + " at index " + idx)
    val mssg = DeliteMasterMessage.newBuilder 
      .setType(DeliteMasterMessage.Type.DATA)
      .setData(RequestData.newBuilder.setId(Id.newBuilder.setId(id)).setIdx(idx))
      .build.toByteString.asReadOnlyByteBuffer

    val resBytes = network.sendMessageSync(networkMap.get(location), Message.createBufferMessage(mssg)).get.asInstanceOf[BufferMessage].buffers(0).array
    val result = DeliteSlaveMessage.parseFrom(resBytes)
    result.getType match {
      case DeliteSlaveMessage.Type.RESULT => sendDebugMessage("remote received"); result.getResult
    }
  }

  var loopStart = 0
  var loopSize = -1

  def getBlockSize(file: java.io.File): (Long,Long) = { //TODO: decouple and possibly specialize framework codegen
    if (driver != null) {
      val length = file.length
      sendDebugMessage("slaves: " + slaveIdx + " out of " + numSlaves)
      val start = length * slaveIdx / numSlaves
      val end = length * (slaveIdx+1) / numSlaves
      sendDebugMessage("file: from " + start + " to " + end + " out of " + length)
      (start,end)
    }
    else {
      (0L, Long.MaxValue)
    }
  }

  def scheduleOn(op: RemoteOp) = {
    op.getType match {
      case RemoteOp.Type.MULTILOOP =>
        graph.totalOps.find(_.id == op.getId.getId) match {
          case Some(o) if (Config.numCuda>0 && o.supportsTarget(Targets.Cuda)) => Targets.Cuda
          case _ => Targets.Scala
        }
      case _ =>
        Targets.Scala
    }
  }

}


