package ppl.delite.runtime

import org.apache.mesos._
import org.apache.mesos.Protos._
import com.google.protobuf.ByteString
import java.util.concurrent.locks.ReentrantLock
import java.util.HashMap
import ppl.delite.runtime.data.DeliteArray
import ppl.delite.runtime.executor.ThreadPool
import ppl.delite.runtime.codegen.DeliteExecutable
import ppl.delite.runtime.messages.Messages._
import ppl.delite.runtime.messages._


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

          DeliteMesosExecutor.networkMap.put(-1, new ConnectionManagerId(info.getMasterAddress, info.getMasterPort))
          
          DeliteMesosExecutor.sendDebugMessage("my master is " + info.getMasterAddress + ":" + info.getMasterPort)
          DeliteMesosExecutor.sendDebugMessage("my connection is " + DeliteMesosExecutor.network.id.host + ":" + DeliteMesosExecutor.network.id.port)
          DeliteMesosExecutor.sendDebugMessage("my input args are " + args.mkString(", "))

          Delite.embeddedMain(args, Map())

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
    DeliteMesosExecutor.sendDebugMessage("message received!")

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

  private val network = new ConnectionManager
  private val networkMap = new HashMap[Int,ConnectionManagerId]
  var numSlaves = 0
  var slaveIdx = 0
  
  var executor: ThreadPool = _
  var classLoader = this.getClass.getClassLoader
  val results = new HashMap[String,Any]

  private var id = 0
  private def nextId = { id += 1; id-1 }
  private var size = -1

  def getResult(id: String) = {
    val data = results.get(id).asInstanceOf[DeliteArray[_]]
    if (size == -1) {
      size = data.length
    }
    else {
      if (size != data.length) throw new RuntimeException("inconsistent data sizes, don't know how to partition")
    }
    data
  }

  def processSlaves(info: CommInfo) {
    numSlaves = info.getSlaveAddressCount
    slaveIdx = info.getSlaveIdx
    assert(network.id == new ConnectionManagerId(info.getSlaveAddress(slaveIdx), info.getSlavePort(slaveIdx)))
    for (i <- 0 until numSlaves) {
      networkMap.put(i, new ConnectionManagerId(info.getSlaveAddress(i), info.getSlavePort(i)))
    }
    DeliteMesosExecutor.sendDebugMessage("my peers are " + info.getSlaveAddressList.toArray.mkString(", "))
  
    val message = Message.createBufferMessage(java.nio.ByteBuffer.allocate(10).put(Array.tabulate[Byte](10)(x => x.toByte)))
    network.sendMessageUnsafe(networkMap.get(-1), message)
  }

  def main(args: Array[String]) {
    network.onReceiveMessage((msg: Message, id: ConnectionManagerId) => { 
      sendDebugMessage("Received [" + msg + "] from [" + id + "]")
      None
    })

    driver = new MesosExecutorDriver(new DeliteMesosExecutor)
    driver.run()
  }

  def sendDebugMessage(message: String) {
    val mssg = DeliteSlaveMessage.newBuilder
      .setType(DeliteSlaveMessage.Type.DEBUG)
      .setDebug(DebugMessage.newBuilder.setMessage(message))
      .build
    driver.sendFrameworkMessage(mssg.toByteArray)
  }

  def awaitWork() {
    sendDebugMessage("awaiting work!")

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
      case work: RemoteOp => launchWork(work)
      case request: RequestData => getData(request)
    }
    awaitWork()
  }

  def launchWork(op: RemoteOp) {
    val id = op.getId.getId
    size = -1 //TODO: need a more robust partitioning solution

    val cls = op.getType match {
      case RemoteOp.Type.INPUT => classLoader.loadClass("generated.scala.kernel_"+id)
      case RemoteOp.Type.MULTILOOP => classLoader.loadClass("MultiLoopHeader_"+id)
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

    val result = try { op.getType match {
      case RemoteOp.Type.INPUT =>
        method.invoke(null, args:_*)
      case RemoteOp.Type.MULTILOOP =>
        val header = method.invoke(null, args:_*)
        val workLock = new ReentrantLock
        val done = workLock.newCondition
        var notDone = Config.numThreads
        var result: Any = null
        
        for (i <- 0 until Config.numThreads) {
          val loopCls = classLoader.loadClass("MultiLoop_"+id+"_Chunk_"+i)
          val loopM = loopCls.getDeclaredMethods.find(_.getName == "apply").get
          val exec = new DeliteExecutable {
            def run() { try {
              val res = loopM.invoke(null, header)
              workLock.lock() //maps should just sync internally, this is redundant for reduce, filter, etc.
              try {
                notDone -= 1
                if (i == 0)
                  result = res
                if (notDone == 0)
                  done.signal
              }
              finally {
                workLock.unlock()
              }
            } catch {
              case i: java.lang.reflect.InvocationTargetException => if (i.getCause != null) throw i.getCause else throw i
            } }
          }
          executor.submitOne(i, exec)
        }

        workLock.lock()
        try {
          while (notDone != 0) {
            done.await
         }
        }
        finally {
          workLock.unlock()
        }
        result
    } } catch {
      case i: java.lang.reflect.InvocationTargetException => if (i.getCause != null) throw i.getCause else throw i
    }

    val returnResult = ReturnResult.newBuilder.setId(op.getId)
    //activation record fields; could use the DEG instead
    val methods = result.getClass.getDeclaredMethods.filter(m => m.getName.startsWith("x") && !m.getName.contains("_")).toSeq.sortBy(_.getName)
    for (method <- methods) {
      val res = method.invoke(result)
      returnResult.addOutput(Serialization.serialize(res, true))
    }

    val mssg = DeliteSlaveMessage.newBuilder
      .setType(DeliteSlaveMessage.Type.RESULT)
      .setResult(returnResult)
      .build

    driver.sendFrameworkMessage(mssg.toByteArray)
  }

  def getData(request: RequestData) {    
    val ser = Serialization.serialize(results.get(request.getId.getId))
    val mssg = DeliteSlaveMessage.newBuilder
      .setType(DeliteSlaveMessage.Type.RESULT)
      .setResult(ReturnResult.newBuilder
        .setId(request.getId)
        .addOutput(ser)
      ).build

    driver.sendFrameworkMessage(mssg.toByteArray)
  }

  def getLoopSize: Int = {         
    if (size == -1) throw new RuntimeException(id + ": loop over unknown size... don't know how to partition")
    size
  }

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

}
