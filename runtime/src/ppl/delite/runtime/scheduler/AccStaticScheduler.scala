package ppl.delite.runtime.scheduler

import ppl.delite.runtime.Config
import ppl.delite.runtime.DeliteMesosScheduler
import ppl.delite.runtime.graph._
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.cost._
import ppl.delite.runtime.codegen.kernels.cuda.SingleTask_GPU_Generator
import ppl.delite.runtime.codegen.{Compilers,CCompile}

class AccStaticScheduler(numScala: Int, numCpp: Int, numCuda: Int, numOpenCL: Int, numMaxJ: Int) extends StaticScheduler with ParallelUtilizationCostModel with ScheduleOptimizer {
  private val totalScala = Config.numThreads
  private val totalCpp = Config.numCpp
  private val gpu = totalScala + totalCpp
  private val numResources = totalScala + totalCpp + Config.numCuda + Config.numOpenCL + Config.numMaxJ

  def schedule(graph: DeliteTaskGraph) {
    //traverse nesting & schedule sub-graphs, starting with outermost graph
    scheduleFlat(graph)
  }

  protected def scheduleSequential(graph: DeliteTaskGraph, resource: Int) = scheduleFlat(graph, resource)

  protected def scheduleFlat(graph: DeliteTaskGraph) = scheduleFlat(graph, -1)

  protected def scheduleFlat(graph: DeliteTaskGraph, resource: Int) {
    val opQueue = new OpList
    val schedule = PartialSchedule(numResources)
    enqueueRoots(graph, opQueue)
    while (!opQueue.isEmpty) {
      val op = opQueue.remove
      if (resource >= 0)
        addSequential(op, graph, schedule, resource)
      else
        scheduleOne(op, graph, schedule)
      enqueueRoots(graph, opQueue)
    }
    ensureScheduled(graph)
    graph.schedule = schedule
  }

  //TODO: the redundancy between addSequential() and scheduleOne() with a singleton resource list is confusing
  protected def scheduleOne(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule) {
    def selectTarget(op: DeliteOP, sequential: Boolean) = scheduleOnTarget(op) match {
      case Targets.Scala =>
        val resources = if (sequential) Seq(0) else Range(0, numScala)
        scheduleMultiCore(op, graph, schedule, resources)
      case Targets.Cpp =>
        val resources = if (sequential) Seq(totalScala) else Range(totalScala, totalScala+numCpp)
        scheduleMultiCore(op, graph, schedule, resources)
      case Targets.Cuda => scheduleGPU(op, graph, schedule)
      case Targets.OpenCL => scheduleGPU(op, graph, schedule)
    }

    def scheduleMultiLoop(l: OP_MultiLoop) {
      if (shouldParallelize(l, Map[String,Int]())) selectTarget(op, false)
      else selectTarget(op, true)
    }

    def checkPartition(partition: Partition) = {
      if (partition == Local) {
        val distributedInputs = op.getInputs map { i => i._1.outputPartition } collect { case d@Distributed(x) => d }
        if (!distributedInputs.isEmpty) {
          DeliteMesosScheduler.warn("op " + op.id + " is partitioned locally but consumes distributed partitions " + distributedInputs.map(_.id.mkString("[",",","]")).mkString(","))
        }
      }
    }

    // stencil is also checked later (in DeliteMesosScheduler), when dispatching the job. But let's do a sanity check here.
    // inputDistributedPartitions foreach { p =>
    //   val ids = p.id
    //   ids foreach { i =>
    //     stencilOrElse(i)(Empty) match {
    //       case All => DeliteMesosScheduler.warn("stencil of op " + id + " for distributed input " + i + " is ALL")
    //       case _ => // really we should check for matching stencils with ops in 'ids'
    //     }
    //   }
    // }

    op match {
      case c: OP_Nested => addNested(c, graph, schedule, Range(0, numResources))

      case l: OP_MultiLoop if Config.clusterMode == 1 =>
        val partition = {
          if (l.getOutputs.exists(o => l.outputType(o) == "Unit") && !l.getInputs.exists(t => t._1.outputType(t._2) == "generated.scala.io.DeliteFileOutputStream")) {
            // The issue with foreaches right now is that effects are not visible outside of the node they run on.
            // i.e., if a slave performs a bunch of writes locally, there is no coherence protocol for the master
            // to see those updates.
            DeliteMesosScheduler.warn("op " + op.id + " is a multiloop with Unit return type: forcing local (distributed not supported)")
            Local
          }
          else {
            op.partition
          }
        }

        checkPartition(partition)
        println("scheduling loop op " + op.id + " as " + partition)
        if (partition.isInstanceOf[Distributed]) {
          OpHelper.remote(op, graph)
          cluster(op, schedule, Range(0, numScala))
        }
        else {
          scheduleMultiLoop(l)
        }

      case l: OP_MultiLoop => scheduleMultiLoop(l)

      case _ if Config.clusterMode == 1 =>
        checkPartition(op.partition)
        selectTarget(op, false)

      case _ => selectTarget(op, false)
    }
  }

  /**
   * Schedule a DeliteOp on a multi-core CPU, on the given list of resource IDs (thread IDs).
   * If the DeliteOP is data parallel, the op is split across all the given IDs in the
   * resource list. If not, it is scheduled as a single unit on the first ID in the
   * resource list.
   * @param op: DeliteOp being scheduled on CPU
   * @param graph: Application DEG
   * @param schedule: [[PartialSchedule]] object into which scheduling information is being added
   * @param resourceList: List of resource IDs (integers) on which the DeliteOp is to be scheduled
   */
  protected def scheduleMultiCore(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule, resourceList: Seq[Int]) {
    if (op.isDataParallel)
      split(op, graph, schedule, resourceList)
    else {
      if (Config.enableTaskParallelism) cluster(op, schedule, resourceList)
      else scheduleOn(op, schedule, resourceList(0))
    }
  }

  protected def scheduleGPU(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule) {
    if (op.isDataParallel)
      split(op, graph, schedule, Seq(gpu))
    else
      scheduleOn(op, schedule, gpu)
  }

  private var nextThread = 0

  protected def cluster(op: DeliteOP, schedule: PartialSchedule, resourceList: Seq[Int]) {
    //look for best place to put this op (simple nearest-neighbor clustering)
    var i = 0
    var notDone = true
    val deps = op.getDependencies
    while (i < resourceList.length && notDone) {
      val last = schedule(resourceList(i)).peekLast
      if (last != null && deps.contains(last)) {
        scheduleOn(op, schedule, resourceList(i))
        notDone = false
        if (nextThread == i) nextThread = (nextThread + 1) % resourceList.length
      }
      i += 1
    }
    //else submit op to next thread in the rotation (round-robin)
    if (notDone) {
      nextThread = (nextThread + 1) % resourceList.length
      scheduleOn(op, schedule, resourceList(nextThread))
    }
  }

  override protected def split(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule, resourceList: Seq[Int]) {
    OpHelper.scheduledTarget(resourceList(0)) match { //TODO: fix - target the same for all resources?
      case Targets.Cuda => splitGPU(op, graph, schedule)
      case Targets.OpenCL => splitGPU(op, graph, schedule)
      case _ => super.split(op, graph, schedule, resourceList)
    }
  }

  private def splitGPU(op: DeliteOP, graph: DeliteTaskGraph, schedule: PartialSchedule) {
    op.scheduledResource = gpu //TODO: fix this - part of scheduleOn
    val chunk = OpHelper.split(op, 1, graph, OpHelper.scheduledTarget(op))(0)
    scheduleOn(chunk, schedule, gpu)
  }

  /**
   * Returns the most suitable target for a given DeliteOP. Currently, there
   * is a fixed, strict order of target preference for every DeliteOP (FPGA > GPU > Cpp > Scala)
   * if (DeliteOP can run on GPU) <prefer Cuda over OpenCL>
   * This order is defined in the implementation implicitly.
   * No cost model exists to choose target for DeliteOPs.
   * @param op: [[DeliteOp]] node being scheduled
   */
  protected def scheduleOnTarget(op: DeliteOP) = {
    if (scheduleOnFPGA(op)) {
      if (op.supportsTarget(Targets.MaxJ)) Targets.MaxJ
      else sys.error(s"""$op cannot be scheduled on FPGA as it is not supported by MaxJ!""")
    }
    else if (scheduleOnGPU(op)) {
      if (op.supportsTarget(Targets.Cuda)) Targets.Cuda else Targets.OpenCL
    }
    else if (op.supportsTarget(Targets.Cpp) && numCpp > 0) Targets.Cpp
    else if (op.supportsTarget(Targets.Scala) && numScala > 0) Targets.Scala
    else sys.error(op + " cannot be run on any available hardware target")
  }

  //TODO: Separate hardware and programming model
  protected def scheduleOnGPU(op:DeliteOP) = {
    if (numCuda + numOpenCL == 0) false
    else if (Config.gpuWhiteList.size > 0) { // If white-list exists, then only white-list ops are scheduled on GPU
      if (Config.gpuWhiteList.contains(op.id)) true
      else false
    }
    else { // Otherwise, all the ops except black-list ops are scheduled on GPU
      if (Config.gpuBlackList.contains(op.id)) false
      else if (!op.supportsTarget(Targets.Cuda) && !op.supportsTarget(Targets.OpenCL)) false
      else true
    }
  }

  protected def scheduleOnFPGA(op:DeliteOP) = {
    if (numMaxJ == 0) false
    else if (!op.supportsTarget(Targets.MaxJ)) false
    else true
  }

}
