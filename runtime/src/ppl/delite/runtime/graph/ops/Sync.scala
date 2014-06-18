package ppl.delite.runtime.graph.ops

import collection.mutable
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.scheduler.{OpHelper, PartialSchedule}
import collection.mutable.{ArrayBuffer,HashSet,HashMap}
import ppl.delite.runtime.graph.targets.Targets

/*
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object Sync {

  //optimizations:
  //don't send+receive or await+notify on the same thread
  //send trumps notify (receive trumps await)
  //don't send (notify) multiple times to the same node
  //don't receive (await) multiple times on the same thread

  //TODO: consider case: syncing with x also implicitly syncs with y if x syncd with y

  private var syncSet = new mutable.HashMap[Sync, Sync] //basically a Set, but can retrieve the original instance
  private var freeList = new mutable.ArrayBuffer[Free]()
  private var loopUpdates = new mutable.HashSet[Sync]


  def send(sym: String, from: DeliteOP, to: DeliteOP) = {
    val sender = SendData(sym, from, node(to.scheduledResource))
    sender match {
      case _ if (from.scheduledResource == to.scheduledResource) => null //same thread
      case _ if (shouldView(sym, from, to)) => sendView(sym, from, to)
      case _ if (syncSet contains sender) => syncSet(sender).asInstanceOf[SendData] //redundant send
      case _ => addSend(sender, from)
    }
  }

  def sendView(sym: String, from: DeliteOP, to: DeliteOP) = {
    val sender = SendView(sym, from)
    sender match {
      case _ if (syncSet contains sender) => syncSet(sender).asInstanceOf[SendView]
      case _ => addSend(sender, from)
    }
  }

  def receive(sender: Send, to: DeliteOP) {
    sender match {
      case null =>
      case s:SendView => receiveView(s, to)
      case s:SendData =>
        val receiver = ReceiveData(s, to.scheduledResource)
        if (s ne null) assert(node(to.scheduledResource) == s.toNode, "invalid send/receive pair") //factor out toNode?
        receiver match {
          case _ if (syncSet contains receiver) => //redundant receive
          case _ => addReceive(receiver, to)
        }
    }
  }

  def receiveView(sender: SendView, to: DeliteOP) {
    val receiver = ReceiveView(sender, to.scheduledResource)
    receiver match {
      case _ if (syncSet contains receiver) =>
      case _ => addReceive(receiver, to)
    }
  }

  //TODO: if send trumps notify, need to add a receive in place of await!
  //TODO: in sharedMemory can just send the pointer anyway, can be used later (Notify => SendView)
  def notify(from: DeliteOP, to: DeliteOP) = {
    val notifier = Notify(from, node(to.scheduledResource))
    val potentialSenders: Set[Send] = from.getOutputs.map(sym => SendData(sym, from, node(to.scheduledResource))) ++ from.getOutputs.map(sym => SendView(sym, from)) ++ from.getMutableInputs.map(mi => SendUpdate(mi._2, from, node(to.scheduledResource)))
    // want to replace notify with existing senders only when the corresponding receiver already exists on the target schedule	
    // (e.g., shed-1 sendData to sched-2, and sched-1 noitfy to sched-3: better not to replace await with recvData unless sched-3 already has it)
    val otherSenders = potentialSenders.filter(s => syncSet.contains(s) && s.receivers.map(_.scheduledResource).contains(to.scheduledResource))
    notifier match {
      case _ if (from.scheduledResource == to.scheduledResource) => null
      case _ if (syncSet contains notifier) => syncSet(notifier).asInstanceOf[Notify]
      case _ if (otherSenders nonEmpty) => syncSet(otherSenders.head).asInstanceOf[Send] //does it matter which one?
      case _ => addSend(notifier, from)
    }
  }

  def await(sender: Send, to: DeliteOP) = {
    sender match {
      case null =>
      case s: SendData => receive(s, to)
      case s: SendView => receiveView(s, to)
      case s: SendUpdate => awaitUpdate(s, to)
      case n: Notify =>
        val awaiter = Await(n, to.scheduledResource)
        if (sender ne null) assert(node(to.scheduledResource) == n.toNode, "invalid notify/await pair")
        awaiter match {
          case _ if (syncSet contains awaiter) =>
            // since ops in a schedule are traversed in order while adding syncs,
            // it is guaranteed that 'awaiter' is a sync before current op
            // so we don't need to add a new notify/await
          case _ => addReceive(awaiter, to)
        }
    }
  }

  //TODO: updates don't work across loop iterations (write seen as happening after read rather than before in DEG)
  //TODO: update trumps notify
  //TODO: updatee is in an outer scope - delay update
  def update(sym: String, from: DeliteOP, to: DeliteOP, isAntiDep: Boolean = false) = {
    val updater = SendUpdate(sym, from, node(to.scheduledResource))
    val source = from.getMutableInputs.find(_._2 == sym).get._1
    updater match {
      case _ if (from.scheduledResource == to.scheduledResource) => null //same thread
      case _ if (shouldView(sym, source, from) && shouldView(sym, source, to)) => null //update handled by hardware //TODO: if the updater has the *same* view as the receiver
      case _ if (syncSet contains updater) => syncSet(updater).asInstanceOf[SendUpdate] //multiple readers after mutation
      case _ => addSend(updater, from, isAntiDep)
    }
  }

  def awaitUpdate(updater: SendUpdate, to: DeliteOP, isAntiDep: Boolean = false) {
    val updatee = ReceiveUpdate(updater, to.scheduledResource)
    if (updater ne null) assert(node(updatee.atResource) == updater.toNode, "invalid update pair")
    updater match {
      case null => 
      case _ if (syncSet contains updatee) =>
      case _ => addReceive(updatee, to, isAntiDep)
      /*
      case _ =>
        // For previous notify/await pairs from the same op to the same node && same destination op, promote to sendUpdate/recvUpdate
        // NOTE: Consider a mutable op and two dependent ops (on the same schedule, one await and another on recvUpdate)
        //       If the order from the sender side is notify and then sendUpdate, and recvUpdate comes earlier than await?
        val potentialNotifyAwait = Await(Notify(updater.from,node(to.scheduledResource)),to.scheduledResource)
        syncSet.get(potentialNotifyAwait) match {
          case Some(na@Await(Notify(_,_),_)) if na.to == to => removeSendReceive(na, to)
          case _ => //
        }
        addReceive(updatee, to, isAntiDep)
      */
    }
  }

  private def addSend[T <: Send](sender: T, from: DeliteOP, isAntiDep: Boolean = false) = {
    if (isAntiDep)
      loopUpdates += sender
    else 
      syncSet += Pair(sender,sender)
    sender
  }

  private def addReceive[T <: Receive](receiver: T, to: DeliteOP, isAntiDep: Boolean = false) {
    if(isAntiDep)
      loopUpdates += receiver
    else 
      syncSet += Pair(receiver,receiver)
    receiver.setReceiveOp(to)
    receiver.sender.receivers += receiver
  }

  private def addSyncToSchedule() {
    val (updateSyncs,nonUpdateSyncs) = syncSet.values.partition(s => s.isInstanceOf[SendUpdate] || s.isInstanceOf[ReceiveUpdate])

    for (sync <- nonUpdateSyncs) {
      sync match {
        case s: Send if !s.isInstanceOf[SendUpdate] =>
          schedule.insertAfter(s, s.from)
        case r: Receive if !r.isInstanceOf[ReceiveUpdate] =>
          schedule.insertBefore(r, r.to)
      }
    }
    // update nodes needs to be added afterwards 
    // e.g., when sched-1(C++) creates a symbol, sched-2(Java) updates and sched-3(C++) uses it,
    // sched-3 should get the symbol first from shed-1 and then receive update.
    for (sync <- updateSyncs) {
      sync match {
        case s: SendUpdate =>
          schedule.insertAfter(s, s.from)
        case r: ReceiveUpdate =>
          schedule.insertBefore(r, r.to)
      }
    }
    // updates for loop carried dependencies are appended at the end of the schedule
    // SendUpdate should be all added before receiveUpdate syncs are added to prevent deadlock
    // (e.g., sched-1 and sched-2 both has loop updates to send as in )
    loopUpdates collect { case s: SendUpdate => schedule(s.from.scheduledResource).add(s) }
    loopUpdates collect { case r: ReceiveUpdate => schedule(r.to.scheduledResource).add(r) }
  }

  private def addFreeToSchedule() {
    for (f <- freeList) schedule.insertAfter(f, f.op)
  }

  def shouldView(sym: String, from: DeliteOP, to: DeliteOP) = typeIsViewable(from.outputType(sym)) && (sharedMemory(from.scheduledResource, to.scheduledResource) || canAcquire(from.scheduledResource, to.scheduledResource))

  def typeIsViewable(outputType: String) = { //make a distinction between copyByValue (primitive) and copyByReference (reference) types
    !Targets.isPrimitiveType(outputType)
  }

  def canAcquire(from: Int, to: Int) = (OpHelper.scheduledTarget(from), OpHelper.scheduledTarget(to)) match {
    //case (Targets.Scala, Targets.Cpp) => true
    case _ => false
  }

  def sharedMemory(from: Int, to: Int) = node(from) == node(to)

  def node(resource: Int) = OpHelper.scheduledTarget(resource) match {
    case Targets.Scala => 0 //only 1 Scala node
    case Targets.Cpp => 1 //only 1 C++ node
    case Targets.Cuda => resource //every GPU is it's own node
    case Targets.OpenCL => resource
  }

  def dataDeps(op: DeliteOP) = op.getInputs.toSet -- _graph.inputs
  def allDeps(op: DeliteOP) = op.getDependencies -- _graph.inputOps
  def otherDeps(op: DeliteOP) = allDeps(op) -- dataDeps(op).map(_._1)
  def mutableDeps(op: DeliteOP) = op.getMutableInputs -- _graph.inputs
  def mutableDepConsumers(op: DeliteOP, sym: String) = op.consumers.filter(c => c.getInputs.exists(_._2 == sym))

  private def schedule = _graph.schedule
  private var _graph: DeliteTaskGraph = _
  private val visitedGraphs = new mutable.HashSet[DeliteTaskGraph]

  private var scopeIsLoop: Boolean = false
   
  // Get all the mutators of sym
  def allMutators(sym: String) = {
    val mset = new mutable.HashSet[DeliteOP]
    for (resource <- schedule; o <- resource) {
      o.getMutableInputs.find(_._2 == sym) match {
        case Some(_) => mset += o
        case _ => //
      }
    }
    mset
  }

  // Get the set of last mutators of sym
  def lastMutators(sym: String) = {
    allMutators(sym).filter(m => allMutators(sym).filter(_.getDependencies.contains(m)).isEmpty) 
  }

  // Get the set of last mutators of sym ocurring before op
  def lastMutators(sym: String, op: DeliteOP) = {
    val mutators = allMutators(sym).filter(op.getDependencies.contains(_))
    mutators.filter(m => mutators.filter(_.getDependencies.contains(m)).isEmpty) 
  }

  def addSync(graph: DeliteTaskGraph) {
    _graph = graph
    visitedGraphs += graph //don't want to add sync to a graph more than once

    for (resource <- schedule; op <- resource) {
      //println("op: " +op.id)
      //add in this order to avoid the need for deletions

      // data deps
      for ((dep,sym) <- dataDeps(op)) {
        receive(send(sym, dep, op), op)
      }

      // mutable input deps
      for ((dep,sym) <- op.getInputs) {
        //println("dep,sym:" + dep.id + "," + sym)
        for(m <- lastMutators(sym,op)) {
          awaitUpdate(update(sym, m, op), op)
        }
      }

      // control deps
      for (dep <- otherDeps(op)) { //could also be all deps
        await(notify(dep, op), op)
      }

      // anti deps
      if (scopeIsLoop) {
        //println("anti-deps:" + op.getAntiDeps.mkString(","))
        for (dep <- op.getAntiDeps) {
          //println("anti-dep:" + dep)
          val antiDepInputs = op.getMutableInputs.filter(i => i._1.isInstanceOf[OP_Input] && dep.getInputs.contains(i))
          for(o <- antiDepInputs if(lastMutators(o._2).contains(op))) {
            //println("op: " + op.id + " adding anti deps:" + dep.id + ":" + syms.mkString(",")) 
            awaitUpdate(update(o._2, op, dep, true), dep, true)
          }
        }
      }

      // Add Free nodes to the schedule if needed
      writeDataFrees(op)

      if (op.isInstanceOf[OP_Nested]) {
        val saveSync = syncSet
        val saveFree = freeList
        val saveUpdate = loopUpdates
        val saveScopeIsLoop = scopeIsLoop
        if(op.isInstanceOf[OP_While]) scopeIsLoop = true else scopeIsLoop = false
        for (graph <- op.asInstanceOf[OP_Nested].nestedGraphs if !(visitedGraphs contains graph)) {
          syncSet = new mutable.HashMap[Sync,Sync]
          freeList = new mutable.ArrayBuffer[Free]
          loopUpdates = new mutable.HashSet[Sync]
          addSync(graph)
        }
        syncSet = saveSync
        freeList = saveFree
        loopUpdates = saveUpdate
        scopeIsLoop = saveScopeIsLoop
        _graph = graph
      }
    }

    // Should be in this order: frees for an op should be emitted after all syncs for the op are emitted (sync may need to use it)
    addFreeToSchedule()
    addSyncToSchedule()
    syncSet = new mutable.HashMap[Sync,Sync]
    freeList = new mutable.ArrayBuffer[Free]
  }

  protected def writeDataFrees(op: DeliteOP) {
    val items = ArrayBuffer[(DeliteOP,String)]()

    def opFreeable(op: DeliteOP, sym: String) = {
      //TODO: Better to make OP_Condition extending OP_Executable?
      (op.isInstanceOf[OP_Executable] || op.isInstanceOf[OP_Condition]) /*&& available.contains(op,sym)*/ && op.outputType(sym)!="Unit"
    }

    //free outputs
    //TODO: Handle alias for Condition op output
    for (name <- op.getOutputs if(opFreeable(op,name))) {
      if (op.getConsumers.filter(c => c.getInputs.contains((op,name)) && c.scheduledResource == op.scheduledResource).isEmpty && !Targets.isPrimitiveType(op.outputType(name))) {
        items += Pair(op,name)
      }
    }

    //free inputs
    for ((in,name) <- op.getInputs if(opFreeable(in,name))) {
      var free = true
      if (Targets.isPrimitiveType(in.outputType(name)) && (in.scheduledResource!=op.scheduledResource)) free = false
      //for ((i,n) <- aliases.get(in,name); c <- i.getConsumers.filter(c => c.getInputs.contains(i,n) && c.scheduledResource == op.scheduledResource)) {
      for (c <- in.getConsumers.filter(c => c.getInputs.contains(in,name) && c.scheduledResource == op.scheduledResource)) {
        if (schedule(c.scheduledResource).indexOf(op) < schedule(c.scheduledResource).indexOf(c)) free = false
      }
      if (free) {
        items += Pair(in,name)
      }
    }
    if (items.size > 0) freeList += Free(op,items.toList)
  }

}

abstract class Sync extends DeliteOP {
  val id = "Sync"
  def isDataParallel = false
  def task = null
  private[graph] var outputTypesMap: Map[Targets.Value, Map[String,String]] = null
}

abstract class PCM_M extends DeliteOP {
  val id = "MemoryManagement"
  def isDataParallel = false
  def task = null
  private[graph] var outputTypesMap: Map[Targets.Value, Map[String,String]] = null
}

abstract class Send extends Sync {
  val from: DeliteOP
  scheduledResource = from.scheduledResource

  val receivers = new mutable.HashSet[Receive]
}

abstract class Receive extends Sync {
  var to: DeliteOP = _
  def setReceiveOp(op: DeliteOP) {
    to = op
    scheduledResource = to.scheduledResource
  }

  val sender: Send
}

//default for data transfers: copy semantics
case class SendData(sym: String, from: DeliteOP, toNode: Int) extends Send
case class ReceiveData(sender: SendData, atResource: Int) extends Receive

//optimization of Send/Receive data, only copy pointer, share data
case class SendView(sym: String, from: DeliteOP) extends Send
case class ReceiveView(sender: SendView, atResource: Int) extends Receive

//control signals
case class Notify(from: DeliteOP, toNode: Int) extends Send
case class Await(sender: Notify, atResource: Int) extends Receive

//update existing data, separated from Send/Receive data for analysis purposes
case class SendUpdate(sym: String, from: DeliteOP, toNode: Int) extends Send
case class ReceiveUpdate(sender: SendUpdate, atResource: Int) extends Receive

//Free nodes (TODO: Better to have a Free node for each symbol?)
case class Free(op: DeliteOP, items: List[(DeliteOP,String)]) extends PCM_M
