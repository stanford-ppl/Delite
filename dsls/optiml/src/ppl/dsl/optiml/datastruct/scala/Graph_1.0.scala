package ppl.delite.dsl.optiml

import collection.Set
import ppl.delite.core.ops._
import ppl.delite.core.{DeliteProxyFactory, DeliteDSLType, Delite, DeliteUnit, DeliteFunc, DeliteCollection}
import collection.mutable.{Queue, ArrayBuffer, Map, HashSet}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock, ReentrantLock}

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 11/29/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Graph {
  abstract class ProxyFactory[V, E] extends DeliteProxyFactory[Graph[V, E]] {
    def newProxy(): Graph[V, E]
  }

  object Consistency extends Enumeration {
    type Consistency = Value
    val Auto, Vertex, Edge, Full = Value
  }

  case class OP_untilConvergedSingle[V, E](g: Graph[V, E], f: (Graph[V, E]#Vertex) => Unit, c: Consistency.Consistency, sched: Scheduler[V] = new FifoScheduler[V]) extends DeliteOP_SingleTask[Graph[V, E]]() {
    val locks = new ConcurrentHashMap[V, ReentrantLock]

    def task = {
      sched.addTasks(g.vertexList)

      while (sched.hasTask) {
        val vertexData = sched.getTask()

        val vertex = g.generateVertex(vertexData)
        val sortedVertices = g.fullVertices(vertex)
        f(vertex)

        if (vertex.tasks.size > 0) {
          sched.addTasks(vertex.tasks)
        }
      }

      g
    }
  }


}

trait Graph[V, E] extends DeliteDSLType {
  import Graph._

  //type DSLType = Graph[V,E]

  def vertexList(): List[V]

  def vertexSet(): Set[V]

  def edgeSet(): Set[E]

  def addVertex(v: V)

  def addEdge(e: E, a: V, b: V)

  def removeEdge(a: V, b: V)

  def adjacent(a: V, b: V): Boolean

  def neighborsOf(a: V): Seq[V]

  def edgesOf(a: V): Seq[E]

  def containsEdge(e: E): Boolean

  def containsVertex(v: V): Boolean

  protected var _sorted = false
  def sorted: Boolean = _sorted
  def sort(): Unit

  class Vertex(v: V) {
    val data = v
    var edgeAccess = false
    var neighborAccess = false
    val tasks = new ArrayBuffer[V]

    val es = edgesOf(v)
    val nbrs = neighborsOf(v)

    def edges = {
      edgeAccess = true
      es
    }

    def neighbors = {
      neighborAccess = true
      nbrs
    }

    def addTask(v: V) {
      tasks += v
    }
  }

  def generateVertex(v: V): Vertex

  def untilConvergedSingle(c: Consistency.Consistency, sched: Scheduler[V] = new FifoScheduler[V])(f: (Graph[V, E]#Vertex) => Unit)(implicit pFact: Graph.ProxyFactory[V, E]): Unit = {
    if(!sorted) {
      sort()
    }

    Delite.run(OP_untilConvergedSingle[V, E](this, f, c, sched)).force
  }

  def untilConvergedTask(c: Consistency.Consistency)(f: (Vertex) => Unit)(implicit mV: ClassManifest[V]): Unit = {
    if(!sorted) {
      sort()
    }

    implicit val proxyFactory = new Vector.ProxyFactory[V]
    val locks = new ConcurrentHashMap[V, ReentrantLock]
    val queue = new Queue[V]
    queue ++= vertexList

    case class OP_untilConvergedTask(v: Vertex) extends DeliteOP_SingleTask[Vector[V]]() {
      def task = {
        val vertices = fullVertices(v)
        lockVerticesFull(vertices, locks)
        f(v)
        unlockVerticesFull(vertices, locks)

        val tasks = Vector[V](v.tasks.length)

        var i = 0
        while (i < v.tasks.length) {
          tasks(i) = v.tasks(i)
          i += 1
        }

        tasks
      }
    }

    while (!queue.isEmpty) {
      val queue2 = new Queue[Vector[V]]

      do {
        val vertexData = queue.dequeue()

        val vertex = generateVertex(vertexData)
        queue2 += Delite.run(OP_untilConvergedTask(vertex))
      } while (!queue.isEmpty)

      val taskSet = new HashSet[V]

      for (local <- queue2) {
        val len = local.length
        var i = 0
        while (i < len) {
          val v = local.dc_apply(i)
          if (!taskSet.contains(v)) {
            taskSet += v
            queue += v
          }
          i += 1
        }
      }
    }
  }

  def untilConvergedData(c: Consistency.Consistency)(f: (Graph[V, E]#Vertex) => Unit)(implicit mV: ClassManifest[V]): Unit = {
    if(!sorted) {
      sort()
    }
        
    // Ugh
    implicit val proxyFactory = new Vector.ProxyFactory[V]

    // List of locks
    val locks = new ConcurrentHashMap[V, ReentrantLock]

    // Copy all vertices over into starting list
    var vertices = Vector[V](vertexList.length)

    var i = 0
    for(v <- vertexList) {
      vertices(i) = v
      i += 1
    }

    while (!vertices.isEmpty) {
      vertices = Delite.run(new GraphOP_untilConvergedData[V,E](this, vertices, f, locks))
    }
  }

  def untilConvergedData2(c: Consistency.Consistency)(f: (Graph[V, E]#Vertex) => Unit)(implicit mV: ClassManifest[V]): Unit = {
    if(!sorted) {
      sort()
    }

    // Ugh
    implicit val proxyFactory = new Vector.ProxyFactory[V]

    // List of locks
    val locks = new ConcurrentHashMap[V, ReentrantReadWriteLock]

    // Copy all vertices over into starting list
    var vertices = Vector[V](vertexList.length)

    var i = 0
    for(v <- vertexList) {
      vertices(i) = v
      i += 1
    }

    while (!vertices.isEmpty) {
      vertices = Delite.run(new GraphOP_untilConvergedData2[V,E](this, c, vertices, f, locks))
    }
  }

  def fullVertices(v: Vertex): Seq[V] = {
    val sorted = new ArrayBuffer[V](v.neighbors.size + 1)

    var i = 0
    var j = 0
    while(i + j < sorted.length) {
      if(j < 1 && System.identityHashCode(v.data) < System.identityHashCode(v.neighbors(i))) {
        sorted(i) = v.data
        j += 1
      }
      else {
        sorted(i + j) = v.neighbors(i)
        i += 1
      }
    }
    sorted
  }

  def edgeVertices(v: Vertex): Seq[(V, LockType)] = {
    val sorted = new ArrayBuffer[(V, LockType)](v.neighbors.size + 1)

    var i = 0
    var j = 0
    while(i + j < sorted.length) {
      if(j < 1 && System.identityHashCode(v.data) < System.identityHashCode(v.neighbors(i))) {
        sorted(i) = (v.data, WriteLock())
        j += 1
      }
      else {
        sorted(i + j) = (v.neighbors(i), ReadLock())
        i += 1
      }
    }
    sorted
  }

  def lockVerticesFull(vertices: Seq[V], locks: ConcurrentHashMap[V, ReentrantLock]) = {
    var i = 0

    while (i < vertices.length) {
      locks.get(vertices(i)).lock()
      i += 1
    }
  }

  def unlockVerticesFull(vertices: Seq[V], locks: ConcurrentHashMap[V, ReentrantLock]) = {
    var i = vertices.length - 1

    while (i >= 0) {
      locks.get(vertices(i)).unlock()
      i -= 1
    }
  }

  def lockVerticesEdge(v: V, vertices: Seq[V], locks: ConcurrentHashMap[V, ReentrantReadWriteLock]) = {
    var i = 0

    while (i < vertices.length) {
      if(vertices(i) == v) {
        locks.get(vertices(i)).writeLock().lock()
      }
      else {
        locks.get(vertices(i)).readLock.lock()
      }
      i += 1
    }
  }

  def unlockVerticesEdge(v: V, vertices: Seq[V], locks: ConcurrentHashMap[V, ReentrantReadWriteLock]) = {
    var i = vertices.length - 1

    while (i >= 0) {
      if(vertices(i) == v) {
        locks.get(vertices(i)).writeLock().unlock()
      }
      else {
        locks.get(vertices(i)).readLock.unlock()
      }
      i -= 1
    }
  }

  def vertexVertices(v: Vertex, locks: ConcurrentHashMap[V, ReentrantReadWriteLock]):Seq[(V, Lock)] = {
    Seq((v.data, locks.get(v.data).writeLock)) 
  }

  def fullVertices2(v: Vertex, locks: ConcurrentHashMap[V, ReentrantReadWriteLock]): Seq[(V, Lock)] = {
    val sorted = new ArrayBuffer[(V, Lock)](v.neighbors.size + 1)

    var i = 0
    var j = 0
    while(i + j < sorted.length) {
      if(j < 1 && System.identityHashCode(v.data) < System.identityHashCode(v.neighbors(i))) {
        sorted(i) = (v.data, locks.get(v.data).writeLock())
        j += 1
      }
      else {
        sorted(i + j) = (v.neighbors(i), locks.get(v.neighbors(i)).writeLock())
        i += 1
      }
    }
    sorted
  }

  def edgeVertices2(v: Vertex, locks: ConcurrentHashMap[V, ReentrantReadWriteLock]): Seq[(V, Lock)] = {
    val sorted = new ArrayBuffer[(V, Lock)](v.neighbors.size + 1)

    var i = 0
    var j = 0
    while(i + j < sorted.length) {
      if(j < 1 && System.identityHashCode(v.data) < System.identityHashCode(v.neighbors(i))) {
        sorted(i) = (v.data, locks.get(v.data).writeLock())
        j += 1
      }
      else {
        sorted(i + j) = (v.neighbors(i), locks.get(v.neighbors(i)).readLock())
        i += 1
      }
    }
    sorted
  }

 def lockVertices(vertices: Seq[(V, Lock)]) = {
    var i = 0

    while (i < vertices.length) {
      vertices(i)._2.lock()
      i += 1
    }
  }

  def unlockVertices(vertices: Seq[(V, Lock)]) = {
    var i = vertices.length - 1

    while (i >= 0) {
      vertices(i)._2.unlock()
      i -= 1
    }
  }

  def consistencyVertices(v: Vertex, c: Consistency.Consistency, locks: ConcurrentHashMap[V, ReentrantReadWriteLock]) : Seq[(V, Lock)] = {
    c match {
      case Consistency.Vertex => vertexVertices(v, locks)
      case Consistency.Edge => edgeVertices2(v, locks)
      case Consistency.Full => fullVertices2(v, locks)
      case _ => Seq()
    }
  }
}

abstract class LockType
case class ReadLock extends LockType
case class WriteLock extends LockType