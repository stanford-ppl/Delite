package ppl.delite.runtime.codegen.examples

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.ArrayBlockingQueue
import java.util.ArrayList

/**
 * Author: Kevin J. Brown
 * Date: 12/19/10
 * Time: 9:44 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * An example of a thread-safe container for some (specialized) type T
 * All threads should communicate results through these containers
 * Only one instance of this class exists
 *  therefore if multiple dynamic instances of a given static symbol exist either
 *  1) a barrier is required in order to reuse the container (first example)
 *  2) the container must be able to hold multiple instances at once (second example)
 * Note that in both cases reads of the container need to be destructive to allow garbage collection
 */

/**
 * The obvious alternative implementation is to allow multiple dynamic instances of each container
 * However, this requires a fork-join model as one thread must allocate the instance and then pass all the references to other threads
 */

/**
 * This is a blocking container specialized to the type T that allows storage of N items at a time
 * Allows one producer and an arbitrary but fixed number of consumers
 * The producer and each consumer can run ahead/behind each other by up to the capacity of the container, which can be chosen freely
 */

object ExampleContainer {

  type T = Array[Int] //T is specialized for each unique result
  val capacity = 16 //the size sets the amount of run-ahead possible

  private val numConsumers: Int = 5 //this is known and injected from schedule
  private val count = new Array[Int](capacity) //the current number of outstanding consumptions, when 0 the container entry is empty
  private val takeIndices = new Array[Int](numConsumers)

  private var size: Int = 0 //current number of live results
  private var putIndex: Int = 0 //index for next put
  private val items = new Array[T](capacity)

  private val lock = new ReentrantLock
  private val notEmpty = lock.newCondition //condition for waiting gets
  private val notFull = lock.newCondition //condition for waiting sets

  def get1: T = get(0)
  def get3: T = get(1)
  def get7: T = get(2)

  def get(thread: Int): T = {
    val takeIndex = takeIndices(thread)
    val lock = this.lock // b/c Doug Lea does it :-) (I assume making it local to the method helps the JIT somehow(?))
    lock.lock
    try {
      while (takeIndex == putIndex)
        notEmpty.await
      extract(thread, takeIndex)
    }
    finally {
      lock.unlock
    }
  }

  private def extract(thread: Int, takeIndex: Int): T = {
    val items = this.items
    val res = items(takeIndex)
    takeIndices(thread) = (takeIndex + 1) % items.length
    val count = this.count
    count(takeIndex) -= 1
    if (count(takeIndex) == 0) {
      items(takeIndex) = null.asInstanceOf[T] //clear container
      size -= 1
      notFull.signal //one thread (producer) waiting
    }
    res
  }

  def set(result: T) {
    val items = this.items
    val lock = this.lock
    lock.lock
    try {
      while (size == items.length)
        notFull.await
      insert(result)
    }
    finally {
      lock.unlock
    }
  }

  private def insert(result: T) {
    val items = this.items
    items(putIndex) = result
    count(putIndex) = numConsumers
    putIndex = (putIndex + 1) % items.length
    size += 1
    notEmpty.signalAll //n threads (all consumers) waiting
  }

}

/**
 * This is a blocking queue (not specialized here, but can be if this ever goes into use)
 * Queue is backed by an array of fixed capacity; this capacity determines how much the producer can "run-ahead" of the consumer and can be chosen freely
 * Requires a unique queue for each producer-consumer pair
 * NOTE: we don't allow multiple consumers (like the blocking container) because this forces all consumers to stay in sync (or it's no longer a queue)
 *  and the implementation becomes much more complicated
 *  Therefore maximum dynamic run-ahead/behind flexibility requires a separate queue for each consumer, so producers must add result to multiple queues rather than a single shared one
 */
object ExampleQueue {

  type T = Array[Int]

  val queue = new ArrayBlockingQueue[T](16) //the size sets the amount of "run-ahead" a producer can do

  def get: T = queue.take

  def set(result: T) = queue.put(result)

}

/**
 * The most basic container implementation
 * Cannot be used more than once
 * Gets are not destructive
 * Here just for reference and complexity comparison with the above versions
 */
object ExampleStaticContainer {

  type T = Array[Int] //T is specialized for each unique result

  private var notReady: Boolean = true
  private var _result: T = _

  private val lock = new ReentrantLock
  private val cond = lock.newCondition

  def get: T = {
    if (notReady) block //lock-free optimization: safe b/c boolean is a one-way switch, useful for repeated gets
    _result
  }

  private def block {
    val lock = this.lock
    lock.lock
    try {
      while (notReady)
        cond.await
    }
    finally {
      lock.unlock
    }
  }

  def set(result: T) {
    val lock = this.lock
    lock.lock
    try {
      _result = result
      notReady = false
      cond.signalAll
    }
    finally {
      lock.unlock
    }
  }

}