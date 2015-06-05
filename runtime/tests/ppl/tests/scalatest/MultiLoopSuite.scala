package ppl.tests.scalatest

import org.scalatest._
import ppl.delite.runtime.sync.MultiLoopSync
import generated.scala.ResourceInfo


class MultiLoopSuite extends Suite {

  //TODO: this functionality is duplicated in both in Scala and C++: how can we test the C++ implementation as well?

  val verbose = System.getProperty("tests.verbose", "false") != "false"

  def clue(implicit numThreads: Int, loopSize: Long) = s"for numThreads: $numThreads, loopSize: $loopSize"

  def checkThreadScheduling(resourceInfo: ResourceInfo)(implicit numThreads: Int, loopSize: Long): Unit = {
    val sync = new MultiLoopSync(loopSize, 0, resourceInfo)
    if (loopSize >= resourceInfo.availableThreads) { //big loop: should use all threads 
      assertResult(resourceInfo.availableThreads, clue)(sync.threads.length)
      for ((r,i) <- sync.threads.zipWithIndex) {
        assertResult(resourceInfo.threadId + i, clue)(r.threadId) //assigned threads should be contiguous
        assertResult(1, clue)(r.availableThreads)
        assertResult(i, clue)(r.groupId)
        assertResult(resourceInfo.availableThreads, clue)(r.groupSize)
      }
    } else { //small loop: should use (roughly) loopSize threads and then use extra threads for nested loops
      assert(sync.threads.length >= loopSize && sync.threads.length <= resourceInfo.availableThreads, clue)
      val newAvailable = resourceInfo.availableThreads / sync.threads.length
      for (r <- sync.threads) {
        assertResult(newAvailable, clue)(r.availableThreads)
        assertResult(sync.threads.length, clue)(r.groupSize)
        if (loopSize > 0) { //can fork nested loops
          if (loopSize == 1) checkThreadScheduling(r)(numThreads, 2)
          else checkThreadScheduling(r)
        }
      }
    }
  }

  def testThreadScheduling {
    for (numThreads <- Seq(1, 3, 4, 16, 19, 1024)) {
      for (loopSize <- Seq(0, 1, 7, 29, 187, 256, 1024, 10000)) {
        if (verbose) System.err.println(s"numThreads: $numThreads, loopSize: $loopSize")
        checkThreadScheduling(ResourceInfo(0, numThreads, 0, 0))(numThreads, loopSize)
      }
    }
  }

}
