package ppl.tests.scalatest

import org.scalatest._
import ppl.delite.runtime.sync.MultiLoopSync
import generated.scala.ResourceInfo


class MultiLoopSuite extends Suite {

  val numThreads = 16
  val resourceInfo = ResourceInfo(0, numThreads, 0, 0, numThreads)
  val bigLoopSize = 1000 // > numThreads
  val smallLoopSize = 4 // < numThreads

  def testThreadScheduling {
    val sync1 = new MultiLoopSync(bigLoopSize, 0, resourceInfo) //loopSize > numThreads
    assert(sync1.threads.length == numThreads)
    for ((i,r) <- (0 until numThreads) zip sync1.threads) {
      assert(r.threadId == i)
      assert(r.availableThreads == 1)
    }

    val sync2 = new MultiLoopSync(smallLoopSize, 0, resourceInfo)
    assert (sync2.threads.length == smallLoopSize)
    val newAvailable = numThreads / smallLoopSize
    var idx = 0
    for (r <- sync2.threads) {
      assert(r.availableThreads == newAvailable)

      val syncI = new MultiLoopSync(bigLoopSize, 0, r)
      assert(syncI.threads.length == newAvailable)
      for (r2 <- syncI.threads) {
        assert(r2.threadId == idx)
        assert(r2.availableThreads == 1)
        idx += 1
      }
    }
  }
}
