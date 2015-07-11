package generated.scala

final case class ResourceInfo (
  final val threadId: Int,
  final val numThreads: Int,
  final val slaveId: Int,
  final val numSlaves: Int,
  final val groupId: Int,
  final val groupSize: Int,
  final val availableThreads: Int
) {
  //workaround for a weird scoping issue with copy constructors in MultiLoopSync.scala
  final def copySync(threadId: Int, groupId: Int, groupSize: Int, availableThreads: Int) = copy(threadId = threadId, groupId = groupId, groupSize = groupSize, availableThreads = availableThreads)
}

object ResourceInfo {
  def apply(threadId: Int, numThreads: Int, slaveId: Int, numSlaves: Int): ResourceInfo = apply(threadId, numThreads, slaveId, numSlaves, -1, -1, numThreads)
}
