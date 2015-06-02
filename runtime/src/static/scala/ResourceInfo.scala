package generated.scala

final case class ResourceInfo (
  final val threadId: Int,
  final val numThreads: Int,
  final val slaveId: Int,
  final val numSlaves: Int,
  final val availableThreads: Int
) {
  //workaround for a weird scoping issue with copy constructors in MultiLoopSync.scala
  final def copySync(threadId: Int, availableThreads: Int) = copy(threadId = threadId, availableThreads = availableThreads)
}
