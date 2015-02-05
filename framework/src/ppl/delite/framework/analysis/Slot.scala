  package ppl.delite.framework.analysis

  // TODO: this should probably be in LMS

  // For use in intermediate results in analysis stages

  // Very much like Option, but with 'Unknown' as the empty option 
  // Not intended to be as complete as Option, but can add methods as needed 
  // (see Option.scala in Scala github)
  sealed abstract class Slot[+A] {
    def isKnown: Boolean
    def isUnknown: Boolean = !isKnown
    def isEmpty: Boolean
    def get: A
    final def getOrElse[B >: A](default: => B): B = if (isEmpty) default else this.get
  }
  // Equivalent to "Some(x)"
  final case class Known[+A](x: A) extends Slot[A] {
    override def isKnown = true
    override def isEmpty = false
    override def get = x 
    override def clone() = Known(x.clone())
  }
  // "Not yet defined"
  final case object Unknown extends Slot[Nothing] {
    override def isKnown = false
    override def isEmpty = true
    override def get = throw new NoSuchElementException("Unknown.get")
    override def clone() = Unknown
  }
