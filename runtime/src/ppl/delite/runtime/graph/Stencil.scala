package ppl.delite.runtime.graph

import ppl.delite.runtime.data.RemoteDeliteArray

abstract class Stencil {
  def combine(other: Stencil): Stencil
  def withArray(array: RemoteDeliteArray[_]): Option[Array[Int]]
}

case object Empty extends Stencil {
  def combine(other: Stencil) = other
  
  def withArray(array: RemoteDeliteArray[_]) = {
    println("WARNING: Unknown stencil on RemoteArray: will be satisfied with remote reads")
    None
  }
}

case object All extends Stencil {
  def combine(other: Stencil) = All
  
  def withArray(array: RemoteDeliteArray[_]) = {
    println("WARNING: 'All' stencil on RemoteArray: will be satisfied with remote reads")
    None
  }
}

case class Const(const: Int) extends Stencil {
  def combine(other: Stencil) = {
    //TODO: what should be the combine result with a const stencil?
    println("WARNING: combine Const stencil with other stencil")
    other
  }

  def withArray(array: RemoteDeliteArray[_]) = {
    println("WARNING: 'Const' stencil on RemoteArray: will be satisfied with remote reads")
    None
  }
}

case object One extends Stencil {
  def combine(other: Stencil) = other match {
    case One => One
    case _ => other.combine(this)
  }

  def withArray(array: RemoteDeliteArray[_]) = Some(array.offsets)
}

case class Interval(start: String, stride: String, length: String) extends Stencil {
  def combine(other: Stencil) = other match {
    case Empty => other.combine(this)
    case All => other.combine(this)
    case Const(c) => other.combine(this)
    case One => this
    case Interval(s,t,l) if (s == start && t == stride && l == length) => this 
    case _ => sys.error("don't know how to combine stencils " + this + " and " + other)
  }

  def withArray(array: RemoteDeliteArray[_]) = sys.error(this + " was never evaluated")
}

case class KnownInterval(start: Int, stride: Int, length: Int) extends Stencil {
  def combine(other: Stencil) = other match {
    case Empty => other.combine(this)
    case All => other.combine(this)
    case Const(c) => other.combine(this)
    case One => this
    case KnownInterval(s,t,l) if (t == stride) => if (l > length) other else this
    case _ => sys.error("don't know how to combine stencils " + this + " and " + other)
  }

  def withArray(array: RemoteDeliteArray[_]) = {
    assert(stride == 1 && start == length)
    Some(array.offsets.map(_ / start))
  }
}

object Stencil {
  def apply(stencil: String): Stencil = stencil match {
    case "none" => Empty
    case "all" => All
    case "one" => One
    case c if c.startsWith("const") =>
      val const = c.substring(6,c.length-1).toInt
      Const(const)
    case r if r.startsWith("range") => 
      val syms = r.substring(6,r.length-1).split(",")
      Interval(syms(0), syms(1), syms(2))
    case _ => sys.error("unrecognized stencil type: " + stencil)
  }
}

abstract class Partition {
  def combine(other: Partition): Partition
}

case object Local extends Partition {
  def combine(other: Partition) = other
}

case class Distributed(id: Set[String]) extends Partition {
  def combine(other: Partition) = other match {
    case Local => this
    case Distributed(j) if (id == j) => this
    case Distributed(j) => Distributed(id ++ j)
  }
}
