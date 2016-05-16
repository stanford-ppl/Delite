package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.Traversal
import scala.collection.mutable.HashMap

// "Hungry" because it effectively eats Reflects unless you tell it not to.
// Use when the default case for traversing nodes is to ignore Reflect effect wrappers
// REFACTOR: This can be moved to LMS if desired
trait HungryTraversal extends Traversal {
  import IR._

  val recurseAlways: Boolean = false  // Always follow default traversal scheme
  val recurseElse: Boolean = true     // Follow default traversal scheme when node was not matched

  override def traverseStm(stm: Stm) = {
    stm match {
      case TP(lhs, rhs) =>
        traverse(lhs, rhs)
        if (recurseAlways) blocks(rhs).foreach{blk => traverseBlock(blk)}

      case TTP(s, m, d) =>
        traverse(s, m, d)
        // TODO: Recursive traversal for TTP?
    }
  }
  def traverse(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    case Reflect(d, u, es) => traverse(lhs, d)
    case _ => if (recurseElse) blocks(rhs).foreach{blk => traverseBlock(blk)}
  }

  def traverse(lhs: List[Exp[Any]], mhs: List[Def[Any]], rhs: FatDef): Unit = {}
}

