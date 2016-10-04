package ppl.delite.framework.analysis

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{Traversal, FatExpressions}
import scala.virtualization.lms.common.EffectExp

/* Super simple base for detecting 1D affine access patterns
 * "Index" is kept abstract here so array address types can be DSL specific
 * Assumes CSE has correctly determined loop invariant values (uses scoping to detect invariant-ness)
 * Also assumes ND accesses have not yet been flattened, so that each dimension is addressed separately
 * (This is much easier to detect than an arbitrary affine function, but feel free to extend as needed)
 **/
trait AffineAnalysisExp extends EffectExp with FatExpressions {
  type Index

  // Variations used here allow Index to be abstract (otherwise can't as easily define stride of 1)
  sealed abstract class IndexPattern
  // a*i + b, where a and b must be loop invariant
  case class AffineAccess(a: Exp[Index], i: Sym[Index], b: Exp[Index]) extends IndexPattern
  // i + b, where b must be loop invariant
  case class OffsetAccess(i: Exp[Index], b: Exp[Index]) extends IndexPattern
  // a*i, where a must be loop invariant
  case class StridedAccess(a: Exp[Index], i: Exp[Index]) extends IndexPattern
  // linear access with some loop iterator
  case class LinearAccess(i: Exp[Index]) extends IndexPattern
  // loop invariant access (but may change with outer loops)
  case class InvariantAccess(b: Exp[Index]) extends IndexPattern
  // anything else
  case object RandomAccess extends IndexPattern



  /* Abstract functions. To use, fill in w/ relevant node matching
   * These are defined on Exp rather than Def to allow accessing metadata
   * without mucking with the (symbol,def) table.
   * Use Def or Deff unapply methods to get corresponding Def of symbols
   **/
  // Pair of symbols for nodes used in address calculation addition nodes
  def indexPlusUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])]
  // Pair of symbols for nodes used in address calculation multiplication nodes
  def indexTimesUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])]
  // List of loop scopes. Each scope contains a list of iterators and blocks to traverse for loop nodes
  def loopUnapply(x: Exp[Any]): Option[List[(List[Sym[Index]], List[Block[Any]])]]
  // Memory being read + list of addresses (for N-D access)
  def readUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])]
  // Memory being written + list of addresses (for N-D access)
  def writeUnapply(x: Exp[Any]): Option[(Exp[Any], List[Exp[Index]])]
  // Usually None, but allows other exceptions for symbols being loop invariant
  def invariantUnapply(x: Exp[Index]): Option[Exp[Index]] = None

  object Plus {
    def unapply(x: Exp[Index]) = indexPlusUnapply(x)
  }
  object Times {
    def unapply(x: Exp[Index]) = indexTimesUnapply(x)
  }
  object LoopLevels {
    def unapply(x: Exp[Any]) = loopUnapply(x)
  }
  object MemRead {
    def unapply(x: Exp[Any]) = readUnapply(x)
  }
  object MemWrite {
    def unapply(x: Exp[Any]) = writeUnapply(x)
  }

  case class AccessPattern(indices: List[IndexPattern]) extends Metadata

  object accessPatternOf {
    def apply(x: Exp[Any]) = meta[AccessPattern](x).get.indices
    def update(x: Exp[Any], indices: List[IndexPattern]) { setMetadata(x, AccessPattern(indices)) }
  }

  private def mirrorIndexPattern(pattern: IndexPattern, f: Transformer) = pattern match {
    case AffineAccess(a,i,b) => AffineAccess(f(a),f(i).asInstanceOf[Sym[Index]],f(b))
    case OffsetAccess(i,b) => OffsetAccess(f(i),f(b))
    case StridedAccess(a,i) => StridedAccess(f(a),f(i))
    case LinearAccess(i) => LinearAccess(f(i))
    case InvariantAccess(b) => InvariantAccess(f(b))
    case RandomAccess => RandomAccess
  }

  // Metadata mirroring
  override def mirror[T<:Metadata](m: T, f: Transformer) = (m match {
    case AccessPattern(indices) => AccessPattern(indices.map{p=> mirrorIndexPattern(p,f)})
    case _ => super.mirror(m,f)
  }).asInstanceOf[T]
}

trait AffineAnalyzer extends Traversal {
  val IR: AffineAnalysisExp
  import IR._

  override val eatReflect = true

  var outerIndices = Set[Sym[Index]]()
  var loopIndices = Set[Sym[Index]]()
  var boundIndexPatterns = Map[Exp[Index], List[IndexPattern]]()

  // The list of statements which can be scheduled prior to block traversals
  // (All statements in all scopes of below this scope)
  // If a statement is not contained in this set, it must be loop invariant (because it was already scheduled)
  var loopScope: Seq[Stm] = Nil

  def inLoop[T](indices: List[Sym[Index]])(x: => T): T = {
    val prevOuter = outerIndices
    val prevScope = loopScope
    outerIndices = loopIndices
    loopScope = innerScope
    loopIndices ++= indices
    val out = x
    loopIndices = outerIndices
    loopScope = prevScope
    outerIndices = prevOuter
    out
  }

  object LoopIndex {
    def unapply(x: Exp[Index]): Option[Sym[Index]] = x match {
      case s: Sym[Index] if loopIndices.contains(s) => Some(s)
      case _ => None
    }
  }
  object Invariant {
    def unapply(x: Exp[Index]): Option[Exp[Index]] = x match {
      case Const(_) => Some(x)
      case Param(_) => Some(x)
      case s: Sym[_] if !loopScope.exists(stm => (stm defines s).isDefined) => Some(x)
      case _ => IR.invariantUnapply(x)
    }
  }

  override def traverse(lhs: Sym[Any], rhs: Def[Any]): Unit = lhs match {
    case LoopLevels(levels) =>
      debugs(s"[Loop] $lhs = $rhs")
      debugs(s"  Levels: ${levels.length} ")
      debugs(s"  Current indices: $loopIndices")
      levels.foreach{ case (indices, blocks) =>
        debugs(s"  Traversing loop level with $indices")
        inLoop(indices){blocks.foreach{blk => traverseBlock(blk) }}
      }

    case MemRead(mem, addresses) =>
      debugs(s"[Read] $lhs = $rhs")
      tab += 1
      debugs(s"Memory: $mem")
      debugs(s"ND Address: $addresses")
      debugs(s"Current indices: $loopIndices")
      accessPatternOf(lhs) = extractAccessPatterns(addresses)
      debugs(s"Access pattern: ${accessPatternOf(lhs)}")
      tab -= 1

    case MemWrite(mem, addresses) =>
      debugs(s"[Write] $lhs = $rhs")
      tab += 1
      debugs(s"Memory: $mem")
      debugs(s"ND Address: $addresses")
      debugs(s"Current indices: $loopIndices")
      accessPatternOf(lhs) = extractAccessPatterns(addresses)
      debugs(s"Access pattern: ${accessPatternOf(lhs)}")
      tab -= 1

    case _ => super.traverse(lhs, rhs)
  }

  def extractAccessPatterns(xs: List[Exp[Index]]) = xs.flatMap{
    case x if boundIndexPatterns.contains(x) => boundIndexPatterns(x)
    case x => List(extractIndexPattern(x))
  }

  def extractIndexPattern(x: Exp[Index]) = x match {
    case Plus(Times(LoopIndex(i), Invariant(a)), Invariant(b)) => AffineAccess(a,i,b) // i*a + b
    case Plus(Times(Invariant(a), LoopIndex(i)), Invariant(b)) => AffineAccess(a,i,b) // a*i + b
    case Plus(Invariant(b), Times(LoopIndex(i), Invariant(a))) => AffineAccess(a,i,b) // b + i*a
    case Plus(Invariant(b), Times(Invariant(a), LoopIndex(i))) => AffineAccess(a,i,b) // b + a*i
    case Plus(LoopIndex(i), Invariant(b)) => OffsetAccess(i,b)                        // i + b
    case Plus(Invariant(b), LoopIndex(i)) => OffsetAccess(i,b)                        // b + i
    case Times(LoopIndex(i), Invariant(a)) => StridedAccess(a,i)                      // i*a
    case Times(Invariant(a), LoopIndex(i)) => StridedAccess(a,i)                      // a*i
    case LoopIndex(i) => LinearAccess(i)                                              // i
    case Invariant(b) => InvariantAccess(b)                                           // b
    case _ => RandomAccess                                                            // other
  }
}
