package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.{HashMap,HashSet}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteMultiArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

import ppl.delite.framework.analysis.Slot // change later..

// TODO: Assuming for now that we can have aliased views with different
// target ranks.
// for example: 
//    val x = MultiArray[Int](1, 2, 3)
//    val y = MultiArray[Int](3, 3)          // 2D -> 3D view should be inserted here
//    val z = MultiArray[MultiArray[Int]](2)
//    z(0) = y
//    z(1) = x.slice2D(1,0::3,0::3) 


// Note that views adopt child of target
// e.g. val x = MultiArray[MultiArray[Int]](...)   // child is MultiArray
//      val y = x.slice(...)                       // child is MultiArray

// Possible to have initialization be something like
// val x = MultiArray[MultiArray](...)
// x.forIndices{i => x(i) = f(i)}
// - OR -
// x.mmap{e => MultiArray[Int](...)}  
// - OR -
// x.mzip(y){(a,b) => f(b)}

// Without these mutable updates, no way to know what concrete inner type is
// (This should probably result in an error)

trait DeliteMultiArrayMetadata extends DeliteMetadata {
  case class Rank(rank: Int) {
    override def makeString(prefix: String = "") = prefix + rank
    override def multiLine = false
  }
  case class Buffer(buff: Boolean) {
    override def makeString(prefix: String = "") = prefix + buff
    override def multiLine = false
    def isBuffer: Boolean = buff
  }
  case class TargetRank(rank: Option[Int]) {
    override def makeString(prefix: String = "") = prefix + rank.map{_.toString}.getOrElse("[Not a View]")
    override def multiLine = false
    def isView: Boolean = rank.isDefined
  }

  override def matches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: Rank, b: Rank) => (a.rank == b.rank)
    case (a: Buffer, b: Buffer) => (a.buff == b.buff)
    case (a: TargetRank, b: TargetRank) => 
      (a.rank, b.rank) match {
        case (Some(t1),Some(t2)) => t1 == t2
        case (None, None) => true
        case _ => false
      }
    case _ => super.matches(a,b)
  }
  override def canMeet(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: Rank, b: Rank) => (a.rank == b.rank)
    case (a: Buffer, b: Buffer) => true
    case (a: TargetRank, b: TargetRank) => true
    case _ => super.canMeet(a,b)
  }
  override def meet(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: Rank, b: Rank) if canMeet(a,b) => Rank(a.rank)
    case (a: Buffer, b: Buffer) => Buffer(a.buff && b.buff)
    case (a: TargetRank, b: TargetRank) => 
      (a.rank, b.rank) match {
        case (Some(rank1),Some(rank2)) => TargetRank(Math.max(rank1,rank2))
        case (None,Some(rank2)) => TargetRank(Some(rank2))
        case (Some(rank2),None) => TargetRank(Some(rank1))
        case (None,None) => TargetRank(None)
      }
    case _ => super.canMeet(a,b)
  }
  override def isComplete(a: Metadata): Boolean = a match {
    case Rank(_) => true
    case Buffer(_) => true
    case TargetRank(_) => true
    case _ => super.canMeet(a,b)
  }
  override def metaKey(a: Metadata): String = a match {
    case Rank(_) => "rank"
    case Buffer(_) => "isBuffer"
    case TargetRank(_) => "targetRank"
    case _ => super.metakey(a)
  }

  def rank(p: SymbolProperties): Option[Rank] = {
    if (!p.isInstanceOf[MultiArrayProperties]) 
      warn("Attempted to get rank of non-MultiArray symbol")
    p("rank").map{_.asInstanceOf[Rank]}
  }
  def targetRank(p: SymbolProperties): Option[TargetRank] = {
    if (!p.isInstanceOf[MultiArrayProperties])
      warn("Attempted to get target rank of non-MultiArray symbol")
    p("targetRank").map{_asInstanceOf[TargetRank]}
  }
  def bufferable(p: SymbolProperties): Option[Buffer] = {
    if (!p.isInstanceOf[MultiArrayProperties])
      warn("Attempted to get buffer property of non-MultiArray symbol")
    p("isBuffer").map{_.asInstanceOf[Buffer]}
  }
}

trait MultiArrayAnalysisStageOne extends AnalysisBase with DeliteMultiArrayMetadata {
  override val IR: DeliteOpsExp
  import IR._

  // Defaults for view and bufferable are both false
  def rank(e: Exp[Any]): Option[Rank] = metadata.get(e).map{rank(_)}
  def targetRank(e: Exp[Any]): TargetRank = metadata.get(e).map{targetRank(_)}.getOrElse{TargetRank(None)}
  def bufferable(e: Exp[Any]): Buffer = metadata.get(e).map{bufferable(_)}.getOrElse{Buffer(false)}

  def isView(e: Exp[Any]): Boolean = targetRank(e).isView
  def isBuffer(e: Exp[Any]): Boolean = bufferable(e).isBuffer
}

trait RankAnalysis extends MultiArrayAnalysisStageOne {
  val name = "Rank Analysis"

  override def dataComplete(a: MultiArrayProperties): Boolean = rank(a).isDefined

  /**
   * Propagate rank info through IR
  */
  override def processOp[A](e: Exp[A], d: Def[_]): Unit = d match {
    case DeliteMultiArraySortIndices(_,_) => setMetadata(e, Rank(1))
    case DeliteStringSplit(_,_,_) => setMetadata(e, Rank(1))
    case DeliteMultiArrayMapFilter(_,_,_) => setMetadata(e, Rank(1)) 
    case DeliteMultiArrayFlatMap(_,_) => setMetadata(e, Rank(1))
    
    case DeliteMultiArrayNew(dims) => setMetadata(e, Rank(dims.length))

    case DeliteMultiArrayView(t,_,_,dims) => 
      setMetadata(e, Rank(dims.length))  // set rank
      // Check that view's rank can never be larger than its target
      // Switch target's Rank metadata to TargetRank metadata if it exists
      // and the target is not itself a view
      if (isView(t)) setMetadata(e, targetRank(t))
      else           setMetadata(e, rank(t).map{meta => TargetRank(meta.rank)})

    // will check rank(ma) == c.length later
    case DeliteMultiArrayPermute(ma,_) => setMetadata(e, rank(ma))   
    case DeliteMultiArrayReshape(ma,dims) => setMetadata(e, Rank(dims.length))
    case DeliteMultiArrayFromFunction(dims,_) => setMetadata(e, Rank(dims.length))
    case DeliteMultiArrayMap(ma,f) => setMetadata(e, rank(ma))
     // will check rank(ma) == rank(mb) later
    case DeliteMultiArrayZipWith(ma,mb,f) => setMetadata(e, rank(ma))  

    case op @ DeliteMultiArrayNDMap(in,mdims,_) => 
      setMetadata(op.ma, Rank(mdims.length))
      setMetadata(e, rank(in))

    case _ => // Nothing
  }

  override def run[A](b: Block[A]): Unit = {
    super.run(b)

    val incomplete = checkCompleteness(b)

    if (iter > MAX_ITERS && !incomplete.isEmpty) {
      result("Maximum iterations exceeded before all ranks were fully known")
      result("Try increasing the maximum number of iterations", tagged = false)
      sys.exit(0)
    }
    else if (!incomplete.isEmpty) {
      result("Unable to statically determine the ranks of some arrays: ")
      // print symbols
      for (s <- incomplete) {
        result(strDef(s), tagged = false)
        result(makeString(metadata(s)), tagged = false)
      }
      sys.exit(0)
    }
    else {
      log("Completed. Found ranks: ")
      for ((k,v) <- metadata) {
        log("sym: " + strDef(k), tagged = false)
        log(v.makeString(), tagged = false)
        log("", tagged = false)
      }
    }
    // TODO: Remove this later
    fatalerr("Done")
  }
}

// MultiArray Sanity Checking
trait RankChecking extends MultiArrayAnalysisStageOne {
  val name = "MASC-1"

  def processOp[A](s: Exp[A], d: Def[_]): Unit = d match {
    // Mutations --- may not need these if this is already being checked in staging
    case op @ DeliteMultiArrayUpdate(ma,_,_) => 
      check(isMutable(ma), "Cannot update immutable data structure", op.ctx)
    case op @ DeliteMultiArrayMutableMap(ma,_) =>
      check(isMutable(ma), "Cannot mutable-map immutable data structure", op.ctx)
    case op @ DeliteMultiArrayMutableZipWith(ma,mb,_) => 
      check(isMutable(ma), "Cannot mutable-zipwith immutable data structure", op.ctx)
      check(rank(ma) == rank(mb), "Ranks in inputs to mutable-zipwith must match", op.ctx)
      // TODO: is this needed?
      if (!matches(getChild(ma), getChild(mb))) {
        check(false, "Metadata for MultiArray elements in mutable-zipwith do not match: ", op.ctx)
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getChild(mb)), tagged = false)
      }

    case op @ DeliteMultiArrayZipWith(ma,mb,_) =>
      check(rank(ma) == rank(mb), "Ranks in inputs to zipwith must match", op.ctx)
      // TODO: is this needed?
      if (!matches(getChild(ma), getChild(mb))) {
        check(false, "Metadata for MultiArray elements in zipwith do not match: ", op.ctx)
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getChild(mb)), tagged = false)
      }

    case op @ DeliteMultiArrayReduce(ma,_,z) if isDataStructure(op.mA) =>
      if (!matches(getChild(ma), getProps(z))) {
        check(false, "Metadata for MultiArray elements and zero in reduce do not match: ", op.ctx)
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getProps(z)), tagged = false)
      }
    case op @ DeliteMultiArrayNDMap(in,mdims,f) =>
      check(rank(op.ma) == rank(op.body.res), "Input and output rank must match in ND-Map", op.ctx)
    case op @ DeliteMultiArrayNDInsert(ma,rhs,axis,i) =>
      check(isMutable(ma), "Cannot insert into immutable data structure", op.ctx)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", op.ctx)
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Insert", op.ctx)
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank", op.ctx)
    case op @ DeliteMultiArrayNDAppend(ma,rhs,axis) =>
      check(isMutable(ma), "Cannot append to immutable data structure", op.ctx)
      check(isBuffer(ma), "MultiArray must be bufferable for append operation", op.ctx)
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Append", op.ctx)
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank", op.ctx)
    case op @ DeliteMultiArrayInsertAll(ma,rhs,axis,i) =>
      check(isMutable(ma), "Cannot insert into immutable data structure", op.ctx)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", op.ctx)
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Insert All", op.ctx)
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank", op.ctx)
    case op @ DeliteMultiArrayAppendAll(ma,rhs,axis) =>
      check(isMutable(ma), "Cannot append to immutable data structure", op.ctx)
      check(isBuffer(ma), "MultiArray must be bufferable for append operation", op.ctx)
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Append All", op.ctx)
      check(axis > 0 && axis <= rank(ma), "Append axis must be greater than zero and less than or equal to MultiArray's rank", op.ctx)
    case op @ DeliteMultiArrayInsert(ma,x,i) => 
      check(isMutable(ma), "Cannot insert into immutable data structure", op.ctx)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", op.ctx)
      check(rank(ma) == 1, "Element Insert is only defined on 1D arrays", op.ctx)
    case op @ DeliteMultiArrayAppend(ma,x) =>
      check(isMutable(ma), "Cannot append to immutable data structure", op.ctx)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", op.ctx)
      check(rank(ma) == 1, "Element Append is only defined on 1D arrays", op.ctx)
    case op @ DeliteMultiArrayRemove(ma,axis,start,end) =>
      check(isMutable(ma), "Cannot remove elements from an immutable data structure", op.ctx)
      check(isBuffer(ma), "MultiArray must be bufferable for remove operation", op.ctx)
      check(axis > 0 && axis <= rank(ma), "Removal axis must be greater than zero and less than or equal to MultiArray's rank", op.ctx)

    case op @ DeliteMultiArrayMkString(ma,dels) =>
      check(rank(ma) == dels.length, "Number of delimeters given in MkString must match input MultiArray's rank", op.ctx)

    case op @ DeliteMultiArrayMapFilter(ma,_,_) => 
      check(rank(ma) == 1, "MapFilter is undefined for " + rank(ma) + "D arrays", op.ctx)
    case op @ DeliteMultiArrayFlatMap(ma,f) => 
      check(rank(ma) == 1, "FlatMap is undefined for " + rank(ma) + "D arrays", op.ctx)     

    case op @ DeliteMatrixMultiply(lhs,rhs) =>
      check(rank(lhs) == 2, "Left hand side of matrix multiply must be a 2D array", op.ctx)
      check(rank(rhs) == 2, "Right hand side of matrix multiply must be a 2D array", op.ctx)
    case op @ DeliteMatrixVectorMultiply(mat,vec) =>
      check(rank(mat) == 2, "Matrix argument to matrix-vector multiply must be a 2D array", op.ctx)
      check(rank(vec) == 2, "Vector argument to matrix-vector multiply must be a 1D array", op.ctx)
  
    case _ => 
      //Nothing
  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(s, Reflect(d,_,_)) => processOp(s,d)
      case TP(s, d) => processOp(s,d)
      case _ => // Nothing
    }
    origTraversal(stm)
  }

}