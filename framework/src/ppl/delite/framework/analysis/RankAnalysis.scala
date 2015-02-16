package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.{HashMap,HashSet}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.DeliteMultiArrayOpsExp
import ppl.delite.framework.Config

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
  case class Rank(rank: Int) extends Metadata {
    val name = "rank"
    override def makeString(prefix: String = "") = prefix + rank
  }
  case class Buffer(buff: Boolean) extends Metadata {
    val name = "isBuffer"
    override def makeString(prefix: String = "") = prefix + buff
    def isBuffer: Boolean = buff
  }
  case class TargetRank(rank: Option[Int]) extends Metadata {
    val name = "targetRank"
    override def makeString(prefix: String = "") = prefix + rank.map{_.toString}.getOrElse("[Not a View]")
    def isView: Boolean = rank.isDefined
  }

  override def metadataMatches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: Rank, b: Rank) => (a.rank == b.rank)
    case (a: Buffer, b: Buffer) => (a.buff == b.buff)
    case (a: TargetRank, b: TargetRank) => 
      (a.rank, b.rank) match {
        case (Some(t1),Some(t2)) => t1 == t2
        case (None, None) => true
        case _ => false
      }
    case _ => super.metadataMatches(a,b)
  }
  override def canMeetMetadata(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (a: Rank, b: Rank) => (a.rank == b.rank)
    case (a: Buffer, b: Buffer) => true
    case (a: TargetRank, b: TargetRank) => true
    case _ => super.canMeetMetadata(a,b)
  }
  override def meetMetadata(a: Metadata, b: Metadata): Metadata = (a,b) match {
    case (a: Rank, b: Rank) if canMeetMetadata(a,b) => Rank(a.rank)
    case (a: Buffer, b: Buffer) => Buffer(a.buff && b.buff)
    case (a: TargetRank, b: TargetRank) => 
      (a.rank, b.rank) match {
        case (Some(rank1),Some(rank2)) => TargetRank(Some(Math.max(rank1,rank2)))
        case (None,Some(rank2)) => TargetRank(Some(rank2))
        case (Some(rank1),None) => TargetRank(Some(rank1))
        case (None,None) => TargetRank(None)
      }
    case _ => super.meetMetadata(a,b)
  }
}

trait MultiArrayAnalysisStageOne extends AnalysisBase {
  val IR: DeliteOpsExp
  import IR._

  val IRMetadata: DeliteMultiArrayMetadata 
  import IRMetadata._

  // TODO: This would make more sense in MultiArrayMetadata, but
  // need to reference it for the Exp overloaded version of method
  // (or could have different names...)
  def getRank(p: SymbolProperties): Option[Rank] = p("rank").map{_.asInstanceOf[Rank]}
  def getTargetRank(p: SymbolProperties): Option[TargetRank] = p("targetRank").map{_.asInstanceOf[TargetRank]}
  def getBufferable(p: SymbolProperties): Option[Buffer] = p("isBuffer").map{_.asInstanceOf[Buffer]}

  
  def getRank(e: Exp[Any]): Option[Rank] = metadata.get(e) match {case Some(p) => getRank(p); case None => None}
  def getTargetRank(e: Exp[Any]): Option[TargetRank] = metadata.get(e) match {case Some(p) => getTargetRank(p); case None => None}
  def getBufferable(e: Exp[Any]): Option[Buffer] = metadata.get(e) match {case Some(p) => getBufferable(p); case None => None}

  // Defaults for view and bufferable are both false
  def isView(e: Exp[Any]): Boolean = getTargetRank(e).getOrElse{TargetRank(None)}.isView
  def isBuffer(e: Exp[Any]): Boolean = getBufferable(e).getOrElse{Buffer(false)}.isBuffer
}

trait RankAnalysis extends MultiArrayAnalysisStageOne {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp
  import IR._

  val IRMetadata: DeliteMultiArrayMetadata = 
    new DeliteMultiArrayMetadata {
      override def dataComplete(a: ArrayProperties): Boolean = a("rank").isDefined
    }
  import IRMetadata._

  val name = "Rank Analysis"

  // Propagate rank info through IR
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
      if (isView(t)) setMetadata(e, getTargetRank(t))
      else           setMetadata(e, getRank(t).map{meta => TargetRank(Some(meta.rank))})

    case DeliteMultiArrayPermute(ma,_) => setMetadata(e, getRank(ma))   
    case DeliteMultiArrayReshape(ma,dims) => setMetadata(e, Rank(dims.length))
    case DeliteMultiArrayFromFunction(dims,_) => setMetadata(e, Rank(dims.length))
    case DeliteMultiArrayMap(ma,f) => setMetadata(e, getRank(ma))
    case DeliteMultiArrayZipWith(ma,mb,f) => setMetadata(e, getRank(ma))  

    case op@DeliteMultiArrayNDMap(in,mdims,_) => 
      setMetadata(op.ma, Rank(mdims.length))
      setMetadata(e, getRank(in))

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
        log(makeString(v), tagged = false)
        log("", tagged = false)
      }
    }
  }
}

// MultiArray Sanity Checking
trait RankChecking extends MultiArrayAnalysisStageOne {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp
  import IR._

  val IRMetadata: DeliteMultiArrayMetadata = new DeliteMultiArrayMetadata{}
  import IRMetadata._

  val name = "Rank Analysis"

  // This will throw an exception if getRank returns None, but completeness
  // check should guarantee that all expressions have a rank
  def rank(e: Exp[Any]): Int = getRank(e).get.rank

  // TODO: probably shouldn't be passing LHS just to get SourceContext list here
  // Find a better way to pass the source context in these statements
  def processOp[A](e: Exp[A], d: Def[_]): Unit = d match {
    case op@DeliteMultiArrayPermute(ma,config) =>
      check(rank(ma) == config.length, "Number of dimensions given in permute conifguration must match input MultiArray rank", e)

    // Mutations --- TODO: are these checks necessary?
    case DeliteMultiArrayUpdate(ma,_,_) => 
      check(isMutable(ma), "Cannot update immutable data structure", e)
    case DeliteMultiArrayMutableMap(ma,_) =>
      check(isMutable(ma), "Cannot mutable-map immutable data structure", e)
    case DeliteMultiArrayMutableZipWith(ma,mb,_) => 
      check(isMutable(ma), "Cannot mutable-zipwith immutable data structure", e)
      check(rank(ma) == rank(mb), "Ranks in inputs to mutable-zipwith must match", e)
      // TODO: is this needed?
      if (!matches(getChild(ma), getChild(mb))) {
        check(false, "Metadata for MultiArray elements in mutable-zipwith do not match: ", e)
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getChild(mb)), tagged = false)
      }

    case DeliteMultiArrayZipWith(ma,mb,_) =>
      check(rank(ma) == rank(mb), "Ranks in inputs to zipwith must match", e)
      // TODO: is this needed?
      if (!matches(getChild(ma), getChild(mb))) {
        check(false, "Metadata for MultiArray elements in zipwith do not match: ", e)
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getChild(mb)), tagged = false)
      }

    case op@DeliteMultiArrayReduce(ma,_,z) if isDataStructure(op.mA) =>
      if (!matches(getChild(ma), getProps(z))) {
        check(false, "Metadata for MultiArray elements and zero in reduce do not match: ", e)
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getProps(z)), tagged = false)
      }
    case op@DeliteMultiArrayNDMap(in,mdims,f) =>
      check(rank(op.ma) == rank(op.body.res), "Input and output rank must match in ND-Map", e)
    case DeliteMultiArrayNDInsert(ma,rhs,axis,i) =>
      check(isMutable(ma), "Cannot insert into immutable data structure", e)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", e)
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Insert", e)
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank", e)
    case DeliteMultiArrayNDAppend(ma,rhs,axis) =>
      check(isMutable(ma), "Cannot append to immutable data structure", e)
      check(isBuffer(ma), "MultiArray must be bufferable for append operation", e)
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Append", e)
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank", e)
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) =>
      check(isMutable(ma), "Cannot insert into immutable data structure", e)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", e)
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Insert All", e)
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank", e)
    case DeliteMultiArrayAppendAll(ma,rhs,axis) =>
      check(isMutable(ma), "Cannot append to immutable data structure", e)
      check(isBuffer(ma), "MultiArray must be bufferable for append operation", e)
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Append All", e)
      check(axis > 0 && axis <= rank(ma), "Append axis must be greater than zero and less than or equal to MultiArray's rank", e)
    case DeliteMultiArrayInsert(ma,x,i) => 
      check(isMutable(ma), "Cannot insert into immutable data structure", e)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", e)
      check(rank(ma) == 1, "Element Insert is only defined on 1D arrays", e)
    case DeliteMultiArrayAppend(ma,x) =>
      check(isMutable(ma), "Cannot append to immutable data structure", e)
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation", e)
      check(rank(ma) == 1, "Element Append is only defined on 1D arrays", e)
    case DeliteMultiArrayRemove(ma,axis,start,end) =>
      check(isMutable(ma), "Cannot remove elements from an immutable data structure", e)
      check(isBuffer(ma), "MultiArray must be bufferable for remove operation", e)
      check(axis > 0 && axis <= rank(ma), "Removal axis must be greater than zero and less than or equal to MultiArray's rank", e)

    case DeliteMultiArrayMkString(ma,dels) =>
      check(rank(ma) == dels.length, "Number of delimeters given in MkString must match input MultiArray's rank", e)

    case DeliteMultiArrayMapFilter(ma,_,_) => 
      check(rank(ma) == 1, "MapFilter is undefined for " + rank(ma) + "D arrays", e)
    case DeliteMultiArrayFlatMap(ma,f) => 
      check(rank(ma) == 1, "FlatMap is undefined for " + rank(ma) + "D arrays", e)     

    case DeliteMatrixMultiply(lhs,rhs) =>
      check(rank(lhs) == 2, "Left hand side of matrix multiply must be a 2D array", e)
      check(rank(rhs) == 2, "Right hand side of matrix multiply must be a 2D array", e)
    case DeliteMatrixVectorMultiply(mat,vec) =>
      check(rank(mat) == 2, "Matrix argument to matrix-vector multiply must be a 2D array", e)
      check(rank(vec) == 2, "Vector argument to matrix-vector multiply must be a 1D array", e)
  
    case _ => 
      //Nothing
  }

  override def analyzeStm(stm: Stm): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => processOp(s,d)
    case TP(s, d) => processOp(s,d)
    case _ => // Nothing
  }

}