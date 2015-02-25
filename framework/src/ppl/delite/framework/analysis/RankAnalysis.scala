package ppl.delite.framework.analysis

import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.datastructures.DeliteMultiArrayOpsExp

import ppl.delite.framework.Util._

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
    def name = "rank"
    override def makeString(prefix: String) = rank.toString
  }
  case class Buffer(buff: Boolean) extends Metadata {
    def name = "isBuffer"
    override def makeString(prefix: String) = buff.toString
    def isBuffer: Boolean = buff
  }
  case class TargetRank(rank: Option[Int]) extends Metadata {
    def name = "targetRank"
    override def makeString(prefix: String) = rank.map{_.toString}.getOrElse("[Not a View]")
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

  override val IRMetadata: DeliteMultiArrayMetadata 
  import IRMetadata._

  // TODO: This would make more sense in MultiArrayMetadata, but
  // need to reference it for the Exp overloaded version of method
  // (or could have different names...)
  def getRank(p: SymbolProperties): Option[Rank] = p("rank").map{_.asInstanceOf[Rank]}
  def getTargetRank(p: SymbolProperties): Option[TargetRank] = p("targetRank").map{_.asInstanceOf[TargetRank]}
  def getBufferable(p: SymbolProperties): Option[Buffer] = p("isBuffer").map{_.asInstanceOf[Buffer]}

  def getRank(e: Exp[Any]): Option[Rank] = getMetadata(e, "rank").map{_.asInstanceOf[Rank]}
  def getTargetRank(e: Exp[Any]): Option[TargetRank] = getMetadata(e, "targetRank").map{_.asInstanceOf[TargetRank]}
  def getBufferable(e: Exp[Any]): Option[Buffer] = getMetadata(e, "isBuffer").map{_.asInstanceOf[Buffer]}

  // Defaults for view and bufferable are both false
  def isView(e: Exp[Any]): Boolean = getTargetRank(e).getOrElse{TargetRank(None)}.isView
  def isBuffer(e: Exp[Any]): Boolean = getBufferable(e).getOrElse{Buffer(false)}.isBuffer
}

trait RankAnalysis extends MultiArrayAnalysisStageOne {
  val IR: DeliteOpsExp
  import IR._

  override val IRMetadata: DeliteMultiArrayMetadata = 
    new DeliteMultiArrayMetadata {
      def dataComplete(a: SymbolProperties): Boolean = a match {
        case a: ArrayProperties => a("rank").isDefined && isComplete(a.children)
        case _ => true
      }
    }
  import IRMetadata._

  val name = "Rank Analysis"

  override def createMetadataFromType[A](tp: Manifest[A]): List[Metadata] = {
    if (isSubtype(tp.erasure, classOf[DeliteArray1D[_]])) List(Rank(1))
    else if (isSubtype(tp.erasure, classOf[DeliteArray2D[_]])) List(Rank(2))
    else if (isSubtype(tp.erasure, classOf[DeliteArray3D[_]])) List(Rank(3))
    else if (isSubtype(tp.erasure, classOf[DeliteArray4D[_]])) List(Rank(4))
    else if (isSubtype(tp.erasure, classOf[DeliteArray5D[_]])) List(Rank(5))
    else Nil
  }

  // Propagate rank info through IR
  override def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    case DeliteMultiArraySortIndices(_,_) => setMetadata(e, Rank(1))
    case DeliteStringSplit(_,_,_) => setMetadata(e, Rank(1))
    case DeliteMultiArrayMapFilter(_,_,_) => setMetadata(e, Rank(1)) 
    case DeliteMultiArrayFlatMap(_,_) => setMetadata(e, Rank(1))
    
    case DeliteMultiArrayNew(dims) => setMetadata(e, Rank(dims.length))

    case DeliteMultiArrayView(t,_,_,dims) => 
      setMetadata(e, Rank(dims.length))
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

  def listComplete(incomplete: List[Exp[Any]]) {
    log("--------------------", tagged = false)
    log("Completed: ", tagged = false)
    for ((k,v) <- metadata) {
      if (!incomplete.contains(k)) {
        log("sym: " + strDef(k), tagged = false)
        log(k + makeString(v), tagged = false)
        log("", tagged = false)
      }
    }
  }
  def listIncomplete(incomplete: List[Exp[Any]]) {
    log("-------------------", tagged = false)
    log("Incomplete: ", tagged = false)
    for (s <- incomplete) {
      result(strDef(s), tagged = false)
      result(s + makeString(metadata.get(s)), tagged = false)
      result("", tagged = false)
    }
  }
 
  override def run[A](b: Block[A]): Unit = {
    super.run(b)

    val incomplete = checkCompleteness(b)

    if (iter > MAX_ITERS && !incomplete.isEmpty) {
      result("Maximum iterations exceeded before all ranks were fully known")
      result("Try increasing the maximum number of iterations")
      listComplete(incomplete)
      listIncomplete(incomplete)
      
      throw new Exception("MultiArray metadata incomplete")
      //sys.exit(0)
    }
    else if (!incomplete.isEmpty) {
      result("Unable to statically determine the ranks of some arrays!")
      listComplete(incomplete)
      listIncomplete(incomplete)
      
      throw new Exception("MultiArray metadata incomplete")
      //sys.exit(0)
    }
    else {
      log("Completed. Found ranks: ")
      listComplete(incomplete)
    }
  }
}

// MultiArray Sanity Checking
trait RankChecking extends MultiArrayAnalysisStageOne {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp
  import IR._

  override val IRMetadata = new DeliteMultiArrayMetadata{ def dataComplete(a: SymbolProperties): Boolean = true }
  import IRMetadata._

  val name = "Rank Sanity Check"

  // This will throw an exception if getRank returns None, but completeness
  // check should guarantee that all expressions have a rank
  def rank(e: Exp[Any]): Int = getRank(e).get.rank

  // TODO: probably shouldn't be passing LHS just to get SourceContext list here
  // Find a better way to pass the source context in these statements
  def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    case op@DeliteMultiArrayPermute(ma,config) =>
      check(rank(ma) == config.length, "Number of dimensions given in permute conifguration must match input MultiArray rank", e)

    // Mutations --- TODO: are these checks necessary?
    case DeliteMultiArrayMutableZipWith(ma,mb,_) => 
      check(rank(ma) == rank(mb), "Ranks in inputs to mutable-zipwith must match")
      // TODO: is this needed?
      if (!matches(getChild(ma), getChild(mb))) {
        check(false, "Metadata for MultiArray elements in mutable-zipwith do not match: ")
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getChild(mb)), tagged = false)
      }

    case DeliteMultiArrayZipWith(ma,mb,_) =>
      check(rank(ma) == rank(mb), "Ranks in inputs to zipwith must match")
      // TODO: is this needed?
      if (!matches(getChild(ma), getChild(mb))) {
        check(false, "Metadata for MultiArray elements in zipwith do not match: ")
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getChild(mb)), tagged = false)
      }

    case op@DeliteMultiArrayReduce(ma,_,z) if isDataStructure(op.mA) =>
      if (!matches(getChild(ma), getProps(z))) {
        check(false, "Metadata for MultiArray elements and zero in reduce do not match: ")
        result(makeString(getChild(ma)), tagged = false)
        result(makeString(getProps(z)), tagged = false)
      }
    case op@DeliteMultiArrayNDMap(in,mdims,f) =>
      check(rank(op.ma) == rank(op.body.res), "Input and output rank must match in ND-Map")
    case DeliteMultiArrayNDInsert(ma,rhs,axis,i) =>
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation")
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Insert")
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank")
    case DeliteMultiArrayNDAppend(ma,rhs,axis) =>
      check(isBuffer(ma), "MultiArray must be bufferable for append operation")
      check(rank(ma) == rank(rhs) + 1, "Right hand side must have rank one less than left hand side in ND-Append")
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank")
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) =>
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation")
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Insert All")
      check(axis > 0 && axis <= rank(ma), "Insert axis must be greater than zero and less than or equal to MultiArray's rank")
    case DeliteMultiArrayAppendAll(ma,rhs,axis) =>
      check(isBuffer(ma), "MultiArray must be bufferable for append operation")
      check(rank(ma) == rank(rhs), "Ranks of left and right hand side must match in Append All")
      check(axis > 0 && axis <= rank(ma), "Append axis must be greater than zero and less than or equal to MultiArray's rank")
    case DeliteMultiArrayInsert(ma,x,i) => 
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation")
      check(rank(ma) == 1, "Element Insert is only defined on 1D arrays")
    case DeliteMultiArrayAppend(ma,x) =>
      check(isBuffer(ma), "MultiArray must be bufferable for insert operation")
      check(rank(ma) == 1, "Element Append is only defined on 1D arrays")
    case DeliteMultiArrayRemove(ma,axis,start,end) =>
      check(isBuffer(ma), "MultiArray must be bufferable for remove operation")
      check(axis > 0 && axis <= rank(ma), "Removal axis must be greater than zero and less than or equal to MultiArray's rank")

    case DeliteMultiArrayMkString(ma,dels) =>
      check(rank(ma) == dels.length, "Number of delimeters given in MkString must match input MultiArray's rank")

    case DeliteMultiArrayMapFilter(ma,_,_) => 
      check(rank(ma) == 1, "MapFilter is undefined for " + rank(ma) + "D arrays")
    case DeliteMultiArrayFlatMap(ma,f) => 
      check(rank(ma) == 1, "FlatMap is undefined for " + rank(ma) + "D arrays")     

    case DeliteMatrixMultiply(lhs,rhs) =>
      check(rank(lhs) == 2, "Left hand side of matrix multiply must be a 2D array")
      check(rank(rhs) == 2, "Right hand side of matrix multiply must be a 2D array")
    case DeliteMatrixVectorMultiply(mat,vec) =>
      check(rank(mat) == 2, "Matrix argument to matrix-vector multiply must be a 2D array")
      check(rank(vec) == 2, "Vector argument to matrix-vector multiply must be a 1D array")
  
    case _ => 
      //Nothing
  }

  override def analyzeStm(stm: Stm)(implicit ctx: AnalysisContext): Unit = stm match {
    case TP(s, Reflect(d,_,_)) => processOp(s,d)
    case TP(s, d) => processOp(s,d)
    case _ => // Nothing
  }

  override def run[A](b: Block[A]): Unit = {
    super.run(b)
    if (hadErrors) {fatalerr("Rank sanity check completed with errors")}
  }

}