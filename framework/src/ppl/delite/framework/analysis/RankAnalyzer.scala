package ppl.delite.framework.analysis

import ppl.delite.framework.visit._
import ppl.delite.framework.visit.Meetable._

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._

// TODO: Assuming for now that we can always have aliased views with different
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
trait RankMetadata extends DeliteMetadata {
  // Tristate implementation type for Buffers and Views
  sealed abstract class ImplType { def toString: String }
  object NotType extends ImplType { override def toString = "false"}
  object TrueType extends ImplType { override def toString = "true" }
  object PhysType extends ImplType { override def toString = "phys only" }

  implicit object ImplTypeIsMeetable extends Meetable[ImplType] {
    def _matches(a: ImplType, b: ImplType) = (a == b)
    def _incompatibilities(a: ImplType, b: ImplType, t: MeetFunc) = Nil
    def _canMeet(a: ImplType, b: ImplType, t: MeetFunc) = true
    def _meet(a: ImplType, b: ImplType, t: MeetFunc) = (a,b,t) match {
      case (_ ,i2,MetaOverwrite) => i2
      case (i1,i2,_) => if (i1 != i2) PhysType else i2
    }
    def _isComplete(a: ImplType) = true
    def _makeString(a: ImplType, prefix: String) = a.toString
    def _multiLine(a: ImplType) = false
  }

  case class MRank(rank: Int) extends Metadata {
    def name = "rank"
    override def makeString(prefix: String) = rank.toString
  }
  case class MBuffer(impl: ImplType) extends Metadata {
    def name = "isBuffer"
    override def makeString(prefix: String) = impl.toString
    def isTrueBuffer = (impl == TrueType)
    def isPhysBuffer = (impl == PhysType || impl == TrueType)
  }
  case class MView(impl: ImplType) extends Metadata {
    def name = "isView"
    override def makeString(prefix: String) = impl.toString
    def isTrueView = (impl == TrueType)
    def isPhysView = (impl == PhysType || impl == TrueType) 
  }

  override def metadataMatches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (MRank(r1), MRank(r2)) => (r1 == r2)
    case (MBuffer(i1), MBuffer(i2)) => matches(i1,i2)
    case (MView(i1), MView(i2)) => matches(i1,i2)
    case _ => super.metadataMatches(a,b)
  }
  override def metadataIncompatibilities(a: Metadata, b: Metadata, t: MeetFunc): List[String] = (a,b,t) match {
    case (MRank(_), MRank(_), MetaOverwrite) => Nil
    case (MRank(r1), MRank(r2), _) if r1 != r2 => List("incompatible ranks (" + r1 + " != " + r2 + ")")
    case _ => super.metadataIncompatibilities(a,b,t)
  }
  override def canMeetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Boolean = (a,b,t) match {
    case (MRank(_), MRank(_), MetaOverwrite) => true
    case (MRank(r1), MRank(r2), _) => (r1 == r2)
    case (MBuffer(_), MBuffer(_), _) => true
    case (MView(_), MView(_), _) => true
    case _ => super.canMeetMetadata(a,b,t)
  }
  override def meetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Metadata = (a,b,t) match {
    case (MRank(_), MRank(r2), MetaOverwrite) => MRank(r2)
    case (MRank(r1), MRank(r2), _) if r1 == r2 => MRank(r2)
    case (MBuffer(_), MBuffer(i2), MetaOverwrite) => MBuffer(i2)
    case (MBuffer(i1), MBuffer(i2), t) => MBuffer(meet(i1,i2,t))
    case (MView(_), MView(i2), MetaOverwrite) => MView(i2)
    case (MView(i1), MView(i2), t) => MView(meet(i1,i2,t))
    case _ => super.meetMetadata(a,b,t)
  }
}

trait MultiArrayHelperStageOne extends MetadataTransformer {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with RankMetadata
  import IR._

  def getRank(p: SymbolProperties) = p("rank").map{_.asInstanceOf[MRank]}
  def getView(p: SymbolProperties) = p("isView").map{_.asInstanceOf[MView]}
  def getBuffer(p: SymbolProperties) = p("isBuffer").map{_.asInstanceOf[MBuffer]}

  def getRank(e: Exp[Any]) = getMetadata(e, "rank").map{_.asInstanceOf[MRank]}
  def getView(e: Exp[Any]) = getMetadata(e, "isView").map{_.asInstanceOf[MView]}
  def getBuffer(e: Exp[Any]) = getMetadata(e, "isBuffer").map{_.asInstanceOf[MBuffer]}

  // Defaults for view and bufferable are both false
  def isPhysView(p: SymbolProperties) = getView(p).map{_.isPhysView}.getOrElse(false)
  def isPhysView(e: Exp[Any]) = getView(e).map{_.isPhysView}.getOrElse(false)
  def isPhysBuffer(p: SymbolProperties) = getBuffer(p).map{_.isPhysBuffer}.getOrElse(false)
  def isPhysBuffer(e: Exp[Any]) = getBuffer(e).map{_.isPhysBuffer}.getOrElse(false)

  def isTrueView(p: SymbolProperties) = getView(p).map{_.isTrueView}.getOrElse(false)
  def isTrueView(e: Exp[Any]) = getView(e).map{_.isTrueView}.getOrElse(false)
  def isTrueBuffer(p: SymbolProperties) = getBuffer(p).map{_.isTrueBuffer}.getOrElse(false)
  def isTrueBuffer(e: Exp[Any]) = getBuffer(e).map{_.isTrueBuffer}.getOrElse(false)
}

trait MultiArrayAnalyzerBase extends AnalyzerBase { 
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with DeliteMetadata
  import IR._

  override def forwardPropagate[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    case op@DeliteMultiArrayView(t,_,_,_) => setChild(e, getChild(t))
    case op@DeliteMultiArrayPermute(ma,_) => setChild(e, getChild(ma))
    case op@DeliteMultiArrayReshape(ma,_) => setChild(e, getChild(ma))
    case op@DeliteMultiArrayApply(ma,_)   => setProps(e, getChild(ma))
    
    case op@DeliteMultiArrayFromFunction(dims,_) => setChild(e, getProps(op.body.res))
    case op@DeliteMultiArrayMap(ma,_)            => setChild(e, getProps(op.body.res))
    case op@DeliteMultiArrayZipWith(ma,mb,_)     => setChild(e, getProps(op.body.res))
    case op@DeliteMultiArrayMapFilter(ma,_,_)    => setChild(e, getProps(op.mapFunc.res))
    case op@DeliteMultiArrayFlatMap(ma,_)        => setChild(e, getChild(op.body.res))

    // TODO: Is there a way to avoid having to manually propagate to rVs here?
    case op@DeliteMultiArrayReduce(ma,_,_) =>
      setProps(op.rV._1, getChild(ma))
      setProps(op.rV._2, getChild(ma)) 
      setProps(e, getProps(op.body.res))
    case op@DeliteMultiArrayNDMap(in,_,_) =>
      setChild(op.rV, getChild(in))
      setChild(e, getChild(op.body.res))
    
    // Aliasing from updates/inserts/etc
    case op@DeliteMultiArrayUpdate(ma,_,x) => 
      ctx.setSymPos(ma)
      setChild(ma, attemptMeet(getChild(ma), getProps(x), func = MetaUpdate))
    case op@DeliteMultiArrayInsert(ma,x,_) =>
      ctx.setSymPos(ma)
      setChild(ma, attemptMeet(getChild(ma), getProps(x), func = MetaUpdate))
    case op@DeliteMultiArrayAppend(ma,x) =>
      ctx.setSymPos(ma)
      setChild(ma, attemptMeet(getChild(ma), getProps(x), func = MetaUpdate))
    case op@DeliteMultiArrayInsertAll(ma,x,_,_) =>
      ctx.setSymPos(ma)
      setChild(ma, attemptMeet(getChild(ma), getChild(x), func = MetaUpdate))

    case _ => super.forwardPropagate(e,d)
  }
  
  override def tracerToProperties(t: AtomicTracer, child: Option[SymbolProperties]) = t match {
    case MultiArrayTracer(_) => ArrayProperties(child, NoData)
    case _ => super.tracerToProperties(t, child)
  }

  override def getAtomicWriteRhs(d: AtomicWrite[Any])(implicit ctx: AnalysisContext): Option[Exp[Any]] = d match {
    case DeliteMultiArrayUpdate(_,_,x) => Some(x)
    case DeliteMultiArrayInsert(_,x,_) => Some(x)
    case DeliteMultiArrayAppend(_,x) => Some(x)
    case DeliteMultiArrayInsertAll(_,x,_,_) => Some(x)
    case DeliteMultiArrayRemove(_,_,_,_) => None
    case _ => super.getAtomicWriteRhs(d)
  }
}

trait RankAnalyzer extends MultiArrayAnalyzerBase with MultiArrayHelperStageOne {
  import IR._
  override val name = "Rank Analyzer"
  
  def setRank(e: Exp[Any], rank: Int)(implicit ctx: AnalysisContext) { setMetadata(e, MRank(rank)) }
  def setRank(e: Exp[Any], rank: Option[MRank])(implicit ctx: AnalysisContext) { setMetadata(e, rank) }
  def enableBuffer(e: Exp[Any])(implicit ctx: AnalysisContext) { setMetadata(e, MBuffer(TrueType)) }

  // Rank Analysis is run before MultiArray transformations - all preexisting arrays are rank 1
  override def defaultTypeMetadata[A](tp: Manifest[A]): List[Metadata] = {
    if      (isSubtype(tp.erasure, classOf[DeliteArray[_]]))   List(MRank(1))
    else if (isSubtype(tp.erasure, classOf[DeliteArray1D[_]])) List(MRank(1))
    else if (isSubtype(tp.erasure, classOf[DeliteArray2D[_]])) List(MRank(2))
    else if (isSubtype(tp.erasure, classOf[DeliteArray3D[_]])) List(MRank(3))
    else if (isSubtype(tp.erasure, classOf[DeliteArray4D[_]])) List(MRank(4))
    else if (isSubtype(tp.erasure, classOf[DeliteArray5D[_]])) List(MRank(5))
    else Nil
  }

  override def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    // --- Rank rules for specific IR nodes
    case DeliteMultiArrayNew(dims)            => setRank(e, dims.length)
    case DeliteMultiArrayPermute(ma,_)        => setRank(e, getRank(ma))   
    case DeliteMultiArrayReshape(_,dims)      => setRank(e, dims.length)
    case DeliteMultiArrayFromFunction(dims,_) => setRank(e, dims.length)
    case DeliteMultiArrayMap(ma,_)            => setRank(e, getRank(ma))
    case DeliteMultiArrayZipWith(ma,_,_)      => setRank(e, getRank(ma))  

    case DeliteMultiArrayView(t,_,_,dims) => 
      setRank(e, dims.length)
      if (isPhysBuffer(t)) setMetadata(e, MBuffer(PhysType))
      setMetadata(e, MView(TrueType))

    case op@DeliteMultiArrayNDMap(in,mdims,_) => 
      setRank(op.rV, mdims.length)
      setRank(e, getRank(in))

    // --- Buffer rules
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayInsert(ma,x,i) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayAppend(ma,x) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayRemove(ma,axis,start,end) if isMutable(ma) => enableBuffer(ma)
 
    case _ => // Nothing
  } 
  override def completed(e: Exp[Any]) = { 
    !hasMultiArrayChild(e.tp) || (getProps(e) match {
      case Some(a) => isComplete(a) && a("rank").isDefined
      case _ => false 
    })
  }

  var incomplete: List[Exp[Any]] = Nil

  override def postprocess[A:Manifest](b: Block[A]) = { 
    printmsg("-------------------")
    printmsg("End of retry # " + (retries + 1).toString)
    incomplete = checkCompleteness(b) 
    //for (s <- incomplete) {
      //warn(quotePos(s.pos) + ": Unable to statically determine the rank of " + s.tp + "\n\t" + quoteCode(s.pos).getOrElse(strDef(s)))
      //setMetadata(s, MRank(1))(AnalysisContext(s.pos,s.pos,""))
    //}
    warn(incomplete.isEmpty, "Unable to statically determine the ranks of some arrays! This can occur if the type 'MultiArray' is used " + 
                              "as an inner data structure type without element initialization. Proceeding assuming unknown ranks are 1D.")

    printmsg("Incomplete: ")
    for (s <- incomplete) {
      printmsg(quotePos(s.pos))
      printmsg("sym: " + strDef(s))
      if (data.contains(s))
        printmsg(s + makeString(data(s)))
      else 
        printmsg(s + ": [None]")
      printmsg("")
    } 
    printmsg("\nComplete: ")
    // List metadata
    for ((k,v) <- metadata) {
      if (isMultiArrayType(k.tp)) {
        printmsg(quotePos(k.pos))     // change to printdbg later
        printmsg("sym: " + strDef(k))
        printmsg(k + makeString(v))
        printmsg("")
      }
    }
    printmsg("\n")

    if (!incomplete.isEmpty) resume()
    else {
      // Nothing - move printing here later
    }
    super.postprocess(b)
  }
  override def hasCompleted = incomplete.isEmpty
  override def failedToConverge() { 
    warn("Maximum iterations exceeded before all ranks were fully known")
    warn("Try increasing the maximum number of iterations")
  }
}

trait MultiArrayHelperStageTwo extends MultiArrayHelperStageOne {
  import IR._
  // This will throw an exception if get* returns None, but completeness
  // check should guarantee that all MultiArray expressions have a rank
  def rank(e: Exp[Any]) = getRank(e).get.rank
  def rank(p: SymbolProperties) = getRank(p).get.rank
}

// MultiArray Sanity Checking
trait RankChecker extends MultiArrayAnalyzerBase with MultiArrayHelperStageTwo {
  import IR._
  override val name = "Rank Sanity Check"

  override val autopropagate = false

  def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: AnalysisContext): Unit = d match {
    case op@DeliteMultiArrayPermute(ma,config) =>
      check(rank(ma) == config.length, "Number of dimensions given in permute conifguration must match input MultiArray rank")

    case DeliteMultiArrayZipWith(ma,mb,_) =>
      check(rank(ma) == rank(mb), "Ranks in inputs to zip-with must match")

    case op@DeliteMultiArrayReduce(ma,_,z) if isDataStructureType(op.mA) =>
      if (!matches(getChild(ma), getProps(z))) {
        check(false, "Metadata for MultiArray elements and zero in reduce do not match: \n" + 
                     makeString(getChild(ma)) + "\n" + makeString(getProps(z))
             )
      }
    case op@DeliteMultiArrayNDMap(in,mdims,f) =>
      check(rank(in) > rank(op.rV), "Inner rank of ND map must be less than input rank")
      check(rank(op.rV) == rank(op.body.res), "Input and output rank must match in ND Map")
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) =>
      check(isTrueBuffer(ma), "MultiArray must be bufferable for insert operation")
      check(rank(ma) == rank(rhs) || rank(ma) == rank(rhs)+1, "Ranks of right hand size must be equal or one less than rank of rhs in insert all")
      check(axis >= 0 && axis < rank(ma), "Insert axis must be non-negative and less than MultiArray's rank")
    case DeliteMultiArrayInsert(ma,x,i) => 
      check(isTrueBuffer(ma), "MultiArray must be bufferable for insert operation")
      check(rank(ma) == 1, "Element Insert is only defined on 1D arrays")
    case DeliteMultiArrayRemove(ma,axis,start,end) =>
      check(isTrueBuffer(ma), "MultiArray must be bufferable for remove operation")
      check(axis >= 0 && axis < rank(ma), "Removal axis must be non-negative and less than MultiArray's rank")

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
  
    case _ => //Nothing
  }
}
