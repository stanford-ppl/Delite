package ppl.delite.framework.analysis

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.Meetable._

import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._

trait MultiArrayAnalyzerBase extends AnalyzerBase { 
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with MetadataOps
  import IR._

  override def forwardPropagate[A](e: Exp[A], d: Def[_])(implicit ctx: SourceContext): Unit = d match {
    case op@DeliteMultiArrayView(t,_,_,_,_) => setChild(e, getChild(t))
    case op@DeliteMultiArrayPermute(ma,_)   => setChild(e, getChild(ma))
    case op@DeliteMultiArrayReshape(ma,_)   => setChild(e, getChild(ma))
    case op@DeliteMultiArrayPermuteView(ma,_) => setChild(e, getChild(ma))
    case op@DeliteMultiArrayReshapeView(ma,_,_) => setChild(e, getChild(ma))
    case op@DeliteMultiArrayApply(ma,_)     => setProps(e, getChild(ma))
    
    case op@DeliteMultiArrayReadFile(_,_,_)      => setChild(e, getProps(op.body))
    case op@DeliteMultiArrayFromFunction(dims,_) => setChild(e, getProps(op.body))
    case op@DeliteMultiArrayMap(ma,_)            => setChild(e, getProps(op.body))
    case op@DeliteMultiArrayZipWith(ma,mb,_)     => setChild(e, getProps(op.body))
    case op@DeliteMultiArrayMapFilter(ma,_,_)    => setChild(e, getProps(op.mapFunc))
    case op@DeliteMultiArrayFlatMap(ma,_)        => setChild(e, getChild(op.body))

    case op@DeliteMultiArrayGroupBy(ma,_,_) => 
      setField(e, ArrayProperties(getProps(op.keyFunc),NoData), "keys")
      setField(e, ArrayProperties(Some(ArrayProperties(getProps(op.valFunc),NoData)),NoData), "vals")

    // TODO: Is there a way to avoid having to manually propagate to rVs here?
    case op@DeliteMultiArrayGroupByReduce(ma,_,_,_) => 
      setField(e, ArrayProperties(getProps(op.keyFunc),NoData), "keys")
      setField(e, ArrayProperties(getProps(op.redFunc),NoData), "vals")
      setProps(op.rV._1, getProps(op.valFunc))
      setProps(op.rV._2, getProps(op.valFunc))

    case op@BlockAssemble(idims,bfs,odims, _,_,_,_) =>


      setProps(op.rV._1, getProps(op.tile))
      setProps(op.rV._2, getProps(op.tile))

    case op@DeliteMultiArrayFilterReduce(ma,_,_,_,_) => 
      setChild(op.rV._1, getChild(ma))
      setChild(op.rV._2, getChild(ma))
      setProps(e, getProps(op.redFunc))

    case op@DeliteMultiArrayReduce(ma,_,_) =>
      setProps(op.rV._1, getChild(ma))
      setProps(op.rV._2, getChild(ma)) 
      setProps(e, getProps(op.body))
    case op@DeliteMultiArrayFold(ma,_,_) =>
      setProps(op.rV._1, getChild(ma))
      setProps(op.rV._2, getChild(ma))
      setProps(e, getProps(op.body))
    case op@DeliteMultiArrayNDMap(in,_,_) =>
      setChild(op.rV, getChild(in))
      setChild(e, getChild(op.body))
    
    // Aliasing from updates/inserts/etc
    case op@DeliteMultiArrayUpdate(ma,_,x) => 
      setChild(ma, attemptMeet(getChild(ma), getProps(x), func = MetaUpdate))
    case op@DeliteMultiArrayInsert(ma,x,_) =>
      setChild(ma, attemptMeet(getChild(ma), getProps(x), func = MetaUpdate))
    case op@DeliteMultiArrayAppend(ma,x) =>
      setChild(ma, attemptMeet(getChild(ma), getProps(x), func = MetaUpdate))
    case op@DeliteMultiArrayInsertAll(ma,x,_,_) =>
      setChild(ma, attemptMeet(getChild(ma), getChild(x), func = MetaUpdate))

    case op@DeliteMultiMapGet(dm,_) => 
      setProps(e, getField(dm, "vals").flatMap{getChild(_)})
    case op@DeliteMultiMapSize(dm) =>
      setProps(e, getField(dm, "size"))
    case op@DeliteMultiMapKeys(dm) =>
      setProps(e, getField(dm, "keys"))
    case op@DeliteMultiMapVals(dm) =>
      setProps(e, getField(dm, "vals"))
    case op@DeliteMultiMapFromArrays(k,v) =>
      setField(e, getProps(k), "keys")
      setField(e, getProps(v), "vals")

    case _ => super.forwardPropagate(e,d)
  }
  
  override def tracerToProperties(t: AtomicTracer, child: Option[SymbolProperties]): Option[SymbolProperties] = t match {
    case MultiArrayTracer(_) => Some(ArrayProperties(child, NoData))
    case _ => super.tracerToProperties(t, child)
  }

  override def getAtomicWriteRhs(d: AtomicWrite[Any])(implicit ctx: SourceContext): Option[SymbolProperties] = d match {
    case DeliteMultiArrayUpdate(_,_,x) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteMultiArrayInsert(_,x,_) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteMultiArrayAppend(_,x) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteMultiArrayInsertAll(_,x,_,_) => Some(ArrayProperties(getChild(x), NoData))
    case DeliteMultiArrayRemove(_,_,_,_) => None
    case _ => super.getAtomicWriteRhs(d)
  }
}

trait RankMetadata extends SymbolMetadata {
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

  case class MayUpdate(mayUpdate: Boolean) extends Metadata {
    def name = "mayUpdate"
    override def makeString(prefix: String) = mayUpdate.toString
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
    case (MayUpdate(b1), MayUpdate(b2)) => (b1 == b2)
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
    case (MayUpdate(_), MayUpdate(_), _) => true
    case _ => super.canMeetMetadata(a,b,t)
  }
  override def meetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Metadata = (a,b,t) match {
    case (MRank(_), MRank(r2), MetaOverwrite) => MRank(r2)
    case (MRank(r1), MRank(r2), _) if r1 == r2 => MRank(r2)
    case (MBuffer(_), MBuffer(i2), MetaOverwrite) => MBuffer(i2)
    case (MBuffer(i1), MBuffer(i2), t) => MBuffer(meet(i1,i2,t))
    case (MView(_), MView(i2), MetaOverwrite) => MView(i2)
    case (MView(i1), MView(i2), t) => MView(meet(i1,i2,t))
    case (MayUpdate(b1), MayUpdate(b2), _) => MayUpdate(b1 || b2)
    case _ => super.meetMetadata(a,b,t)
  }
}

trait RankMetadataOps extends EffectExp with MetadataOps with RankMetadata {
  // Rank Analysis is run before MultiArray transformations - all preexisting arrays are rank 1
  override def defaultTypeMetadata[A](tp: Manifest[A]): List[Metadata] = {
    if      (isSubtype(tp.erasure, classOf[DeliteArray[_]]))   List(MRank(1))
    else if (isSubtype(tp.erasure, classOf[DeliteArray1D[_]])) List(MRank(1))
    else if (isSubtype(tp.erasure, classOf[DeliteArray2D[_]])) List(MRank(2))
    else if (isSubtype(tp.erasure, classOf[DeliteArray3D[_]])) List(MRank(3))
    else if (isSubtype(tp.erasure, classOf[DeliteArray4D[_]])) List(MRank(4))
    else if (isSubtype(tp.erasure, classOf[DeliteArray5D[_]])) List(MRank(5))
    else Nil
  } ++ super.defaultTypeMetadata(tp)

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

  def getMayUpdate(p: SymbolProperties) = p("mayUpdate").map{_.asInstanceOf[MayUpdate]}
  def getMayUpdate(e: Exp[Any]) = getMetadata(e, "mayUpdate").map{_.asInstanceOf[MayUpdate]}
  def mayUpdate(p: SymbolProperties) = getMayUpdate(p).map{_.mayUpdate}.getOrElse(false)
  def mayUpdate(e: Exp[Any]) = getMayUpdate(e).map{_.mayUpdate}.getOrElse(false)

  // This will throw an exception if get returns None, but completeness check after 
  // rank analysis is run should guarantee that all MultiArray expressions have a rank
  def rank(e: Exp[Any]) = extractMetadata(e, "rank").asInstanceOf[MRank].rank
  def rank(p: SymbolProperties) = extractMetadata(p, "rank").asInstanceOf[MRank].rank
}

trait RankAnalyzer extends MultiArrayAnalyzerBase {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with RankMetadataOps
  import IR._
  override val name = "Rank Analyzer"
  
  var incomplete: List[Exp[Any]] = Nil

  override def hasCompleted = incomplete.isEmpty
  override def failedToConverge() { 
    cwarn("Maximum iterations exceeded before all ranks were fully known")
    cwarn("Try increasing the maximum number of iterations")
  }

  override def completed(e: Exp[Any]) = { 
    !hasMultiArrayTpe(e.tp) || (getProps(e) match {
      case Some(a) => isComplete(a) && getRank(a).isDefined
      case _ => false 
    })
  }

  def setRank(e: Exp[Any], rank: Int)(implicit ctx: SourceContext) { setMetadata(e, MRank(rank)) }
  def setRank(e: Exp[Any], rank: Option[MRank])(implicit ctx: SourceContext) { setMetadata(e, rank) }
  def setUpdated(e: Exp[Any])(implicit ctx: SourceContext) { setMetadata(e, MayUpdate(true)) }
  def enableBuffer(e: Exp[Any])(implicit ctx: SourceContext) { 
    setMetadata(e, MBuffer(TrueType)) 
    setUpdated(e)
  }

  override def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: SourceContext): Unit = d match {
    // --- Rank rules for specific IR nodes
    case DeliteMultiArrayNew(dims)            => setRank(e, dims.length)
    case DeliteMultiArrayPermute(ma,_)        => setRank(e, getRank(ma))   
    case DeliteMultiArrayReshape(_,dims)      => setRank(e, dims.length)
    
    case DeliteMultiArrayReadFile(_,dels,_)   => setRank(e, dels.length)
    case DeliteMultiArrayFromFunction(dims,_) => setRank(e, dims.length)
    case DeliteMultiArrayMap(ma,_)            => setRank(e, getRank(ma))
    case DeliteMultiArrayZipWith(ma,_,_)      => setRank(e, getRank(ma))  

    case DeliteMultiArrayView(t,_,_,dims,_) => 
      setRank(e, dims.length)
      if (isPhysBuffer(t)) setMetadata(e, MBuffer(PhysType))
      setMetadata(e, MView(TrueType))

    // PermuteView and ReshapeView are "PhysType" views, since they create
    // a view of all of the underlying data
    case DeliteMultiArrayPermuteView(ma,_) => 
      setRank(e, getRank(ma))
      if (isPhysBuffer(ma)) setMetadata(e, MBuffer(PhysType))
      setMetadata(e, MView(PhysType))
    case DeliteMultiArrayReshapeView(ma,dims,_) => 
      setRank(e, dims.length)
      if (isPhysBuffer(ma)) setMetadata(e, MBuffer(PhysType))
      setMetadata(e, MView(PhysType))

    case op@DeliteMultiArrayNDMap(in,mdims,_) => 
      setRank(op.rV, mdims.length)
      setRank(e, getRank(in))

    case op@DeliteMultiArrayFilterReduce(ma,_,_,_,_) =>
      setRank(op.rV._1, getRank(ma).map{r => MRank(r.rank - 1) })
      setRank(op.rV._2, getRank(ma).map{r => MRank(r.rank - 1) })
      setMetadata(op.rV._1, MView(TrueType))
      setMetadata(op.rV._2, MView(TrueType))
      setRank(e, getRank(ma).map{r => MRank(r.rank - 1) })

    // --- Buffer rules
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayInsert(ma,x,i) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayAppend(ma,x) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayRemove(ma,axis,start,end) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayUpdate(ma,_,_) => setUpdated(ma)
 
    case _ => // Nothing
  } 

  private def listMultiArrays() {
    printmsg("\nComplete: ")
    // List metadata
    for ((k,v) <- symbolData) {
      if (isMultiArrayTpe(k.tp)) {
        printmsg(quotePos(k.pos))     // change to printdbg later
        printmsg("sym: " + strDef(k))
        printmsg(k + makeString(v))
        printmsg("")
      }
    }
    printmsg("\n")
  }

  override def postprocess[A:Manifest](b: Block[A]) = { 
    incomplete = checkCompleteness(b) 
    
    if (incomplete.nonEmpty) {
      printmsg("Incomplete: ")
      for (s <- incomplete) {
        cwarn(quotePos(s.pos) + ": Unable to statically determine the rank of " + s.tp + "\n\t" + quoteCode(s.pos).getOrElse(strDef(s)))
        printmsg(quotePos(s.pos))
        printmsg("sym: " + strDef(s))
        getProps(s) match {
          case Some(p) => printmsg(s + makeString(p))
          case None => printmsg(s + ": [None]")
        }
        printmsg("")
        setMetadata(s, MRank(1))(mpos(s.pos))
      } 

      cwarn("Unable to statically determine the ranks of some arrays! This can occur if the type 'MultiArray' is used " + 
           "as an inner data structure type without element initialization. Proceeding assuming unknown ranks are 1D.")
      // Try analysis again with assumption that unknowns are of rank 1
      resume()
    }
    if (debugMode) listMultiArrays()
    super.postprocess(b)
  }
}

// MultiArray Sanity Checking
trait RankChecker extends MultiArrayAnalyzerBase {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with RankMetadataOps
  import IR._
  override val name = "Rank Sanity Check"

  override val autopropagate = false

  def processOp[A](e: Exp[A], d: Def[_])(implicit ctx: SourceContext): Unit = d match {
    case op@DeliteMultiArrayPermute(ma,config) =>
      check(rank(ma) == config.length, "Number of dimensions given in permute conifguration must match input MultiArray rank")

    case op@DeliteMultiArrayPermuteView(ma,config) => 
      check(rank(ma) == config.length, "Number of dimensions given in permuteView configuration must match input MultiArray rank")

    case op@DeliteMultiArrayReshapeView(ma,shape,unsafe) =>
      check(!isTrueView(ma) || unsafe, "Creation of a reshaped view of a view is generally not possible. If you know your use case is sound, use the option UNSAFE = true. This will create a reshape view of the underlying data.")

    case DeliteMultiArrayZipWith(ma,mb,_) =>
      check(rank(ma) == rank(mb), "Ranks in inputs to MultiArray ZipWith must match")

    // TODO: Check that rank of zero matches rank of child if child is multiarray
    case op@DeliteMultiArrayFold(ma,_,z) => 
      //check(rank(ma) == rank(z), "Rank of input and zero must match in MultiArray Fold")
    case op@DeliteMultiArrayReduce(ma,_,z) => 
      // ...
    case op@DeliteMultiArrayFilterReduce(ma,zero,_,_,axis) => 
      //check(rank(ma) == rank(zero) + 1, "Rank of zero must be one less than rank of input to FilterReduce")
      //check(rank(ma) == rank(op.redFunc.res) + 1, "Rank of result of reduction must be one less than rank of input to FilterReduce")
      check(axis >= 0 && axis < rank(ma), "Reduction axis must be non-negative and less than MultiArray's rank")
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

    case DeliteMultiArrayMkString(ma,dels,_) =>
      check(rank(ma) == dels.length, "Number of delimeters given to MkString (" + dels.length + ") must match input MultiArray's rank (" + rank(ma) + ")")
    case DeliteMultiArrayWriteFile(ma,dels,_,_) => 
      check(rank(ma) == dels.length, "Number of delimeters given to WriteFile (" + dels.length + ") must match input MultiArray's rank (" + rank(ma) + ")")

    case DeliteMultiArrayMapFilter(ma,_,_) => 
      check(rank(ma) == 1, "MapFilter is undefined for " + rank(ma) + "D arrays")
    case DeliteMultiArrayFlatMap(ma,f) => 
      check(rank(ma) == 1, "FlatMap is undefined for " + rank(ma) + "D arrays")     

    case DeliteMatrixMultiply(lhs,rhs,_) =>
      check(rank(lhs) == 2, "Left hand side of matrix multiply must be a 2D array")
      check(rank(rhs) == 2, "Right hand side of matrix multiply must be a 2D array")
    case DeliteMatrixVectorMultiply(mat,vec,_) =>
      check(rank(mat) == 2, "Matrix argument to matrix-vector multiply must be a 2D array")
      check(rank(vec) == 1, "Vector argument to matrix-vector multiply must be a 1D array")
  
    case _ => //Nothing
  }
}
