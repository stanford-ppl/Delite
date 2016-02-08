package ppl.delite.framework.analysis

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._

trait MultiArrayAnalyzerBase extends AnalyzerBase {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp
  import IR._

  override def forwardPropagateTP[A](e: Exp[A], d: Def[_])(implicit ctx: SourceContext): Unit = d match {
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
    case op@DeliteMultiArrayUpdate(ma,_,x) => setChild(ma, meet(UpdateAlias, getChild(ma), getProps(x)) )
    case op@DeliteMultiArrayInsert(ma,x,_) => setChild(ma, meet(UpdateAlias, getChild(ma), getProps(x)) )
    case op@DeliteMultiArrayAppend(ma,x) =>   setChild(ma, meet(UpdateAlias, getChild(ma), getProps(x)) )
    case op@DeliteMultiArrayInsertAll(ma,x,_,_) => setChild(ma, meet(UpdateAlias, getChild(ma), getChild(x)) )

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

    case _ => super.forwardPropagateTP(e,d)
  }

  override def tracerToProperties(t: AtomicTracer, child: Option[SymbolProperties]): Option[SymbolProperties] = t match {
    case MultiArrayTracer(_) => Some(ArrayProperties(child, NoData))
    case _ => super.tracerToProperties(t, child)
  }

  override def getAtomicWriteRHS(d: AtomicWrite[Any])(implicit ctx: SourceContext): Option[SymbolProperties] = d match {
    case DeliteMultiArrayUpdate(_,_,x) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteMultiArrayInsert(_,x,_) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteMultiArrayAppend(_,x) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteMultiArrayInsertAll(_,x,_,_) => Some(ArrayProperties(getChild(x), NoData))
    case DeliteMultiArrayRemove(_,_,_,_) => None
    case _ => super.getAtomicWriteRHS(d)
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
    def _incompatibilities(a: ImplType, b: ImplType)(implicit t: MeetFunc) = Nil
    def _canMeet(a: ImplType, b: ImplType)(implicit t: MeetFunc) = true
    def _meet(a: ImplType, b: ImplType)(implicit t: MeetFunc) = t match {
      case MetaOverwrite => b
      case _ => if (a != b) PhysType else b
    }
    def _isComplete(a: ImplType) = true
    def _makeString(a: ImplType, prefix: String) = a.toString
    def _multiLine(a: ImplType) = false
  }

  case class MayUpdate(mayUpdate: Boolean) extends Metadata { self =>
    override def makeString(prefix: String) = mayUpdate.toString
    def _matches(that: self.type) = this.mayUpdate == that.mayUpdate
    def _incompatibilities(that: self.type)(implicit t: MeetFunc) = Nil
    def _meet(that: self.type)(implicit t: MeetFunc) = MayUpdate(this.mayUpdate || that.mayUpdate)
  }

  case class MRank(rank: Int) extends Metadata { self =>
    override def makeString(prefix: String) = rank.toString
    def _matches(that: self.type) = this.rank == that.rank
    def _incompatibilities(that: self.type)(implicit t: MeetFunc) = {
      if (this.rank != that.rank) List(s"incompatible ranks (${this.rank} != ${that.rank})") else Nil
    }
    def _meet(that: self.type)(implicit t: MeetFunc) = MRank(this.rank)
  }
  case class MBuffer(impl: ImplType) extends Metadata { self =>
    override def makeString(prefix: String) = impl.toString
    def isTrueBuffer = (impl == TrueType)
    def isPhysBuffer = (impl == PhysType || impl == TrueType)
    def _matches(that: self.type) = matches(this.impl, that.impl)
    def _incompatibilities(that: self.type)(implicit t: MeetFunc) = Nil
    def _meet(that: self.type)(implicit t: MeetFunc) = t match {
      case MetaOverwrite => MBuffer(that.impl)
      case _ => MBuffer(meet(this.impl, that.impl))
    }
  }
  case class MView(impl: ImplType) extends Metadata { self =>
    override def makeString(prefix: String) = impl.toString
    def isTrueView = (impl == TrueType)
    def isPhysView = (impl == PhysType || impl == TrueType)
    def _matches(that: self.type) = matches(this.impl, that.impl)
    def _incompatibilities(that: self.type)(implicit t: MeetFunc) = Nil
    def _meet(that: self.type)(implicit t: MeetFunc) = t match {
      case MetaOverwrite => MView(that.impl)
      case _ => MView(meet(this.impl, that.impl))
    }
  }

  /*override def metadataMatches(a: Metadata, b: Metadata): Boolean = (a,b) match {
    case (MRank(r1), MRank(r2)) => (r1 == r2)
    case (MBuffer(i1), MBuffer(i2)) => matches(i1,i2)
    case (MView(i1), MView(i2)) => matches(i1,i2)
    case (MayUpdate(b1), MayUpdate(b2)) => (b1 == b2)
    case _ => super.metadataMatches(a,b)
  }
  override def metadataIncompatibilities(a: Metadata, b: Metadata, t: MeetFunc): List[String] = (a,b,t) match {
    case (MRank(_), MRank(_), MetaOverwrite) => Nil
    case (MRank(r1), MRank(r2), _)
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
  }*/
}

trait RankMetadataOps extends EffectExp with MetadataOps with RankMetadata {
  // Rank Analysis is run before MultiArray transformations - all preexisting arrays are rank 1
  override def defaultMetadata[A](tp: Manifest[A]): List[Metadata] = {
    if      (isSubtype(tp.erasure, classOf[DeliteArray[_]]))   List(MRank(1))
    else if (isSubtype(tp.erasure, classOf[DeliteArray1D[_]])) List(MRank(1))
    else if (isSubtype(tp.erasure, classOf[DeliteArray2D[_]])) List(MRank(2))
    else if (isSubtype(tp.erasure, classOf[DeliteArray3D[_]])) List(MRank(3))
    else if (isSubtype(tp.erasure, classOf[DeliteArray4D[_]])) List(MRank(4))
    else if (isSubtype(tp.erasure, classOf[DeliteArray5D[_]])) List(MRank(5))
    else Nil
  } ++ super.defaultMetadata(tp)

  def getRank(p: SymbolProperties): Option[MRank] = p[MRank] //p(classOf[MRank])
  def getView(p: SymbolProperties): Option[MView] = p[MView] //p(classOf[MView])
  def getBuffer(p: SymbolProperties): Option[MBuffer] = p[MBuffer] //p(classOf[MBuffer])

  def getRank(e: Exp[Any]) = meta[MRank](e) //getMetadata(e, classOf[MRank])
  def getView(e: Exp[Any]) = meta[MView](e) //getMetadata(e, classOf[MView])
  def getBuffer(e: Exp[Any]) = meta[MBuffer](e) //getMetadata(e, classOf[MBuffer])

  // Defaults for view and bufferable are both false
  def isPhysView(p: SymbolProperties) = getView(p).map{_.isPhysView}.getOrElse(false)
  def isPhysView(e: Exp[Any]) = getView(e).map{_.isPhysView}.getOrElse(false)
  def isPhysBuffer(p: SymbolProperties) = getBuffer(p).map{_.isPhysBuffer}.getOrElse(false)
  def isPhysBuffer(e: Exp[Any]) = getBuffer(e).map{_.isPhysBuffer}.getOrElse(false)

  def isTrueView(p: SymbolProperties) = getView(p).map{_.isTrueView}.getOrElse(false)
  def isTrueView(e: Exp[Any]) = getView(e).map{_.isTrueView}.getOrElse(false)
  def isTrueBuffer(p: SymbolProperties) = getBuffer(p).map{_.isTrueBuffer}.getOrElse(false)
  def isTrueBuffer(e: Exp[Any]) = getBuffer(e).map{_.isTrueBuffer}.getOrElse(false)

  def getMayUpdate(p: SymbolProperties) = p[MayUpdate] //p(classOf[MayUpdate])
  def getMayUpdate(e: Exp[Any]) = meta[MayUpdate](e)  //getMetadata(e, classOf[MayUpdate])
  def mayUpdate(p: SymbolProperties) = getMayUpdate(p).map{_.mayUpdate}.getOrElse(false)
  def mayUpdate(e: Exp[Any]) = getMayUpdate(e).map{_.mayUpdate}.getOrElse(false)

  // This will throw an exception if get returns None, but completeness assert after
  // rank analysis is run should guarantee that all MultiArray expressions have a rank
  def rank(e: Exp[Any]) = getRank(e).get.rank
  def rank(p: SymbolProperties) = getRank(p).get.rank

  def setRank(e: Exp[Any], rank: Int)(implicit ctx: SourceContext) { setMetadata(e, MRank(rank)) }
  def setRank(e: Exp[Any], rank: Option[MRank])(implicit ctx: SourceContext) { setMetadata(e, rank) }
  def setUpdated(e: Exp[Any])(implicit ctx: SourceContext) { setMetadata(e, MayUpdate(true)) }
  def enableBuffer(e: Exp[Any])(implicit ctx: SourceContext) {
    setMetadata(e, MBuffer(TrueType))
    setUpdated(e)
  }

  override def isComplete[T:Meetable](a: T) = a match {
    case ap: ArrayProperties => super.isComplete(a) && getRank(ap).isDefined
    case _ => super.isComplete(a)
  }
}

trait RankAnalyzer extends MultiArrayAnalyzerBase {
  val IR: DeliteOpsExp with DeliteMultiArrayOpsExp with RankMetadataOps
  import IR._
  override val name = "Rank Analyzer"
  override val debugMode = true

  override def hasCompleted = incomplete.isEmpty
  override def failedToConverge() {
    printerr("Maximum iterations exceeded before all ranks were fully known")
    printerr("Try increasing the maximum number of iterations")
  }

  override def completed(e: Exp[Any]) = !hasMultiArrayType(e.tp) || isComplete(getProps(e))

  override def processTP(e: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = d match {
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

    // --- Buffer rules
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayInsert(ma,x,i) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayAppend(ma,x) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayRemove(ma,axis,start,end) if isMutable(ma) => enableBuffer(ma)
    case DeliteMultiArrayUpdate(ma,_,_) => setUpdated(ma)

    case _ => super.processTP(e,d)
  }

  private def listMultiArrays() {
    println("\nComplete: ")
    // List metadata
    for ((k,v) <- metadata) {
      if (isMultiArrayType(k.tp) && !incomplete.contains(k)) {
        println(quoteTopPos(k))     // change to printdbg later
        println(quoteDef(k))
        println("sym: " + strDef(k))
        println(k + makeString(v))
        println("")
      }
    }
    println("\n")
  }

  override def postprocess[A:Manifest](b: Block[A]) = {
    incomplete = getIncompleteSyms(b)

    if (incomplete.nonEmpty) {
      println("Incomplete: ")
      for (s <- incomplete) {
        println(quoteTopPos(s) + ": Unable to statically determine the rank of " + s.tp + "\n\t" + quoteDef(s))
        println("sym: " + strDef(s))
        getProps(s) match {
          case Some(p) => println(s + makeString(p))
          case None => println(s + ": [None]")
        }
        println("")

        // Hack for setting all arrays which are unknown to be of arity 1
        def recursiveSet(sp: SymbolProperties): SymbolProperties = sp match {
          case p: ScalarProperties => p
          case p: StructProperties => StructProperties(p.children.map(recursiveSet), p.data)
          case p: ArrayProperties if getRank(p).isDefined => ArrayProperties(p.child.map(recursiveSet), p.data)
          case p: ArrayProperties =>
            // TODO: This is rather hacky - should have a better mechanism for adding metadata any level
            val newProps = meet(UpdateAlias, sp, ArrayProperties(None, PropMap(classOf[MRank], MRank(1))) ).asInstanceOf[ArrayProperties]
            ArrayProperties(newProps.child.map(recursiveSet), newProps.data)
        }
        setProps(s, recursiveSet(props(s)))
      }

      println("Unable to statically determine the ranks of some arrays! This can occur if the type 'MultiArray' is used " +
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

  override def processTP(e: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit = d match {
    case op@DeliteMultiArrayPermute(ma,config) =>
      assert(rank(ma) == config.length, "Number of dimensions given in permute conifguration must match input MultiArray rank")

    case op@DeliteMultiArrayPermuteView(ma,config) =>
      assert(rank(ma) == config.length, "Number of dimensions given in permuteView configuration must match input MultiArray rank")

    case op@DeliteMultiArrayReshapeView(ma,shape,unsafe) =>
      assert(!isTrueView(ma) || unsafe, "Creation of a reshaped view of a view is generally not possible. If you know your use case is sound, use the option UNSAFE = true. This will create a reshape view of the underlying data.")

    case DeliteMultiArrayZipWith(ma,mb,_) =>
      assert(rank(ma) == rank(mb), "Ranks in inputs to MultiArray ZipWith must match")

    // TODO: Check that rank of zero matches rank of child if child is multiarray
    case op@DeliteMultiArrayFold(ma,_,z) =>
      //assert(rank(ma) == rank(z), "Rank of input and zero must match in MultiArray Fold")
    case op@DeliteMultiArrayReduce(ma,_,z) =>
      // ...

    case op@DeliteMultiArrayNDMap(in,mdims,f) =>
      assert(rank(in) > rank(op.rV), "Inner rank of ND map must be less than input rank")
      assert(rank(op.rV) == rank(op.body.res), "Input and output rank must match in ND Map")
    case DeliteMultiArrayInsertAll(ma,rhs,axis,i) =>
      assert(isTrueBuffer(ma), "MultiArray must be bufferable for insert operation")
      assert(rank(ma) == rank(rhs) || rank(ma) == rank(rhs)+1, "Ranks of right hand size must be equal or one less than rank of rhs in insert all")
      assert(axis >= 0 && axis < rank(ma), "Insert axis must be non-negative and less than MultiArray's rank")
    case DeliteMultiArrayInsert(ma,x,i) =>
      assert(isTrueBuffer(ma), "MultiArray must be bufferable for insert operation")
      assert(rank(ma) == 1, "Element Insert is only defined on 1D arrays")
    case DeliteMultiArrayRemove(ma,axis,start,end) =>
      assert(isTrueBuffer(ma), "MultiArray must be bufferable for remove operation")
      assert(axis >= 0 && axis < rank(ma), "Removal axis must be non-negative and less than MultiArray's rank")

    case DeliteMultiArrayMkString(ma,dels,_) =>
      assert(rank(ma) == dels.length, "Number of delimeters given to MkString (" + dels.length + ") must match input MultiArray's rank (" + rank(ma) + ")")
    case DeliteMultiArrayWriteFile(ma,dels,_,_) =>
      assert(rank(ma) == dels.length, "Number of delimeters given to WriteFile (" + dels.length + ") must match input MultiArray's rank (" + rank(ma) + ")")

    case DeliteMultiArrayMapFilter(ma,_,_) =>
      assert(rank(ma) == 1, "MapFilter is undefined for " + rank(ma) + "D arrays")
    case DeliteMultiArrayFlatMap(ma,f) =>
      assert(rank(ma) == 1, "FlatMap is undefined for " + rank(ma) + "D arrays")

    case DeliteMatrixMultiply(lhs,rhs,_) =>
      assert(rank(lhs) == 2, "Left hand side of matrix multiply must be a 2D array")
      assert(rank(rhs) == 2, "Right hand side of matrix multiply must be a 2D array")
    case DeliteMatrixVectorMultiply(mat,vec,_) =>
      assert(rank(mat) == 2, "Matrix argument to matrix-vector multiply must be a 2D array")
      assert(rank(vec) == 1, "Vector argument to matrix-vector multiply must be a 1D array")

    case _ => //Nothing
  }
}